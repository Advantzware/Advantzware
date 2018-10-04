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
/* {custom/globdefs.i}        */
/*                            */
/* {sys/inc/var.i NEW SHARED} */
/*                            */
/* ASSIGN                     */
/*  cocode = g_company        */
/*  locode = g_loc.           */

DEFINE STREAM oe-prmtx-bak.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust-no end_cust-no ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvertDateToINo C-Win 
FUNCTION ConvertDateToINo RETURNS CHARACTER
  ( ipdtDate AS DATE  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvertINoToDate C-Win 
FUNCTION ConvertINoToDate RETURNS DATE
  ( ipcINoDate AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 6.24 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 6.24 COL 57 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 2
     btn-process AT ROW 9.81 COL 22
     btn-cancel AT ROW 9.81 COL 52
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 10.81.

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
         TITLE              = "Convert Price Matrix Effective Dates"
         HEIGHT             = 11
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


ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Convert Price Matrix Effective Dates */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Convert Price Matrix Effective Dates */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* From Customer# */
DO:
   assign {&self-name}.
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
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

  IF ll THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* To Customer# */
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

  RUN enable_UI.

  APPLY "entry" TO begin_cust-no.

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
  DISPLAY begin_cust-no end_cust-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_cust-no end_cust-no btn-process btn-cancel 
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
DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
DEFINE BUFFER bf-reftable FOR reftable.
DEFINE BUFFER bf-company FOR company.

DEFINE VARIABLE iCounter AS INT NO-UNDO.
DEFINE VARIABLE dtEffectiveDate LIKE oe-prmtx.eff-date NO-UNDO.
DEFINE VARIABLE cRefTableDate AS CHAR NO-UNDO.
DEFINE VARIABLE cINoDate AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE("general").
DISABLE TRIGGERS FOR LOAD OF bf-oe-prmtx.

OUTPUT STREAM oe-prmtx-bak TO VALUE("C:\tmp\oe-prmtx-bak" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".d") APPEND.

FOR EACH bf-company,
    EACH bf-oe-prmtx
    WHERE bf-oe-prmtx.company EQ bf-company.company
      AND bf-oe-prmtx.cust-no GE begin_cust-no
      AND bf-oe-prmtx.cust-no LE end_cust-no
    EXCLUSIVE-LOCK:

    EXPORT STREAM oe-prmtx-bak bf-oe-prmtx.

    FIND FIRST bf-reftable
        WHERE bf-reftable.rec_key  EQ bf-oe-prmtx.rec_key
          AND bf-reftable.company  EQ "oe-prmtx"
        USE-INDEX rec_key 
       EXCLUSIVE-LOCK NO-ERROR.
    IF  LENGTH(bf-oe-prmtx.i-no) EQ 108 THEN
        cINoDate = SUBSTRING(bf-oe-prmtx.i-no,101,8).
    ELSE
        cINoDate = "".
    cRefTableDate = IF AVAILABLE bf-reftable THEN bf-reftable.CODE ELSE "".                                                                        
    dtEffectiveDate = bf-oe-prmtx.eff-date.
/*     IF NOT AVAIL bf-reftable THEN DO:                 */
/*         CREATE bf-reftable.                           */
/*         ASSIGN                                        */
/*             bf-reftable.rec_key = bf-oe-prmtx.rec_key */
/*             bf-reftable.company  = "oe-prmtx".        */
/*     END.                                              */
    IF dtEffectiveDate GE 1/1/1900 THEN DO:
        cRefTableDate = STRING(dtEffectiveDate,"99/99/9999").
        cINoDate = ConvertDateToINo(dtEffectiveDate).
    END.
    ELSE DO:
        IF (cRefTableDate EQ "" OR cRefTableDate EQ "01/01/001") AND CINoDate NE "" THEN DO:
            dtEffectiveDate = ConvertINoToDate(cINoDate).
            cRefTableDate = STRING(dtEffectiveDate).
        END.
        ELSE IF (cRefTableDate NE "" AND cRefTableDate NE "01/01/001") AND cINoDate EQ "" THEN DO:
            dtEffectiveDate = DATE(cRefTableDate).
            cINoDate = ConvertDateToIno(dtEffectiveDate).
        END.
        ELSE IF (cRefTableDate NE "" AND cRefTableDate NE "01/01/001") AND cINoDate NE ""  THEN DO:
            dtEffectiveDate = ConvertINoToDate(cINoDate).
            cRefTableDate = STRING(dtEffectiveDate).
        END.
        ELSE IF bf-oe-prmtx.eff-date GT 01/01/1900 THEN DO:
            dtEffectiveDate = bf-oe-prmtx.eff-date.
            cRefTableDate = STRING(dtEffectiveDate,"99/99/99").
            cINoDate = ConvertDateToIno(dtEffectiveDate).
        END.
        ELSE DO:
            cRefTableDate = STRING(TODAY,"99/99/99").
            dtEffectiveDate = TODAY.
            cINoDate = ConvertDateToIno(TODAY).
        END.
    END.
    bf-oe-prmtx.eff-date = dtEffectiveDate.
    IF AVAIL bf-reftable THEN bf-reftable.CODE = cRefTableDate.
    bf-oe-prmtx.i-no = TRIM(SUBSTRING(bf-oe-prmtx.i-no,1,15)).

END.

OUTPUT STREAM oe-prmtx-bak CLOSE.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvertDateToINo C-Win 
FUNCTION ConvertDateToINo RETURNS CHARACTER
  ( ipdtDate AS DATE  ) :
/*------------------------------------------------------------------------------
  Purpose:  Takes date and converts to the "YYYYMMDD" format
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConverted AS CHARACTER   NO-UNDO.

    cConverted = STRING(ipdtDate,"99999999").
    cConverted = SUBSTRING(cConverted,5,4) + 
                 SUBSTRING(cConverted,1,2) + 
                 SUBSTRING(cConverted,3,2).
    RETURN cConverted.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvertINoToDate C-Win 
FUNCTION ConvertINoToDate RETURNS DATE
  ( ipcINoDate AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Takes the "YYYYMMDD" format and converts to real date
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtConverted AS DATE    NO-UNDO.
    DEFINE VARIABLE cConverted AS CHARACTER   NO-UNDO.

    IF LENGTH(ipcINoDate) = 8 THEN DO:
        cConverted =  
            SUBSTRING(ipcINoDate,5,2) + "/" + 
            SUBSTRING(ipcINODate,7,2) + "/" +
            SUBSTRING(ipcINoDate,1,4).
        dtConverted = DATE(cConverted).
    END.
    IF dtConverted EQ ? THEN dtConverted = TODAY.
    RETURN dtConverted .  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

