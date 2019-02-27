&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

DEFINE TEMP-TABLE ttReportMaster
    FIELD Hotkey AS CHAR
    FIELD ProgramOrig AS CHAR
    FIELD ProgramOld AS CHAR
    FIELD ProgramNew AS CHAR.


DEFINE STREAM initRpts-log.

DEFINE VARIABLE gcReportHotkeyList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcReportProgramListOrig AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcReportProgramListOld AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcReportProgramListNew AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttReportMaster

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 ttReportMaster.Hotkey ttReportMaster.ProgramOrig ttReportMaster.ProgramOld ttReportMaster.ProgramNew   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH ttReportMaster
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH ttReportMaster.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 ttReportMaster
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 ttReportMaster


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 BROWSE-3 btn-process btn-cancel 

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

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 8.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      ttReportMaster SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _FREEFORM
  QUERY BROWSE-3 DISPLAY
      ttReportMaster.Hotkey FORMAT "x(5)" COLUMN-LABEL "Hotkey"
    ttReportMaster.ProgramOrig FORMAT "x(20)" COLUMN-LABEL "Original Program"
    ttReportMaster.ProgramOld  FORMAT "x(20)" COLUMN-LABEL "Old Report Program"
    ttReportMaster.ProgramNew  FORMAT "x(20)" COLUMN-LABEL "New Report Program"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 6.43 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-3 AT ROW 5.76 COL 3 WIDGET-ID 100
     btn-process AT ROW 12.91 COL 20
     btn-cancel AT ROW 12.91 COL 50
     "Reports to Process:" VIEW-AS TEXT
          SIZE 31 BY .71 AT ROW 4.91 COL 3 WIDGET-ID 6
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 13.71.

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
         TITLE              = "Initialize REPORTS NK1"
         HEIGHT             = 13.81
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


/* BROWSE-TAB BROWSE-3 RECT-17 FRAME-A */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttReportMaster.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Initialize REPORTS NK1 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Initialize REPORTS NK1 */
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
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
          " for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

  IF ll THEN RUN RunProcess.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
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
  RUN InitializeLists.
  RUN enable_UI.

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
  ENABLE RECT-17 BROWSE-3 btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeLists C-Win 
PROCEDURE InitializeLists :
/*------------------------------------------------------------------------------
  Purpose:    Fill the global character variables with comman separated values.
            Display them in the list on the window.  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iEntry AS INTEGER     NO-UNDO.

gcReportHotkeyList = 
    "IC1," +    /*1*/
    "AR12," +   /*2*/
    "PR1," +    /*3*/
    "IL6," +    /*4*/
    "IR1," +    /*5*/
    "IC3," +    /*6*/
    "IL8," +    /*7*/
    "HL," +     /*8*/
    "HR5," +    /*9*/
    "IR9," +    /*10*/
    "OR5," +    /*11*/
    "OR2," +    /*12*/
    "MR8," +    /*13*/
    "OR11," +   /*14*/
    "MR3," +    /*15*/
    "OZ1," +    /*16*/
    "IR15," +   /*17*/
    "IR2,"  +   /*18*/
    "".  
gcReportProgramListOrig = 
    "r-cycnt.," +    /*1*/
    "r-custt.," +   /*2*/
    "r-sonord.," +    /*3*/
    "r-fgpstr.," +    /*4*/
    "r-fgrord.," +    /*5*/
    "r-phce&p.," +    /*6*/
    "r-stajob.," +    /*7*/
    "r-lastvs.," +     /*8*/
    "r-itcshp.," +    /*9*/
    "r-itmlst.," +    /*10*/
    "r-booked.," +    /*11*/
    "r-sched.," +    /*12*/
    "r-inkmch.," +    /*13*/
    "r-booko#.," +   /*14*/
    "r-ibtag.," +    /*15*/
    "r-hots.," +    /*16*/
    "r-agewip.," +   /*17*/
    "r-fgohbb.,"  +   /*18*/
    "".  
gcReportProgramListOld = 
    "r-cycntA.," +    /*1*/
    "r-custtA.," +   /*2*/
    "r-sonordA.," +    /*3*/
    "r-fgpstrA.," +    /*4*/
    "r-fgrordA.," +    /*5*/
    "r-phce&pA.," +    /*6*/
    "r-stajobA.," +    /*7*/
    "r-lastvsA.," +     /*8*/
    "r-itcshA.," +    /*9*/
    "r-itmlstA.," +    /*10*/
    "r-bookedA.," +    /*11*/
    "r-schedA.," +    /*12*/
    "r-inkmcA.," +    /*13*/
    "r-booko#A.," +   /*14*/
    "r-ibtagA.," +    /*15*/
    "r-hotsA.," +    /*16*/
    "IR15-NA," +   /*17*/
    "r-fgohbA.,"  +   /*18*/
    "".  

gcReportProgramListNew = 
    "r-cycntN.," +    /*1*/
    "r-custtN.," +   /*2*/
    "r-sonordN.," +    /*3*/
    "r-fgpstrN.," +    /*4*/
    "r-fgrordN.," +    /*5*/
    "r-phce&pN.," +    /*6*/
    "r-stajobN.," +    /*7*/
    "r-lastvsN.," +     /*8*/
    "r-itcshN.," +    /*9*/
    "r-itmlstN.," +    /*10*/
    "r-bookedN.," +    /*11*/
    "r-schedN.," +    /*12*/
    "r-inkmcN.," +    /*13*/
    "r-booko#N.," +   /*14*/
    "r-ibtagN.," +    /*15*/
    "r-hotsN.," +    /*16*/
    "r-agewipN.," +   /*17*/
    "r-fgohbN.,"  +   /*18*/
    "".  

/*trim last comma*/
gcReportHotkeyList = TRIM(gcReportHotkeyList,",").
gcReportProgramListOrig = TRIM(gcReportProgramListOrig,",").
gcReportProgramListNew = TRIM(gcReportProgramListNew,",").

/*Build Report List*/
DO iEntry = 1 TO NUM-ENTRIES(gcReportHotkeyList):
    CREATE ttReportMaster.
    ASSIGN 
        ttReportMaster.Hotkey = ENTRY(iEntry,gcReportHotkeyList)
        ttReportMaster.ProgramOrig = ENTRY(iEntry,gcReportProgramListOrig)
        ttReportMaster.ProgramOld = ENTRY(iEntry,gcReportProgramListOld)
        ttReportMaster.ProgramNew = ENTRY(iEntry,gcReportProgramListNew).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcess C-Win 
PROCEDURE RunProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-sys-ctrl FOR sys-ctrl.
DEFINE BUFFER bf-sys-ctrl-shipto FOR sys-ctrl-shipto.
DEFINE BUFFER bf-user-print FOR user-print.
DEFINE BUFFER bf-user-printA FOR user-print.
DEFINE BUFFER bf-user-printN FOR user-print.

DEFINE VARIABLE iEntry  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cHotkey AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrgOrg AS CHARACTER FORMAT "x(20)"  NO-UNDO.
DEFINE VARIABLE cPrgOld AS CHARACTER FORMAT "x(20)"  NO-UNDO.
DEFINE VARIABLE cPrgNew AS CHARACTER FORMAT "x(20)"  NO-UNDO.

SESSION:SET-WAIT-STATE("general").
DISABLE TRIGGERS FOR LOAD OF bf-user-print.
DISABLE TRIGGERS FOR LOAD OF bf-sys-ctrl-shipto.

OUTPUT STREAM initRpts-log TO VALUE("C:\tmp\initRpts-log" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".log") APPEND.

FOR EACH bf-sys-ctrl 
    WHERE bf-sys-ctrl.NAME EQ "REPORTS" 
    NO-LOCK:
    FOR EACH ttReportMaster:
        ASSIGN 
            cHotkey = ttReportMaster.Hotkey
            cPrgOrg = ttReportMaster.ProgramOrig
            cPrgOld = ttReportMaster.ProgramOld
            cPrgNew = ttReportMaster.ProgramNew.

        /*initialize NK1s*/
        FIND FIRST bf-sys-ctrl-shipto OF bf-sys-ctrl 
            WHERE bf-sys-ctrl-shipto.char-fld EQ cHotkey NO-LOCK NO-ERROR.
        IF NOT AVAIL bf-sys-ctrl-shipto THEN DO:
            CREATE bf-sys-ctrl-shipto.
            ASSIGN 
                bf-sys-ctrl-shipto.company = bf-sys-ctrl.company
                bf-sys-ctrl-shipto.NAME = bf-sys-ctrl.NAME
                bf-sys-ctrl-shipto.log-fld = bf-sys-ctrl.log-fld
                bf-sys-ctrl-shipto.char-fld = cHotkey
                bf-sys-ctrl-shipto.descrip = "Selectable Report Columns".
            PUT STREAM initRpts-log "Create NK1: " cHotkey FORMAT "X(5)" " for company " bf-sys-ctrl-shipto.company SKIP.
        END.

        /*copy old userprint into new userprint (one for old report program and one for new report program)*/
        FOR EACH bf-user-print 
            WHERE bf-user-print.company EQ bf-sys-ctrl.company
              AND bf-user-print.program-id EQ cPrgOrg
            NO-LOCK:
            FIND FIRST bf-user-printA 
                WHERE bf-user-printA.company EQ bf-user-print.company
                  AND bf-user-printA.program-id EQ cPrgOld
                  AND bf-user-printA.USER-ID EQ bf-user-print.USER-ID
                NO-LOCK NO-ERROR.
            IF NOT AVAIL bf-user-printA THEN DO:
                PUT STREAM initRpts-log "Create Old: " cPrgOld FORMAT "X(20)" " for " bf-user-print.USER-ID SKIP.
                CREATE bf-user-printA.
                BUFFER-COPY bf-user-print EXCEPT rec_key TO bf-user-printA.
                bf-user-printA.program-id = cPrgOld.
            END.
            ELSE
                PUT STREAM initRpts-log cPrgOld FORMAT "X(20)" " already exists for " bf-user-print.USER-ID "(" bf-user-print.company ")" SKIP.
            FIND FIRST bf-user-printN 
                WHERE bf-user-printN.company EQ bf-user-print.company
                  AND bf-user-printN.program-id EQ cPrgNew
                  AND bf-user-printN.USER-ID EQ bf-user-print.USER-ID
                NO-LOCK NO-ERROR.
            IF NOT AVAIL bf-user-printN THEN DO:
                PUT STREAM initRpts-log "Create New: " cPrgNew  FORMAT "X(20)" " for " bf-user-print.USER-ID SKIP.
                CREATE bf-user-printN.
                BUFFER-COPY bf-user-print EXCEPT rec_key TO bf-user-printN.
                bf-user-printN.program-id = cPrgNew.
            END.
            ELSE
                PUT STREAM initRpts-log cPrgNew FORMAT "X(20)" " already exists for " bf-user-print.USER-ID "(" bf-user-print.company ")" SKIP.
        END. /*each bf-user-print*/
    END. /* each ttReportMaster*/    
END. /* each bf-sys-ctrl = REPORTS*/

OUTPUT STREAM initRpts-log CLOSE.
STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

