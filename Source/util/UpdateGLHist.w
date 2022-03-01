&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/UpdateGLHist.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:  Sachin Chahal

  Created: 13  Jan, 2022

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cocode        AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
     
RUN spGetSessionParam (
    INPUT "Company", 
    OUTPUT cocode
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit RECT-15 btSimulatePurge RECT-16 ~
run-no new-date fi_file btStartProcess tb_OpenCSV 
&Scoped-Define DISPLAYED-OBJECTS run-no current-date current-period ~
current-year new-date new-period new-year fi_file tb_OpenCSV 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 7.2 BY 1.67.

DEFINE BUTTON btSimulatePurge 
     IMAGE-UP FILE "Graphics/32x32/simulate.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Simulate" 
     SIZE 19 BY 1.14
     BGCOLOR 14 .

DEFINE BUTTON btStartProcess 
     IMAGE-UP FILE "Graphics/32x32/execute.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Start Process" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE current-date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Current Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE current-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Current Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE current-year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Current Year" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\UpdateGLHist.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52 BY 1.

DEFINE VARIABLE new-date AS DATE FORMAT "99/99/9999":U INITIAL TODAY 
     LABEL "New Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE new-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "New Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE new-year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "New Year" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Run#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 96 BY 1.91
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 10.67.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExit AT ROW 1.14 COL 88.8 WIDGET-ID 24
     btSimulatePurge AT ROW 14.24 COL 29.6 WIDGET-ID 10
     run-no AT ROW 3.67 COL 39 COLON-ALIGNED HELP
          "Enter Run Number" WIDGET-ID 38
     current-date AT ROW 5.33 COL 39 COLON-ALIGNED WIDGET-ID 44
     current-period AT ROW 6.86 COL 39 COLON-ALIGNED WIDGET-ID 42
     current-year AT ROW 6.86 COL 59 COLON-ALIGNED WIDGET-ID 50
     new-date AT ROW 8.57 COL 39 COLON-ALIGNED WIDGET-ID 48
     new-period AT ROW 10.14 COL 39 COLON-ALIGNED WIDGET-ID 46
     new-year AT ROW 10.14 COL 59 COLON-ALIGNED WIDGET-ID 52
     fi_file AT ROW 12 COL 19 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 54
     btStartProcess AT ROW 14.24 COL 56.8 WIDGET-ID 12
     tb_OpenCSV AT ROW 12 COL 87.6 RIGHT-ALIGNED WIDGET-ID 56
     RECT-15 AT ROW 1 COL 1 WIDGET-ID 20
     RECT-16 AT ROW 2.95 COL 3 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 15.52
         BGCOLOR 15  WIDGET-ID 100.


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
         TITLE              = "Update GLHist"
         HEIGHT             = 15.52
         WIDTH              = 96
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN current-date IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       current-date:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "parm".

/* SETTINGS FOR FILL-IN current-period IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN current-year IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "parm".

ASSIGN 
       new-date:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "parm".

/* SETTINGS FOR FILL-IN new-period IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN new-year IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       run-no:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update GLHist */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update GLHist */
DO:
      
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulatePurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulatePurge C-Win
ON CHOOSE OF btSimulatePurge IN FRAME DEFAULT-FRAME /* Simulate */
DO:
    RUN pRunProcess(
        INPUT NO
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStartProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStartProcess C-Win
ON CHOOSE OF btStartProcess IN FRAME DEFAULT-FRAME /* Start Process */
DO:
    RUN pRunProcess(
        INPUT YES
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME DEFAULT-FRAME /* Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME new-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL new-date C-Win
ON LEAVE OF new-date IN FRAME DEFAULT-FRAME /* New Date */
DO:
    ASSIGN {&self-name}.
    IF LASTKEY NE -1 THEN DO:
        RUN valid-new-date NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME DEFAULT-FRAME /* Name */
DO:
    DEFINE VARIABLE ls-filename   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-ok         AS LOG       NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE ls-filename
        TITLE "Select File to Save "
        FILTERS "Excel Files    (*.csv)" "*.csv",
        "All Files    (*.*) " "*.*"
        INITIAL-DIR "c:\tmp"
        MUST-EXIST
        USE-FILENAME
        UPDATE ll-ok.

    IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-no C-Win
ON HELP OF run-no IN FRAME DEFAULT-FRAME /* Run# */
DO:
    DEFINE VARIABLE lv AS CHARACTER NO-UNDO.
    lv = {&self-name}:SCREEN-VALUE.
    RUN run-no-help (INPUT-OUTPUT lv).
    {&self-name}:SCREEN-VALUE = lv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run-no C-Win
ON LEAVE OF run-no IN FRAME DEFAULT-FRAME /* Run# */
DO:
    ASSIGN {&self-name}.
    
    IF LASTKEY NE -1 THEN DO:
        RUN valid-run-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME DEFAULT-FRAME /* Open CSV? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
    {sys/inc/f3helpw.i}     
    RUN enable_UI.
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
  DISPLAY run-no current-date current-period current-year new-date new-period 
          new-year fi_file tb_OpenCSV 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btExit RECT-15 btSimulatePurge RECT-16 run-no new-date fi_file 
         btStartProcess tb_OpenCSV 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplExpire AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN valid-run-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-new-date NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
        ASSIGN {&DISPLAYED-OBJECTS}.
    END.
    
    ASSIGN fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE =  cFileName.
    
    MESSAGE "Do you want to start the process with the selected parameters?"
        VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO
        UPDATE lResponse AS LOGICAL.
    
    IF NOT lResponse THEN 
        RETURN.
    
    SESSION:SET-WAIT-STATE("General"). 
    STATUS DEFAULT "Processing...".  
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN util/UpdateGLHistProc.p( 
            INPUT cocode, 
            INPUT INTEGER(run-no:SCREEN-VALUE), 
            INPUT DATE(new-date:SCREEN-VALUE),
            INPUT iplExpire, 
            INPUT cFileName).                 
    END.    
    
    SESSION:SET-WAIT-STATE("").   
    STATUS DEFAULT "". 
    
    IF NOT iplExpire THEN 
    DO:
        IF NOT tb_OpenCSV THEN DO:        
            MESSAGE  "Simulation Completed." SKIP(1)
            "~"OK~" to open CSV file?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
            TITLE "" UPDATE lChoice AS LOGICAL.

            IF lChoice THEN
            DO:
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
            END. /* IF lChoice THEN */
        END. /* IF NOT tb_OpenCSV THEN DO */
        ELSE DO:
            MESSAGE "Simulation Completed." SKIP
            "Check " + TRIM(cFileName) + " CSV file."
            VIEW-AS ALERT-BOX INFORMATION.
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
        END.
    END.
    ELSE
        MESSAGE "Completed updating the GL Run # " + STRING(run-no) SKIP
        "from Date: " + STRING(current-date) + " Period: " + STRING(current-period) + " Year: " + STRING(current-year)  SKIP
        "to Date: "+ STRING(new-date) + " Period: " + STRING(new-period) + " Year: " + STRING(new-year)
            VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-no-help C-Win 
PROCEDURE run-no-help :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-focus-val AS CHARACTER NO-UNDO.

    DEFINE VARIABLE char-val   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE look-recid AS RECID     NO-UNDO. 


    RUN windows/l-runno.w (cocode, io-focus-val, OUTPUT char-val).
    IF char-val NE "" AND ENTRY(1,char-val) NE io-focus-val THEN
        io-focus-val = ENTRY(1,char-val).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-new-date C-Win 
PROCEDURE valid-new-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-msg = "".

    IF new-date:SCREEN-VALUE EQ ? THEN
    DO:
        v-msg = TRIM(new-date:LABEL) + " can`t be blank.".
    END.
        
    FIND FIRST period NO-LOCK
             WHERE period.company EQ cocode
             AND period.pst LE DATE(new-date:SCREEN-VALUE)
             AND period.pend GE DATE(new-date:SCREEN-VALUE) NO-ERROR.

    IF v-msg EQ "" THEN
      IF NOT AVAIL period THEN
        v-msg = TRIM(new-date:LABEL) + " doesn't exist in period".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO new-date.
      RETURN ERROR.
    END.
    
    IF AVAIL period THEN
    DO:
        ASSIGN
            new-period:SCREEN-VALUE = STRING(period.pnum)
            new-year:SCREEN-VALUE   = STRING(period.yr)
            .
    END.
    
    
  END. /* DO WITH FRAME {&FRAME-NAME} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-run-no C-Win 
PROCEDURE valid-run-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.

  DEFINE BUFFER bf-glhist FOR glhist.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-msg = "".

    IF run-no:SCREEN-VALUE EQ "" THEN
    DO:
        v-msg = TRIM(run-no:LABEL) + " can`t be blank, try help...".
    END.
     
    FIND FIRST bf-glhist
        WHERE bf-glhist.company EQ cocode
          AND bf-glhist.tr-num EQ INT(run-no:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

    IF v-msg EQ "" THEN
      IF NOT AVAIL bf-glhist THEN
        v-msg = TRIM(run-no:LABEL) + " doesn't exist, try help...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO run-no.
      RETURN ERROR.
    END.
    
    IF AVAIL bf-glhist THEN
    DO:
        ASSIGN
            current-date:SCREEN-VALUE = STRING(bf-glhist.tr-date)
            current-period:SCREEN-VALUE = STRING(bf-glhist.period)
            current-year:SCREEN-VALUE = STRING(bf-glhist.glYear)
            .
    END.
    
    
  END. /* DO WITH FRAME {&FRAME-NAME} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

