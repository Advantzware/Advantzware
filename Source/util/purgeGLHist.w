&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
{custom/globdefs.i}
{sys/inc/var.i "new shared"}

DEF VAR cMessage AS CHAR NO-UNDO.
DEF VAR daTargetDate AS DATE NO-UNDO.
DEF VAR hPurge AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR iStartYear AS INT NO-UNDO.
DEF VAR iEndYear AS INT NO-UNDO.
DEF VAR iEndPeriod AS INT NO-UNDO.
DEF VAR lError AS LOG NO-UNDO.
DEF VAR lTested AS LOG NO-UNDO.
DEF VAR lReviewed AS LOG NO-UNDO.
DEF VAR lPurged AS LOG NO-UNDO.
DEF VAR lWarned AS LOG NO-UNDO.
DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
{src/adm2/widgetprto.i}
{util/ttPurge.i NEW}

ASSIGN
    cocode = g_company
    locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bExit fiEndYear fiEndPeriod fiOutputDir ~
eInstructions bTest bReset 
&Scoped-Define DISPLAYED-OBJECTS fiEndYear fiEndPeriod fiOutputDir ~
fiAnalyzingYear fiAnalyzingPeriod fiInstructionsLabel eInstructions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON bPurge 
     LABEL "PURGE" 
     SIZE 15 BY 1.43
     FONT 6.

DEFINE BUTTON bReset 
     LABEL "RESET" 
     SIZE 15 BY 1.43
     FONT 6.

DEFINE BUTTON bReview 
     LABEL "REVIEW" 
     SIZE 15 BY 1.43
     FONT 6.

DEFINE BUTTON bTest 
     LABEL "TEST" 
     SIZE 15 BY 1.43
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 99 BY 8.57 NO-UNDO.

DEFINE VARIABLE fiAnalyzingPeriod AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiAnalyzingYear AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndPeriod AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndYear AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Consolidate/Purge GL Records Through...Year" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiInstructionsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Instructions:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE fiOutputDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Purge data directory" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     bExit AT ROW 1.95 COL 116
     fiEndYear AT ROW 1.71 COL 48 COLON-ALIGNED
     fiEndPeriod AT ROW 1.71 COL 66 COLON-ALIGNED
     fiOutputDir AT ROW 3.14 COL 48 COLON-ALIGNED
     fiAnalyzingYear AT ROW 4.57 COL 48 COLON-ALIGNED NO-TAB-STOP 
     fiAnalyzingPeriod AT ROW 4.57 COL 66 COLON-ALIGNED NO-TAB-STOP 
     fiInstructionsLabel AT ROW 6.24 COL 5 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     eInstructions AT ROW 7.19 COL 8 NO-LABEL NO-TAB-STOP 
     bTest AT ROW 7.19 COL 110
     bReview AT ROW 9.1 COL 110
     bPurge AT ROW 11 COL 110
     bReset AT ROW 14.1 COL 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.6 BY 14.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consolidate/Purge GL Account Records"
         HEIGHT             = 14.91
         WIDTH              = 127.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 148
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 148
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bPurge IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bReview IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiAnalyzingPeriod IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiAnalyzingPeriod:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiAnalyzingYear IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiAnalyzingYear:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiInstructionsLabel IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiInstructionsLabel:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consolidate/Purge GL Account Records */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consolidate/Purge GL Account Records */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bTest wWin
ON CHOOSE OF bTest IN FRAME fMain /* TEST */
OR CHOOSE OF bReview IN FRAME fMain
OR CHOOSE OF bPurge IN FRAME fMain
OR CHOOSE OF bExit IN FRAME fMain
OR CHOOSE OF bReset IN FRAME fMain 
OR MOUSE-SELECT-DOWN OF bExit
DO:
    DEF VAR cFileList AS CHAR NO-UNDO.
    DEF VAR iErrorCount AS INT NO-UNDO.
    DEF VAR iProcessedCount AS INT NO-UNDO.
    DEF VAR iWarningCount AS INT NO-UNDO.
    DEF VAR cOutputDirName AS CHAR NO-UNDO.
    
    ASSIGN 
        cOutputDirName = SESSION:TEMP-DIRECTORY + "GLaccountPurge-" + 
                         STRING(YEAR(TODAY),"9999") +
                         STRING(MONTH(TODAY),"99") +
                         STRING(DAY(TODAY),"99") + "-" + 
                         STRING(TIME,"99999")  .
        
       
    CASE SELF:NAME:
        WHEN "bTest" THEN DO:
        
            RUN valid-year(OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
            
            STATUS DEFAULT "Analyzing files and running tests...".
            OS-CREATE-DIR VALUE (cOutputDirName).
            ASSIGN 
                fiOutputDir:SCREEN-VALUE = cOutputDirName. 
                
             cFileName     = "_ConsolidationReport" + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + "_" + STRING(TIME) + ".csv" .
               
            /* Find the oldest glhist record in the table */
            FIND FIRST glHist NO-LOCK WHERE 
                glHist.company EQ cocode
                USE-INDEX tr-date 
                NO-ERROR.
            IF NOT AVAIL glHist THEN DO:
                MESSAGE 
                    "There is a problem with your glhist table." SKIP 
                    "Please contact ASI Support for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.

                ASSIGN 
                iStartYear = INTEGER(fiEndYear:SCREEN-VALUE)
                    iEndYear = INTEGER(fiEndYear:SCREEN-VALUE)
                    iEndPeriod = INTEGER(fiEndPeriod:SCREEN-VALUE).

                STATUS DEFAULT "Analyzing GL account records...".
                RUN purgeClearTempTable IN hPurge .
                
                DO iCtr = iStartYear TO iEndYear:
                    ASSIGN 
                        fiAnalyzingYear:SCREEN-VALUE = STRING(iCtr,"9999").
                    DO jCtr = 1 TO company.num-per:
                        IF iCtr EQ iEndYear
                        AND jCtr GT iEndPeriod THEN LEAVE. 
                        ASSIGN 
                            fiAnalyzingPeriod:SCREEN-VALUE = STRING(jCtr,"99").
                        RUN testAccounts IN hPurge (
                            cocode, /* Company */
                            iCtr,   /* Year */
                            jCtr,   /* Period */
                            fiOutputDir:SCREEN-VALUE,
                            OUTPUT lError,
                            OUTPUT cMessage).
                        IF lError THEN DO:
                            MESSAGE 
                                cMessage VIEW-AS ALERT-BOX.
                            RETURN NO-APPLY.
                        END.
                    END.
                END.
            
            ASSIGN 
                lTested = TRUE
                lReviewed = FALSE 
                lPurged = TRUE.
            STATUS INPUT "Generating report...".
            STATUS DEFAULT "Generating report...".
            
            RUN outputGLAccountFile IN hPurge (fiOutputDir:SCREEN-VALUE,cFileName,cocode).
            
            APPLY 'value-changed' TO fiOutputDir.
            STATUS DEFAULT "Analysis complete.  Press Review to open the results list.".
        END.
        WHEN "bReview" THEN DO:
            STATUS INPUT "Opening file for review...".
            STATUS DEFAULT "Opening file for review...".
            OS-COMMAND SILENT VALUE ("START " + fiOutputDir:SCREEN-VALUE + "\" + cFileName).
           
            /* Use opsys action to give Excel time to open */
           // OS-COMMAND SILENT VALUE ("PING 127.0.0.1 -n 5"). 
            
            STATUS DEFAULT "Review complete. Records can now be processed.".
            ASSIGN 
                lTested = TRUE
                lReviewed = FALSE   
                lPurged = FALSE.
            END.
        WHEN "bPurge" THEN DO:
          
            STATUS DEFAULT "Consolidating records...".
            RUN purgeGLhistFromFile IN hPurge (
                fiOutputDir:SCREEN-VALUE + "\" + cFileName,
                cFileName,
                cocode,
                OUTPUT lError,
                OUTPUT cMessage).
            IF lError THEN DO:
                STATUS INPUT "Issue with consolidating records.  Process halted.".
                STATUS DEFAULT "Issue with consolidating records.  Process halted.".
                MESSAGE 
                    cMessage
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                STATUS INPUT "Consolidation complete.  Backup files stored in directory.".
                STATUS DEFAULT "Consolidation complete.  Backup files stored in directory.".
                APPLY 'value-changed' TO fiOutputDir.
                ASSIGN 
                    lTested = FALSE 
                    lReviewed = TRUE  
                    lPurged = TRUE.
                APPLY 'choose' TO bReset.
            END.
        END.
        WHEN "bReset" THEN DO:
            ASSIGN 
                fiEndYear:SCREEN-VALUE = STRING(YEAR(TODAY) - 7)
                fiEndPeriod:SCREEN-VALUE = STRING(company.num-per)
                fiOutputDir:SCREEN-VALUE = SESSION:TEMP-DIRECTORY
                lTested = FALSE 
                lReviewed = TRUE  
                lPurged = TRUE   
                .  
                    
            APPLY 'entry' TO fiEndYear.
        END.
        WHEN "bExit" THEN DO:
            APPLY 'window-close' TO wWin.
        END.
    END.                                     
    ASSIGN 
        bTest:SENSITIVE = NOT lTested 
        bReview:SENSITIVE = NOT lReviewed
        bPurge:SENSITIVE = NOT lPurged 
        .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndPeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndPeriod wWin
ON LEAVE OF fiEndPeriod IN FRAME fMain /* Period */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) LT 1
    OR INTEGER(SELF:SCREEN-VALUE) GT company.num-per THEN DO:
        MESSAGE 
            "Invalid period entered.  Must be between 1 and " + STRING(company.num-per) + "."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndYear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndYear wWin
ON LEAVE OF fiEndYear IN FRAME fMain /* Consolidate/Purge GL Records Through...Year */
DO:     
    IF LASTKEY = 617 THEN RETURN.  /* User pressed Exit button */
    RUN valid-year(OUTPUT lReturnError) NO-ERROR.
    IF lReturnError THEN RETURN NO-APPLY.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOutputDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOutputDir wWin
ON HELP OF fiOutputDir IN FRAME fMain /* Purge data directory */
DO:
    DEF VAR cSelectedDir AS CHAR NO-UNDO.
    SYSTEM-DIALOG GET-DIR cSelectedDir
        INITIAL-DIR SESSION:TEMP-DIRECTORY 
        TITLE "Select a GL Account Purge Directory".
    ASSIGN 
        SELF:SCREEN-VALUE = cSelectedDir.
    APPLY 'value-changed' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOutputDir wWin
ON VALUE-CHANGED OF fiOutputDir IN FRAME fMain /* Purge data directory */
DO:
    DEF VAR cFileName AS CHAR.
    DEF VAR lHasDumpFile AS LOG NO-UNDO.
    INPUT FROM OS-DIR(SELF:SCREEN-VALUE).
    REPEAT:
        IMPORT cFileName.
        IF INDEX(cFileName,".d") NE 0 THEN ASSIGN 
                lHasDumpFile = TRUE.
    END. 
    ASSIGN 
        bReview:SENSITIVE = SEARCH(SELF:SCREEN-VALUE + "\" + "_ConsolidationReport.csv") NE ?
        bPurge:SENSITIVE = NOT lHasDumpFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


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
    RUN enable_UI.
    
    RUN util/purgeProcs.p PERSISTENT SET hPurge.
 
    /* Finds current company */
    FIND FIRST company NO-LOCK WHERE 
        company.company EQ cocode
        NO-ERROR.
    IF NOT AVAIL company THEN DO:
        MESSAGE 
            "There is an issue with your company code." SKIP 
            "Please exit Advantzware and login again."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    ASSIGN 
        eInstructions:SCREEN-VALUE IN FRAME {&frame-name} = 
            "This function will review GL transactions from the earliest record through the Year/Period specified above, and allows " +
            "the user to created 'consolidated' records (one per year/period/account) for these transactions." + CHR(10) + CHR(10) +
            "When 'TEST' is selected, the program will generate a list of records to be consolidated, as well as the proposed " +
            "consolidated entries.  This list, as a .CSV file, can be found in the directory specified below." + CHR(10) + CHR(10) +
            "When 'PURGE' is selected, entries in the list file will be processed so that the original entries will be deleted, " +
            "and the consolidated entries will be added.  The original (deleted) items will be exported into the same directory, " + 
            "so that they can be restored if required." + CHR(10) + 
            "NOTE: IF YOU ELECT TO RESTORE (IMPORT) THE PURGED RECORDS, YOU MUST RUN 'Purge GL Detail' FIRST."
        fiEndYear:SCREEN-VALUE = STRING(YEAR(TODAY) - 7)
        fiEndPeriod:SCREEN-VALUE = STRING(company.num-per)
        fiOutputDir:SCREEN-VALUE = SESSION:TEMP-DIRECTORY.  
        
    APPLY 'entry' TO fiEndYear.
  
    /* IF NOT THIS-PROCEDURE:PERSISTENT THEN */
    {methods/nowait.i}
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY fiEndYear fiEndPeriod fiOutputDir fiAnalyzingYear fiAnalyzingPeriod 
          fiInstructionsLabel eInstructions 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE bExit fiEndYear fiEndPeriod fiOutputDir eInstructions bTest bReset 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-year wWin 
PROCEDURE valid-year :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturn AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValid    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ldDate    AS DATE    NO-UNDO.
  
    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:        
        IF INTEGER(fiEndYear:SCREEN-VALUE) GT (YEAR(TODAY) - 7) THEN DO:   
            FIND FIRST users NO-LOCK WHERE 
                users.user_id EQ USERID("ASI")
                NO-ERROR.       
            IF NOT AVAIL users 
            OR users.securityLevel LT 900 THEN DO:
                MESSAGE 
                    "Advantzware requires the last 7 years for prior period comparison purposes. Process not allowed.."
                    VIEW-AS ALERT-BOX .
                oplReturn = YES.
                APPLY "entry" TO fiEndYear.
            END.
            ELSE DO:
                MESSAGE 
                    "Warning: Deleting/consolidating GL history records less than 7 years old is NOT recommended. Are you sure?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue2 AS LOG.
                IF NOT lContinue2 THEN DO:
                    oplReturn = YES.
                    APPLY 'entry' TO fiEndYear.
                END.
            END.
        END. 
    END. 
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

