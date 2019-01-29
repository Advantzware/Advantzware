&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: wPrgmrToolbox.w

  Description: Programmer Toolbox Launcher

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Mark Tyndall

  Created: 9.17.2018

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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cDLC AS CHARACTER NO-UNDO.
DEF VAR cDevCfg AS CHAR NO-UNDO.
DEF VAR cRunCfg AS CHAR NO-UNDO.
DEF VAR cCfgCfg AS CHAR NO-UNDO.
DEF VAR cState AS CHAR NO-UNDO.
DEF VAR cProdDb AS CHAR NO-UNDO.
DEF VAR cTestDb AS CHAR NO-UNDO.
DEF VAR cProdAud AS CHAR NO-UNDO.
DEF VAR cTestAud AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDataDigger btEditor btLockMon ~
btMonitorUsers rsDB1 btProTools rsDB2 btSwitchMode 
&Scoped-Define DISPLAYED-OBJECTS fiMode rsDB1 rsDB2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btDataDigger 
     IMAGE-UP FILE "Graphics/32x32/industrial_robot.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run DataDigger" 
     SIZE 8 BY 1.91 TOOLTIP "Data Digger".

DEFINE BUTTON btEditor 
     IMAGE-UP FILE "Graphics/32x32/edit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run Editor" 
     SIZE 8 BY 1.91 TOOLTIP "Progress Editor".

DEFINE BUTTON btLockMon 
     IMAGE-UP FILE "Graphics/32x32/lock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run Lock Monitor" 
     SIZE 8 BY 1.91 TOOLTIP "Lock Monitor".

DEFINE BUTTON btMonitorUsers 
     IMAGE-UP FILE "Graphics/32x32/users3.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Monitor Users" 
     SIZE 8 BY 1.91 TOOLTIP "User Monitor".

DEFINE BUTTON btProTools 
     IMAGE-UP FILE "Graphics/32x32/tools.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run ProTools" 
     SIZE 8 BY 1.91 TOOLTIP "Pro Tools".

DEFINE BUTTON btSwitchMode 
     IMAGE-UP FILE "Graphics/32x32/gearwheels.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Switch Mode" 
     SIZE 8 BY 1.91 TOOLTIP "Switch Mode (Dev/Run)".

DEFINE VARIABLE fiMode AS CHARACTER FORMAT "X(256)":U INITIAL "RUN" 
     LABEL "Current Mode" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE rsDB1 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PROD", "PROD",
"TEST", "TEST"
     SIZE 22 BY .95 NO-UNDO.

DEFINE VARIABLE rsDB2 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "ASI", "ASI",
"AUD", "AUD"
     SIZE 22 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 55 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 86 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btDataDigger AT ROW 1.48 COL 3 WIDGET-ID 20
     btEditor AT ROW 1.48 COL 21
     btLockMon AT ROW 1.48 COL 12 WIDGET-ID 18
     fiMode AT ROW 1.95 COL 70 COLON-ALIGNED
     btMonitorUsers AT ROW 1.48 COL 39
     rsDB1 AT ROW 3.86 COL 7 NO-LABEL
     btProTools AT ROW 1.48 COL 30 WIDGET-ID 2
     rsDB2 AT ROW 3.86 COL 31 NO-LABEL
     btSwitchMode AT ROW 1.48 COL 48
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 22
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.2 BY 4.1.


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
         TITLE              = "Programmer's Toolbox"
         HEIGHT             = 4.1
         WIDTH              = 86.2
         MAX-HEIGHT         = 4.1
         MAX-WIDTH          = 86.2
         VIRTUAL-HEIGHT     = 4.1
         VIRTUAL-WIDTH      = 86.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FILL-IN fiMode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Programmer's Toolbox */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Programmer's Toolbox */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDataDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDataDigger C-Win
ON CHOOSE OF btDataDigger IN FRAME DEFAULT-FRAME /* Run DataDigger */
OR CHOOSE OF btLockMon
OR CHOOSE OF btProTools
OR CHOOSE OF btMonitorUsers
OR CHOOSE OF btEditor
OR CHOOSE OF btSwitchMode
DO:
    DEF VAR cCmdString AS CHAR NO-UNDO.
    CASE SELF:NAME:
        WHEN "btDataDigger" THEN
            RUN datadigger/datadigger.p.
        WHEN "btLockMon" THEN
            RUN util/wlockmon.w.
        WHEN "btProTools" THEN
            RUN protools/_protool.r.
        WHEN "btMonitorUsers" THEN DO:
            IF rsDB1:SCREEN-VALUE = "Prod"
            AND rsDB2:SCREEN-VALUE = "ASI" THEN ASSIGN  
                cCmdString = cDLC + "\bin\proshut.bat" + " -db " + cProdDB.
            ELSE IF rsDB1:SCREEN-VALUE = "Test"
            AND rsDB2:SCREEN-VALUE = "ASI" THEN ASSIGN  
                cCmdString = cDLC + "\bin\proshut.bat" + " -db " + cTestDB.
            ELSE IF rsDB1:SCREEN-VALUE = "Prod"
            AND rsDB2:SCREEN-VALUE = "AUD" THEN ASSIGN  
                cCmdString = cDLC + "\bin\proshut.bat" + " -db " + cProdAud.
            ELSE IF rsDB1:SCREEN-VALUE = "Test"
                AND rsDB2:SCREEN-VALUE = "AUD" THEN ASSIGN  
                    cCmdString = cDLC + "\bin\proshut.bat" + " -db " + cTestAud.
            OS-COMMAND VALUE(cCmdString).
        END.
        WHEN "btEditor" THEN 
            RUN _edit.r.
        WHEN "btSwitchMode" THEN DO:
            IF cState EQ "Run" THEN DO:
                OS-RENAME VALUE (cCfgCfg) VALUE(cDLC + "\progress.run").
                OS-RENAME VALUE (cDevCfg) VALUE(cDLC + "\progress.cfg").
                ASSIGN
                    fiMode:SCREEN-VALUE = "DEV" 
                    cState = "Dev".
            END.
            ELSE IF cState EQ "Dev" THEN DO:
                OS-RENAME VALUE (cCfgCfg) VALUE(cDLC + "\progress.dev").
                OS-RENAME VALUE (cRunCfg) VALUE(cDLC + "\progress.cfg").
                ASSIGN 
                    fiMode:SCREEN-VALUE = "RUN"
                    cState = "Run".
            END.
            MESSAGE 
                "You should close and reopen your session now to return to " + cState + " mode."
                VIEW-AS ALERT-BOX INFO.
        END. 
    END CASE.
    
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
  RUN enable_UI.
  
    GET-KEY-VALUE SECTION 'Startup' KEY 'DLC' VALUE cDLC.
    ASSIGN 
        cDevCfg = cDLC + "\progress.dev"
        cRunCfg = cDLC + "\progress.run"
        cCfgCfg = cDLC + "\progress.cfg".
        
    /* Make sure progress.cfg is where we expect it; otherwise disable this feature */    
    IF SEARCH (cCfgCfg) EQ ? THEN DO:
        MESSAGE 
            "Unable to locate a valid progress.cfg."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            btSwitchMode:VISIBLE = FALSE 
            fiMode:VISIBLE = FALSE .
    END. 
    
    /* Make sure progress.dev has been installed */    
    IF SEARCH (cDevCfg) EQ ? 
    AND SEARCH (cRunCfg) EQ ? THEN 
    DO:
        MESSAGE 
            "Unable to locate progress.cfg options"
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            btSwitchMode:SENSITIVE  = FALSE. 
    END. 
    
    /* Now figure out if we're in dev mode or run mode */
    IF SEARCH (cDevCfg) NE ? THEN ASSIGN
        cState = "Run"
        fiMode:SCREEN-VALUE = "RUN".       
    ELSE IF SEARCH (cRunCfg) NE ? THEN ASSIGN
            cState = "Dev"
            fiMode:SCREEN-VALUE = "DEV".       
  
    
    /* Find where the prod DB lives */
    /* I'm sure there's a more efficient way to do this, but in a hurry */
    ASSIGN 
        cProdDB = "c:\asigui\databases\prod\asiProd.db".
    IF SEARCH (cProdDB) EQ ? THEN DO:
        ASSIGN 
            cProdDB = "d:\asigui\databases\prod\asiProd.db".
        IF SEARCH (cProdDB) EQ ? THEN DO: 
            ASSIGN 
                cProdDB = "e:\asigui\databases\prod\asiProd.db".
            IF SEARCH (cProdDB) EQ ? THEN DO:
                ASSIGN 
                    cProdDB = "f:\asigui\databases\prod\asiProd.db".
                IF SEARCH (cProdDB) EQ ? THEN DO:
                    ASSIGN 
                        cProdDB = "c:\asi\databases\prod\asiProd.db".
                    IF SEARCH (cProdDB) EQ ? THEN DO:
                        ASSIGN 
                            cProdDB = "d:\asi\databases\prod\asiProd.db".
                        IF SEARCH (cProdDB) EQ ? THEN DO:
                            ASSIGN 
                                cProdDB = "e:\asi\databases\prod\asiProd.db".
                            IF SEARCH (cProdDB) EQ ? THEN DO:
                                ASSIGN 
                                    cProdDB = "e:\asi\databases\prod\asiProd.db".
                                IF SEARCH (cProdDB) EQ ? THEN DO:
                                    /* This handles local programming and AWS */
                                    /* Everything in the field should have been found by now */
                                    ASSIGN
                                        cProdDB = "c:\asigui\databases\Test\asiTest168.db".
                                END.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.    
    ASSIGN 
        cTestDb = REPLACE (cProdDb,"prod","test")
        cProdAud = REPLACE (cProdDb, "asiprod", "audprod")
        cProdAud = REPLACE (cProdDb, "prod\", "audit\")
        cTestAud = REPLACE (cProdAud,"prod","test").
    IF SEARCH (cTestDb) EQ ? THEN ASSIGN 
        rsDB1:SENSITIVE = FALSE.
    IF cProdDB EQ "" THEN ASSIGN
        btMonitorUsers:VISIBLE = FALSE 
        rsDB1:VISIBLE = FALSE 
        rsDB2:VISIBLE = FALSE .
     
    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult      AS LOG    NO-UNDO.
    DEF VAR iNumOK AS INT NO-UNDO .
    
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.

    /* Admin user access */
    RUN epCanAccess IN hPgmSecurity ("util/wPgmrToolbox.w", "LockMon", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btLockMon:SENSITIVE = FALSE.
    ELSE ASSIGN iNumOK = iNumOK + 1.

    RUN epCanAccess IN hPgmSecurity ("util/wPgmrToolbox.w", "MonitorUsers", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN 
            btMonitorUsers:VISIBLE = FALSE
            rsDB1:SENSITIVE          = FALSE 
            rsDB2:SENSITIVE          = FALSE .
    ELSE ASSIGN iNumOK = iNumOK + 1.

    /* ASI User access */
    RUN epCanAccess IN hPgmSecurity ("util/wPgmrToolbox.w", "ProTools", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btProTools:VISIBLE = FALSE.
    ELSE ASSIGN iNumOK = iNumOK + 1.
        

    RUN epCanAccess IN hPgmSecurity ("util/wPgmrToolbox.w", "DataDigger", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btDataDigger:VISIBLE = FALSE.
    ELSE ASSIGN iNumOK = iNumOK + 1.

    RUN epCanAccess IN hPgmSecurity ("util/wPgmrToolbox.w", "SwitchMode", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN 
        btSwitchMode:VISIBLE = FALSE
        fiMode:SENSITIVE = FALSE.
    ELSE ASSIGN iNumOK = iNumOK + 1.

    RUN epCanAccess IN hPgmSecurity ("util/wPgmrToolbox.w", "RunEditor", OUTPUT lResult).
    IF NOT lResult THEN ASSIGN btEditor:VISIBLE = FALSE.
    ELSE ASSIGN iNumOK = iNumOK + 1.


    DELETE OBJECT hPgmSecurity.
    IF iNumOK = 0 THEN DO:
        MESSAGE 
            "You don't have sufficient permissions to run functions from this window."
            VIEW-AS ALERT-BOX ERROR.
        RETURN .
    END.    

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
  DISPLAY fiMode rsDB1 rsDB2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btDataDigger btEditor btLockMon btMonitorUsers rsDB1 btProTools rsDB2 
         btSwitchMode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

