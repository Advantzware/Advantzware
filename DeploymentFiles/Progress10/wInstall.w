&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*---------------------------------------------------------------------------*/
/*  File:           wInstall.w                                               */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Application to install/upgrade ASI installation          */
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:                                                       */
/*  External files:                                                          */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/
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
DEF STREAM sINI.

DEF VAR cInstallVersion AS CHAR NO-UNDO.
DEF VAR cSiteName AS CHAR NO-UNDO.
DEF VAR cDbDir AS CHAR NO-UNDO.
DEF VAR cProgressDir AS CHAR NO-UNDO.
DEF VAR cBackupDir AS CHAR NO-UNDO.
DEF VAR cRootFolder AS CHAR NO-UNDO.
DEF VAR cRcodeFolder AS CHAR NO-UNDO.
DEF VAR cLine AS CHAR.
DEF VAR cLastSection AS CHAR.
DEF VAR iLastSectionLine AS INT.
DEF VAR cLastValue AS CHAR.
DEF VAR iCnt AS INT NO-UNDO.

{src/adm2/widgetprto.i}

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
&Scoped-Define ENABLED-OBJECTS RECT-1 fiVersion fiProDir fiDB fiBack fiRoot ~
fiRcode tgUpgrade TOGGLE-1 rsDeltaDB fiPathToASIPf fiPathToNosweatPF btGo 
&Scoped-Define DISPLAYED-OBJECTS fiVersion fiProDir fiDB fiBack fiRoot ~
fiRcode tgUpgrade TOGGLE-1 rsDeltaDB fiPathToASIPf fiPathToNosweatPF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btGo 
     LABEL "Start Install" 
     SIZE 32 BY 1.15.

DEFINE VARIABLE fiBack AS CHARACTER FORMAT "X(256)":U INITIAL "n:~\Backup16.n" 
     LABEL "Backup Folder" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE fiDB AS CHARACTER FORMAT "X(256)":U INITIAL "N:~\db" 
     LABEL "Database Folder" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE fiPathToASIPf AS CHARACTER FORMAT "X(256)":U 
     LABEL "Full Path to ASI PF File" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE fiPathToNosweatPF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Full path to NOSWEAT PF File" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE fiProDir AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\progress~\openedge" 
     LABEL "Progress Folder" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE fiRcode AS CHARACTER FORMAT "X(256)":U INITIAL "rcode" 
     LABEL "Rcode folder name (E.g. rcode)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiRoot AS CHARACTER FORMAT "X(256)":U INITIAL "N:" 
     LABEL "Folder that contains rcode folder (E.g. N: )" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE fiVersion AS CHARACTER FORMAT "X(256)":U INITIAL "sitename (no spaces!!!)" 
     LABEL "Site Name (No spaces)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE rsDeltaDB AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "ASI", "ASI",
"NOSWEAT", "Nosweat"
     SIZE 29 BY 1.42 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 112 BY 15.69.

DEFINE VARIABLE tgUpgrade AS LOGICAL INITIAL no 
     LABEL "Upgrade Existing Install" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiVersion AT ROW 2.92 COL 48 COLON-ALIGNED WIDGET-ID 2
     fiProDir AT ROW 4.58 COL 48 COLON-ALIGNED WIDGET-ID 16
     fiDB AT ROW 6.23 COL 48 COLON-ALIGNED WIDGET-ID 4
     fiBack AT ROW 7.92 COL 48 COLON-ALIGNED WIDGET-ID 6
     fiRoot AT ROW 9.35 COL 48 COLON-ALIGNED WIDGET-ID 8
     fiRcode AT ROW 10.77 COL 48 COLON-ALIGNED WIDGET-ID 10
     tgUpgrade AT ROW 12.19 COL 50 WIDGET-ID 18
     TOGGLE-1 AT ROW 13.38 COL 50 WIDGET-ID 26
     rsDeltaDB AT ROW 13.38 COL 50 NO-LABEL WIDGET-ID 28
     fiPathToASIPf AT ROW 15.04 COL 48 COLON-ALIGNED WIDGET-ID 20
     fiPathToNosweatPF AT ROW 16.46 COL 48 COLON-ALIGNED WIDGET-ID 24
     btGo AT ROW 18.62 COL 43 WIDGET-ID 12
     "Delta.df (if any) applies to:" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 13.77 COL 22 WIDGET-ID 32
     RECT-1 AT ROW 2.42 COL 4 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.6 BY 20 WIDGET-ID 100.


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
         TITLE              = "Install Advantzware 16.n"
         HEIGHT             = 20
         WIDTH              = 118.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Install Advantzware 16.n */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Install Advantzware 16.n */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo wWin
ON CHOOSE OF btGo IN FRAME fMain /* Start Install */
DO:
    ASSIGN 
        fiVersion
        fiProDir
        fiDB
        fiBack
        fiRoot
        fiRcode
        tgUpgrade
        rsDeltaDb
        fiPathToASIPf
        fiPathToNosweatPF.
    
    IF SEARCH(".\delta.df") <> ? THEN DO:
        MESSAGE 
            "Reminder:  delta.df files can applied with this" SKIP
            "script only if they do nothing else but add new fields!"
            VIEW-AS ALERT-BOX.
    END.
    
    OS-COMMAND 
        VALUE("installPatchv2.bat") 
        VALUE(fiVersion) /* Really sitename */
        VALUE('"' + fiDB + '"') 
        VALUE('"' + fiProDir + '"') 
        VALUE('"' + fiBack + '"') 
        VALUE(fiRoot)
        VALUE(fiRcode) 
        VALUE(tgUpgrade) 
        VALUE(rsDeltaDB) 
        VALUE(fiPathToASIPf)
        VALUE(fiPathToNosweatPF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBack wWin
ON HELP OF fiBack IN FRAME fMain /* Backup Folder */
OR HELP OF fiDb
OR HELP OF fiProDir
OR HELP OF fiRoot
DO:
    DEF VAR v-file-path AS CHAR NO-UNDO.

    CASE SELF:NAME:
        WHEN "fiBack" THEN DO:
            SYSTEM-DIALOG GET-DIR 
                v-file-path
                TITLE "Select backup Files Path".
            ASSIGN 
                fiBack:SCREEN-VALUE = v-file-path.  
        END.
        WHEN "fiDb" THEN DO:
            SYSTEM-DIALOG GET-DIR 
                v-file-path
                TITLE "Select Database Files Path".
            ASSIGN 
                fiDB:SCREEN-VALUE = v-file-path.  
        END.
        WHEN "fiProDir" THEN DO:
            SYSTEM-DIALOG GET-DIR 
                v-file-path
                TITLE "Select Path Progress Folder".
            ASSIGN
                fiProDir:SCREEN-VALUE = v-file-path.  
        END.
        WHEN "fiRoot" THEN DO:
            SYSTEM-DIALOG GET-DIR 
                v-file-path
                TITLE "Select Path to R-code Path".
            ASSIGN
                fiRoot:SCREEN-VALUE = v-file-path.    
        END.
    END CASE.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBack wWin
ON LEAVE OF fiBack IN FRAME fMain /* Backup Folder */
OR LEAVE OF fiDb
OR LEAVE OF fiPathToAsiPf
OR LEAVE OF fiPathToNosweatPf
OR LEAVE OF fiProDir
OR LEAVE OF fiRcode
OR LEAVE OF fiVersion
DO:
    CASE SELF:NAME:
        WHEN "fiBack" THEN DO:
            ASSIGN 
                fiBack
                FILE-INFO:FILE-NAME = fiBack.
            IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
                MESSAGE 
                    "Error - invalid folder entered!"
                    VIEW-AS ALERT-BOX.
                RETURN.
            END.
        END.
        WHEN "fiDb" THEN DO:
            ASSIGN 
                fiDb.
                FILE-INFO:FILE-NAME = fiDb.
           IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
                MESSAGE 
                    "Error - invalid folder entered!"
                    VIEW-AS ALERT-BOX.
                RETURN.
           END.
        END.
        WHEN "fiPathToAsiPf" THEN DO:
            ASSIGN 
                fiPathToASIPf.
            IF SEARCH(fiPathToASIPf) = ? THEN DO:
                MESSAGE 
                    "Error - invalid pf file entered!"
                    VIEW-AS ALERT-BOX.
                RETURN.
            END.
        END.
        WHEN "fiPathToNosweatPf" THEN DO:
            ASSIGN 
                fiPathToNosweatPF.
            IF SEARCH(fiPathToNosweatPF) = ? THEN DO:
                MESSAGE 
                    "Error - invalid pf file entered!"
                    VIEW-AS ALERT-BOX.
                RETURN.
            END.
        END.
        WHEN "fiProDir" THEN DO:
            ASSIGN 
                fiProDir
                FILE-INFO:FILE-NAME = fiProDir.
            IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
                MESSAGE 
                    "Error - invalid folder entered!"
                    VIEW-AS ALERT-BOX.
                RETURN.
            END.
        END.
        WHEN "fiRcode" THEN DO:
            ASSIGN 
                fiRoot 
                fiRcode
                FILE-INFO:FILE-NAME = fiRoot + "/" + fiRcode.
            IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
                MESSAGE 
                    "Error - invalid folder entered!"
                    VIEW-AS ALERT-BOX.
                RETURN.
            END.
        END.
        WHEN "fiVersion" THEN DO:
            ASSIGN 
                fiVersion.
            IF INDEX(fiVersion, " ") <> 0 THEN DO:
                MESSAGE 
                    "Site Name cannot contain spaces!" 
                    VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

{src/adm2/windowmn.i}

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
  DISPLAY fiVersion fiProDir fiDB fiBack fiRoot fiRcode tgUpgrade TOGGLE-1 
          rsDeltaDB fiPathToASIPf fiPathToNosweatPF 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 fiVersion fiProDir fiDB fiBack fiRoot fiRcode tgUpgrade 
         TOGGLE-1 rsDeltaDB fiPathToASIPf fiPathToNosweatPF btGo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cAdvantzwareINI AS CHAR NO-UNDO.
    DEF VAR cInput AS CHAR NO-UNDO.
    DEF VAR cTag AS CHAR NO-UNDO.

  RUN SUPER.

    ASSIGN 
        cAdvantzwareINI = SEARCH("..\rcode\advantzware.ini").
    IF cAdvantzwareINI NE ? THEN DO:
        INPUT STREAM sIni FROM VALUE(cAdvantzwareINI).
        REPEAT:
            ASSIGN 
                cInput = "".
            IMPORT cInput.
            IF cInput EQ "" THEN NEXT.
            ASSIGN 
                cInput = TRIM(cInput).
            IF cInput BEGINS "[" THEN ASSIGN 
                cInput = TRIM(cInput, "[")
                cInput = TRIM(cInput, "]")
                cTAG = cInput.
            ELSE DO:
                CASE cTag:
                    WHEN "SITENAME" THEN cSiteName = cInput.
                    WHEN "InstallVersion" THEN cInstallVersion = cInput.
                    WHEN "dbDir" THEN cDbDir = cInput.
                    WHEN "progressDir" THEN cProgressDir = cInput.
                    WHEN "backupDir" THEN cBackupDir = cInput.
                    WHEN "rootFolder" THEN cRootFolder = cInput.
                    WHEN "rcodeFolder" THEN cRcodeFolder = cInput.    
                END CASE.
            END. /* Is a value */
        END. /* Repeat */
        INPUT STREAM sIni CLOSE.
    
        ASSIGN
            fiProDir:SCREEN-VALUE IN FRAME fMain = cProgressDir
            fiDB:SCREEN-VALUE = cDbDir
            fiBack:SCREEN-VALUE = cBackupDir
            fiRoot:SCREEN-VALUE = cRootFolder
            fiRcode:SCREEN-VALUE = cRcodeFolder
            fiVersion:SCREEN-VALUE = cSiteName
            .
    END. /* Is an INI file */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

