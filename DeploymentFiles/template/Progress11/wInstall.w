&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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
DEF VAR cInstallVersion AS CHAR NO-UNDO.
DEF VAR cSiteName AS CHAR NO-UNDO.
DEF VAR cDbDir AS CHAR NO-UNDO.
DEF VAR cProgressDir AS CHAR NO-UNDO.
DEF VAR cBackupDir AS CHAR NO-UNDO.
DEF VAR cRootFolder AS CHAR NO-UNDO.
DEF VAR cRcodeFolder AS CHAR NO-UNDO.
DEF STREAM sINI.
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
     SIZE 32 BY 1.14.

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
     SIZE 29 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 112 BY 15.71.

DEFINE VARIABLE tgUpgrade AS LOGICAL INITIAL no 
     LABEL "Upgrade Existing Install" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiVersion AT ROW 2.91 COL 48 COLON-ALIGNED WIDGET-ID 2
     fiProDir AT ROW 4.57 COL 48 COLON-ALIGNED WIDGET-ID 16
     fiDB AT ROW 6.24 COL 48 COLON-ALIGNED WIDGET-ID 4
     fiBack AT ROW 7.91 COL 48 COLON-ALIGNED WIDGET-ID 6
     fiRoot AT ROW 9.33 COL 48 COLON-ALIGNED WIDGET-ID 8
     fiRcode AT ROW 10.76 COL 48 COLON-ALIGNED WIDGET-ID 10
     tgUpgrade AT ROW 12.19 COL 50 WIDGET-ID 18
     TOGGLE-1 AT ROW 13.38 COL 50 WIDGET-ID 26
     rsDeltaDB AT ROW 13.38 COL 50 NO-LABEL WIDGET-ID 28
     fiPathToASIPf AT ROW 15.05 COL 48 COLON-ALIGNED WIDGET-ID 20
     fiPathToNosweatPF AT ROW 16.48 COL 48 COLON-ALIGNED WIDGET-ID 24
     btGo AT ROW 18.62 COL 43 WIDGET-ID 12
     "Delta.df (if any) applies to:" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 13.76 COL 22 WIDGET-ID 32
     RECT-1 AT ROW 2.43 COL 4 WIDGET-ID 14
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
         WIDTH              = 118.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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
    if search(".\delta.df") ne ? then do:
      message "Reminder:  delta.df files can applied with this" skip
              "script only if they do nothing else but add new fields!" skip
              view-as alert-box.
    end.
    OS-COMMAND VALUE("installPatchv2.bat") VALUE(fiVersion) /* Really sitename */
       VALUE('"' + fiDB + '"') VALUE('"' + fiProDir + '"') VALUE('"' + fiBack + '"') VALUE(fiRoot)
       VALUE(fiRcode) value(tgUpgrade) VALUE(rsDeltaDB) VALUE(fiPathToASIPf)
       VALUE(fiPathToNosweatPF).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBack wWin
ON HELP OF fiBack IN FRAME fMain /* Backup Folder */
DO:
    DEF VAR v-file-path AS CHAR NO-UNDO.

    SYSTEM-DIALOG GET-DIR v-file-path
        TITLE "Select backup Files Path".

    fiBack:SCREEN-VALUE = v-file-path.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBack wWin
ON LEAVE OF fiBack IN FRAME fMain /* Backup Folder */
DO:
    assign fiBack.
  FILE-INFO:FILE-NAME = fiBack.
  
   IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
       MESSAGE "Error - invalid folder entered!"
       VIEW-AS ALERT-BOX.
       RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDB wWin
ON HELP OF fiDB IN FRAME fMain /* Database Folder */
DO:
    DEF VAR v-file-path AS CHAR NO-UNDO.

    SYSTEM-DIALOG GET-DIR v-file-path
        TITLE "Select Database Files Path".

    fiDB:SCREEN-VALUE = v-file-path.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDB wWin
ON LEAVE OF fiDB IN FRAME fMain /* Database Folder */
DO:
    assign fiDb.
  FILE-INFO:FILE-NAME = fiDb.
  
   IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
       MESSAGE "Error - invalid folder entered!"
       VIEW-AS ALERT-BOX.
       RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPathToASIPf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPathToASIPf wWin
ON LEAVE OF fiPathToASIPf IN FRAME fMain /* Full Path to ASI PF File */
DO:
  assign fiPathToASIPf.
  if search(fiPathToASIPf) eq ? then do:

       MESSAGE "Error - invalid pf file entered!"
       VIEW-AS ALERT-BOX.
       RETURN.

  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPathToNosweatPF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPathToNosweatPF wWin
ON LEAVE OF fiPathToNosweatPF IN FRAME fMain /* Full path to NOSWEAT PF File */
DO:
    assign fiPathToNosweatPF.
    
    if search(fiPathToNosweatPF) eq ? then do:

       MESSAGE "Error - invalid pf file entered!"
       VIEW-AS ALERT-BOX.
       RETURN.

    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiProDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiProDir wWin
ON HELP OF fiProDir IN FRAME fMain /* Progress Folder */
DO:
    DEF VAR v-file-path AS CHAR NO-UNDO.

    SYSTEM-DIALOG GET-DIR v-file-path
        TITLE "Select Path Progress Folder".

    fiProDir:SCREEN-VALUE = v-file-path.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiProDir wWin
ON LEAVE OF fiProDir IN FRAME fMain /* Progress Folder */
DO:
  assign fiProDir.
  FILE-INFO:FILE-NAME = fiProDir.
  
   IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
       MESSAGE "Error - invalid folder entered!"
       VIEW-AS ALERT-BOX.
       RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRcode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRcode wWin
ON LEAVE OF fiRcode IN FRAME fMain /* Rcode folder name (E.g. rcode) */
DO:
    assign fiRoot fiRcode .
  FILE-INFO:FILE-NAME = fiRoot + "/" + fiRcode.
  
   IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
       MESSAGE "Error - invalid folder entered!"
       VIEW-AS ALERT-BOX.
       RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRoot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRoot wWin
ON HELP OF fiRoot IN FRAME fMain /* Folder that contains rcode folder (E.g. N: ) */
DO:
    DEF VAR v-file-path AS CHAR NO-UNDO.

    SYSTEM-DIALOG GET-DIR v-file-path
        TITLE "Select Path to R-code Path".

    fiRoot:SCREEN-VALUE = v-file-path.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiVersion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiVersion wWin
ON LEAVE OF fiVersion IN FRAME fMain /* Site Name (No spaces) */
DO:
  assign fiVersion.
  if index(fiVersion, " ") gt 0 then do:
    message "Site Name cannot contain spaces!" view-as alert-box.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
def var cLine as char.
def var cLastSection as char.
def var iLastSectionLine as int.
def var cLastValue as char.
def var iCnt as int no-undo.

if search("n:\rcode\advantzware.ini") ne ? then do:

    input from value("n:\rcode\advantzware.ini").
    iCnt = 0. iLastSectionLine = 0.
    repeat:
        cLine = "".
        import delimiter "|" cLine.
        iCnt = iCnt + 1.
        if cLine begins "[" then do:
          cLastSection = cLine.
          iLastSectionLine = iCnt.
        end.
        else do:
          cLastValue = cLine.
          if iLastSectionLine eq iCnt - 1 then do:
            case cLastSection:
              when "[SiteName]" then do:
              end.
              when "[dbdir]" then do:
                fiDB:screen-value = cLastValue.
              end.
              when "[progressDir]" then do:
                fiProDir:screen-value = cLastValue.
              end.
              when "[backupDir]" then do:
                fiBack:screen-value = cLastValue.
              end.
              when "[rootFolder]" then do:
                fiRoot:screen-value = cLastValue.           
              end.
              when "[rcodeFolder]" then do:
                 fiRcode:screen-value = cLastValue.
              end.
            end case.
          end. /* if found a value */
            
        end. /* else do */
    end. /* repeat */
    input close.
end. /* if search ... */
/* Include custom  Main Block code for SmartWindows. */

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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  cAdvantzwareINI = SEARCH("..\rcode\advantzware.ini").
  IF cAdvantzwareINI NE ? THEN DO:
    INPUT STREAM sIni FROM VALUE(cAdvantzwareINI).
    REPEAT:
      cInput = "".
      IMPORT cInput.
      IF cInput EQ "" THEN NEXT.
      cInput = TRIM(cInput).
      IF cInput BEGINS "[" THEN DO:
        cInput = TRIM(cInput, "[").
        cInput = TRIM(cInput, "]").
        cTAG = cInput.
      END. /* Is a tag */
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
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
        fiProDir:SCREEN-VALUE = cProgressDir
        fiDB:SCREEN-VALUE = cDbDir
        fiBack:SCREEN-VALUE = cBackupDir
        fiRoot:SCREEN-VALUE = cRootFolder
        fiRcode:SCREEN-VALUE = cRcodeFolder
        fiVersion:SCREEN-VALUE = cSiteName
        .
     END.
  END. /* Is an INI file */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

