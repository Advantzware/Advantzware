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
def var cocode as char.
def var cIniLoc as char.
cocode = '001'.
cIniLoc = "c:\temp\advantzware.ini".

/* PurgeFolderNK1Desc, PurgeFolderNK1Char, PurgeFolderNames, PurgeFolderLogFiles */

define temp-table ttFolders
  field folderType as char
  field folderName as char format "x(40)"
  field keepDays as int
  .
  
  def var cInifileLoc as char no-undo.
  def var cIniLine as char no-undo.
  def var cVarName as char no-undo.
  def var cvarValue as char no-undo.
  def stream sIniFile.
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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 edDirectoryList btStartProcess ~
btCancel edProcessStatus 
&Scoped-Define DISPLAYED-OBJECTS edDirectoryList edProcessStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btStartProcess 
     LABEL "Start Process" 
     SIZE 27 BY 1.14.

DEFINE VARIABLE edDirectoryList AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 100 BY 12.86
     FONT 0 NO-UNDO.

DEFINE VARIABLE edProcessStatus AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 102 BY 15.48 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 15.95.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 16.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     edDirectoryList AT ROW 2.43 COL 8 NO-LABEL WIDGET-ID 2
     btStartProcess AT ROW 15.95 COL 24 WIDGET-ID 6
     btCancel AT ROW 16 COL 58 WIDGET-ID 8
     edProcessStatus AT ROW 18.81 COL 7 NO-LABEL WIDGET-ID 4
     "File/Folders to Purge:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 40 WIDGET-ID 10
          FGCOLOR 9 
     "File/Folders Processed" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 17.91 COL 36 WIDGET-ID 12
          FGCOLOR 9 
     RECT-1 AT ROW 1.71 COL 4 WIDGET-ID 14
     RECT-2 AT ROW 18.14 COL 4 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 33.76
         CANCEL-BUTTON btCancel WIDGET-ID 100.


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
         TITLE              = "Purge Old Log Files"
         HEIGHT             = 33.76
         WIDTH              = 113
         MAX-HEIGHT         = 33.76
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 33.76
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
ON END-ERROR OF wWin /* Purge Old Log Files */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Purge Old Log Files */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWin
ON CHOOSE OF btCancel IN FRAME fMain /* Cancel */
DO:
  run exitObject.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStartProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStartProcess wWin
ON CHOOSE OF btStartProcess IN FRAME fMain /* Start Process */
DO:
    for each ttFolders:
      edProcessStatus:INSERT-STRING(ttFolders.folderName + chr(13)).
      if ttFolders.folderType EQ "Purge" then 
        run purgeFolder (input ttFolders.folderName, input ttFolders.keepDays).
      if ttFolders.folderType EQ "Logfile" then 
        run truncateFile (input ttFolders.folderName, input ttFolders.keepDays).
      process events.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

    cIniFileLoc = cIniLoc.  
    INPUT stream sIniFile FROM VALUE(SEARCH(cIniFileLoc)).
    REPEAT:
        cIniLine = "".
        IMPORT stream sIniFile UNFORMATTED cIniLine.
        IF INDEX(cIniLine,"=") = 0 THEN NEXT.
        ASSIGN
            cVarName = ENTRY(1,cIniLine,"=")
            cVarValue = ENTRY(2,cIniLine,"=")
            .
        case cVarName:
          when "PurgeFolderNK1Desc" then 
            run processCommaDelimList (input cVarValue, "NK1Desc").
          when "PurgeFolderNK1Char" then 
            run processCommaDelimList (input cVarValue, "NK1Char").
          when "PurgeFolderNames" then 
            run processCommaDelimList (input cVarValue, "Folder").
          when "PurgeFolderLogFiles" then 
            run processCommaDelimList (input cVarValue, "LogFile").
            
        end case.
    END.
    INPUT stream sIniFile CLOSE.
    def var a as char.
    for each ttFolders:
    a = ttFolders.folderName.
    substring(a, 42) =                         
                         (if ttFolders.folderType EQ "Purge" then " Purge Older Than "
                          + string(ttFolders.keepDays) + " Days"
                         else " keep " + string(ttFolders.keepDays) + " lines")
                         + chr(13).
      edDirectoryList:INSERT-STRING(a).
    end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTTFolders wWin 
PROCEDURE createTTFolders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     def input parameter ipcFolderType as char no-undo.
     def input parameter ipcfolderName as char no-undo.
     def input parameter ipiKeepDays as int no-undo.
         create ttFolders.
         assign ttfolders.folderType = ipcFolderType
                ttfolders.folderName = ipcfolderName
                ttfolders.keepDays = ipiKeepDays
                .
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
  DISPLAY edDirectoryList edProcessStatus 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 edDirectoryList btStartProcess btCancel edProcessStatus 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processCommaDelimList wWin 
PROCEDURE processCommaDelimList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      def input parameter ipcList as char no-undo.
      def input parameter ipcListType as char no-undo.
      
      def var iElement as int.
      def var iElementInt as int.
      
      do iElement = 1 to num-entries(ipcList) by 2:
      
        iElementInt = iElement + 1.
        
        case ipcListType:
          when "NK1Desc" THEN do          :
            find first sys-ctrl no-lock 
              where sys-ctrl.company eq cocode
                and sys-ctrl.name eq entry(iElement, ipcList)     
              no-error.
            if avail sys-ctrl then do:
              run createTTfolders (input "Purge", sys-ctrl.descrip, integer(entry(iElementInt, ipcList))).
            end.              
          end.
          
          when "NK1Char" THEN do          :
            find first sys-ctrl no-lock 
              where sys-ctrl.company eq cocode
                and sys-ctrl.name eq entry(iElement, ipcList)     
              no-error.
            if avail sys-ctrl then do:
               run createTTfolders (input "Purge", sys-ctrl.char-fld, integer(entry(iElementInt, ipcList))).

            end.              
          end.
          
          when "Folder" THEN do          :

            run createTTfolders (input "Purge", entry(iElement, ipcList), integer(entry(iElementInt, ipcList))).
                       
          end.   
          when "Logfile" THEN do          :

            run createTTfolders (input "Logfile", entry(iElement, ipcList), integer(entry(iElementInt, ipcList))).
                       
          end.   
                                     
        end case.  
      end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeFolder wWin 
PROCEDURE purgeFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      def input parameter ipcDirectory as char no-undo.
      def input parameter ipiLimit as int no-undo init 30.
      def var cDirectory as char no-undo.
      cDirectory = '"' + trim(ipcDirectory) + '"'.
      file-info:file-name = cDirectory.
      if file-info:file-name eq ? or index(file-info:file-type, "D") eq 0 or
        index(file-info:file-type, "R") eq 0 then return.
         
      def var cCommand as char no-undo.
      /*
      cCommand =    
'forfiles -p "' + cDirectory + '"' + '-s -m *.* -d -' + string(ipiLimit) + ' -c "cmd /c del @path"    '.
        message "cdir" cCommand view-as alert-box.
       */
      os-command silent value(
'forfiles -p ' + cDirectory + '  -m *.* -d -' + string(7) + ' -c "cmd /c del @path"  ')
      pause.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE truncateFile wWin 
PROCEDURE truncateFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      def input parameter ipcFileName as char no-undo.
      def input parameter ipiLimit as int no-undo init 30.
      def var cFileName as char no-undo.
      def var cCommand as char no-undo.
      cFileName = trim(ipcFileName).
      file-info:file-name = cFileName.
      if file-info:file-name eq ? or index(file-info:file-type, "F") eq 0 or
        index(file-info:file-type, "R") eq 0 then return.      
        
        cCommand = 
        "powershell -Command Get-Content " + cFileName 
                          +  " -totalcount " + string(ipiLimit)
                          + " >  " + cFileName + ".tmp " 
                          .
         os-command value(cCommand).              
         os-delete value(cFileName). 
         os-rename value(cFileName + ".tmp") value(cFilename).         
 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

