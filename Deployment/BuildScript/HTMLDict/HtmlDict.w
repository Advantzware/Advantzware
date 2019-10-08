&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME fr-dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fr-dialog 
/*----------------------------------------------------------------------------

  Name        : htmldict.w

  Description : DataBase Dictionary Generator, part one.
                This program lists all connected DataBases, and allows selection
                of some or all to be processed.
          
                Each DataBase that is processed will generate a subdirectory with
                web pages, each corresponding to one of the tables within the
                DataBase.  Additionally, some additional 00-*.html files will be 
                created:
                
                  00-area.html      : overview of the areas of the database
                  00-crc.txt        : list of crc's for all the files in the database. This
                                      is used to determine whether the schema should be 
                                      regenerated or not.
                  00-cross.html     : cross-index of the fieldnames in the db. For each 
                                      fieldname the files are listed in which they occur
                  00-index.html     : frameset
                  00-list.html      : list of file names (left frame)
                  00-main.html      : main page with a list of the files in the db and 
                                      their description (right frame)
                  00-primeindex.txt : list with all single-field primary indexes. These 
                                      are used to provide a link to the 'master table' 
                                      of a given field when it is a field that occurs
                                      as the only field in the primary index of a table
                                      
                In the main directory the following two files are created:
                  00-index.html     : the main entry point for the list of db's. The tool
                                      generates a subdirectory for each database it processes
                                      and at the end of a generation process a new main 
                                      page is generated, holding links to all direcotories.
                  styles.css        : cascaded style sheet for the database. This file is
                                      created every time, so making changes to this file
                                      is rather pointless.
          
 -----------------------------------------------------------------------------
  date        by   description
  ----------  ---  ------------------------------------------------------
  01/11/1996  tom  created (thanks to Gerry Duprey for the idea)
  04/03/1996  mjb  changed lots of stuff to automate this.
  04/17/1996  mjb  put an html link back to the dict.html page at the bottom
                   of every table listing.
  08/30/1996  bmy  protect against hashes in URL names via encoding
  09/24/1996  dmk  changed links to use _dump-name.html instead of
                   _file-name.html to match with actual names of link files
  02/06/1998  tom  adapted to genericize
  02/06/2001  jtp  fixed *.htm -> *.html, added Next/Prev buttons
  03/12/2001  jtp  converted to two *.w files from htmldict.p
                   html-db.w & html-one.w HTML dictionary report
  03/14/2001  jtp  chaned absolute file references in the pages to relative
                   refrences.
  03/19/2001  jtp  fixed problem with "?" appearing over table
  12/03/2001  jtp  Added 'view-as' info from database
  12/06/2001  jtp  Added 'Field Cross Reference' code
  25/05/2005  pt   - Changed runtime arguments to parameters
                   - Make use of a CSS file
                   - Create an area page
                   - Cleaned up the code
                   - Use multiple frames
                   
----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Global Variable Definitions ---                                      */

/* Preprocessors */
{ version.i }
&scoped-define softwarekey VCD Automatisering\HtmlDict

define variable glAutoRun    as logical   no-undo.
define variable gcInstallDir as character  no-undo.


procedure ShellExecuteA external "shell32.dll":
  /*----------------------------------------------------------------------------
    Name         : ShellExecuteA
    Description  : Start a program with the associated executable. 
    ----------------------------------------------------------------------------*/
  define input  parameter hwnd         as long. /* Handle to parent window */
  define input  parameter lpOperation  as char. /* Operation to perform: open, print */
  define input  parameter lpFile       as char. /* Document or executable name */
  define input  parameter lpParameters as char. /* Command line parameters to executable in lpFile */
  define input  parameter lpDirectory  as char. /* Default directory */
  define input  parameter nShowCmd     as long. /* whether shown when opened: 
                                                    0 hidden, 1 normal, minimized 2, maximized 3, 
                                                    0 if lpFile is a document */
  define return parameter hInstance    as long. /* Less than or equal to 32 */
end procedure. /* ShellExecuteA */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fr-dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 slDatabase btnGenerate btnDone ~
rsFieldOrder tgUseFrames tgSkipIfUnchanged tgShowHtml tgSaveSettings ~
fiTriggerPath fiOutputDirectory btnBrowseDir fiStatus 
&Scoped-Define DISPLAYED-OBJECTS slDatabase rsFieldOrder tgUseFrames ~
tgSkipIfUnchanged tgShowHtml tgSaveSettings fiTriggerPath fiOutputDirectory ~
fiStatus fcDatabases 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcVersion fr-dialog 
FUNCTION fcVersion returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBrowseDir 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "select output directory".

DEFINE BUTTON btnDone AUTO-END-KEY 
     LABEL "&Done" 
     SIZE 14.8 BY 1.14 TOOLTIP "exit the program"
     FONT 4.

DEFINE BUTTON btnGenerate 
     LABEL "&Generate" 
     SIZE 14.8 BY 1.14 TOOLTIP "generate the html files"
     FONT 4.

DEFINE VARIABLE fcDatabases AS CHARACTER FORMAT "X(256)":U 
     LABEL "Databases" 
      VIEW-AS TEXT 
     SIZE .2 BY .57 NO-UNDO.

DEFINE VARIABLE fiOutputDirectory AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Output directory" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 TOOLTIP "the directory the files will be written to"
     FONT 4 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 90 BY 1 TOOLTIP "current status" NO-UNDO.

DEFINE VARIABLE fiTriggerPath AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Trigger path" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 TOOLTIP "the directories that will be searched for trigger-code" NO-UNDO.

DEFINE VARIABLE rsFieldOrder AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "n&ame", "_field-name",
"&order", "_order"
     SIZE 20 BY 1 TOOLTIP "how should the fields in the report be sorted" NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 10.24.

DEFINE VARIABLE slDatabase AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 20.6 BY 7.48 TOOLTIP "list of connected databases"
     FONT 4 NO-UNDO.

DEFINE VARIABLE tgSaveSettings AS LOGICAL INITIAL yes 
     LABEL "Sa&ve settings" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .86 TOOLTIP "save the settings on this screen in the registry"
     FONT 4 NO-UNDO.

DEFINE VARIABLE tgShowHtml AS LOGICAL INITIAL yes 
     LABEL "&Show HTML after generating process" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.4 BY .86
     FONT 4 NO-UNDO.

DEFINE VARIABLE tgSkipIfUnchanged AS LOGICAL INITIAL yes 
     LABEL "S&kip if schema is unchanged" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE tgUseFrames AS LOGICAL INITIAL yes 
     LABEL "Use &frames" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fr-dialog
     slDatabase AT ROW 1.48 COL 21 NO-LABEL
     btnGenerate AT ROW 1.48 COL 59
     btnDone AT ROW 1.48 COL 75
     rsFieldOrder AT ROW 4.1 COL 64 NO-LABEL
     tgUseFrames AT ROW 5.05 COL 43
     tgSkipIfUnchanged AT ROW 6.1 COL 43
     tgShowHtml AT ROW 7.05 COL 43
     tgSaveSettings AT ROW 8.05 COL 43
     fiTriggerPath AT ROW 9.1 COL 19 COLON-ALIGNED
     fiOutputDirectory AT ROW 10.29 COL 19 COLON-ALIGNED
     btnBrowseDir AT ROW 10.29 COL 86
     fiStatus AT ROW 11.48 COL 1 NO-LABEL
     fcDatabases AT ROW 1.48 COL 18.8 COLON-ALIGNED
     "Initially sort fields by :" VIEW-AS TEXT
          SIZE 20 BY .86 AT ROW 4.1 COL 43
     RECT-6 AT ROW 1.24 COL 1
     SPACE(0.00) SKIP(1.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "HTML generator"
         DEFAULT-BUTTON btnGenerate CANCEL-BUTTON btnDone.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fr-dialog 
/* ************************* Included-Libraries *********************** */

/* {src/template/dialog.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX fr-dialog
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fr-dialog:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN fcDatabases IN FRAME fr-dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME fr-dialog
   ALIGN-L                                                              */
ASSIGN 
       fiStatus:READ-ONLY IN FRAME fr-dialog        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fr-dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fr-dialog fr-dialog
ON WINDOW-CLOSE OF FRAME fr-dialog /* HTML generator */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseDir fr-dialog
ON CHOOSE OF btnBrowseDir IN FRAME fr-dialog /* ... */
do:

  run btnBrowseDirChoose.

end. /* choose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGenerate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGenerate fr-dialog
ON CHOOSE OF btnGenerate IN FRAME fr-dialog /* Generate */
do:
  run btnGenerateChoose.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fcDatabases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcDatabases fr-dialog
ON ENTRY OF fcDatabases IN FRAME fr-dialog /* Databases */
DO:
  apply 'entry' to slDatabase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fr-dialog 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Show progress */
&if integer(entry(1,proversion,'.')) >= 9 &then
  subscribe to "htmlDictStatus" anywhere.
&endif
  

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:

  frame {&frame-name}:title = frame {&frame-name}:title + ' build ' + fcVersion().
  run initializeObject.
  run enable_UI.

  run initializeScreen.

  if glAutoRun = true then
    run btnGenerateChoose.
  else 
    wait-for GO OF FRAME {&FRAME-NAME}.
end.


/* Save settings of the screen */
if tgSaveSettings:checked in frame {&frame-name} then
  run saveSettings.

run disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browseForFolder fr-dialog 
PROCEDURE browseForFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define input  parameter pcTitle          as character  no-undo.
  define output parameter pcSelectedFolder as character  no-undo.
  
  define variable oServer as com-handle no-undo.
  define variable oFolder as com-handle no-undo.
  define variable oParent as com-handle no-undo.
  define variable cFolder as character  no-undo.
  define variable iCount  as integer    no-undo.
  
  if pcTitle = '' or pcTitle = ? then pcTitle = 'Selecteer een directory.'.

  create 'Shell.Application' oServer.
  
  oFolder = oServer:BrowseForFolder(current-window:hwnd,pcTitle,0).
  
  if valid-handle(oFolder) = True then
  do:
    assign cFolder = oFolder:Title
           oParent = oFolder:ParentFolder
           iCount  = 0.

    repeat:
      if iCount >= oParent:Items:Count then leave.

      if oParent:Items:Item(iCount):Name = cFolder then
      do:
        pcSelectedFolder = oParent:Items:Item(iCount):Path.
        leave.
      end.
      assign iCount = iCount + 1.
    end.
  end.
  
  release object oParent no-error.
  release object oFolder no-error.
  release object oServer no-error.

end procedure. /* browseForFolder */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnBrowseDirChoose fr-dialog 
PROCEDURE btnBrowseDirChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define variable cFolderName as character no-undo.

  do with frame {&frame-name}:
  
    /* Pre v10 solution */
    &if integer(entry(1,proversion,'.')) >= 10 &then
    
      /* This only works for v10+ */
      system-dialog get-dir cFolderName
          initial-dir fiOutputDirectory:screen-value
          title 'Select directory'.
    &else
    
      run browseForFolder
        ( input 'Select directory'
        , output cFolderName
        ).
    &endif
  
    if cFolderName ne ? then
      fiOutputDirectory:screen-value = cFolderName.
  end. 

end procedure. /* btnBrowseDirChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnGenerateChoose fr-dialog 
PROCEDURE btnGenerateChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define variable iDb           as integer    no-undo.
  define variable cDatabaseList as character  no-undo.
  define variable cSep          as character  no-undo.
  define variable cMainPage     as character  no-undo.
  define variable iReturnResult as integer    no-undo.

  do with frame {&frame-name}:
  
    /* Make a list of selected databases */
    do iDb = 1 to num-entries(slDatabase:list-items):
      if slDatabase:is-selected( iDb ) then
        assign 
          cDatabaseList = cDatabaseList + cSep + entry(iDb,slDatabase:list-items)
          cSep = ','.
    end.

    /* Show hourglass */
    session:set-wait-state('general':u).

    /* Create pages for tables */
    run value( search(gcInstallDir + 'HtmlDict2.p') )
      ( input cDatabaseList
      , input fiOutputDirectory:screen-value
      , input tgUseFrames:checked
      , input tgSkipIfUnchanged:checked 
      , input fiTriggerPath:screen-value
      , input rsFieldOrder:screen-value
      ).

    /* Show status */
    run htmlDictStatus ( input 'Alles klar, Herr Kommissar!').
  
    /* Hide hourglass */
    session:set-wait-state('':u).
  
    /* Show the html when we're done. */
    if tgShowHtml:checked then
    do:
      cMainPage = substitute("&1/00-index.html",fiOutputDirectory:screen-value).
      run ShellExecuteA(0, "open", cMainPage, "", "", 0, output iReturnResult).
    end.
  end.
end procedure. /* btnGenerateChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fr-dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fr-dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fr-dialog  _DEFAULT-ENABLE
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
  DISPLAY slDatabase rsFieldOrder tgUseFrames tgSkipIfUnchanged tgShowHtml 
          tgSaveSettings fiTriggerPath fiOutputDirectory fiStatus fcDatabases 
      WITH FRAME fr-dialog.
  ENABLE RECT-6 slDatabase btnGenerate btnDone rsFieldOrder tgUseFrames 
         tgSkipIfUnchanged tgShowHtml tgSaveSettings fiTriggerPath 
         fiOutputDirectory btnBrowseDir fiStatus 
      WITH FRAME fr-dialog.
  {&OPEN-BROWSERS-IN-QUERY-fr-dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRegistryKey fr-dialog 
PROCEDURE getRegistryKey :
/*----------------------------------------------------------------------------
  Name         : getRegistryKey

  Description  : get a key in the registry from HKEY_CURRENT_USER/software .

  Parameters   : I  char ipcSection = the registry section
                 I  char ipcKey     = the key
                  O char ipcValue   = result value
  ----------------------------------------------------------------------------

  When        Who What
  ----------- --- ------------------------------------------------------------
  09-07-2005  pt  Created
  ----------------------------------------------------------------------------*/

  define input  parameter ipcSection as character  no-undo.
  define input  parameter ipcKey     as character  no-undo.
  define output parameter opcValue   as character  no-undo.

  load "Software" base-key "HKEY_CURRENT_USER".
  use "Software".
  get-key-value section ipcSection
                key ipcKey
                value opcValue.
  unload "Software".

  return.

end procedure. /* getRegistryKey */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HtmlDictStatus fr-dialog 
PROCEDURE HtmlDictStatus :
/*----------------------------------------------------------------------------
    Name         : HtmlDictStatus
  
    Description  : Show the current progress on the screen. 
  
    Parameters   : I  character  picStatusText = text to show
    ----------------------------------------------------------------------------
  
    When        Who What
    ----------- --- ------------------------------------------------------------
    26-05-2005  pt  Created
    ----------------------------------------------------------------------------*/

  define input  parameter picStatusText as character  no-undo.

  do with frame {&frame-name}:
    fiStatus:screen-value = string(time,'hh:mm:ss') + ' ' + picStatusText.
    process events.
  end.

end procedure. /* HtmlDictStatus */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fr-dialog 
PROCEDURE initializeObject :
/*----------------------------------------------------------------------------
  Name         : initializeObject

  Description  : local initialize 

  Parameters   : <none>
  ----------------------------------------------------------------------------

  When        Who What
  ----------- --- ------------------------------------------------------------
  26-05-2005  pt  Created
  ----------------------------------------------------------------------------*/

  define variable iDb      as integer    no-undo.
           
  /* Determine the dir where all the install files reside */
  &if "{&uib_is_running}" <> "" &then
    gcInstallDir = 'c:\HtmlDict\'.
  &else
    gcInstallDir = replace(this-procedure:file-name,'/', '\').
    gcInstallDir = substring(gcInstallDir,1,r-index(gcInstallDir,'~\':u), 'character':u).
  &endif
  
  .ASSIGN gcInstallDir = 'c:\HtmlDict\'.

  do with frame {&frame-name}:
    assign slDatabase:list-items = ''.

    do iDb = 1 to num-dbs:
      slDatabase:add-last( ldbname(iDb) ).
    end.

    slDatabase:add-last( '_progress' ).
  end.

  /* subscribe to HtmlDictStatus event */
  {&v9_begin}
  subscribe to "HtmlDictStatus" anywhere.
  {&v9_end}

end procedure. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeScreen fr-dialog 
PROCEDURE initializeScreen :
/*----------------------------------------------------------------------------
    Name         : initializeScreen
  
    Description  : get screen settings from registry
  
    Parameters   : <none>
    ----------------------------------------------------------------------------
  
    When        Who What
    ----------- --- ------------------------------------------------------------
    26-05-2005  pt  Created
    ----------------------------------------------------------------------------*/
  
  define variable cProperty               as character  no-undo.
  define variable cSetting                as character  no-undo.
  define variable iSetting                as integer    no-undo.
  define variable iPar                    as integer    no-undo.
  define variable cIconFile               as character  no-undo.

  define variable cWinX                   as character  no-undo.
  define variable iWinX                   as integer    no-undo.
  define variable cWinY                   as character  no-undo.
  define variable iWinY                   as integer    no-undo.
  define variable cFieldOrder             as character  no-undo.
  define variable cUseFrames              as character  no-undo.
  define variable cSkipIfUnchanged        as character  no-undo.
  define variable cShowHtml               as character  no-undo.
  define variable cOutputDirectory        as character  no-undo.
  define variable cSaveSettings           as character  no-undo.
  define variable cAutoRun                as character  no-undo.
  define variable cTriggerPath            as character  no-undo.

  do with frame {&frame-name}:

    /* If pre-10 then disable browsedir button */
    &if integer(entry(1,proversion,'.')) < 10 &then
      btnBrowseDir:visible = false.
    &endif

    /* Take settings from registry */

    /* x pos of window */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'x'
                       , output cWinX 
                       ).

    /* x pos of window */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'y'
                       , output cWinY 
                       ).

    /* Field sort order */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'FieldOrder'
                       , output cFieldOrder
                       ).
  
    /* use frames */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'UseFrames'
                       , output cUseFrames
                       ).
    
    /* check update */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'SkipIfUnchanged'
                       , output cSkipIfUnchanged
                       ).
  
  
    /* Show HTML after generation process */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'ShowHtml'
                       , output cShowHtml
                       ).
  
  
    /* Save settings */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'SaveSettings'
                       , output cSaveSettings
                       ).
    
  
    /* Output directory */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'OutputDirectory'
                       , output cOutputDirectory
                       ).

    /* Trigger path */
    run getRegistryKey ( input '{&softwareKey}' 
                       , input 'TriggerPath'
                       , output cTriggerPath
                       ).

    /* Take settings from startup parameters */
    do iPar = 1 to num-entries(session:parameter,' ').
      if num-entries( entry(iPar,session:parameter,' '),'=') = 2 then 
      do:
        cProperty = entry(1,entry(iPar,session:parameter,' '),'='). 
        cSetting  = entry(2,entry(iPar,session:parameter,' '),'='). 

        case cProperty:
          when 'x'                      then cWinX            = cSetting.
          when 'y'                      then cWinY            = cSetting.
          when 'FieldOrder'             then cFieldOrder      = cSetting.
          when 'UseFrames'              then cUseFrames       = cSetting.
          when 'SkipIfUnchanged'        then cSkipIfUnchanged = cSetting.
          when 'ShowHtml'               then cShowHtml        = cSetting.
          when 'SaveSettings'           then cSaveSettings    = cSetting.
          when 'OutputDirectory'        then cOutputDirectory = cSetting.
          when 'TriggerPath'            then cTriggerPath     = cSetting.
          when 'AutoRun'                then cAutoRun         = cSetting.
        end case.

      end.
    end.


    /* Apply settings */
    assign iWinX = integer(cWinX) no-error.
    assign iWinY = integer(cWinY) no-error.
  
    if iWinX < session:width-pixels then frame {&FRAME-NAME}:x = iWinX.
    if iWinY < session:height-pixels then frame {&FRAME-NAME}:y = iWinY.

    if cUseFrames <> ? and cUseFrames <> '' then
      assign tgUseFrames:checked = (cUseFrames = 'TRUE').
    
    if cSkipIfUnchanged <> ? and cSkipIfUnchanged <> '' then
      assign tgSkipIfUnchanged:checked = (cSkipIfUnchanged = 'TRUE').

    if cShowHtml <> ? and cShowHtml <> '' then
      assign tgShowHtml:checked = (cShowHtml = 'TRUE').

    if cOutputDirectory <> ? and cOutputDirectory <> '' then
      assign fiOutputDirectory:screen-value = cOutputDirectory.

    if cTriggerPath <> ? and cTriggerPath <> '' then
      assign fiTriggerPath:screen-value = cTriggerPath.

    if cFieldOrder <> ? and cFieldOrder <> '' then
      assign rsFieldOrder:screen-value = cFieldOrder.

    if cSaveSettings <> ? and cSaveSettings <> '' then
      assign tgSaveSettings:checked = (cSaveSettings = 'TRUE').

    /*     message                                    */
    /*                                                */
    /*      skip 'WinX            =' cWinX            */
    /*      skip 'WinY            =' cWinY            */
    /*      skip 'FieldOrder      =' cFieldOrder      */
    /*      skip 'UseFrames       =' cUseFrames       */
    /*      skip 'SkipIfUnchanged =' cSkipIfUnchanged */
    /*      skip 'ShowHtml        =' cShowHtml        */
    /*      skip 'SaveSettings    =' cSaveSettings    */
    /*      skip 'OutputDirectory =' cOutputDirectory */
    /*      skip 'AutoRun         =' cAutoRun         */
    /*                                                */
    /*       view-as alert-box info buttons ok.       */

    /* Selected items in selection list */
    slDatabase:screen-value = slDatabase:list-items.

    /* Auto-go? */
    glAutoRun = ( cAutoRun = 'TRUE' ).

    /* No database connected? */
    if num-dbs = 0 then do:
      run htmlDictStatus ( input 'No databases connected.').

      disable btnGenerate.
      assign glAutoRun = false.
    end.
  end.

end procedure. /* initializeScreen */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSettings fr-dialog 
PROCEDURE saveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Version info */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'Build'
                       , input fcVersion()
                       ).

    /* x */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'x'
                       , input string( frame {&FRAME-NAME}:x )
                       ).
  
    /* y */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'y'
                       , input string( frame {&FRAME-NAME}:y )
                       ).
  
    /* use frames */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'UseFrames'
                       , input string( tgUseFrames:checked,'TRUE/FALSE' )
                       ).
  
    /* check update */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'SkipIfUnchanged'
                       , input string( tgSkipIfUnchanged:checked,'TRUE/FALSE' )
                       ).
  
  
    /* Show HTML after generation process */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'ShowHtml'
                       , input string( tgShowHtml:checked,'TRUE/FALSE' )
                       ).
  
  
    /* Output directory */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'OutputDirectory'
                       , input fiOutputDirectory:screen-value
                       ).
  
    /* Trigger path */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'TriggerPath'
                       , input fiTriggerPath:screen-value
                       ).

  
    /* Sort order */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'FieldOrder'
                       , input rsFieldOrder:screen-value 
                       ).
  
    /* Save settings */
    run setRegistryKey ( input '{&softwareKey}' 
                       , input 'SaveSettings'
                       , input string( tgSaveSettings:checked,'TRUE/FALSE' )
                       ).

end procedure. /* saveSettings */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRegistryKey fr-dialog 
PROCEDURE setRegistryKey :
/*----------------------------------------------------------------------------
  Name         : setRegistryKey

  Description  : set a key in the registry under HKEY_CURRENT_USER/software .

  Parameters   : I  char ipcSection = the registry section
                 I  char ipcKey     = the key
                 I  char ipcValue   = the value
  ----------------------------------------------------------------------------

  When        Who What
  ----------- --- ------------------------------------------------------------
  09-07-2005  pt  Created
  ----------------------------------------------------------------------------*/
    
  define input  parameter ipcSection as character  no-undo.
  define input  parameter ipcKey     as character  no-undo.
  define input  parameter ipcValue   as character  no-undo.

  load "Software" base-key "HKEY_CURRENT_USER".
  use "Software".
  put-key-value section ipcSection
                    key ipcKey
                  value ipcValue.
  unload "Software".

  return.
end procedure. /* setRegistryKey */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcVersion fr-dialog 
FUNCTION fcVersion returns character
  ( /* parameter-definitions */ ) :

  /*----------------------------------------------------------------------------
    Name         : fcVersion
    Description  : Give back the version of the program.
    ----------------------------------------------------------------------------*/

  return "{&buildnr}".   /* Function return value. */

end function. /* fcVersion */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

