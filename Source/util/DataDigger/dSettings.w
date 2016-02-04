&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME frSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS frSettings 
/*----------------------------------------------------------------------------
              (c) Copyright VCD 1999

 Naam         : d-
 Pakket       : 
 Omschrijving : 
 Parameters   : 
 Opmerkingen  : 
 -----------------------------------------------------------------------------
 versie datum      door omschrijving
 ------ ---------- ---- ------------------------------------------------------
 1.0         -1999      Initieel aangemaakt
----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{ DataDigger.i }

/* Parameters Definitions ---                                           */
define input  parameter pcSettingsFile as character   no-undo.
define output parameter plSuccess      as logical     no-undo.

/* Global Variable Definitions ---                                      */
define variable gcDB                   as character  no-undo.
define variable gcTable                as character  no-undo.
define variable gcFileName             as character  no-undo.
define variable gcLastFile             as character  no-undo.
define variable gcFileViewCmd          as character  no-undo.
define variable glNoRecordsWarning     as logical     no-undo.
define variable gcSessionNumericFormat as character   no-undo.
define variable gcSessionDateFormat    as character   no-undo.
define variable glAborted              as logical     no-undo.

define stream strDump.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frSettings

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 btnRawEdit cbDoubleClick ~
tgSaveDataFilters ficSettingsFile tgAddDataColumnForRecid ~
tgAddDataColumnForRowid tgAutoExpandQueryEditor tgShowHiddenTables ~
fiMaxColumns fiMaxQueryHistory cbViewType tgResetWindowPosition ~
tgResetBrowseColumns tgResetMessages BtnOK BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS cbDoubleClick tgSaveDataFilters ~
ficSettingsFile tgAddDataColumnForRecid tgAddDataColumnForRowid ~
tgAutoExpandQueryEditor tgShowHiddenTables fiMaxColumns fiMaxQueryHistory ~
cbViewType tgResetWindowPosition tgResetBrowseColumns tgResetMessages 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnRawEdit 
     LABEL "&Raw Edit" 
     SIZE-PIXELS 70 BY 24 TOOLTIP "direct editing of the settings file".

DEFINE VARIABLE cbDoubleClick AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Double click on data record" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "DUMP","EDIT","VIEW" 
     DROP-DOWN-LIST
     SIZE-PIXELS 70 BY 21 TOOLTIP "what to do when you double click on a record in the data browse" NO-UNDO.

DEFINE VARIABLE cbViewType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Default &View Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "HTML","TXT","XLS" 
     DROP-DOWN-LIST
     SIZE-PIXELS 70 BY 21 NO-UNDO.

DEFINE VARIABLE ficSettingsFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Settings file" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 400 BY 21 NO-UNDO.

DEFINE VARIABLE fiMaxColumns AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Maximum nr of &columns in data browser" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 70 BY 21 NO-UNDO.

DEFINE VARIABLE fiMaxQueryHistory AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Maximum nr of &queries to save" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 70 BY 21 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 545 BY 305.

DEFINE VARIABLE tgAddDataColumnForRecid AS LOGICAL INITIAL no 
     LABEL "Add Data Column For &Recid" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 370 BY 17 NO-UNDO.

DEFINE VARIABLE tgAddDataColumnForRowid AS LOGICAL INITIAL no 
     LABEL "Add Data Column For R&owid" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 370 BY 17 NO-UNDO.

DEFINE VARIABLE tgAutoExpandQueryEditor AS LOGICAL INITIAL no 
     LABEL "Automatically &Expand The Query Editor After Right Click On Index" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 370 BY 17 NO-UNDO.

DEFINE VARIABLE tgResetBrowseColumns AS LOGICAL INITIAL no 
     LABEL "Reset &Browse Columns" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 270 BY 17 NO-UNDO.

DEFINE VARIABLE tgResetMessages AS LOGICAL INITIAL no 
     LABEL "Reset &Messages" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 265 BY 17 NO-UNDO.

DEFINE VARIABLE tgResetWindowPosition AS LOGICAL INITIAL no 
     LABEL "Reset &Window Position And Size" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 260 BY 17 NO-UNDO.

DEFINE VARIABLE tgSaveDataFilters AS LOGICAL INITIAL no 
     LABEL "Save Data Filters" 
     VIEW-AS TOGGLE-BOX
     SIZE 74 BY .81 TOOLTIP "save and restore the filter values on the databrowse" NO-UNDO.

DEFINE VARIABLE tgShowHiddenTables AS LOGICAL INITIAL no 
     LABEL "Show &Hidden Tables In Table Browse" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 370 BY 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frSettings
     btnRawEdit AT Y 5 X 485 WIDGET-ID 90
     cbDoubleClick AT Y 235 X 260 COLON-ALIGNED WIDGET-ID 92
     tgSaveDataFilters AT ROW 5.29 COL 17 WIDGET-ID 94
     ficSettingsFile AT Y 5 X 70 COLON-ALIGNED WIDGET-ID 54 NO-TAB-STOP 
     tgAddDataColumnForRecid AT Y 50 X 80 WIDGET-ID 58
     tgAddDataColumnForRowid AT Y 70 X 80 WIDGET-ID 60
     tgAutoExpandQueryEditor AT Y 110 X 80 WIDGET-ID 64
     tgShowHiddenTables AT Y 130 X 80 WIDGET-ID 68
     fiMaxColumns AT Y 160 X 260 COLON-ALIGNED WIDGET-ID 62
     fiMaxQueryHistory AT Y 185 X 260 COLON-ALIGNED WIDGET-ID 66
     cbViewType AT Y 210 X 260 COLON-ALIGNED WIDGET-ID 70
     tgResetWindowPosition AT Y 270 X 80 WIDGET-ID 84
     tgResetBrowseColumns AT Y 290 X 80 WIDGET-ID 86
     tgResetMessages AT Y 310 X 80 WIDGET-ID 88
     BtnOK AT Y 350 X 395 WIDGET-ID 74
     BtnCancel AT Y 350 X 475 WIDGET-ID 76
     RECT-5 AT Y 35 X 10 WIDGET-ID 72
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 568 BY 410
         TITLE "DataDigger Settings"
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB frSettings 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX frSettings
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frSettings:SCROLLABLE       = FALSE.

ASSIGN 
       ficSettingsFile:READ-ONLY IN FRAME frSettings        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME frSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frSettings frSettings
ON WINDOW-CLOSE OF FRAME frSettings /* DataDigger Settings */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK frSettings
ON CHOOSE OF BtnOK IN FRAME frSettings /* OK */
DO:
  run btnOkChoose.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRawEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRawEdit frSettings
ON CHOOSE OF btnRawEdit IN FRAME frSettings /* Raw Edit */
DO:
  /* Start default editor for ini file */
  os-command no-wait start value( pcSettingsFile ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK frSettings 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  run initializeObject.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnOkChoose frSettings 
PROCEDURE btnOkChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define variable cLine             as character   no-undo.
  define variable lHelpSectionFound as logical     no-undo.
  define variable cMessageList      as character   no-undo.
  define variable iMessage          as integer     no-undo.

  do with frame {&frame-name}:

    setRegistry('DataDigger','AddDataColumnForRecid', string(tgAddDataColumnForRecid:checked) ).
    setRegistry('DataDigger','AddDataColumnForRowid', string(tgAddDataColumnForRowid:checked) ).
    setRegistry('DataDigger','SaveDataFilters'      , string(tgSaveDataFilters      :checked) ).
    setRegistry('DataDigger','AutoExpandQueryEditor', string(tgAutoExpandQueryEditor:checked) ).
    setRegistry('DataDigger','ShowHiddenTables'     , string(tgShowHiddenTables     :checked) ).

    setRegistry('DataDigger','MaxColumns'     , fiMaxColumns:screen-value      ). 
    setRegistry('DataDigger','MaxQueryHistory', fiMaxQueryHistory:screen-value ).  
    setRegistry('DataDigger','ViewType'       , cbViewType:screen-value        ).
    setRegistry('DataDigger','DataDoubleClick', cbDoubleClick:screen-value     ).

    if tgResetWindowPosition:checked then
    do:
      setRegistry('DataDigger','Window:height', ? ).
      setRegistry('DataDigger','Window:width' , ? ).
      setRegistry('DataDigger','Window:x'     , ? ).
      setRegistry('DataDigger','Window:y'     , ? ).
    end.

    if tgResetBrowseColumns:checked then
    do:
      /* Field browse */
      setRegistry('DataDigger','ColumnSortFields', ? ).
      setRegistry('DataDigger','ColumnWidth:lShow', ? ).
      setRegistry('DataDigger','ColumnWidth:iOrder', ? ).
      setRegistry('DataDigger','ColumnWidth:cFieldName', ? ).
      setRegistry('DataDigger','ColumnWidth:cDataType', ? ).
      setRegistry('DataDigger','ColumnWidth:cFormat', ? ).
      setRegistry('DataDigger','ColumnWidth:cLabel', ? ).

      /* Table browse */
      setRegistry('DataDigger','ColumnSortTables', ? ).
      setRegistry('DataDigger','ColumnWidth:cTableName', ? ).
      setRegistry('DataDigger','ColumnWidth:cDatabase', ? ).
      setRegistry('DataDigger','ColumnWidth:iNumQueries', ? ).
      setRegistry('DataDigger','ColumnWidth:tLastUsed', ? ).

      /* Index browse */
      setRegistry('DataDigger','ColumnSortIndexes', ? ).
      setRegistry('DataDigger','ColumnWidth:cIndexName', ? ).
      setRegistry('DataDigger','ColumnWidth:cIndexFlags', ? ).
      setRegistry('DataDigger','ColumnWidth:cIndexFields', ? ).
    end. 

    if tgResetMessages:checked then
    do:
      /* Parse settings file for answers */
      input from value(pcSettingsFile).
      repeat:
        import unformatted cLine.

        /* Look for help section */
        if cLine = '[DataDigger:help]' then lHelpSectionFound = yes.
        if not lHelpSectionFound then next. 

        if cLine matches '*:answer=*' then 
          cMessageList = cMessageList + ',' + entry(1,cLine,'=').
      end. 
      input close. 

      do iMessage = 1 to num-entries(cMessageList):
        setRegistry('DataDigger:help', entry(iMessage,cMessageList), ?).
      end.
    end. 

    assign plSuccess = true.

  end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI frSettings  _DEFAULT-DISABLE
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
  HIDE FRAME frSettings.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI frSettings  _DEFAULT-ENABLE
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
  DISPLAY cbDoubleClick tgSaveDataFilters ficSettingsFile 
          tgAddDataColumnForRecid tgAddDataColumnForRowid 
          tgAutoExpandQueryEditor tgShowHiddenTables fiMaxColumns 
          fiMaxQueryHistory cbViewType tgResetWindowPosition 
          tgResetBrowseColumns tgResetMessages 
      WITH FRAME frSettings.
  ENABLE RECT-5 btnRawEdit cbDoubleClick tgSaveDataFilters ficSettingsFile 
         tgAddDataColumnForRecid tgAddDataColumnForRowid 
         tgAutoExpandQueryEditor tgShowHiddenTables fiMaxColumns 
         fiMaxQueryHistory cbViewType tgResetWindowPosition 
         tgResetBrowseColumns tgResetMessages BtnOK BtnCancel 
      WITH FRAME frSettings.
  {&OPEN-BROWSERS-IN-QUERY-frSettings}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject frSettings 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  define variable lSetting as logical     no-undo.

  do with frame {&frame-name}:
    ficSettingsFile:screen-value = pcSettingsFile.

    tgAddDataColumnForRecid:checked = logical( getRegistry('DataDigger','AddDataColumnForRecid') ) no-error.
    tgAddDataColumnForRowid:checked = logical( getRegistry('DataDigger','AddDataColumnForRowid') ) no-error.
    tgSaveDataFilters      :checked = logical( getRegistry('DataDigger','SaveDataFilters') )       no-error.
    tgAutoExpandQueryEditor:checked = logical( getRegistry('DataDigger','AutoExpandQueryEditor') ) no-error.
    tgShowHiddenTables     :checked = logical( getRegistry('DataDigger','ShowHiddenTables'     ) ) no-error.

    fiMaxColumns:screen-value      = getRegistry('DataDigger','MaxColumns') no-error.
    fiMaxQueryHistory:screen-value = getRegistry('DataDigger','MaxQueryHistory') no-error.
    cbViewType:screen-value        = getRegistry('DataDigger','ViewType') no-error.
    cbDoubleClick:screen-value     = getRegistry('DataDigger','DataDoubleClick') no-error.

  end.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

