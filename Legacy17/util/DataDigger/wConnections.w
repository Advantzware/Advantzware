&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Name         : wConnections.w
  Description  : Maintain connections for DataDigger
  ---------------------------------------------------------------------- 
  15-10-2009 pti Created
  ----------------------------------------------------------------------*/

/* Parameters Definitions ---                                           */

define input parameter pcCommand   as character no-undo.
define input parameter pcAttribute as character no-undo. 
define output parameter pcResult   as character no-undo. 

{ datadigger.i } 

/* /* Local Variable Definitions ---                                       */    */
/* define temp-table ttConnection no-undo                                        */
/*   field iConnectionNr as integer                                              */
/*   field cLogicalName  as character column-label "Logical Name" format "x(20)" */
/*   field cDescription  as character column-label "Description"  format "x(28)" */
/*   field cDatabaseName as character column-label "Database"     format "x(20)" */
/*   field cParameters   as character                                            */
/*   field lConnected    as logical   column-label "Con"                         */
/*   index iPrim is primary unique iConnectionNr.                                */

define variable gcRecordState as character no-undo initial 'nodata'.

/* Forward defs */
/* function getUsername     returns character in super. */
/* function getProgramDir   returns character in super. */
/* function getDatabaseList returns character in super. */
/* function getMaxLength    returns integer             */
/*     ( pcSection as character                         */
/*     ) in super.                                      */
/* function setRegistry     returns character           */
/*     ( pcSection as character                         */
/*     , pcKey     as character                         */
/*     , pcValue   as character                         */
/*     ) in super.                                      */
/* function getRegistry     returns character           */
/*   ( pcSection as character                           */
/*   , pcKey     as character                           */
/*   ) in super.                                        */
/* function getMatchesValue returns character           */
/*   ( hFillIn as handle ) in super.                    */
/* function getTableList    returns character           */
/*   ( input  pcDatabaseName     as character           */
/*   , input  pcTableFilter      as character           */
/*   , input  plShowHiddenTables as logical             */
/*   ) in super.                                        */
/* function getPrimaryFields     returns character      */
/*   ( input pcDatabaseName as character                */
/*   , input pcTableName    as character                */
/*     ) in super.                                      */
/*                                                      */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME brConnections

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttConnection

/* Definitions for BROWSE brConnections                                 */
&Scoped-define FIELDS-IN-QUERY-brConnections ttConnection.cLogicalName ttConnection.cDescription ttConnection.lConnected   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brConnections   
&Scoped-define SELF-NAME brConnections
&Scoped-define QUERY-STRING-brConnections FOR EACH ttConnection
&Scoped-define OPEN-QUERY-brConnections OPEN QUERY {&SELF-NAME} FOR EACH ttConnection.
&Scoped-define TABLES-IN-QUERY-brConnections ttConnection
&Scoped-define FIRST-TABLE-IN-QUERY-brConnections ttConnection


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-brConnections}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-20 btnNew btnConnect ~
btnDisconnect btnCancel btnTest btnDelete btnEdit btnCopy btnSave btnUndo ~
brConnections 
&Scoped-Define DISPLAYED-OBJECTS fiLogicalName fiDatabaseName edParameters ~
fiDescription 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNewConnectionNr Dialog-Frame 
FUNCTION getNewConnectionNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "go back without connecting"
     BGCOLOR 8 .

DEFINE BUTTON btnConnect DEFAULT 
     LABEL "&Connect" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "connect selected database"
     BGCOLOR 8 .

DEFINE BUTTON btnCopy 
     LABEL "&Copy" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "copy and edit currently selected connection".

DEFINE BUTTON btnDelete 
     LABEL "&Delete" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "delete the currently selected connection".

DEFINE BUTTON btnDisconnect 
     LABEL "&Disconnect" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "disconnect selected database".

DEFINE BUTTON btnEdit 
     LABEL "&Edit" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "change settings of currently selected connection".

DEFINE BUTTON btnNew 
     LABEL "&New" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "create a new connection".

DEFINE BUTTON btnSave DEFAULT 
     LABEL "&Save" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "save changes"
     BGCOLOR 8 .

DEFINE BUTTON btnTest DEFAULT 
     LABEL "&Test" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "test the currently selected connection"
     BGCOLOR 8 .

DEFINE BUTTON btnUndo DEFAULT 
     LABEL "&Undo" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "cancel changes"
     BGCOLOR 8 .

DEFINE VARIABLE edParameters AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 370 BY 115 TOOLTIP "the connection parameters for this database"
     FONT 2 NO-UNDO.

DEFINE VARIABLE fiDatabaseName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database name" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 285 BY 21 TOOLTIP "the physical database name to connect to" NO-UNDO.

DEFINE VARIABLE fiDescription AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 285 BY 21 TOOLTIP "the description of this connection" NO-UNDO.

DEFINE VARIABLE fiLogicalName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logical Name" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 285 BY 21 TOOLTIP "the logical name for this connection (no spaces)" NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 375 BY 35.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 385 BY 265.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brConnections FOR 
      ttConnection SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brConnections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brConnections Dialog-Frame _FREEFORM
  QUERY brConnections DISPLAY
      ttConnection.cLogicalName
ttConnection.cDescription
ttConnection.lConnected
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 13
          &ELSE SIZE-PIXELS 300 BY 265 &ENDIF FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiLogicalName AT Y 60 X 384 COLON-ALIGNED WIDGET-ID 4
     btnNew AT Y 20 X 314 WIDGET-ID 2
     fiDatabaseName AT Y 110 X 384 COLON-ALIGNED WIDGET-ID 6
     edParameters AT Y 150 X 309 NO-LABEL WIDGET-ID 10
     btnConnect AT Y 280 X 10 WIDGET-ID 48
     btnDisconnect AT Y 280 X 95 WIDGET-ID 52
     btnCancel AT Y 280 X 610 WIDGET-ID 50
     btnTest AT Y 20 X 519 WIDGET-ID 42
     btnDelete AT Y 20 X 464 WIDGET-ID 14
     btnEdit AT Y 20 X 414 WIDGET-ID 12
     btnCopy AT Y 20 X 364 WIDGET-ID 46
     fiDescription AT Y 85 X 384 COLON-ALIGNED WIDGET-ID 38
     btnSave AT Y 20 X 579 WIDGET-ID 22
     btnUndo AT Y 20 X 629 WIDGET-ID 24
     brConnections AT Y 5 X 0 WIDGET-ID 200
     "Connection Details" VIEW-AS TEXT
          SIZE-PIXELS 95 BY 13 AT Y 0 X 309 WIDGET-ID 30
     "Connection Parameters:" VIEW-AS TEXT
          SIZE-PIXELS 120 BY 13 AT Y 135 X 309 WIDGET-ID 18
     RECT-3 AT Y 5 X 304 WIDGET-ID 28
     RECT-20 AT Y 15 X 309 WIDGET-ID 40
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 701 BY 340
         TITLE "Database Connections"
         CANCEL-BUTTON btnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB brConnections btnUndo Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       brConnections:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR EDITOR edParameters IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDatabaseName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDescription IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLogicalName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brConnections
/* Query rebuild information for BROWSE brConnections
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttConnection.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brConnections */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Database Connections */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brConnections
&Scoped-define SELF-NAME brConnections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConnections Dialog-Frame
ON DEFAULT-ACTION OF brConnections IN FRAME Dialog-Frame
DO:
  if available ttConnection then
    if ttConnection.lConnected then 
      apply 'choose' to btnDisconnect. 
    else 
      apply 'choose' to btnConnect. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConnections Dialog-Frame
ON START-SEARCH OF brConnections IN FRAME Dialog-Frame
DO:
  run openConnectionQuery(input self:current-column:name,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConnections Dialog-Frame
ON VALUE-CHANGED OF brConnections IN FRAME Dialog-Frame
DO:
  run viewConnection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnConnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnConnect Dialog-Frame
ON CHOOSE OF btnConnect IN FRAME Dialog-Frame /* Connect */
DO:
  define variable iError as integer     no-undo.
  define variable cError as character   no-undo initial 'Errors:'.
  define variable lAlreadyConnected as logical no-undo. 

  /* Warn if already connected */
  lAlreadyConnected = (lookup( fiLogicalName:screen-value, getDatabaseList()) > 0).

  /* Try to establish a connection */
  session:set-wait-state('general').
  connect value(fiDatabaseName:screen-value)
          value(edParameters:screen-value)
          value(substitute(' -ld &1', fiLogicalName:screen-value))
    no-error.
  session:set-wait-state('').

  /* Refresh connection status */
  ttConnection.lConnected = (lookup( ttConnection.cLogicalName, getDatabaseList() ) > 0). 
  brConnections:refresh().
  run viewConnection.

  /* If no success, show why */
  if error-status:error then
  do:
    do iError = 1 TO error-status:num-messages:
      cError = substitute('&1~n&2 (&3)'
                         , cError
                         , error-status:get-message(iError)
                         , error-status:get-number(iError)
                         ).
    end.
    message cError view-as alert-box info buttons ok.
    return no-apply.
  end.
  else 
  /* Success, but report if db was already connected */
  if lAlreadyConnected then 
  do:
    message 'Database already connected' view-as alert-box info buttons ok.
    return no-apply.
  end.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy Dialog-Frame
ON CHOOSE OF btnCopy IN FRAME Dialog-Frame /* Copy */
DO:
  define variable iNewNr as integer no-undo. 
  define buffer bfOriginalConnection for ttConnection.

  gcRecordState = 'new'.

  /* Find last nr */
  iNewNr = getNewConnectionNr().

  find bfOriginalConnection 
    where rowid(bfOriginalConnection) = rowid(ttConnection).

  create ttConnection.
  buffer-copy bfOriginalConnection to ttConnection
    assign ttConnection.iConnectionNr = getNewConnectionNr()
           ttConnection.lConnected    = false.

  run viewConnection. 
  run setToolbar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete Dialog-Frame
ON CHOOSE OF btnDelete IN FRAME Dialog-Frame /* Delete */
do:
  define variable lOk as logical no-undo initial false.
  define variable iDb as integer no-undo. 

  message 'Delete this connection?' view-as alert-box info buttons yes-no-cancel update lOk.

  if lOk = true then
  do:
    /* Save to registry */
    iDb = ttConnection.iConnectionNr.
    setRegistry('Connections', substitute('&1-ldbname'    , string(iDb,'999')), ? ).
    setRegistry('Connections', substitute('&1-description', string(iDb,'999')), ? ).
    setRegistry('Connections', substitute('&1-pdbname'    , string(iDb,'999')), ? ).
    setRegistry('Connections', substitute('&1-parameters' , string(iDb,'999')), ? ).

    delete ttConnection.
    {&OPEN-QUERY-brConnections}
    run viewConnection.
  end.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDisconnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDisconnect Dialog-Frame
ON CHOOSE OF btnDisconnect IN FRAME Dialog-Frame /* Disconnect */
DO:
  DEFINE VARIABLE cCurrentDb  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lDisconnect AS LOGICAL     NO-UNDO.

  cCurrentDb = fiLogicalName:screen-value.

  /* Cannot disconnect last database */
/*   if num-dbs = 1 then                                                                                               */
/*   do:                                                                                                               */
/*     message substitute('DataDigger needs at least 1 database, so you cannot disconnect database "&1".', cCurrentDb) */
/*       view-as alert-box info.                                                                                       */
/*     return.                                                                                                         */
/*   end.                                                                                                              */

  message substitute('Are you sure you want to disconnect database "&1"?', cCurrentDb)
    view-as alert-box info buttons yes-no-cancel update lDisconnect.
  if lDisconnect <> yes then return. 

  disconnect value(cCurrentDb).

  ttConnection.lConnected = (lookup( ttConnection.cLogicalName, getDatabaseList() ) > 0). 
  brConnections:refresh().
  run viewConnection.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEdit Dialog-Frame
ON CHOOSE OF btnEdit IN FRAME Dialog-Frame /* Edit */
DO:
  gcRecordState = 'edit'.
  run setToolbar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNew Dialog-Frame
ON CHOOSE OF btnNew IN FRAME Dialog-Frame /* New */
DO:
  gcRecordState = 'new'.

  create ttConnection.
  assign ttConnection.iConnectionNr = getNewConnectionNr().

  run viewConnection. 
  run setToolbar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
or 'RETURN' of fiLogicalName, fiDescription, fiDatabaseName
DO:
  define variable iDb         as integer no-undo. 
  define variable rConnection as rowid   no-undo. 

  /* No spaces in logical name */
  fiLogicalName:screen-value = replace(fiLogicalName:screen-value,' ','_').

  assign 
    ttConnection.cLogicalName  = fiLogicalName:screen-value 
    ttConnection.cDescription  = fiDescription:screen-value 
    ttConnection.cDatabaseName = fiDatabaseName:screen-value
    ttConnection.cParameters   = edParameters:screen-value   
    .

  /* Save to registry */
  iDb = ttConnection.iConnectionNr.
  setRegistry('Connections', substitute('&1-ldbname'    , string(iDb,'999')), ttConnection.cLogicalName  ).
  setRegistry('Connections', substitute('&1-description', string(iDb,'999')), ttConnection.cDescription  ).
  setRegistry('Connections', substitute('&1-pdbname'    , string(iDb,'999')), ttConnection.cDatabaseName ).
  setRegistry('Connections', substitute('&1-parameters' , string(iDb,'999')), replace(ttConnection.cParameters,'~n',chr(1))  ).

  rConnection = rowid(ttConnection).

  run openConnectionQuery(?,?).

  run viewConnection. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTest Dialog-Frame
ON CHOOSE OF btnTest IN FRAME Dialog-Frame /* Test */
DO:
  define variable iError as integer     no-undo.
  define variable cError as character   no-undo initial 'Errors:'.
  define variable lAlreadyConnected as logical no-undo. 

  lAlreadyConnected = (lookup( fiLogicalName:screen-value, getDatabaseList()) > 0).
    
  /* Try to establish a connection */
  session:set-wait-state('general').
  connect value(fiDatabaseName:screen-value)
          value(edParameters:screen-value)
          value(substitute(' -ld &1', fiLogicalName:screen-value))
    no-error.
  session:set-wait-state('').

  /* If no success, show why */
  if error-status:error then
  do:
    do iError = 1 TO error-status:num-messages:
      cError = substitute('&1~n&2 (&3)'
                         , cError
                         , error-status:get-message(iError)
                         , error-status:get-number(iError)
                         ).
    end.
    message cError view-as alert-box info buttons ok.
  end.
  else 
  do:
    /* Otherwise disconnect the db since it's only a test */
    if not lAlreadyConnected then
      disconnect value(ldbname(num-dbs)).
    message 'Connection successful' view-as alert-box info buttons ok.
  end.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUndo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUndo Dialog-Frame
ON CHOOSE OF btnUndo IN FRAME Dialog-Frame /* Undo */
DO:
  if gcRecordState = 'new' then delete ttConnection.

  {&OPEN-QUERY-brConnections}
  run viewConnection.

  if can-find(first ttConnection) then
    gcRecordState = 'display'.
  else 
    gcRecordState = 'nodata'.

  run setToolbar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLogicalName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLogicalName Dialog-Frame
ON ANY-PRINTABLE OF fiLogicalName IN FRAME Dialog-Frame /* Logical Name */
DO:
  if lastkey = 32 then 
  do:
    apply '_'.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  run initializeObject.

  case pcCommand:
    when 'connect'        then run connectDatabase(input pcAttribute, output pcResult).
    when 'getConnections' then run getConnections(output pcResult).
    when 'UI'             then do:
      RUN enable_UI.
      apply "value-changed" to brConnections in frame {&frame-name}.

      WAIT-FOR GO OF FRAME {&FRAME-NAME}.
    end.
  end case.

END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectDatabase Dialog-Frame 
PROCEDURE connectDatabase :
/*------------------------------------------------------------------------
  Name         : connectDatabase
  Description  : Try to connect to a given database. 
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/
  
  define input parameter pcConnection as character no-undo. 
  define output parameter pcError as character no-undo. 

  define variable iError as integer     no-undo.
  define buffer bConnection for ttConnection. 

  /* Find the connection */
  find bConnection where bConnection.cLogicalName = pcConnection no-lock no-error. 
  if not available bConnection then 
  do:
    assign pcError = 'No such connection known'.
    return. 
  end.

  /* Try to establish a connection */
  session:set-wait-state('general').
  connect value(bConnection.cDatabaseName)
          value(bConnection.cParameters)
          value(substitute(' -ld &1', bConnection.cLogicalName))
    no-error.
  session:set-wait-state('').

  /* If no success, show why */
  if error-status:error then
  do:
    pcError = 'Error connecting database:'.

    do iError = 1 TO error-status:num-messages:
      pcError = substitute('&1~n&2 (&3)'
                         , pcError
                         , error-status:get-message(iError)
                         , error-status:get-number(iError)
                         ).
    end.
    return.
  end.

END PROCEDURE. /* connectDatabase */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fiLogicalName fiDatabaseName edParameters fiDescription 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-20 btnNew btnConnect btnDisconnect btnCancel btnTest 
         btnDelete btnEdit btnCopy btnSave btnUndo brConnections 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConnections Dialog-Frame 
PROCEDURE getConnections :
/*------------------------------------------------------------------------
  Name         : getConnections
  Description  : Return a comma separated list of all connections
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define output parameter pcConnectionList as character no-undo. 

  define buffer bConnection for ttConnection. 

  for each bConnection by bConnection.cLogicalName:
    pcConnectionList = pcConnectionList + bConnection.cLogicalName + ','.
  end.
  
  pcConnectionList = trim(pcConnectionList,',').

end procedure. /* getConnections */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------
  Name         : initializeObject
  Description  : Prepare the program. 
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/

  empty temp-table ttConnection.

  define variable iConnection as integer   no-undo. 
  define variable cSetting    as character no-undo. 
  
  /* Arbitrarily test for max 999 connections 
   * connection numbers need to be sequential
   */
  connectionLoop:
  do iConnection = 1 to 999:

    /* Find the ID of the connection. */
    cSetting = getRegistry('Connections', substitute('&1-ldbname',string(iConnection,'999'))).
    if cSetting <> ? then
    do:
      create ttConnection.
      ttConnection.iConnectionNr = iConnection.
      ttConnection.cLogicalName  = cSetting.
    end.
    else leave connectionLoop.
    
    /* The connection exists in the INI file, now read the rest of the info */
    ttConnection.cDescription  = getRegistry('Connections', substitute('&1-description',string(iConnection,'999'))).
    ttConnection.cDatabaseName = getRegistry('Connections', substitute('&1-pdbname'    ,string(iConnection,'999'))).
    ttConnection.cParameters   = getRegistry('Connections', substitute('&1-parameters' ,string(iConnection,'999'))).
    ttConnection.cParameters   = replace(ttConnection.cParameters,chr(1),'~n').
    ttConnection.lConnected    = (lookup( ttConnection.cLogicalName, getDatabaseList() ) > 0).

    /* Protect against blank value */
    if ttConnection.cDescription  = ? then ttConnection.cDescription  = "".
    if ttConnection.cDatabaseName = ? then ttConnection.cDatabaseName = "".
    if ttConnection.cParameters   = ? then ttConnection.cParameters   = "".

  end.

  /* Get sort for Connections */
  do with frame {&frame-name}:
    cSetting = getRegistry('DataDigger','ColumnSortConnections').
    if cSetting <> ? then
      brConnections:set-sort-arrow(integer(entry(1,cSetting)), logical(entry(2,cSetting)) ).
  end.

  run openConnectionQuery(?,?).

end procedure. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openConnectionQuery Dialog-Frame 
PROCEDURE openConnectionQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input parameter pcSortField  as character   no-undo.
  define input parameter plAscending  as logical     no-undo.

  define variable rCurrentRecord   as rowid       no-undo. 
  define variable lAscending       as logical     no-undo.
  define variable cOldSort         as character   no-undo. 
  define variable cNewSort         as character   no-undo. 
  define variable cQuery           as character   no-undo.
  define variable hQuery           as handle      no-undo.

  do with frame {&frame-name}:

    /* Protect routine against invalid input */
    if pcSortField = '' then pcSortField = ?.

    /* Remember record we're on */
    if brConnections:num-selected-rows > 0 then 
      rCurrentRecord = brConnections:query:get-buffer-handle(1):rowid.

    /* Find out what the current sort is */
    run getColumnSort(input brConnections:handle, output cOldSort, output lAscending).

    /* If no new sortfield is provided, we don't want to change the sort.
     * This happens when we press the filter button.
     */
    if pcSortField = ? then
      assign 
        cNewSort   = cOldSort
        lAscending = lAscending. /* dont change order */
    else
    if pcSortField = cOldSort then
      assign 
        cNewSort   = cOldSort
        lAscending = not lAscending. /* invert order */
    else
      /* New field */
      assign 
        cNewSort   = pcSortField
        lAscending = true.

    /* Sort direction might be overruled */
    if plAscending <> ? then lAscending = plAscending.

    /* Wich column should have what arrow? */
    run setSortArrow(brConnections:handle, cNewSort, lAscending).

    /* Rebuild the query */
    if valid-handle(brConnections:query) then do:
      brConnections:query:query-close().
      delete object brConnections:query.
    end.

    create query hQuery.
    hQuery:set-buffers(buffer ttConnection:handle).

    /* Build the query */
    cQuery = 'for each ttConnection where true'.
    cQuery = substitute("&1 by &2 &3", cQuery, cNewSort, string(lAscending,'/descending')).
    hQuery:query-prepare(cQuery).
    hQuery:query-open().
    hQuery:get-first.

    /* Attach query to the browse */
    brConnections:query in frame {&frame-name} = hQuery.

    /* Jump back to selected row */
    if not hQuery:query-off-end 
      and can-find(ttConnection where rowid(ttConnection) = rCurrentRecord) then
    do:
      hQuery:reposition-to-rowid(rCurrentRecord) no-error.
      brConnections:select-focused-row().
    end.

    if available ttConnection then
      gcRecordState = 'display'.
    else
      gcRecordState = 'nodata'.
  end.
  
  run setToolbar.

END PROCEDURE. /* openConnectionQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setToolbar Dialog-Frame 
PROCEDURE setToolbar :
/*------------------------------------------------------------------------
  Name         : setToolbar
  Description  : Set the state of the icons on the toolbar
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/

  do with frame {&frame-name}:
    disable btnNew btnCopy btnEdit btnDelete btnTest btnSave btnUndo 
            fiLogicalName fiDescription fiDatabaseName edParameters
            .
  
    case gcRecordState:
      when 'nodata'  then enable btnNew  .
      when 'display' then enable btnNew  btnCopy btnEdit btnDelete btnTest.
      when 'edit'    then enable btnTest btnSave btnUndo fiLogicalName fiDescription fiDatabaseName edParameters.
      when 'new'     then enable btnTest btnSave btnUndo fiLogicalName fiDescription fiDatabaseName edParameters.
    end case.

    .brConnections:sensitive = (gcRecordState = 'display').

  end.

END PROCEDURE. /* setToolbar */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewConnection Dialog-Frame 
PROCEDURE viewConnection :
/*------------------------------------------------------------------------
  Name         : viewConnection
  Description  : Show the details of the connection on the screen. 
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/
  
  do with frame {&frame-name}:

    if available ttConnection then
      assign 
        fiLogicalName:screen-value  = ttConnection.cLogicalName
        fiDescription:screen-value  = ttConnection.cDescription
        fiDatabaseName:screen-value = ttConnection.cDatabaseName
        edParameters:screen-value   = ttConnection.cParameters
        btnDisconnect:sensitive     = ttConnection.lConnected
        btnConnect:sensitive        = not ttConnection.lConnected
        .  
    else
      assign 
        fiLogicalName:screen-value  = ""
        fiDescription:screen-value  = ""
        fiDatabaseName:screen-value = ""
        edParameters:screen-value   = ""
        btnDisconnect:sensitive     = false
        btnConnect:sensitive        = false
        .

  end.

END PROCEDURE. /* viewConnection */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNewConnectionNr Dialog-Frame 
FUNCTION getNewConnectionNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getNewConnectionNr
  Description  : Return a nr for the new connection.
  ---------------------------------------------------------------------- 
  22-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable iNewNr as integer no-undo.
  define buffer bConnection for ttConnection. 

  find last bConnection no-lock no-error.
  iNewNr = (if available bConnection 
              then bConnection.iConnectionNr 
              else 0 ) + 1.

  return iNewNr.   /* Function return value. */

end function. /* getNewConnectionNr */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

