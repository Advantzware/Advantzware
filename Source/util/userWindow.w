&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/userWindow.w

  Description: Display all the Usre window mainnance

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anjly

  Created:02nd Fed 2021

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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE VARIABLE lReTrigger      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE hdOutputProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdUserWindow    AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
DEFINE VARIABLE selectedProgram AS CHARACTER NO-UNDO.
DEFINE VARIABLE selectedUser    AS CHARACTER NO-UNDO.
DEFINE VARIABLE selectedRowId   AS ROWID     NO-UNDO.
DEFINE VARIABLE SelectedRow     AS INTEGER   NO-UNDO.
    
DEFINE TEMP-TABLE ttUserWindow 
    FIELDS cselect             AS LOGICAL  
    FIELDS usrID               AS CHARACTER
    FIELDS programname         AS CHARACTER
    FIELDS winwidth            AS DECIMAL
    FIELDS winheight           AS DECIMAL
    FIELDS cstate              AS CHARACTER 
    FIELDS prgtitle            AS CHARACTER 
    FIELDS mnemonic            AS CHARACTER 
    INDEX userprogram IS PRIMARY UNIQUE usrID programname 
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME ttUserWindow

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttUserWindow

/* Definitions for BROWSE ttUserWindow                                  */
&Scoped-define FIELDS-IN-QUERY-ttUserWindow ttUserWindow.cselect ttUserWindow.usrID ttUserWindow.mnemonic ttUserWindow.prgtitle ttUserWindow.programname ttUserWindow.winwidth ttUserWindow.winheight ttUserWindow.cstate   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttUserWindow ttUserWindow.cselect   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttUserWindow ttUserWindow
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttUserWindow ttUserWindow
&Scoped-define SELF-NAME ttUserWindow
&Scoped-define QUERY-STRING-ttUserWindow FOR EACH ttUserWindow ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-ttUserWindow OPEN QUERY {&SELF-NAME} FOR EACH ttUserWindow ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-ttUserWindow ttUserWindow
&Scoped-define FIRST-TABLE-IN-QUERY-ttUserWindow ttUserWindow


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-ttUserWindow}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDelete RECT-13 RECT-15 btFilter fiUserID ~
fiHotKey fiTitle cbState btExit fiProgramName ttUserWindow 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiHotKey fiTitle cbState ~
fiProgramName fiPrimaryIDLabel fiHotKeyLabel fiTitleLabel fiStateLabel ~
fiClientIDLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "Delete" 
     SIZE 7.2 BY 1.71 TOOLTIP "Delete selected records"
     BGCOLOR 21 FGCOLOR 21 .

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "Exit" 
     SIZE 7.2 BY 1.71 TOOLTIP "Exit"
     BGCOLOR 21 FGCOLOR 21 .

DEFINE BUTTON btFilter 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U
     IMAGE-DOWN FILE "Graphics/32x32/search_hover_new.png":U
     LABEL "Filter" 
     SIZE 7.2 BY 1.71 TOOLTIP "Filter".

DEFINE VARIABLE cbState AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "ALL","0",
                     "Normal","3",
                     "Custom","2",
                     "Maximized","1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1
     FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiClientIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Program Name:" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiHotKey AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11.2 BY 1.1
     FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiHotKeyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Hot Key:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiPrimaryIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "User ID:" 
      VIEW-AS TEXT 
     SIZE 9.8 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiProgramName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.4 BY 1.1
     FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiStateLabel AS CHARACTER FORMAT "X(256)":U INITIAL "State:" 
      VIEW-AS TEXT 
     SIZE 7.2 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiTitle AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58.6 BY 1.1
     FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiTitleLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Program Title:" 
      VIEW-AS TEXT 
     SIZE 20.6 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.1
     FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 181 BY 2.52
     BGCOLOR 23 FGCOLOR 24 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 0    
     SIZE 183 BY 2.14
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .6 BY 2
     BGCOLOR 24 FGCOLOR 24 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttUserWindow FOR 
      ttUserWindow SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttUserWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttUserWindow C-Win _FREEFORM
  QUERY ttUserWindow NO-LOCK DISPLAY
      ttUserWindow.cselect LABEL-BGCOLOR 14    COLUMN-LABEL "[ ] ALL"
            WIDTH 8 VIEW-AS TOGGLE-BOX
      ttUserWindow.usrID       COLUMN-LABEL "User ID" FORMAT "x(32)":U
            WIDTH 16 LABEL-BGCOLOR 14
      ttUserWindow.mnemonic COLUMN-LABEL "Hot Key" FORMAT "x(32)":U
            WIDTH 10 LABEL-BGCOLOR 14  
      ttUserWindow.prgtitle    COLUMN-LABEL "Program Title"  FORMAT "x(50)":U
            WIDTH 50 LABEL-BGCOLOR 14   
      ttUserWindow.winwidth    COLUMN-LABEL "Width" 
            WIDTH 14 LABEL-BGCOLOR 14
      ttUserWindow.winheight   COLUMN-LABEL "Height" 
            WIDTH 14 LABEL-BGCOLOR 14
      ttUserWindow.cstate      COLUMN-LABEL "State" FORMAT "x(32)":U
            WIDTH 20 LABEL-BGCOLOR 14
      ttUserWindow.programname COLUMN-LABEL "Program Name" FORMAT "x(32)":U
            WIDTH 24 LABEL-BGCOLOR 14
     
          
      ENABLE ttUserWindow.cselect
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 182.4 BY 18.43
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btDelete AT ROW 1.14 COL 165.6 WIDGET-ID 324
     btFilter AT ROW 3.76 COL 173 WIDGET-ID 18
     fiUserID AT ROW 4.33 COL 4.2 NO-LABEL WIDGET-ID 66
     fiHotKey AT ROW 4.33 COL 31.6 COLON-ALIGNED NO-LABEL WIDGET-ID 330
     fiTitle AT ROW 4.33 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 332
     cbState AT ROW 4.33 COL 104.8 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     btExit AT ROW 1.14 COL 174.2 WIDGET-ID 320
     fiProgramName AT ROW 4.33 COL 127.6 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     ttUserWindow AT ROW 6.1 COL 183 RIGHT-ALIGNED WIDGET-ID 200
     fiPrimaryIDLabel AT ROW 3.57 COL 12.2 NO-LABEL WIDGET-ID 64
     fiHotKeyLabel AT ROW 3.57 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 326
     fiTitleLabel AT ROW 3.57 COL 64.8 NO-LABEL WIDGET-ID 328
     fiStateLabel AT ROW 3.57 COL 111.4 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fiClientIDLabel AT ROW 3.57 COL 135.4 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     RECT-13 AT ROW 3.38 COL 2 WIDGET-ID 22
     RECT-14 AT ROW 1 COL 1 WIDGET-ID 322
     RECT-15 AT ROW 3.62 COL 169.6 WIDGET-ID 334
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183.4 BY 23.67
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "User Window"
         HEIGHT             = 23.67
         WIDTH              = 183.4
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 199.8
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 199.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB ttUserWindow fiProgramName DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fiClientIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHotKeyLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPrimaryIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiStateLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTitleLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiUserID IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE ttUserWindow IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       ttUserWindow:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttUserWindow
/* Query rebuild information for BROWSE ttUserWindow
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUserWindow ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE ttUserWindow */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User Window */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Window */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete C-Win
ON CHOOSE OF btDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    APPLY 'value-changed' TO BROWSE {&Browse-name}.  
    FOR EACH ttuserwindow WHERE cselect:
        FOR FIRST userwindow EXCLUSIVE-LOCK 
            WHERE userwindow.usrID       EQ ttuserwindow.usrID 
              AND userwindow.programname EQ ttuserwindow.programname:
            DELETE userwindow.
        END.
    END.
    APPLY "CHOOSE" TO btFilter.                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    IF VALID-HANDLE(hdOutputProcs) THEN
        DELETE PROCEDURE hdOutputProcs.

    IF VALID-HANDLE(hdUserWindow) THEN
        DELETE PROCEDURE hdUserWindow.
                
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFilter C-Win
ON CHOOSE OF btFilter IN FRAME DEFAULT-FRAME /* Filter */
DO:
    DEFINE VARIABLE cQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.

    EMPTY TEMP-TABLE ttUserWindow.
    Filter-Data:
    FOR EACH UserWindow NO-LOCK 
       WHERE UserWindow.usrID EQ (IF fiUserID:SCREEN-VALUE EQ "" THEN UserWindow.usrID ELSE fiUserID:SCREEN-VALUE) 
         AND UserWindow.programName EQ (IF fiProgramName:SCREEN-VALUE EQ "" THEN UserWindow.programName ELSE fiProgramName:SCREEN-VALUE)
         AND UserWindow.state EQ (IF cbState:SCREEN-VALUE EQ "0" THEN UserWindow.state ELSE INTEGER(cbState:SCREEN-VALUE))
        :     
        FOR FIRST prgrms NO-LOCK 
            WHERE ENTRY(1,prgrms.prgmname,".") EQ ENTRY(2,UserWindow.programName,"/") 
            AND  prgrms.dir_group EQ ENTRY(1,UserWindow.programName,"/")
            AND  prgrms.prgtitle MATCHES("*" + fiTitle:SCREEN-VALUE + "*") 
            AND  prgrms.mnemonic EQ (IF fiHotKey:SCREEN-VALUE EQ "" THEN prgrms.mnemonic ELSE fiHotKey:SCREEN-VALUE) 
            :
        
            CREATE ttUserWindow.
            BUFFER-COPY UserWindow TO ttUserWindow. 
            CASE  UserWindow.state:
                WHEN 1 THEN
                    ttUserWindow.cstate = "Maximized".
                WHEN 2 THEN
                    ttUserWindow.cstate = "Custom".
                WHEN 3 THEN
                    ttUserWindow.cstate = "Normal". 
                OTHERWISE
                    ttUserWindow.cstate = "". 
            END CASE.     
            ASSIGN 
                ttUserWindow.prgtitle = prgrms.prgtitle
                ttUserWindow.mnemonic = prgrms.mnemonic.
        END.  
    END.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiHotKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiHotKey C-Win
ON HELP OF fiHotKey IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        cCompany, 
        "apiID", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        fiProgramName:SCREEN-VALUE = IF NUM-ENTRIES(returnFields, "|") GE 4 THEN
                                      ENTRY(4, returnFields, "|")
                                  ELSE
                                      "".
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME ttUserWindow
&Scoped-define SELF-NAME ttUserWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttUserWindow C-Win
ON START-SEARCH OF ttUserWindow IN FRAME DEFAULT-FRAME
DO:
       IF SELF:CURRENT-COLUMN:NAME EQ "cselect" THEN DO:
        lReTrigger = NOT lReTrigger.
        
        FOR EACH ttUserWindow:
            ttUserWindow.cselect = lReTrigger.
        END.
        
        SELF:CURRENT-COLUMN:LABEL = IF lReTrigger THEN
                                        "[*] All"
                                    ELSE
                                        "[ ] All".
                                        
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}  
         btDelete:sensitive IN FRAME {&frame-name} = CAN-FIND(FIRST ttUserWindow WHERE ttUserWindow.cSelect).  
    END.
    ELSE IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        
        IF cColumnLabel EQ cSaveLabel THEN
            lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
            hSaveLabel:LABEL-BGCOLOR = ?.
    
        ASSIGN
            hColumnLabel               = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel                 = hColumnLabel
            cSaveLabel                 = cColumnLabel
            .
        RUN pReopenBrowse.
    END.
    
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{methods/template/brwcustom.i}
{sys/inc/f3helpw.i}
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
  btDelete:sensitive IN FRAME {&frame-name} = FALSE.
  ON 'VALUE-CHANGED' OF ttUserWindow.cSelect IN BROWSE ttUserWindow
  DO:
    FIND CURRENT ttUserWindow NO-ERROR.
    IF AVAILABLE ttUserWindow  THEN
    ASSIGN
      selectedprogram = ttUserWindow.programname
      SELECTEDuser = ttUserWindow.usrid. 

      FOR FIRST ttUserWindow WHERE ttUserWindow.programname = selectedprogram AND 
          ttUserWindow.usrid EQ selecteduser:
          ttUserWindow.cSelect = ttUserWindow.cSelect:CHECKED IN BROWSE {&browse-name}.
          selectedRowId = ROWID(ttUserWindow).
      END.
  
      REPOSITION ttUserWindow TO ROWID selectedRowId. 
      btDelete:sensitive IN FRAME {&frame-name} = CAN-FIND(FIRST ttUserWindow WHERE ttUserWindow.cSelect).
  END.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName ttUserWindow
{methods/sortByProc.i "pByusrID" "ttUserWindow.usrID"}
{methods/sortByProc.i "pBywinwidth" "ttUserWindow.winwidth"}
{methods/sortByProc.i "pBywinheight" "ttUserWindow.winheight"}
{methods/sortByProc.i "pByprogramname" "ttUserWindow.programname"}
{methods/sortByProc.i "pBycstate" "ttUserWindow.cstate"}
{methods/sortByProc.i "pByprgtitle" "ttUserWindow.prgtitle"}
{methods/sortByProc.i "pBymnemonic" "ttUserWindow.mnemonic"}


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
  DISPLAY fiUserID fiHotKey fiTitle cbState fiProgramName fiPrimaryIDLabel 
          fiHotKeyLabel fiTitleLabel fiStateLabel fiClientIDLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btDelete RECT-13 RECT-15 btFilter fiUserID fiHotKey fiTitle cbState 
         btExit fiProgramName ttUserWindow 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReOpenBrowse C-Win 
PROCEDURE pReOpenBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    CASE cColumnLabel:
        WHEN "usrID" THEN
            RUN pByusrID.
        WHEN "programname" THEN
            RUN pByprogramname.
        WHEN "winheight" THEN
            RUN pBywinheight.
        WHEN "winwidth" THEN
            RUN pBywinwidth.
        WHEN "cstate" THEN
            RUN pBycstate.
        WHEN "prgtitle" THEN
            RUN pByprgtitle.
        WHEN "mnemonic" THEN
            RUN pBymnemonic.       
        OTHERWISE
            {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


