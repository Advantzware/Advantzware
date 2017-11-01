&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: about.w

  Description: About Box

  Input Parameters: Calling Program

  Output Parameters: <none>

  Author: Ron Stark

  Created: 03/07/98
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER callingprgm AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE callingprgm AS CHARACTER INITIAL "prgrms." NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE winReSizeDat AS CHARACTER NO-UNDO.
DEFINE VARIABLE winReSize    AS CHARACTER NO-UNDO.
DEFINE VARIABLE sizeRatio    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cWorkDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLinkName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLinkLabel   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLinkURL     AS CHARACTER NO-UNDO.

{system/sysconst.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES userControl

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame userControl.maxAllowedUsers ~
userControl.maxSessionsPerUser userControl.numUsersOverLimit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ~
userControl.maxAllowedUsers userControl.maxSessionsPerUser ~
userControl.numUsersOverLimit 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame userControl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame userControl
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH userControl SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH userControl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame userControl
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame userControl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS userControl.maxAllowedUsers ~
userControl.maxSessionsPerUser userControl.numUsersOverLimit 
&Scoped-define ENABLED-TABLES userControl
&Scoped-define FIRST-ENABLED-TABLE userControl
&Scoped-Define ENABLED-OBJECTS userScreen screenImage autoMaximize winSize ~
Btn_OK link1 link2 asiVersion link3 
&Scoped-Define DISPLAYED-FIELDS userControl.maxAllowedUsers ~
userControl.maxSessionsPerUser userControl.numUsersOverLimit 
&Scoped-define DISPLAYED-TABLES userControl
&Scoped-define FIRST-DISPLAYED-TABLE userControl
&Scoped-Define DISPLAYED-OBJECTS autoMaximize winSize physical_file ~
prgmTitle copyrite link1 link2 asiVersion link3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLinkClick Dialog-Frame 
FUNCTION fLinkClick RETURNS LOGICAL
  (iphLink AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.19.

DEFINE VARIABLE asiVersion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Version" 
      VIEW-AS TEXT 
     SIZE 19.6 BY .62
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE copyrite AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 57 BY .62 NO-UNDO.

DEFINE VARIABLE link1 AS CHARACTER FORMAT "X(256)":U INITIAL "Link 1" 
     LABEL "Link" 
      VIEW-AS TEXT 
     SIZE 52 BY .62 TOOLTIP "Click Link"
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE link2 AS CHARACTER FORMAT "X(256)":U INITIAL "Link 2" 
     LABEL "Link" 
      VIEW-AS TEXT 
     SIZE 52 BY .62 TOOLTIP "Click Link"
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE link3 AS CHARACTER FORMAT "X(256)":U INITIAL "Link 3" 
     LABEL "Link" 
      VIEW-AS TEXT 
     SIZE 52 BY .62 TOOLTIP "Click Link"
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE physical_file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Physical File" 
      VIEW-AS TEXT 
     SIZE 38 BY .62 NO-UNDO.

DEFINE VARIABLE prgmTitle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Title" 
      VIEW-AS TEXT 
     SIZE 50 BY .62 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/asiicon.ico":U
     SIZE 6.6 BY 1.52.

DEFINE IMAGE screenImage
     FILENAME "Graphics\userscreen.jpg":U
     SIZE 56 BY 9.76.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 60 BY .24
     BGCOLOR 9 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 60 BY .24
     BGCOLOR 9 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 60 BY .24
     BGCOLOR 9 .

DEFINE RECTANGLE userScreen
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 60 BY 10.71
     BGCOLOR 0 .

DEFINE VARIABLE winSize AS INTEGER INITIAL 100 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 100 HORIZONTAL 
     TIC-MARKS BOTTOM FREQUENCY 5
     SIZE 60 BY 2.38 NO-UNDO.

DEFINE VARIABLE autoMaximize AS LOGICAL INITIAL no 
     LABEL "Auto Maximize" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      userControl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     autoMaximize AT ROW 11.71 COL 9 WIDGET-ID 2
     winSize AT ROW 23.86 COL 9 NO-LABEL WIDGET-ID 8
     Btn_OK AT ROW 26.24 COL 54
     physical_file AT ROW 1.24 COL 23 COLON-ALIGNED
     prgmTitle AT ROW 2.19 COL 14 COLON-ALIGNED
     copyrite AT ROW 3.14 COL 7 COLON-ALIGNED NO-LABEL
     link1 AT ROW 4.57 COL 14 COLON-ALIGNED WIDGET-ID 24
     link2 AT ROW 5.52 COL 14 COLON-ALIGNED WIDGET-ID 28
     asiVersion AT ROW 6.48 COL 24 COLON-ALIGNED WIDGET-ID 26
     userControl.maxAllowedUsers AT ROW 7.43 COL 37 COLON-ALIGNED WIDGET-ID 32
           VIEW-AS TEXT 
          SIZE 9.2 BY .62
          FGCOLOR 2 
     userControl.maxSessionsPerUser AT ROW 8.38 COL 42 COLON-ALIGNED WIDGET-ID 34
           VIEW-AS TEXT 
          SIZE 5.6 BY .62
          FGCOLOR 2 
     userControl.numUsersOverLimit AT ROW 9.33 COL 40 COLON-ALIGNED WIDGET-ID 36
           VIEW-AS TEXT 
          SIZE 8 BY .62
          FGCOLOR 2 
     link3 AT ROW 10.29 COL 14 COLON-ALIGNED WIDGET-ID 30
     "(value change requires a close/reopen)" VIEW-AS TEXT
          SIZE 38 BY .81 AT ROW 11.71 COL 30 WIDGET-ID 4
          FONT 4
     "NOTE: screen scaling applies to all modules" VIEW-AS TEXT
          SIZE 42 BY .81 AT ROW 26.24 COL 11 WIDGET-ID 18
          FONT 4
     IMAGE-1 AT ROW 1.24 COL 2
     RECT-1 AT ROW 4.1 COL 9
     RECT-2 AT ROW 12.67 COL 9
     userScreen AT ROW 13.14 COL 9 WIDGET-ID 6
     screenImage AT ROW 13.62 COL 11 WIDGET-ID 14
     RECT-3 AT ROW 11.24 COL 9 WIDGET-ID 20
     SPACE(7.39) SKIP(17.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "About Advantzware System".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       asiVersion:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN copyrite IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       link1:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       link2:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       link3:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN physical_file IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN prgmTitle IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.userControl"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* About Advantzware System */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME autoMaximize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL autoMaximize Dialog-Frame
ON VALUE-CHANGED OF autoMaximize IN FRAME Dialog-Frame /* Auto Maximize */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF autoMaximize THEN DO:
    OUTPUT TO VALUE(winReSize).
    OUTPUT CLOSE.
  END.
  ELSE OS-DELETE VALUE(winReSize).

  OUTPUT TO VALUE(winReSizeDat).
  EXPORT winSize sizeRatio.
  OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME link1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL link1 Dialog-Frame
ON LEFT-MOUSE-CLICK OF link1 IN FRAME Dialog-Frame /* Link */
DO:
    fLinkClick (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME link2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL link2 Dialog-Frame
ON LEFT-MOUSE-CLICK OF link2 IN FRAME Dialog-Frame /* Link */
DO:
    fLinkClick (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME link3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL link3 Dialog-Frame
ON LEFT-MOUSE-CLICK OF link3 IN FRAME Dialog-Frame /* Link */
DO:
    fLinkClick (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME winSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winSize Dialog-Frame
ON VALUE-CHANGED OF winSize IN FRAME Dialog-Frame
DO:
  ASSIGN
    winSize
    sizeRatio = (800 + 800 * winSize / 100) / 1600
    screenImage:WIDTH-PIXELS = 280 * sizeRatio
    screenImage:HEIGHT-PIXELS = 205 * sizeRatio.
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
    asiVersion = "{&awversion}".
    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST userControl NO-LOCK NO-ERROR.
    IF AVAILABLE userControl THEN
    DISPLAY
        userControl.maxAllowedUser
        userControl.maxSessionsPerUser
        userControl.numUsersOverLimit
        .
    IF SEARCH("nosweat/about.dat") NE ? THEN DO:
        INPUT FROM VALUE(SEARCH("nosweat/about.dat")) NO-ECHO.
        REPEAT:
            IMPORT cLinkName cLinkLabel cLinkURL.
            CASE cLinkName:
                WHEN "Link1" THEN
                ASSIGN
                    link1 = cLinkLabel
                    link1:PRIVATE-DATA = cLinkURL
                    .
                WHEN "Link2" THEN
                ASSIGN
                    link2 = cLinkLabel
                    link2:PRIVATE-DATA = cLinkURL
                    .
                WHEN "Link3" THEN
                ASSIGN
                    link3 = cLinkLabel
                    link3:PRIVATE-DATA = cLinkURL
                    .
            END CASE.
        END. /* repeat */
        OUTPUT CLOSE.
    END. /* about.dat */
    DISPLAY
        asiVersion
        link1
        link2
        link3
        .    
    ASSIGN
      physical_file:SCREEN-VALUE = ENTRY(NUM-ENTRIES(callingprgm,' '),callingprgm,' ')
      copyrite:SCREEN-VALUE = '{copyrite}'
      callingprgm = ENTRY(NUM-ENTRIES(callingprgm,' '),callingprgm,' ')
      callingprgm = SUBSTR(callingprgm,R-INDEX(callingprgm,'/') + 1)
      callingprgm = SUBSTR(callingprgm,1,LENGTH(callingprgm) - 1)
      .
    FIND prgrms NO-LOCK WHERE prgrms.prgmname EQ callingprgm NO-ERROR.
    IF AVAILABLE prgrms THEN
    ASSIGN
      FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + ' ' + prgrms.prgtitle
      prgmTitle:SCREEN-VALUE = prgrms.prgtitle
      .
    ASSIGN
      winReSize = 'users/' + USERID('NOSWEAT') + '/' + callingprgm + 'winReSize'
      winReSizeDat = 'users/' + USERID('NOSWEAT') + '/winReSize.dat'
      autoMaximize:SCREEN-VALUE = STRING(SEARCH(winReSize) NE ?)
      autoMaximize
      .
    IF SEARCH(winReSizeDat) NE ? THEN DO:
      INPUT FROM VALUE(SEARCH(winReSizeDat)).
      IMPORT winSize sizeRatio.
      INPUT CLOSE.
    END.

    FILE-INFO:FILE-NAME = '.' .
    cWorkDir = file-info:FULL-PATHNAME .
    
    FILE-INFO:FILE-NAME = cWorkDir + '/' + 'users' .
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        OS-CREATE-DIR VALUE(string(cWorkDir) + '/' + 'users' ).

    FILE-INFO:FILE-NAME = cWorkDir + '/' + 'users' + USERID('NOSWEAT') .
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        OS-CREATE-DIR VALUE(string(cWorkDir) + '/' + 'users/' + USERID('NOSWEAT') ).
   
    winSize:SCREEN-VALUE = STRING(winSize).
    APPLY 'VALUE-CHANGED':U TO winSize.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY autoMaximize winSize physical_file prgmTitle copyrite link1 link2 
          asiVersion link3 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE userControl THEN 
    DISPLAY userControl.maxAllowedUsers userControl.maxSessionsPerUser 
          userControl.numUsersOverLimit 
      WITH FRAME Dialog-Frame.
  ENABLE userScreen screenImage autoMaximize winSize Btn_OK link1 link2 
         asiVersion userControl.maxAllowedUsers userControl.maxSessionsPerUser 
         userControl.numUsersOverLimit link3 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLinkClick Dialog-Frame 
FUNCTION fLinkClick RETURNS LOGICAL
  (iphLink AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF iphLink:PRIVATE-DATA NE "" THEN
    OS-COMMAND NO-WAIT START VALUE(iphLink:PRIVATE-DATA).
    ELSE
    MESSAGE iphLink:SCREEN-VALUE "Link Not Yet Implemented"
    VIEW-AS ALERT-BOX.  
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

