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

{system/sysconst.i}
{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS userControl.maxAllowedUsers ~
userControl.maxSessionsPerUser 
&Scoped-define ENABLED-TABLES userControl
&Scoped-define FIRST-ENABLED-TABLE userControl
&Scoped-Define ENABLED-OBJECTS btnProperties userScreen btnSave screenImage ~
properties autoMaximize winSize currentUsers 
&Scoped-Define DISPLAYED-FIELDS userControl.maxAllowedUsers ~
userControl.maxSessionsPerUser 
&Scoped-define DISPLAYED-TABLES userControl
&Scoped-define FIRST-DISPLAYED-TABLE userControl
&Scoped-Define DISPLAYED-OBJECTS autoMaximize winSize physical_file ~
prgmTitle copyrite asiVersion currentUsers 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetKeyValue Dialog-Frame 
FUNCTION fGetKeyValue RETURNS CHARACTER
  (ipcSection AS CHARACTER, ipcKey AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnProperties 
     IMAGE-UP FILE "Graphics/32x32/gearwheels.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.6 BY 1.81 TOOLTIP "View System Settings".

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.6 BY 1.81 TOOLTIP "Save Settings".

DEFINE VARIABLE properties AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132 BY 26.19
     FONT 0 NO-UNDO.

DEFINE VARIABLE asiVersion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Version" 
      VIEW-AS TEXT 
     SIZE 19.6 BY .62
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE copyrite AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 57 BY .62 NO-UNDO.

DEFINE VARIABLE currentUsers AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Current Users Logged In" 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     FGCOLOR 2  NO-UNDO.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnProperties AT ROW 1.24 COL 69 WIDGET-ID 42
     btnSave AT ROW 23.38 COL 61
     properties AT ROW 1 COL 77 NO-LABEL WIDGET-ID 38
     autoMaximize AT ROW 8.86 COL 9 WIDGET-ID 2
     winSize AT ROW 21 COL 9 NO-LABEL WIDGET-ID 8
     physical_file AT ROW 1.24 COL 23 COLON-ALIGNED
     prgmTitle AT ROW 2.19 COL 14 COLON-ALIGNED
     copyrite AT ROW 3.14 COL 7 COLON-ALIGNED NO-LABEL
     asiVersion AT ROW 4.57 COL 17 COLON-ALIGNED WIDGET-ID 26
     userControl.maxAllowedUsers AT ROW 5.52 COL 30 COLON-ALIGNED WIDGET-ID 32
           VIEW-AS TEXT 
          SIZE 9.2 BY .62
          FGCOLOR 2 
     userControl.maxSessionsPerUser AT ROW 6.48 COL 35 COLON-ALIGNED WIDGET-ID 34
           VIEW-AS TEXT 
          SIZE 5.6 BY .62
          FGCOLOR 2 
     currentUsers AT ROW 7.43 COL 36 COLON-ALIGNED WIDGET-ID 44
     "NOTE: screen scaling applies to all modules" VIEW-AS TEXT
          SIZE 42 BY .81 AT ROW 23.86 COL 18 WIDGET-ID 18
          FONT 4
     "(value change requires a close/reopen)" VIEW-AS TEXT
          SIZE 38 BY .81 AT ROW 8.86 COL 30 WIDGET-ID 4
          FONT 4
     IMAGE-1 AT ROW 1.24 COL 2
     RECT-1 AT ROW 4.1 COL 9
     RECT-2 AT ROW 9.81 COL 9
     userScreen AT ROW 10.29 COL 9 WIDGET-ID 6
     screenImage AT ROW 10.76 COL 11 WIDGET-ID 14
     RECT-3 AT ROW 8.38 COL 9 WIDGET-ID 20
     SPACE(140.00) SKIP(18.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "About Advantzware System".

DEFINE FRAME linkFrame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 25.29
         SIZE 74 BY 1.91
         FONT 6
         TITLE "Links" WIDGET-ID 100.


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
/* REPARENT FRAME */
ASSIGN FRAME linkFrame:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN asiVersion IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       asiVersion:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN copyrite IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       currentUsers:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       userControl.maxAllowedUsers:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       userControl.maxSessionsPerUser:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN physical_file IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN prgmTitle IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR properties IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       properties:HIDDEN IN FRAME Dialog-Frame           = TRUE
       properties:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME linkFrame
                                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME linkFrame
/* Query rebuild information for FRAME linkFrame
     _Query            is NOT OPENED
*/  /* FRAME linkFrame */
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


&Scoped-define SELF-NAME btnProperties
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProperties Dialog-Frame
ON CHOOSE OF btnProperties IN FRAME Dialog-Frame
DO:
    ASSIGN
        properties:HIDDEN = YES
        FRAME {&FRAME-NAME}:WIDTH = FRAME {&FRAME-NAME}:WIDTH
                                  + IF FRAME {&FRAME-NAME}:WIDTH LT 100 THEN 133
                                    ELSE -133
        properties:HIDDEN = FRAME {&FRAME-NAME}:WIDTH LT 100
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame
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


&Scoped-define SELF-NAME winSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winSize Dialog-Frame
ON VALUE-CHANGED OF winSize IN FRAME Dialog-Frame
DO:
    ASSIGN
        winSize
        sizeRatio = (800 + 800 * winSize / 100) / 1600
        screenImage:WIDTH-PIXELS = 280 * sizeRatio
        screenImage:HEIGHT-PIXELS = 205 * sizeRatio
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

DELETE WIDGET-POOL "linkPool" NO-ERROR.
CREATE WIDGET-POOL "linkPool" PERSISTENT.        

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:
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
            physical_file:SCREEN-VALUE = ENTRY(NUM-ENTRIES(callingprgm,' '),callingprgm,' ')
            copyrite:SCREEN-VALUE = '{copyrite}'
            callingprgm = ENTRY(NUM-ENTRIES(callingprgm,' '),callingprgm,' ')
            callingprgm = SUBSTR(callingprgm,R-INDEX(callingprgm,'/') + 1)
            callingprgm = SUBSTR(callingprgm,1,LENGTH(callingprgm) - 1)
            asiVersion:SCREEN-VALUE = "{&awversion}".
            .
        FIND FIRST userControl NO-LOCK NO-ERROR.
        IF AVAILABLE userControl THEN
        DISPLAY
            userControl.maxAllowedUser
            userControl.maxSessionsPerUser
            .
        FOR EACH userLog NO-LOCK
            WHERE userLog.logoutDateTime EQ ?
            :
            currentUsers = currentUsers + 1.
        END. /* each userlog */
        DISPLAY currentUsers.
        IF SEARCH(winReSizeDat) NE ? THEN DO:
          INPUT FROM VALUE(SEARCH(winReSizeDat)).
          IMPORT winSize sizeRatio.
          INPUT CLOSE.
        END.
    
        ASSIGN
            FILE-INFO:FILE-NAME = '.'
            cWorkDir = file-info:FULL-PATHNAME
            .        
        FILE-INFO:FILE-NAME = cWorkDir + '/' + 'users' .
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            OS-CREATE-DIR VALUE(string(cWorkDir) + '/' + 'users' ).
    
        FILE-INFO:FILE-NAME = cWorkDir + '/' + 'users' + USERID('NOSWEAT') .
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            OS-CREATE-DIR VALUE(string(cWorkDir) + '/' + 'users/' + USERID('NOSWEAT') ).
       
        winSize:SCREEN-VALUE = STRING(winSize).
        APPLY 'VALUE-CHANGED':U TO winSize.
        RUN pProperties.
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
  HIDE FRAME linkFrame.
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
  DISPLAY autoMaximize winSize physical_file prgmTitle copyrite asiVersion 
          currentUsers 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE userControl THEN 
    DISPLAY userControl.maxAllowedUsers userControl.maxSessionsPerUser 
      WITH FRAME Dialog-Frame.
  ENABLE btnProperties userScreen btnSave screenImage properties autoMaximize 
         winSize userControl.maxAllowedUsers userControl.maxSessionsPerUser 
         currentUsers 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  VIEW FRAME linkFrame.
  {&OPEN-BROWSERS-IN-QUERY-linkFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSysCtrl Dialog-Frame 
PROCEDURE pGetSysCtrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcName    AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER opcDescrip  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcModule   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCharFld  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDateFld  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiIntFld   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecFld   AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplLogFld   AS LOGICAL   NO-UNDO.
    
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ ipcCompany
          AND sys-ctrl.name EQ ipcName
        NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
    ASSIGN
        opcDescrip  = sys-ctrl.descrip
        opcModule   = sys-ctrl.module
        opcCharFld  = sys-ctrl.char-fld
        opcDateFld  = IF sys-ctrl.date-fld EQ ? THEN ""
                      ELSE STRING(sys-ctrl.date-fld,"99/99/9999")
        opiIntFld   = sys-ctrl.int-fld
        opdDecFld   = sys-ctrl.dec-fld
        oplLogFld   = sys-ctrl.log-fld
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLinkClick Dialog-Frame 
PROCEDURE pLinkClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphLink AS HANDLE NO-UNDO.
    
    IF iphLink:PRIVATE-DATA NE ? THEN
    OS-COMMAND NO-WAIT START VALUE(iphLink:PRIVATE-DATA).
    ELSE
    MESSAGE iphLink:SCREEN-VALUE "Link Not Yet Implemented"
    VIEW-AS ALERT-BOX.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProperties Dialog-Frame 
PROCEDURE pProperties :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  {none}
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE aboutINI  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVersion  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDescrip  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cModule   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharFld  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDateFld  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIntFld   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dDecFld   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lLogFld   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDLC      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hLabel    AS HANDLE NO-UNDO.
    DEFINE VARIABLE hLink     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iLinkFGC  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLinkBGC  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLinkFont AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCol      AS DECIMAL   NO-UNDO INITIAL 1.3.
    DEFINE VARIABLE iRow      AS DECIMAL   NO-UNDO INITIAL 1.1.
    DEFINE VARIABLE iGap      AS DECIMAL   NO-UNDO INITIAL .7.
    
    /* get progress installed location */
    GET-KEY-VALUE SECTION 'STARTUP'
        KEY 'DLC'
        VALUE cDLC.
    /* get progress version */
    INPUT FROM VALUE(SEARCH(cDLC + "\version")) NO-ECHO.
    IMPORT UNFORMATTED cVersion.
    INPUT CLOSE.
    
    /* enable use of about.ini */
    aboutINI = SEARCH("nosweat/about.ini").
    LOAD aboutINI.
    USE aboutINI.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            properties:FGCOLOR = INT(fGetKeyValue("Settings","FGCOLOR"))
            properties:BGCOLOR = INT(fGetKeyValue("Settings","BGCOLOR"))
            properties:FONT    = INT(fGetKeyValue("Settings","FONT"))
            iLinkFGC           = INT(fGetKeyValue("Links","FGCOLOR"))
            iLinkBGC           = INT(fGetKeyValue("Links","BGCOLOR"))
            iLinkFont          = INT(fGetKeyValue("Links","FONT"))
            properties:HIDDEN  = YES
            FRAME {&FRAME-NAME}:WIDTH = FRAME {&FRAME-NAME}:WIDTH - 133
            .
        /* get links and dynamically create clickable widgets */
        DO WHILE TRUE:
            /* get link label from about.ini */
            ASSIGN
                idx = idx + 1
                cValue = fGetKeyValue("Links","LinkLabel" + STRING(idx))
                .
            /* if ? then no more links */
            IF cValue EQ ? THEN LEAVE.
            /* if more than 1 link, make room by increasing height */
            IF idx GT 1 THEN
            ASSIGN
                FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + iGap
                FRAME linkFrame:HEIGHT = FRAME linkFrame:HEIGHT + iGap
                properties:HEIGHT = properties:HEIGHT + iGap
                .
            /* create clickable link widget */
            CREATE TEXT hLink IN WIDGET-POOL "linkPool"
                ASSIGN
                    FRAME = FRAME linkFrame:HANDLE
                    NAME = "Link" + STRING(idx)
                    COL = iCol
                    ROW = iRow
                    SENSITIVE = YES
                    READ-ONLY = YES
                    WIDTH = 73
                    HEIGHT = .62
                    FGCOLOR = iLinkFGC
                    BGCOLOR = iLinkBGC
                    FONT = iLinkFont
                    DATA-TYPE = "CHARACTER"
                    FORMAT = "x(256)"
                    TOOLTIP = "Click Link"
                    SCREEN-VALUE = cValue
              TRIGGERS:
                ON LEFT-MOUSE-CLICK
                  PERSISTENT RUN pLinkClick IN THIS-PROCEDURE (hLink).
              END TRIGGERS.
            IF VALID-HANDLE(hLink) THEN DO:
                hLink:MOVE-TO-TOP().
                ASSIGN
                    /* get url from about.ini */
                    hLink:PRIVATE-DATA = fGetKeyValue("Links","LinkURL" + STRING(idx))
                    hLink:HIDDEN = NO
                    iRow = iRow + iGap
                    .
            END. /* if valid handle */
        END. /* do while true */
        
        ASSIGN
            idx = 0
            properties:SCREEN-VALUE = "[Key NK1 Settings]" + CHR(10)
            .
        DO WHILE TRUE:
            /* get nk1 code name from about.ini */
            ASSIGN
                idx = idx + 1
                cValue = fGetKeyValue("NK1","NK1-" + STRING(idx))
                .
            /* if ? then no more nk1 code names */
            IF cValue EQ ? THEN LEAVE.
            /* get nk1 values */
            RUN pGetSysCtrl (g_company, cValue,
                OUTPUT cDescrip,
                OUTPUT cModule,
                OUTPUT cCharFld,
                OUTPUT cDateFld,
                OUTPUT iIntFld,
                OUTPUT dDecFld,
                OUTPUT lLogFld
                ).                        
            properties:SCREEN-VALUE = properties:SCREEN-VALUE
                                    + FILL(" ", 5) + "[" + cValue + "]" + CHR(10)
                                    + FILL(" ",10) + "Description=" + cDescrip + CHR(10)
                                    + FILL(" ",10) + "Module=" + cModule
                                    + "  [Char=" + cCharFld
                                    + "]  [Date=" + cDateFld
                                    + "]  [Int=" + STRING(iIntFld)
                                    + "]  [Dec=" + STRING(dDecFld)
                                    + "]  [Log=" + STRING(lLogFld)
                                    + "]" + CHR(10)
                                    .
        END. /* do while */
        properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10)
                                + "[Connected Database(s)]"
                                .
        /* get connected database(s) */
        DO idx = 1 TO NUM-DBS:
            properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10)
                                    + FILL(" ",5) + PDBNAME(idx).
        END. /* do idx */
        /* show progress version */
        properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10) + CHR(10)
                                + "[Progress Version]" + CHR(10)
                                + FILL(" ",5) + cVersion + CHR(10) + CHR(10)
                                + "[ProPath]"
                                .
        /* show propath but not any progress directories */
        DO idx = 1 TO NUM-ENTRIES(PROPATH):
            IF ENTRY(idx,PROPATH) BEGINS cDLC THEN NEXT.
            properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10)
                                    + FILL(" ",5) + ENTRY(idx,PROPATH)
                                    .
        END. /* do idx */
        /* show startup parameters */
        properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10) + CHR(10)
                                + "[Startup Parameters]"
                                .
        /* show startup parameters values */
        DO idx = 1 TO NUM-ENTRIES(SESSION:STARTUP-PARAMETERS):
            properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10)
                                    + FILL(" ",5) + ENTRY(idx,SESSION:STARTUP-PARAMETERS)
                                    .
        END. /* do idx */
        /* show current users logged in */
        properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10) + CHR(10)
                                + "[Current Users Logged In]"
                                .
        FOR EACH userLog NO-LOCK
            WHERE userLog.logoutDateTime EQ ?
            :
            properties:SCREEN-VALUE = properties:SCREEN-VALUE + CHR(10)
                                    + FILL(" ",5)
                                    + STRING(userLog.loginDateTime) + " "
                                    + userLog.user_id + ": "
                                    + userLog.userName
                                    .
        END. /* each userlog */
    END. /* do with frame */
    
    UNLOAD aboutINI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetKeyValue Dialog-Frame 
FUNCTION fGetKeyValue RETURNS CHARACTER
  (ipcSection AS CHARACTER, ipcKey AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    
    GET-KEY-VALUE SECTION ipcSection
        KEY ipcKey
        VALUE cValue.
    RETURN cValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

