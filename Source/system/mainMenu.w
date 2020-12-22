&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME MAINMENU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS MAINMENU 
/*------------------------------------------------------------------------
 
  File:              mainMenu.w
 
  Description:       Main Menu v3 (Menu Tree)
 
  Input Parameters:  <none>
 
  Output Parameters: <none>
 
  Author:            Ron Stark
 
  Created:           9.25.2018
 
--------------------------------------------------------------------*/
 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
 
CREATE WIDGET-POOL.

/* *************************** Set Function ************************** */

ON F1 HELP.
ON CTRL-F HELP.
ON CTRL-P HELP.

ON 'CTRL-ALT-D':U ANYWHERE
DO:
    RUN aoa/aoaLauncher.w PERSISTENT ("Dashboard").
    RETURN.
END.

ON 'CTRL-ALT-R':U ANYWHERE
DO:
    RUN aoa/aoaLauncher.w PERSISTENT ("Report").
    RETURN.
END.

ON 'CTRL-ALT-P':U ANYWHERE
DO: 
    RUN util/wPgmrToolbox.w.
END.    
   
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

&Scoped-define mainMenuBGColor 1
&Scoped-define mainMenuFGColor 15
&Scoped-define FGColor ?
&Scoped-define BGColor 32

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

/* System Constant Values */
{system/sysconst.i}

DEFINE VARIABLE cBitMap           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCEMenu           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDebug            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEulaFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEulaVersion      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFound            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPositionMnemonic AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProfilerFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShowMnemonic     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceMenu       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTickerInterval   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hMenuLink         AS HANDLE    NO-UNDO EXTENT 8.
DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBGColor          AS INTEGER   NO-UNDO EXTENT 3.
DEFINE VARIABLE idx               AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEditorBGColor    AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iEditorFGColor    AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iEditorFont       AS INTEGER   NO-UNDO INITIAL ?.
DEFINE VARIABLE iFGColor          AS INTEGER   NO-UNDO EXTENT 3.
DEFINE VARIABLE iFrameBGColor     AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iFrameFGColor     AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iLastVersion      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRectangleBGColor AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iRectangleFGColor AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iThisVersion      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTickerInterval   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iProfileStartTime AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSaveBgColor      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lAdmin            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCloseMenu        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lEulaAccepted     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFavorite         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFound            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOK               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSearchOpen       AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lSuperAdmin       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUserExit         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lViewTaskResults  AS LOGICAL   NO-UNDO INITIAL ?.

ASSIGN
    g_mainmenu = THIS-PROCEDURE
    g_company  = ""
    g_loc      = ""
    .
RUN Get_Procedure IN Persistent-Handle ("comp_loc.",OUTPUT run-proc,YES).
IF g_company EQ "" OR g_loc EQ "" THEN DO:
    MESSAGE "No Company and/or Location found for your login ID." SKIP
        "Please Contact System's Administrator." VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
END.
cEulaFile = SEARCH("{&EulaFile}").

&Scoped-define isActive YES
{system/menuTree.i}
{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-USER

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS imageSettings imageCompany menuLinkZoHo ~
imageFolder imagePrinter imageScheduler imageRunUlitity RECT-24 RECT-25 ~
fFollow 
&Scoped-Define DISPLAYED-OBJECTS company_name loc_loc users_user_id ~
Mnemonic fFollow 

/* Custom List Definitions                                              */
/* searchFilters,List-2,List-3,List-4,List-5,colorPallet                */
&Scoped-define searchFilters menuTreeFilter searchSelections btnMoveDown ~
btnMoveUp btnRemove btnFavorite svFavoriteText 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIntVer MAINMENU 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR MAINMENU AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Help 
       MENU-ITEM m_SysCtrl_Usage LABEL "SysCtrl Usage" 
       MENU-ITEM m_Profiler     LABEL "Start/Stop Profiler"
       MENU-ITEM m_Advantzware_Version LABEL "Advantzware Version"
       RULE
       MENU-ITEM m_CheckUpgrade_Advantzware LABEL "Check/Upgrade Advantzware"
       RULE
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE MENU MENU-BAR-MAINMENU MENUBAR
       SUB-MENU  m_Help         LABEL "Help"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE company_name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35 BY .62
     FONT 23 NO-UNDO.

DEFINE VARIABLE fFollow AS CHARACTER FORMAT "X(256)":U INITIAL "Follow Us on" 
      VIEW-AS TEXT 
     SIZE 14 BY .52
     BGCOLOR 31 FONT 23 NO-UNDO.

DEFINE VARIABLE loc_loc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 23 NO-UNDO.

DEFINE VARIABLE Mnemonic AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     FONT 23 NO-UNDO.

DEFINE VARIABLE users_user_id AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 23 NO-UNDO.

DEFINE IMAGE boxes
     FILENAME "Graphics/bg-option2.jpg":U
     SIZE 103.8 BY 20.95.

DEFINE IMAGE imageCompany
     FILENAME "Graphics/32x32/office_building.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Change Company/Location".

DEFINE IMAGE imageFolder
     FILENAME "Graphics/32x32/folder.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "View User Folder".

DEFINE IMAGE imagePrinter
     FILENAME "Graphics/32x32/printerbtn.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Reports".

DEFINE IMAGE imageRunUlitity
     FILENAME "Graphics/32x32/window_gear.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Utility Application".

DEFINE IMAGE imageScheduler
     FILENAME "Graphics/32x32/calendar_clock.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Task Scheduler".

DEFINE IMAGE imageSettings
     FILENAME "Graphics/32x32/gearwheels.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Settings".

DEFINE IMAGE menu-image
     FILENAME "Graphics/logo1.bmp":U CONVERT-3D-COLORS
     SIZE 93.8 BY 4.62.

DEFINE IMAGE menuLink-1
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-2
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-3
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-4
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-5
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-6
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-7
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLink-8
     SIZE 7 BY 1.67.

DEFINE IMAGE menuLinkASI TRANSPARENT
     SIZE 7 BY 1.67 TOOLTIP "Advantzware Link".

DEFINE IMAGE menuLinkZoHo TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Advantzware Help Tickets".

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 92 BY 5.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 2
     BGCOLOR 15 FGCOLOR 24 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 2
     BGCOLOR 15 FGCOLOR 24 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 159.6 BY 2.14
     BGCOLOR 21 FGCOLOR 21 .

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE .6 BY 1.71
     BGCOLOR 35 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE .6 BY 1.71
     BGCOLOR 35 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 48 BY 1.19
     BGCOLOR 15 FGCOLOR 21 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 25 BY 1.19
     BGCOLOR 15 FGCOLOR 21 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 21 BY 1.19
     BGCOLOR 15 FGCOLOR 21 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 9 BY 1.19
     BGCOLOR 15 FGCOLOR 21 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 159.6 BY .05
     BGCOLOR 15 FGCOLOR 15 .

DEFINE VARIABLE menuTreeMsg AS CHARACTER FORMAT "X(256)":U INITIAL " Initializing Menus, One Moment Please ..." 
      VIEW-AS TEXT 
     SIZE 53 BY 2.14
     BGCOLOR 1 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE svFocus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE .2 BY 1 NO-UNDO.

DEFINE VARIABLE upgradeMsg AS CHARACTER FORMAT "X(256)":U INITIAL "   Checking if  Upgrade Exists ..." 
      VIEW-AS TEXT 
     SIZE 53 BY 2.14
     BGCOLOR 1 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE BUTTON btnClear  NO-FOCUS
     LABEL "Clear" 
     SIZE 8.4 BY 1.05 TOOLTIP "Clear Search Filters"
     BGCOLOR 21 FONT 22.

DEFINE BUTTON btnFavorite 
     IMAGE-UP FILE "Graphics/16x16/move_plus.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Fav" 
     SIZE 5 BY 1.19.

DEFINE BUTTON BtnFavorites 
     IMAGE-UP FILE "Graphics/16x16/star.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5.6 BY 1.24 TOOLTIP "Search Menu / Edit Favorites".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/16x16/move_down.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Favorite Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/16x16/move_up.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Favorite Up".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/16x16/move_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Remove Favorite".

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/16x16/filterwindow.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5.6 BY 1.24 TOOLTIP "Search Menu / Edit Favorites".

DEFINE VARIABLE menuTreeFilter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Name Search"
     BGCOLOR 15 FGCOLOR 1 FONT 22 NO-UNDO.

DEFINE VARIABLE svFavoriteText AS CHARACTER FORMAT "X(256)":U INITIAL "Favorites" 
      VIEW-AS TEXT 
     SIZE 10 BY .71
     FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 42 BY 1
     BGCOLOR 15 FGCOLOR 15 .

DEFINE VARIABLE favoritesList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "FAVORITES","FAVORITES" 
     SIZE 45 BY 12.62
     BGCOLOR 15 FONT 22 NO-UNDO.

DEFINE VARIABLE searchSelections AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 56 BY 11.43
     BGCOLOR 15 FONT 22 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-USER
     company_name AT ROW 1.71 COL 13 COLON-ALIGNED NO-LABEL
     loc_loc AT ROW 1.71 COL 61.2 COLON-ALIGNED NO-LABEL
     users_user_id AT ROW 1.71 COL 90.4 COLON-ALIGNED NO-LABEL
     Mnemonic AT ROW 1.71 COL 141 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fFollow AT ROW 26.57 COL 100 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     "Location:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.71 COL 52.6
          FONT 22
     "Company:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 3.6
          FONT 22
     "User ID:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.71 COL 83
          FONT 22
     boxes AT ROW 8.38 COL 56.2
     menu-image AT ROW 3.24 COL 56.2
     RECT-2 AT ROW 1 COL 1
     RECT-5 AT ROW 1.48 COL 3 WIDGET-ID 38
     RECT-6 AT ROW 1.48 COL 82 WIDGET-ID 40
     RECT-7 AT ROW 1.48 COL 51.6 WIDGET-ID 42
     RECT-8 AT ROW 1.48 COL 141 WIDGET-ID 44
     RECT-9 AT ROW 3.29 COL 1 WIDGET-ID 46
     RECT-10 AT ROW 5.81 COL 57 WIDGET-ID 48
     imageSettings AT ROW 1.24 COL 152 WIDGET-ID 52
     imageCompany AT ROW 1.24 COL 73 WIDGET-ID 54
     menuLinkASI AT ROW 3.86 COL 152 WIDGET-ID 56
     RECT-11 AT ROW 3.71 COL 151.6 WIDGET-ID 58
     menuLinkZoHo AT ROW 6 COL 152 WIDGET-ID 64
     menuLink-1 AT ROW 27.62 COL 134.4 WIDGET-ID 66
     menuLink-2 AT ROW 27.62 COL 126.6 WIDGET-ID 68
     menuLink-3 AT ROW 27.62 COL 118.4 WIDGET-ID 70
     menuLink-4 AT ROW 27.62 COL 110.2 WIDGET-ID 72
     menuLink-5 AT ROW 27.62 COL 102 WIDGET-ID 74
     menuLink-6 AT ROW 27.62 COL 93.8 WIDGET-ID 76
     menuLink-7 AT ROW 27.62 COL 85.6 WIDGET-ID 78
     menuLink-8 AT ROW 27.62 COL 77.4 WIDGET-ID 80
     imageFolder AT ROW 1.24 COL 107.4 WIDGET-ID 86
     imagePrinter AT ROW 1.24 COL 116 WIDGET-ID 98
     imageScheduler AT ROW 1.24 COL 124 WIDGET-ID 102
     imageRunUlitity AT ROW 1.24 COL 133 WIDGET-ID 104
     RECT-24 AT ROW 1.19 COL 79.6 WIDGET-ID 106
     RECT-25 AT ROW 1.19 COL 114.2 WIDGET-ID 108
     RECT-12 AT ROW 5.86 COL 151.6 WIDGET-ID 112
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 .

DEFINE FRAME menuTreeFrame
     svFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 82
     menuTreeMsg AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 84
     upgradeMsg AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4.57
         SIZE 55 BY 24.91
         BGCOLOR 31  WIDGET-ID 100.

DEFINE FRAME searchFrame
     btnClear AT ROW 13.86 COL 100 HELP
          "Clear Search Filters" WIDGET-ID 42
     BtnFavorites AT ROW 1.1 COL 1 HELP
          "Search Menu / Edit Favorites" WIDGET-ID 54
     btnSearch AT ROW 1.1 COL 49.8 HELP
          "Search Menu / Edit Favorites" WIDGET-ID 40
     menuTreeFilter AT ROW 1.14 COL 54 COLON-ALIGNED HELP
          "Enter Search Filter" NO-LABEL WIDGET-ID 2
     favoritesList AT ROW 2.29 COL 6 NO-LABEL WIDGET-ID 52
     searchSelections AT ROW 2.29 COL 52 NO-LABEL WIDGET-ID 44
     btnMoveDown AT ROW 5.86 COL 1 HELP
          "Move Favorite Down" WIDGET-ID 58
     btnMoveUp AT ROW 3.48 COL 1 HELP
          "Move Favorite Up" WIDGET-ID 56
     btnRemove AT ROW 4.67 COL 1 HELP
          "Remove Favorite" WIDGET-ID 26
     btnFavorite AT ROW 13.62 COL 52 WIDGET-ID 46
     svFavoriteText AT ROW 13.95 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     "FAVORITES" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 21 WIDGET-ID 62
          BGCOLOR 15 
     "FAVORITES" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.33 COL 21 WIDGET-ID 62
          BGCOLOR 15 FGCOLOR 1 FONT 22
     RECT-23 AT ROW 1.19 COL 7.2 WIDGET-ID 60
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.14
         SIZE 108 BY 14.05
         BGCOLOR 34 FGCOLOR 1  WIDGET-ID 600.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW MAINMENU ASSIGN
         HIDDEN             = YES
         TITLE              = "Main Menu - Advantzware version {&awversion}"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         PRIVATE-DATA       = "Main Menu - Advantzware version"
         KEEP-FRAME-Z-ORDER = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-MAINMENU:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT MAINMENU:LOAD-ICON("Graphics/asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW MAINMENU
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME menuTreeFrame:FRAME = FRAME FRAME-USER:HANDLE
       FRAME searchFrame:FRAME = FRAME FRAME-USER:HANDLE.

/* SETTINGS FOR FRAME FRAME-USER
   FRAME-NAME                                                           */
/* SETTINGS FOR IMAGE boxes IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN company_name IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       imageCompany:PRIVATE-DATA IN FRAME FRAME-USER     = 
                "Change Company/Location".

ASSIGN 
       imageSettings:PRIVATE-DATA IN FRAME FRAME-USER     = 
                "Settings".

/* SETTINGS FOR FILL-IN loc_loc IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE menu-image IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE menuLink-1 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-1:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-2 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-2:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-3 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-3:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-4 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-4:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-5 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-5:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-6 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-6:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-7 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-7:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLink-8 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLink-8:HIDDEN IN FRAME FRAME-USER           = TRUE.

/* SETTINGS FOR IMAGE menuLinkASI IN FRAME FRAME-USER
   NO-ENABLE                                                            */
ASSIGN 
       menuLinkASI:HIDDEN IN FRAME FRAME-USER           = TRUE
       menuLinkASI:PRIVATE-DATA IN FRAME FRAME-USER     = 
                "Advantzware Link".

/* SETTINGS FOR FILL-IN Mnemonic IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN users_user_id IN FRAME FRAME-USER
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME menuTreeFrame
                                                                        */
/* SETTINGS FOR FILL-IN menuTreeMsg IN FRAME menuTreeFrame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       menuTreeMsg:HIDDEN IN FRAME menuTreeFrame           = TRUE.

/* SETTINGS FOR FILL-IN svFocus IN FRAME menuTreeFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN upgradeMsg IN FRAME menuTreeFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME searchFrame
                                                                        */
ASSIGN 
       FRAME searchFrame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnClear IN FRAME searchFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnFavorite IN FRAME searchFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnMoveDown IN FRAME searchFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME searchFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnRemove IN FRAME searchFrame
   1                                                                    */
/* SETTINGS FOR FILL-IN menuTreeFilter IN FRAME searchFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-23 IN FRAME searchFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST searchSelections IN FRAME searchFrame
   1                                                                    */
/* SETTINGS FOR FILL-IN svFavoriteText IN FRAME searchFrame
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(MAINMENU)
THEN MAINMENU:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-USER
/* Query rebuild information for FRAME FRAME-USER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-USER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME menuTreeFrame
/* Query rebuild information for FRAME menuTreeFrame
     _Query            is NOT OPENED
*/  /* FRAME menuTreeFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME searchFrame
/* Query rebuild information for FRAME searchFrame
     _Query            is NOT OPENED
*/  /* FRAME searchFrame */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME FRAME-USER:HANDLE
       ROW             = 18.38
       COLUMN          = 16
       HEIGHT          = 4.76
       WIDTH           = 20
       WIDGET-ID       = 84
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-BEFORE(FRAME searchFrame:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME MAINMENU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MAINMENU MAINMENU
ON WINDOW-RESIZED OF MAINMENU /* Main Menu - Advantzware version {awversion} */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-USER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-USER MAINMENU
ON END-ERROR OF FRAME FRAME-USER
ANYWHERE
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-USER MAINMENU
ON HELP OF FRAME FRAME-USER
DO:
    RUN Get_Procedure IN Persistent-Handle ("popups.",OUTPUT run-proc,YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear MAINMENU
ON CHOOSE OF btnClear IN FRAME searchFrame /* Clear */
DO:
    ASSIGN
        menuTreeFilter:SCREEN-VALUE      = ""
        searchSelections:LIST-ITEM-PAIRS = ?
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFavorite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFavorite MAINMENU
ON CHOOSE OF btnFavorite IN FRAME searchFrame /* Fav */
DO:
    FIND FIRST ttMenuTree
         WHERE ttMenuTree.Mnemonic EQ ENTRY(1,searchSelections:SCREEN-VALUE,"|")
         NO-ERROR.
    IF AVAILABLE ttMenuTree THEN DO:
        ASSIGN
            lFavorite = NOT lFavorite
            ttMenuTree.favorite = lFavorite
            ttMenuTree.favoriteOrder = 9999
            .
        RUN pLoadFavorites.
        RUN pSearchSelections.
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFavorites
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFavorites MAINMENU
ON CHOOSE OF BtnFavorites IN FRAME searchFrame
DO:
    APPLY "CHOOSE":U TO btnSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown MAINMENU
ON CHOOSE OF btnMoveDown IN FRAME searchFrame
DO:
    RUN pMoveFavorite (1.1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp MAINMENU
ON CHOOSE OF btnMoveUp IN FRAME searchFrame
DO:
    RUN pMoveFavorite (-1.1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove MAINMENU
ON CHOOSE OF btnRemove IN FRAME searchFrame
DO:
    FIND FIRST ttMenuTree
         WHERE ttMenuTree.Mnemonic EQ ENTRY(1,favoritesList:SCREEN-VALUE,"|")
         NO-ERROR.
    IF AVAILABLE ttMenuTree THEN DO:
        ASSIGN
            ttMenuTree.favorite = NO
            ttMenuTree.favoriteOrder = 0
            .
        RUN pLoadFavorites.
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch MAINMENU
ON CHOOSE OF btnSearch IN FRAME searchFrame
DO:
    ASSIGN
        FRAME searchFrame:HIDDEN = YES
        lSearchOpen = NOT lSearchOpen
        .
    DO WITH FRAME searchFrame:
        IF lSearchOpen THEN DO:
            ASSIGN
                FRAME searchFrame:VIRTUAL-HEIGHT = btnClear:ROW + btnClear:HEIGHT - .86
                FRAME searchFrame:VIRTUAL-WIDTH  = btnClear:COL + btnClear:WIDTH  - .4
                FRAME searchFrame:HEIGHT = FRAME searchFrame:VIRTUAL-HEIGHT
                FRAME searchFrame:WIDTH  = FRAME searchFrame:VIRTUAL-WIDTH
                favoritesList:HIDDEN = NO
                .
            VIEW {&searchFilters}.
            ENABLE {&searchFilters} btnClear.
            APPLY "ENTRY":U TO menuTreeFilter.
        END. /* if searchopen */
        ELSE DO:
            DISABLE {&searchFilters} btnClear.
            HIDE {&searchFilters} btnClear.
            ASSIGN
                favoritesList:HIDDEN = YES
                FRAME searchFrame:VIRTUAL-HEIGHT = btnSearch:HEIGHT + .2
                FRAME searchFrame:VIRTUAL-WIDTH  = btnSearch:COL + btnSearch:WIDTH
                FRAME searchFrame:HEIGHT = FRAME searchFrame:VIRTUAL-HEIGHT
                FRAME searchFrame:WIDTH  = FRAME searchFrame:VIRTUAL-WIDTH
                .
        END. /* else */
    END. /* with frame */
    FRAME searchFrame:HIDDEN = NO.
    IF lSearchOpen THEN DO:
        ASSIGN 
            cCuePrgmName = FRAME searchFrame:NAME 
            hCueFrame    = FRAME searchFrame:HANDLE
            .
        {system/runCueCard.i}
    END. /* if searchopen */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-USER
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame MAINMENU OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatusDefault    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTaskerNotRunning AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iConfigID         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSaveErrStat      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTaskerNotRunning AS LOGICAL   NO-UNDO.
    
    lSaveErrStat = ERROR-STATUS:ERROR.
    IF lViewTaskResults EQ YES OR lViewTaskResults EQ ? THEN DO:
        RUN spRunCueCard ("Message", cCuePrgmName, hCueWindow, hCueFrame, lCueActive).
        FIND FIRST taskResult NO-LOCK
             WHERE taskResult.user-id EQ USERID("ASI")
               AND taskResult.viewed  EQ NO
            NO-ERROR.    
        IF AVAILABLE taskResult AND
           SEARCH(taskResult.folderFile) NE ? THEN DO TRANSACTION:
            IF lViewTaskResults EQ ? THEN
            MESSAGE
                "An Unviewed Task Result Exists, View Now?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lViewTaskResults.
            IF lViewTaskResults THEN DO:
                PAUSE 2 NO-MESSAGE.
                OS-COMMAND NO-WAIT start VALUE(SEARCH(taskResult.folderFile)).
                FIND CURRENT taskResult EXCLUSIVE-LOCK.
                taskResult.viewed = YES.
                RELEASE taskResult.
            END. /* if lViewTaskResults eq ? */
        END. /* if avail */
    END.
    FIND FIRST config NO-LOCK.
    cStatusDefault = "Task Monitor Last Executed: " + STRING(config.taskerLastExecuted).
    IF config.taskerLastExecuted LT DATETIME(TODAY,TIME * 1000 - 15000) THEN DO:
        cStatusDefault = "Task Monitor Currently Not Running".
        RUN sys/ref/nk1look.p (
            g_company,"TaskerNotRunning","I",NO,NO,"","",
            OUTPUT cTaskerNotRunning,OUTPUT lTaskerNotRunning
            ).
        iConfigID = INTEGER(cTaskerNotRunning).
        IF CAN-FIND(FIRST emailConfig
                    WHERE emailConfig.configID EQ iConfigID
                      AND emailConfig.isActive EQ YES
                      AND emailConfig.notified EQ NO) THEN DO:
            DO TRANSACTION:
                FIND FIRST emailConfig EXCLUSIVE-LOCK
                     WHERE emailConfig.configID EQ iConfigID
                     NO-ERROR NO-WAIT.
                IF AVAILABLE emailConfig AND NOT LOCKED emailConfig THEN
                emailConfig.notified = YES.
            END. /* do trans */
            IF NOT LOCKED emailConfig THEN DO:
                RUN sys/ref/nk1look.p (
                    g_company,"TaskerNotRunning","L",NO,NO,"","",
                    OUTPUT cTaskerNotRunning,OUTPUT lTaskerNotRunning
                    ).
                IF lTaskerNotRunning AND cTaskerNotRunning EQ "Yes" THEN
                RUN spSendEmail (
                    iConfigID,       /* emailConfig.ConfigID */
                    "",              /* Override for Email RecipientsinTo */
                    "",              /* Override for Email RecipientsinReplyTo */
                    "",              /* Override for Email RecipientsinCC */
                    "",              /* Override for Email RecipientsinBCC */
                    "",              /* Override for Email Subject */
                    "",              /* Override for Email Body */
                    "",              /* Email Attachment */
                    OUTPUT lSuccess, /* Email success or not */
                    OUTPUT cMessage  /* Reason for failure in case email is not sent */
                    ).
            END. /* if not locked */
        END. /* if sent eq no */
    END. /* tasker not running */
    RELEASE emailConfig.
/*    STATUS DEFAULT cStatusDefault IN WINDOW {&WINDOW-NAME}.*/
    IF PROFILER:ENABLED THEN 
    RUN pProcessProfiler.
    /* Set error status to saved value since it gets reset in this procedure */
    ERROR-STATUS:ERROR = lSaveErrStat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME favoritesList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL favoritesList MAINMENU
ON DEFAULT-ACTION OF favoritesList IN FRAME searchFrame
DO:
    FIND FIRST ttMenuTree
         WHERE ttMenuTree.Mnemonic EQ SELF:SCREEN-VALUE
         NO-ERROR. 
    IF AVAILABLE ttMenuTree THEN
    RUN pProcessClick.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-USER
&Scoped-define SELF-NAME imageCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageCompany MAINMENU
ON MOUSE-SELECT-CLICK OF imageCompany IN FRAME FRAME-USER
DO:
    RUN custom/comp_loc.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imageFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageFolder MAINMENU
ON MOUSE-SELECT-CLICK OF imageFolder IN FRAME FRAME-USER
DO:
    RUN Get_Procedure IN Persistent-Handle ("userFoldr.", OUTPUT run-proc, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imagePrinter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imagePrinter MAINMENU
ON MOUSE-SELECT-CLICK OF imagePrinter IN FRAME FRAME-USER
DO:
    RUN spSetTaskFilter ("", "", "").
    RUN Get_Procedure IN Persistent-Handle ("dynTasks.", OUTPUT run-proc, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imageRunUlitity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageRunUlitity MAINMENU
ON MOUSE-SELECT-CLICK OF imageRunUlitity IN FRAME FRAME-USER
DO:
    RUN Get_Procedure IN Persistent-Handle ("utillook.", OUTPUT run-proc, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imageScheduler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageScheduler MAINMENU
ON MOUSE-SELECT-CLICK OF imageScheduler IN FRAME FRAME-USER
DO:
    RUN spSetSessionParam ("ParamValueID", "0").
    RUN Get_Procedure IN Persistent-Handle ("dynSched.", OUTPUT run-proc, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME imageSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageSettings MAINMENU
ON MOUSE-SELECT-CLICK OF imageSettings IN FRAME FRAME-USER
DO:
    DEFINE VARIABLE lRebuildMenu AS LOGICAL NO-UNDO.

    RUN system/userSettings.w (OUTPUT lRebuildMenu).
    IF lRebuildMenu THEN DO:
        RUN pGetUserSettings.
        RUN pRebuildMenuTree.
    END. /* if rebuild */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-1 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-1 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-2 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-2 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-3 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-3 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-4 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-4 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-5 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-5 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-6 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-6 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-7 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-7 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (7).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLink-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLink-8 MAINMENU
ON MOUSE-SELECT-CLICK OF menuLink-8 IN FRAME FRAME-USER
DO:
    RUN pMenuLinkClick (8).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLinkASI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLinkASI MAINMENU
ON MOUSE-SELECT-CLICK OF menuLinkASI IN FRAME FRAME-USER
DO:
&IF DEFINED(FWD-VERSION) > 0 &THEN
    open-mime-resource "text/html" string(menuLinkASI:PRIVATE-DATA) false.
&ELSE
    OS-COMMAND NO-WAIT START VALUE(menuLinkasi:PRIVATE-DATA).
&ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLinkZoHo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLinkZoHo MAINMENU
ON MOUSE-SELECT-CLICK OF menuLinkZoHo IN FRAME FRAME-USER
DO:
&IF DEFINED(FWD-VERSION) > 0 &THEN
    open-mime-resource "text/html" string(menuLinkZoHo:PRIVATE-DATA) false.
&ELSE
    OS-COMMAND NO-WAIT START VALUE(menuLinkZoHo:PRIVATE-DATA).
&ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME menuTreeFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuTreeFilter MAINMENU
ON LEAVE OF menuTreeFilter IN FRAME searchFrame
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSearchSelections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuTreeFilter MAINMENU
ON RETURN OF menuTreeFilter IN FRAME searchFrame
DO:
    APPLY "LEAVE":U TO menuTreeFilter.
    APPLY "ENTRY":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Advantzware_Version
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Advantzware_Version MAINMENU
ON CHOOSE OF MENU-ITEM m_Advantzware_Version /* Advantzware Version */
DO:
    RUN Get_Procedure IN Persistent-Handle ("about.",OUTPUT run-proc,YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_CheckUpgrade_Advantzware
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_CheckUpgrade_Advantzware MAINMENU
ON CHOOSE OF MENU-ITEM m_CheckUpgrade_Advantzware /* Check/Upgrade Advantzware */
DO:
    RUN pCheckUpgradeAdvantzware.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit MAINMENU
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
    APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Profiler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Profiler MAINMENU
ON CHOOSE OF MENU-ITEM m_Profiler /* Start/Stop Profiler */
DO:
    RUN pOnOffProfiler.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_SysCtrl_Usage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_SysCtrl_Usage MAINMENU
ON CHOOSE OF MENU-ITEM m_SysCtrl_Usage /* SysCtrl Usage */
DO:
    RUN Get_Procedure IN Persistent-Handle ("sysCtrlU.",OUTPUT run-proc,YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchSelections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchSelections MAINMENU
ON DEFAULT-ACTION OF searchSelections IN FRAME searchFrame
DO:
    FIND FIRST ttMenuTree
         WHERE ttMenuTree.Mnemonic EQ ENTRY(1,SELF:SCREEN-VALUE,"|")
         NO-ERROR. 
    IF AVAILABLE ttMenuTree THEN
    RUN pProcessClick.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchSelections MAINMENU
ON VALUE-CHANGED OF searchSelections IN FRAME searchFrame
DO:
    lFavorite = ENTRY(2,SELF:SCREEN-VALUE,"|") EQ "yes".
    btnFavorite:LOAD-IMAGE("Graphics/16x16/move_"
        + (IF lFavorite THEN "minus" ELSE "plus")
        + ".png")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-USER
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK MAINMENU 


/* ***************************  Main Block  *************************** */
 
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
 
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} 
DO:  
    lCloseMenu = YES.
    IF USERID("ASI") NE "Nosweat" THEN
        MESSAGE 'Exit Advantzware?' VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE lCloseMenu.
    IF NOT lCloseMenu THEN RETURN NO-APPLY.
    RUN pSetUserSettings (NO).
    RUN system/userLogOut.p (NO, 0).
    QUIT. /* kills all processes */
END.

ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE 
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

ON ANY-PRINTABLE OF FRAME {&FRAME-NAME} ANYWHERE
DO:
    IF lSearchOpen EQ NO THEN DO:
        RUN pKeyPress (FRAME {&FRAME-NAME}:HANDLE, LASTKEY).
        RETURN NO-APPLY.
    END. /* search not open */
END.

{sys/inc/f3helpm.i} /* ASI F3 key include */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.*/
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    SESSION:DATA-ENTRY-RETURN = YES.
    DYNAMIC-FUNCTION("sfSetMainMenuHandle", THIS-PROCEDURE).
    RUN spSetSessionParam ("Company", g_company).
    RUN pGetUserSettings.
    ASSIGN
        {&WINDOW-NAME}:COL = 1
        {&WINDOW-NAME}:ROW = 1
        .
    RUN enable_UI.
    RUN system/checkExpiredLicense.p.
    {methods/enhance.i}
    users_user_id = USERID(LDBNAME(1)).
    DISPLAY users_user_id
        WITH FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
    VIEW FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
    RUN pInit.
    /* close search frame */
    APPLY "CHOOSE":U TO btnSearch.
    ASSIGN
      upgradeMsg:HIDDEN = YES
      FRAME menuTreeFrame:VIRTUAL-HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT
      hFocus = svFocus:HANDLE
      .
    DISPLAY menuTreeMsg WITH FRAME menuTreeFrame.
    RUN pBuildttMenuTree("file").
    RUN pGetFavorites.
    RUN pLoadFavorites.
    menuTreeMsg:HIDDEN = YES.
    RUN pMenuSize.
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
    {system/runCueCard.i}
    RUN sys/ref/nk1look.p (
        g_company,"DynTaskTicker","L",NO,NO,"","",
        OUTPUT cFound,OUTPUT lFound
        ).
    IF lFound AND cFound EQ "yes" THEN DO:    
        RUN sys/ref/nk1look.p (
            g_company,"DynTaskTicker","I",NO,NO,"","",
            OUTPUT cTickerInterval,OUTPUT lFound
            ).
        IF lfound THEN
        iTickerInterval = INTEGER(cTickerInterval).
    END. /* if found */
/*    IF iTickerInterval EQ 0 THEN*/
/*    iTickerInterval = 1.        */
    RUN spGetSessionParam ("PSTimer", OUTPUT cDebug).
    chCtrlFrame:PSTimer:Interval = IF cDebug NE "" THEN 0
                                   ELSE iTickerInterval * 1000.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load MAINMENU  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "mainMenu.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "mainMenu.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI MAINMENU  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(MAINMENU)
  THEN DELETE WIDGET MAINMENU.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI MAINMENU  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY company_name loc_loc users_user_id Mnemonic fFollow 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  ENABLE imageSettings imageCompany menuLinkZoHo imageFolder imagePrinter 
         imageScheduler imageRunUlitity RECT-24 RECT-25 fFollow 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-USER}
  DISPLAY menuTreeFilter favoritesList searchSelections svFavoriteText 
      WITH FRAME searchFrame IN WINDOW MAINMENU.
  ENABLE BtnFavorites btnSearch menuTreeFilter favoritesList searchSelections 
         btnMoveDown btnMoveUp btnRemove btnFavorite 
      WITH FRAME searchFrame IN WINDOW MAINMENU.
  VIEW FRAME searchFrame IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-searchFrame}
  DISPLAY svFocus upgradeMsg 
      WITH FRAME menuTreeFrame IN WINDOW MAINMENU.
  ENABLE svFocus 
      WITH FRAME menuTreeFrame IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-menuTreeFrame}
  VIEW MAINMENU.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildttMenuTree MAINMENU 
PROCEDURE pBuildttMenuTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcparentname AS CHARACTER.
   // EMPTY TEMP-TABLE ttMenuTree.

    FOR EACH prgrms NO-LOCK
        WHERE prgrms.menu_item EQ YES
          AND prgrms.menuOrder GT 0
          AND prgrms.menuLevel GT 0
          AND prgrms.mnemonic  NE ""
          AND prgrms.securityLevelUser LE users.securityLevel
          AND prgrms.itemParent = (IF ipcparentname = "file" THEN prgrms.itemParent ELSE ipcparentname) 
        BY prgrms.menuOrder
        :
        IF cCEMenu EQ "Both" OR 
           prgrms.systemType EQ "Both" OR
           prgrms.systemType EQ cCEMenu THEN DO:
            /* xusermenu holds options not active for user */
            lActive = NOT CAN-FIND(FIRST xUserMenu
                                   WHERE xUserMenu.user_id  EQ USERID("ASI")
                                     AND xUserMenu.prgmname EQ prgrms.prgmname).
            RUN pCreatettMenuTree (
                FRAME menuTreeFrame:HANDLE,
                prgrms.menuOrder,
                prgrms.menuLevel,
                INDEX(prgrms.prgmname,".") EQ 0,
                prgrms.itemParent,
                prgrms.prgmname,
                (IF prgrms.customMenuTitle NE "" THEN prgrms.customMenuTitle ELSE prgrms.prgTitle),
                prgrms.menuImage[1],
                prgrms.mnemonic,
                cShowMnemonic,
                cPositionMnemonic,
                lActive,
                ipcparentname 
                ).
        END. /* if ccemenu */
    END. /* each prgrms */
    /* create an Exit option */
    IF ipcparentname = "FILE" THEN
    
    RUN pCreatettMenuTree (
        FRAME menuTreeFrame:HANDLE,
        9999,                 /* order             */
        1,                    /* level             */
        NO,                   /* is menu           */
        "file",               /* parent            */
        "exit",               /* child             */
        "Exit",               /* text              */
        "logout.ico", /* image             */
        "X",                  /* mnemonic          */
        cShowMnemonic,        /* show mnemonic     */
        cPositionMnemonic,    /* position mnemonic */
        YES,                  /* active            */
        "file"
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckUpgradeAdvantzware MAINMENU 
PROCEDURE pCheckUpgradeAdvantzware :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cHelpService  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE upgradeLink   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cThisVer      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVersion      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hSalesSoap    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hWebService   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lContinue     AS LOGICAL   NO-UNDO.

    IF lAdmin THEN DO:
        RUN sys/ref/nk1look.p (
            g_company,"ASIHelpService","C",NO,NO,"","",
            OUTPUT cHelpService,OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MenuLinkUpgrade","DS",NO,NO,"","",
            OUTPUT upgradeLink, OUTPUT lFound
            ).
        IF upgradeLink EQ "" THEN ASSIGN 
            upgradeLink = "https://helpsvr.advantzware.com/patches/asiUpdate.html".
        CREATE SERVER hWebService.
        hWebService:CONNECT(cHelpService) NO-ERROR.
        IF hWebService:CONNECTED() THEN DO:
            RUN Service1Soap SET hSalesSoap ON hWebService .
            RUN HelpVersion IN hSalesSoap (OUTPUT cVersion).
            ASSIGN
                cThisVer     = "{&awversion}"
                iThisVersion = fIntVer(cThisVer)
                iLastVersion = fIntVer(cVersion).
                
            IF iLastVersion GT iThisVersion THEN DO:
                MESSAGE 
                    "Advantzware version " + cVersion + " is now available for download and " + 
                    "installation.  Your current version is " + cThisVer + "." SKIP 
                    "Would you like to begin the upgrade process now?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue.
                IF NOT lContinue THEN RETURN.
                OS-COMMAND NO-WAIT START VALUE(upgradeLink).
                /* Same as window close, without the Exit? dialog box */
                RUN pSetUserSettings (NO).
                RUN system/userLogOut.p (NO, 0).
                QUIT. /* kills all processes */
            END.
            ELSE IF iLastVersion EQ iThisVersion THEN DO:
                MESSAGE 
                    "Congratulations!  The latest release of the Advantzware system is " +
                    "version " + cVersion + ".  Your system has already been upgraded " + 
                    "to the latest version."
                    VIEW-AS ALERT-BOX INFO.
                RETURN.
            END.
            ELSE IF iLastVersion LT iThisVersion THEN DO:
                MESSAGE 
                    "Your current version is greater than the latest released Advantzware version." SKIP 
                    "This may indicate a problem with your system, or with the ASI versioning " + 
                    "process.  Please contact Advantzware Support for assistance with this issue."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
        END. /* if connected */
        ELSE DO:
            MESSAGE 
                "Version checking is not currently available.  Please try again later." SKIP 
                "If this problem persists, please contact Advantzware Support for assistance."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END. /* if user admin */
    ELSE DO:
        MESSAGE 
            "Your user ID does not have sufficient privileges to run this function." SKIP 
            "Please contact your local System Administrator for assistance."
            VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFavorites MAINMENU 
PROCEDURE pGetFavorites :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF AVAILABLE user-print THEN DO:
        DO idx = 3 TO EXTENT(user-print.field-value):
            IF user-print.field-value[idx] EQ "" THEN LEAVE.
            FIND FIRST ttMenuTree
                 WHERE ttMenuTree.treeChild EQ user-print.field-value[idx]
                 NO-ERROR.
            IF AVAILABLE ttMenuTree THEN
            ASSIGN
                ttMenuTree.favorite      = YES
                ttMenuTree.favoriteOrder = idx - 2
                .
        END. /* do idx */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMenuSettings MAINMENU 
PROCEDURE pGetMenuSettings :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cNK1Value     AS CHARACTER NO-UNDO EXTENT 4.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hPgmMstrSecur AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lCanProfile   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAccessList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAccessClose  AS LOGICAL   NO-UNDO. 
       
    RUN sys/ref/nk1look.p (
        g_company,"CEMenu","C",NO,NO,"","",
        OUTPUT cCEMenu,OUTPUT lFound
        ).
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            menuLinkASI:HIDDEN     = YES
            menuLinkASI:SENSITIVE  = NO
            menuLinkZoHo:HIDDEN    = YES
            menuLinkZoHo:SENSITIVE = NO
            menuLinkZoHo:HIDDEN    = YES
            menuLinkZoHo:SENSITIVE = NO
            menuLink-1:HIDDEN      = YES
            menuLink-1:HIDDEN      = NO
            menuLink-2:HIDDEN      = YES
            menuLink-2:HIDDEN      = NO
            menuLink-3:HIDDEN      = YES
            menuLink-3:HIDDEN      = NO
            menuLink-4:HIDDEN      = YES
            menuLink-4:HIDDEN      = NO
            menuLink-5:HIDDEN      = YES
            menuLink-5:HIDDEN      = NO
            menuLink-6:HIDDEN      = YES
            menuLink-6:HIDDEN      = NO
            menuLink-7:HIDDEN      = YES
            menuLink-7:HIDDEN      = NO
            menuLink-8:HIDDEN      = YES
            menuLink-8:HIDDEN      = NO
            .
        menuLink-1:LOAD-IMAGE(?).
        menuLink-2:LOAD-IMAGE(?).
        menuLink-3:LOAD-IMAGE(?).
        menuLink-4:LOAD-IMAGE(?).
        menuLink-5:LOAD-IMAGE(?).
        menuLink-6:LOAD-IMAGE(?).
        menuLink-7:LOAD-IMAGE(?).
        menuLink-8:LOAD-IMAGE(?).
        boxes:LOAD-IMAGE(?).
        RUN sys/ref/nk1look.p (
            g_company,"BitMap","DS",NO,NO,"","",
            OUTPUT cBitMap,OUTPUT lFound
            ).
        IF lFound AND cBitMap NE ? THEN
        boxes:LOAD-IMAGE(cBitMap) NO-ERROR.
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKASI","C",NO,NO,"","",
            OUTPUT cNK1Value[1],OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKASI","DS",NO,NO,"","",
            OUTPUT cNK1Value[2],OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKASI","I",NO,NO,"","",
            OUTPUT cNK1Value[3],OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKASI","L",NO,NO,"","",
            OUTPUT cNK1Value[4],OUTPUT lFound
            ).
        IF SEARCH(cNK1Value[1]) NE ? AND
           cNK1Value[2] NE "" THEN DO:
            ASSIGN
                menuLinkASI:PRIVATE-DATA   = cNK1Value[2]
                menuLinkASI:HIDDEN         = NO
                menuLinkASI:SENSITIVE      = YES
                menuLinkASI:STRETCH-TO-FIT = cNK1Value[4] EQ "YES"
                menuLinkASI:TRANSPARENT    = cNK1Value[3] EQ "1"
                .
            menuLinkASI:LOAD-IMAGE(SEARCH(cNK1Value[1])).
        END. /* if avail */
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKZOHO","C",NO,NO,"","",
            OUTPUT cNK1Value[1],OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKZOHO","DS",NO,NO,"","",
            OUTPUT cNK1Value[2],OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKZOHO","I",NO,NO,"","",
            OUTPUT cNK1Value[3],OUTPUT lFound
            ).
        RUN sys/ref/nk1look.p (
            g_company,"MENULINKZOHO","L",NO,NO,"","",
            OUTPUT cNK1Value[4],OUTPUT lFound
            ).
        IF SEARCH(cNK1Value[1]) NE ? AND
           cNK1Value[2] NE "" THEN DO:
            ASSIGN
                menuLinkZoHo:PRIVATE-DATA   = cNK1Value[2]
                menuLinkZoHo:HIDDEN         = NO
                menuLinkZoHo:SENSITIVE      = YES
                menuLinkZoHo:STRETCH-TO-FIT = cNK1Value[4] EQ "YES"
                menuLinkZoHo:TRANSPARENT    = cNK1Value[3] EQ "1"
                .
            menuLinkZoHo:LOAD-IMAGE(SEARCH(cNK1Value[1])).
        END. /* if avail */

        ASSIGN
            hMenuLink[1] = menuLink-1:HANDLE
            hMenuLink[2] = menuLink-2:HANDLE
            hMenuLink[3] = menuLink-3:HANDLE
            hMenuLink[4] = menuLink-4:HANDLE
            hMenuLink[5] = menuLink-5:HANDLE
            hMenuLink[6] = menuLink-6:HANDLE
            hMenuLink[7] = menuLink-7:HANDLE
            hMenuLink[8] = menuLink-8:HANDLE
            imageSettings:TOOLTIP = "User Settings:" + CHR(10)
                                  + "    Customize Menu" + CHR(10)
                                  + "    Set Language"   + CHR(10)
                                  + "    Set Menu Size"
                                  .
        DO idx = 1 TO EXTENT(hMenuLink):
            RUN sys/ref/nk1look.p (
                g_company,"MENULINK" + STRING(idx),"C",NO,NO,"","",
                OUTPUT cNK1Value[1],OUTPUT lFound
                ).
            RUN sys/ref/nk1look.p (
                g_company,"MENULINK" + STRING(idx),"DS",NO,NO,"","",
                OUTPUT cNK1Value[2],OUTPUT lFound
                ).
            RUN sys/ref/nk1look.p (
                g_company,"MENULINK" + STRING(idx),"I",NO,NO,"","",
                OUTPUT cNK1Value[3],OUTPUT lFound
                ).
            RUN sys/ref/nk1look.p (
                g_company,"MENULINK" + STRING(idx),"L",NO,NO,"","",
                OUTPUT cNK1Value[4],OUTPUT lFound
                ).
            IF SEARCH(cNK1Value[1]) NE ? AND
               cNK1Value[2] NE "" THEN DO:
                ASSIGN
                    hMenuLink[idx]:PRIVATE-DATA   = cNK1Value[2]
                    hMenuLink[idx]:HIDDEN         = NO
                    hMenuLink[idx]:SENSITIVE      = YES
                    hMenuLink[idx]:STRETCH-TO-FIT = cNK1Value[4] EQ "YES"
                    hMenuLink[idx]:TOOLTIP        = cNK1Value[2]
                    hMenuLink[idx]:TRANSPARENT    = cNK1Value[3] EQ "1"
                    .
                hMenuLink[idx]:LOAD-IMAGE(SEARCH(cNK1Value[1])).
            END. /* if search */
        END. /* do idx */
        /* check if upgrade available */
        IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
        RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
        IF VALID-HANDLE(hPgmMstrSecur) THEN DO:
            RUN epCanAccess IN hPgmMstrSecur (
                "system/mainMenu.w",
                "CanUpgrade",
                OUTPUT lAdmin 
                ).
            RUN epCanAccess IN hPgmMstrSecur (
                "system/mainMenu.w",
                "",
                OUTPUT lSuperAdmin 
                ).
            RUN epCanAccess IN hPgmMstrSecur (
                "profiler",
                "",
                OUTPUT lCanProfile 
                ).
            
        END. /* if valid handle */
        RUN methods/prgsecur.p ("Profiler", 
            "Access",
            NO,
            NO,
            NO,
            OUTPUT lCanProfile,
            OUTPUT lAccessClose,
            OUTPUT cAccessList  
            ).         
       
        IF NOT lCanProfile THEN
        MENU-item m_Profiler:SENSITIVE IN MENU m_help = FALSE.             
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserSettings MAINMENU 
PROCEDURE pGetUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLanguage  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWinHeight AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWinWidth  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
    
    ASSIGN
        iLanguage = 1 /* english */
        iMenuSize = 1 /* small menu */
        lMenuImage  = YES
        iFGColor[1] = 15
        iFGColor[2] = 15
        iFGColor[3] = 15
        iBGColor[1] = 1
        iBGColor[2] = 3
        iBGColor[3] = 1
        .
    DO i = 1 TO NUM-ENTRIES(cLanguageList):
        cList = cList + ENTRY(i,cLanguageList) + ","
                      + STRING(i) + ","
                      .
    END. /* do idx */
    cList = TRIM(cList,",").

    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE users THEN DO:
        ASSIGN
            iMenuSize         = LOOKUP(users.menuSize,"Small,Medium,Large")
            cShowMnemonic     = users.showMnemonic
            cPositionMnemonic = users.positionMnemonic
            lMenuImage        = users.showMenuImages
            iFGColor[1]       = users.menuFGColor[1]
            iFGColor[2]       = users.menuFGColor[2]
            iFGColor[3]       = users.menuFGColor[3]
            iBGColor[1]       = users.menuBGColor[1]
            iBGColor[2]       = users.menuBGColor[2]
            iBGColor[3]       = users.menuBGColor[3]
            .
        IF users.use_fonts THEN
        iEditorFont = users.widget_font[5].
        IF users.use_colors THEN
        ASSIGN
            iEditorBGColor    = users.widget_bgc[5]
            iEditorFGColor    = users.widget_fgc[5]
            iFrameBGColor     = users.widget_bgc[7]
            iFrameFGColor     = users.widget_fgc[7]
            iRectangleBGColor = users.widget_bgc[11]
            iRectangleFGColor = users.widget_fgc[11]
            .
        FIND FIRST userLanguage NO-LOCK
             WHERE userLanguage.userLanguage EQ users.userLanguage
             NO-ERROR.
        IF AVAILABLE userLanguage THEN
        iLanguage = userLanguage.languageIdx.
    END. /* avail users */
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "MainMenu"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN
    ASSIGN 
        iWinHeight = DECIMAL(user-print.field-value[1])
        iWinWidth  = DECIMAL(user-print.field-value[2])
        .
    
    IF iLanguage LT 1 THEN iLanguage = 1.
    IF iMenuSize LT 1 THEN iMenuSize = 1.
    cLabelLanguage = ENTRY(iLanguage,cLanguageList).

    IF iWinHeight LT 28.57 THEN
    iWinHeight = 28.57.
    IF iWinWidth  LT 160 THEN
    iWinWidth  = 160.
    ASSIGN
        {&WINDOW-NAME}:HEIGHT = iWinHeight
        {&WINDOW-NAME}:WIDTH  = iWinWidth
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
        .
    
    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmname EQ "mainmenu2."
         NO-ERROR.
    IF AVAILABLE prgrms THEN DO:
        IF prgrms.use_fonts THEN
        iEditorFont           = prgrms.widget_font[5].
        IF prgrms.use_colors THEN
        ASSIGN
            iEditorBGColor    = prgrms.widget_bgc[5]
            iEditorFGColor    = prgrms.widget_fgc[5]
            iFrameBGColor     = prgrms.widget_bgc[7]
            iFrameFGColor     = prgrms.widget_fgc[7]
            iRectangleBGColor = prgrms.widget_bgc[11]
            iRectangleFGColor = prgrms.widget_fgc[11]
            .
    END. /* avail prgrms */
    
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit MAINMENU 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pMenuSize.
    
    RUN pGetMenuSettings.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadFavorites MAINMENU 
PROCEDURE pLoadFavorites :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO WITH FRAME searchFrame:
        favoritesList:LIST-ITEM-PAIRS = ?.
        FOR EACH ttMenuTree
            WHERE ttMenuTree.favorite EQ YES
               BY ttMenuTree.favoriteOrder
            :
            ASSIGN
                idx = idx + 1
                ttMenuTree.favoriteOrder = idx
                .
            favoritesList:ADD-LAST(
                STRING(ttMenuTree.favoriteOrder) + ".  " +
                fTreeText(ttMenuTree.isMenu,
                          ttMenuTree.baseText,
                          ttMenuTree.mnemonic,
                          cShowMnemonic,
                          cPositionMnemonic),ttMenuTree.mnemonic
                          ).
        END. /* each ttmenutree */
    END. /* favoritesFrame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMenuLinkClick MAINMENU 
PROCEDURE pMenuLinkClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiLink AS INTEGER NO-UNDO.
    
&IF DEFINED(FWD-VERSION) > 0 &THEN
    open-mime-resource "text/html" string(hMenuLink[ipiLink]:PRIVATE-DATA) false.
&ELSE
    
    OS-COMMAND NO-WAIT START VALUE(hMenuLink[ipiLink]:PRIVATE-DATA).
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMenuSize MAINMENU 
PROCEDURE pMenuSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    CASE iMenuSize:
        WHEN 1 THEN
        ASSIGN
            cImageFolder  = "Graphics/16X16/"
            dObjectHeight = 1.11    
            dObjectWidth  = 3.20
            iFont         = 32
            .
        WHEN 2 THEN
        ASSIGN
            cImageFolder  = "Graphics/24X24/"
            dObjectHeight = 1.33
            dObjectWidth  = 4.8
            iFont         = 34
            .
        WHEN 3 THEN
        ASSIGN
            cImageFolder  = "Graphics/32X32/"
            dObjectHeight = 2
            dObjectWidth  = 6.40
            iFont         = 36
            .
    END CASE.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    FOR EACH ttMenuTree where ttMenuTree.lWidgetExist:
        IF VALID-HANDLE(ttMenuTree.hLevel) THEN DO:
            ASSIGN
                ttMenuTree.hLevel:HIDDEN = YES
                ttMenuTree.hLevel:ROW    = 1
                ttMenuTree.hLevel:WIDTH  = dObjectWidth
                ttMenuTree.hLevel:HEIGHT = dObjectHeight
                .
            ttMenuTree.hLevel:LOAD-IMAGE(SEARCH(cImageFolder + "plus.ico")).
        END. /* if hlevel */
        IF VALID-HANDLE(ttMenuTree.hImage) THEN DO:
            ASSIGN
                ttMenuTree.hImage:HIDDEN = YES
                ttMenuTree.hImage:ROW    = 1
                ttMenuTree.hImage:WIDTH  = dObjectWidth
                ttMenuTree.hImage:HEIGHT = dObjectHeight
                .
            ttMenuTree.hImage:LOAD-IMAGE(SEARCH(cImageFolder + ttMenuTree.treeImage)).
        END. /* if himage */
        IF VALID-HANDLE(ttMenuTree.hEditor) THEN
        ASSIGN
            ttMenuTree.hEditor:HIDDEN = YES
            ttMenuTree.hEditor:ROW    = 1
            ttMenuTree.hEditor:HEIGHT = dObjectHeight
            ttMenuTree.hEditor:FONT   = iFont
            .
            IF VALID-HANDLE(ttMenuTree.hRectangle) THEN
        ASSIGN
            ttMenuTree.hRectangle:HIDDEN = YES
            ttMenuTree.hRectangle:ROW    = 1
            ttMenuTree.hRectangle:HEIGHT = dObjectHeight
            ttMenuTree.hRectangle:WIDTH   = dObjectWidth
            .
        IF VALID-HANDLE(ttMenuTree.hToggle) THEN
        ASSIGN
            ttMenuTree.hToggle:HIDDEN = YES
            ttMenuTree.hToggle:ROW    = 1
            ttMenuTree.hToggle:HEIGHT = dObjectHeight
            .
    END. /* each ttmenutree */
    
    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMoveFavorite MAINMENU 
PROCEDURE pMoveFavorite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cFavorite AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.

    DO WITH FRAME searchFrame:
        cFavorite = favoritesList:SCREEN-VALUE.
        FIND FIRST ttMenuTree
             WHERE ttMenuTree.Mnemonic EQ ENTRY(1,favoritesList:SCREEN-VALUE,"|")
             NO-ERROR.
        IF AVAILABLE ttMenuTree THEN DO:
            ttMenuTree.favoriteOrder = ttMenuTree.favoriteOrder + ipiMove.
            FOR EACH ttMenuTree
                WHERE ttMenuTree.favorite EQ YES
                   BY ttMenuTree.favoriteOrder
                :
                ASSIGN
                    idx = idx + 1
                    ttMenuTree.favoriteOrder = idx
                    .
            END. /* each ttmenutree */
            RUN pLoadFavorites.
            favoritesList:SCREEN-VALUE = cFavorite.
        END. /* if avail */
    END. /* frame searchframe */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOnOffProfiler MAINMENU 
PROCEDURE pOnOffProfiler :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lProfile AS LOGICAL NO-UNDO.
    IF PROFILER:ENABLED THEN 
    DO:
        ASSIGN 
            PROFILER:PROFILING                       = FALSE                         
            PROFILER:ENABLED                         = FALSE
            company_name:BGCOLOR IN FRAME frame-user = iSaveBgColor
            iProfileStartTime                        = TIME                 
            . 
        PROFILER:WRITE-DATA().
        MESSAGE "Profiler has been turned off and " SKIP 
            "the data file is ready for use:" SKIP(1)
            cProfilerFile
            VIEW-AS ALERT-BOX.
    END.
    ELSE 
    DO:
        MESSAGE "Warning:  The profiler can create a large file" SKIP
            "if left on.  Please do not run unless instructed to" SKIP
            "and remember to turn off profiling" SKIP
            "as soon as the relevant process is complete." SKIP 
            SKIP(1)
            "Do you want to start the profiler?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lProfile.
        IF lProfile THEN 
        DO:
            ASSIGN  
                cProfilerFile                            = SESSION:TEMP-DIRECTORY + "profile" 
                                   + STRING(TODAY, "999999") + "_"
                                   + REPLACE(STRING(TIME, "HH:MM:SS") + ".prof", ":","-")
                PROFILER:ENABLED                         = TRUE
                PROFILER:DESCRIPTION                     = STRING(TODAY,"999999") + "_" + STRING(TIME, "HH:MM:SS")
                PROFILER:FILE-NAME                       = cProfilerFile
                PROFILER:PROFILING                       = TRUE
                PROFILER:TRACE-FILTER                    = "*"
                iSaveBgColor                             = company_name:BGCOLOR IN FRAME frame-user
                company_name:BGCOLOR IN FRAME frame-user = 12
                iProfileStartTime                        = TIME 
                .
        END.
    END. 
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessClick MAINMENU 
PROCEDURE pProcessClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.
    
    /* return if in customize menu mode */
    IF lToggle THEN RETURN.
    
    IF AVAILABLE ttMenuTree THEN DO:
        IF ttMenuTree.isMenu AND NOT ttMenuTree.isOpen THEN
        ASSIGN
            ttMenuTree.hEditor:FONT    = iFont
            ttMenuTree.hEditor:BGCOLOR = iEditorBGColor
            ttMenuTree.hEditor:FGCOLOR = ?
            .
        ELSE
        cMnemonic = ttMenuTree.mnemonic.
        IF NOT ttMenuTree.isMenu THEN DO:
            /* check module license first before run */
            RUN util/CheckModule.p ("ASI", ttMenuTree.treeChild, YES, OUTPUT lAccess) NO-ERROR.
            IF lAccess THEN 
            RUN Get_Procedure IN Persistent-Handle(ttMenuTree.treeChild,OUTPUT run-proc,YES).
        END. /* if program */
    END. /* if avail not ismenu */
    Mnemonic:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMnemonic.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessProfiler MAINMENU 
PROCEDURE pProcessProfiler :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iProfileRunTime AS INTEGER NO-UNDO.
    IF PROFILER:ENABLED THEN DO:
      IF company_name:BGCOLOR IN FRAME frame-user = iSaveBgColor THEN   
        company_name:BGCOLOR IN FRAME frame-user = 12.
      ELSE 
        company_name:BGCOLOR IN FRAME frame-user = iSaveBgColor.
      iProfileRunTime = TIME - iProfileStartTime.
      /* If has been running for more than 20 minutes, stop it automatically */
      IF iProfileRunTime / 60 GT 20 THEN 
        RUN pOnOffProfiler.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildMenuTree MAINMENU 
PROCEDURE pRebuildMenuTree :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    RUN pInitMenuTree.
    RUN pBuildttMenuTree("file").
    RUN pMenuSize.
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
    RUN pGetFavorites.
    RUN pLoadFavorites.
    
    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReset MAINMENU 
PROCEDURE pReset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    {&WINDOW-NAME}:TITLE = fTranslate({&WINDOW-NAME}:PRIVATE-DATA,NO)
                         + " {&awversion}"
                         .
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    FOR EACH ttMenuTree:
        ASSIGN
            ttMenuTree.baseText             = fTranslate(ENTRY(1,ttMenuTree.hEditor:PRIVATE-DATA),NO)
            ttMenuTree.hEditor:FONT         = iFont
            ttMenuTree.hEditor:BGCOLOR      = iEditorBGColor
            ttMenuTree.hEditor:FGCOLOR      = ?
            ttMenuTree.hEditor:SCREEN-VALUE = fTreeText(ttMenuTree.isMenu,
                                              ttMenuTree.baseText,
                                              ttMenuTree.mnemonic,
                                              cShowMnemonic,
                                              cPositionMnemonic
                                              )
            ttMenuTree.hEditor:TOOLTIP      = "HotKey: " + ttMenuTree.mnemonic
            .
        IF VALID-HANDLE(ttMenuTree.hImage) THEN
        ttMenuTree.hImage:HIDDEN = YES.
        IF VALID-HANDLE(ttMenuTree.hToggle) THEN
        ttMenuTree.hToggle:HIDDEN = YES.
    END. /* each ttmenutree */
    RUN pMenuSize.
    RUN pKeyPress (FRAME menuTreeFrame:HANDLE, 32).
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
    RUN pLoadFavorites.
    APPLY "VALUE-CHANGED":U TO menuTreeFilter IN FRAME searchFrame.

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetByUser MAINMENU
PROCEDURE pResetByUser:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRebuildMenu AS LOGICAL NO-UNDO.
    
    IF iplRebuildMenu THEN DO:
        RUN pGetUserSettings.
        RUN pRebuildMenuTree.
    END. /* if rebuild */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSearchSelections MAINMENU 
PROCEDURE pSearchSelections :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME searchFrame:
        ASSIGN 
            menuTreeFilter
            searchSelections:LIST-ITEM-PAIRS = ?
            .
        FOR EACH ttMenuTree
            WHERE ttMenuTree.baseText MATCHES "*" + menuTreeFilter + "*"
              AND ttMenuTree.isActive EQ YES
              AND ttMenuTree.isMenu   EQ NO
            :
            IF NOT CAN-FIND(FIRST xUserMenu
                            WHERE xUserMenu.user_id  EQ USERID("ASI")
                              AND xUserMenu.prgmname EQ ttMenuTree.treeChild) THEN
            searchSelections:ADD-LAST(fTreeText(ttMenuTree.isMenu,
                                                ttMenuTree.baseText,
                                                ttMenuTree.mnemonic,
                                                cShowMnemonic,
                                                cPositionMnemonic)
                + (IF lSuperAdmin THEN "  ( " + STRING(ttMenuTree.treeOrder)
                + " - " + CAPS(ttMenuTree.treeParent) + " )" ELSE "")
                , ttMenuTree.mnemonic
                + "|" + STRING(ttMenuTree.favorite)
                )
                .
        END. /* each ttmenutree */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetUserSettings MAINMENU 
PROCEDURE pSetUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplSaveAll AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST user-print EXCLUSIVE-LOCK
             WHERE user-print.company    EQ g_company
               AND user-print.program-id EQ "MainMenu"
               AND user-print.user-id    EQ USERID("ASI")
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN DO:
            CREATE user-print.
            ASSIGN
                user-print.company    = g_company
                user-print.program-id = "MainMenu"
                user-print.user-id    = USERID("ASI")
                .
        END. /* if not avail */
        ASSIGN
            user-print.field-value    = ""
            user-print.field-value[1] = STRING({&WINDOW-NAME}:HEIGHT)
            user-print.field-value[2] = STRING({&WINDOW-NAME}:WIDTH)
            idx = 2
            .
        FOR EACH ttMenuTree
            WHERE ttMenuTree.favorite EQ YES
               BY ttMenuTree.favoriteOrder
            :
            ASSIGN
                idx = idx + 1
                user-print.field-value[idx] = ttMenuTree.treeChild
                .
        END. /* each ttmenutree */
        RELEASE user-print.
    END. /* do trans */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize MAINMENU 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    FOR EACH ttMenuTree:
        IF VALID-HANDLE(ttMenuTree.hLevel)  THEN
        ASSIGN
            ttMenuTree.hLevel:HIDDEN = YES
            ttMenuTree.hLevel:COL    = 1
            ttMenuTree.hLevel:ROW    = 1
            .
        IF VALID-HANDLE(ttMenuTree.hImage)  THEN
        ASSIGN
            ttMenuTree.hImage:HIDDEN  = YES
            ttMenuTree.hImage:COL     = 1
            ttMenuTree.hImage:ROW     = 1
            .
        IF VALID-HANDLE(ttMenuTree.hEditor) THEN
        ASSIGN
            ttMenuTree.hEditor:HIDDEN = YES
            ttMenuTree.hEditor:COL    = 1
            ttMenuTree.hEditor:ROW    = 1
            .
        IF VALID-HANDLE(ttMenuTree.hToggle) THEN
        ASSIGN
            ttMenuTree.hToggle:HIDDEN = YES
            ttMenuTree.hToggle:COL    = 1
            ttMenuTree.hToggle:ROW    = 1
            .
        IF VALID-HANDLE(ttMenuTree.hRectangle) THEN
            ASSIGN
                ttMenuTree.hRectangle:HIDDEN = YES
                ttMenuTree.hRectangle:COL    = 1
                ttMenuTree.hRectangle:ROW    = 1
                .
    END. /* each ttMenuTree */
    
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME menuTreeFrame.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            /* default frame */
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH = {&WINDOW-NAME}:WIDTH
            /* filter frame */
            FRAME menuTreeFrame:VIRTUAL-HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            FRAME menuTreeFrame:VIRTUAL-WIDTH  = FRAME {&FRAME-NAME}:WIDTH  - 105
            FRAME menuTreeFrame:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT - 2.62
            FRAME menuTreeFrame:WIDTH  = FRAME {&FRAME-NAME}:WIDTH  - 105
            /* menu links, images & rectangles */
            imageSettings:HIDDEN = YES
            menu-image:HIDDEN    = YES
            boxes:HIDDEN         = YES
            Mnemonic:HIDDEN      = YES
            RECT-2:HIDDEN        = YES
            RECT-8:HIDDEN        = YES
            RECT-9:HIDDEN        = YES
            RECT-10:HIDDEN       = YES
            RECT-11:HIDDEN       = YES
            RECT-12:HIDDEN       = YES
            menuLinkASI:HIDDEN   = YES 
            menuLinkZoHo:HIDDEN  = YES
            fFollow:HIDDEN       = YES 
            menuLink-1:HIDDEN    = YES
            menuLink-2:HIDDEN    = YES
            menuLink-3:HIDDEN    = YES
            menuLink-4:HIDDEN    = YES
            menuLink-5:HIDDEN    = YES
            menuLink-6:HIDDEN    = YES
            menuLink-7:HIDDEN    = YES
            menuLink-8:HIDDEN    = YES 
            fFollow:Row = FRAME {&FRAME-NAME}:HEIGHT - 2.5
            
            menuLink-1:ROW       = FRAME {&FRAME-NAME}:HEIGHT - 1.61
            menuLink-2:ROW       = menuLink-1:ROW
            menuLink-3:ROW       = menuLink-1:ROW
            menuLink-4:ROW       = menuLink-1:ROW
            menuLink-5:ROW       = menuLink-1:ROW
            menuLink-6:ROW       = menuLink-1:ROW
            menuLink-7:ROW       = menuLink-1:ROW
            menuLink-8:ROW       = menuLink-1:ROW
            imageSettings:COL    = FRAME {&FRAME-NAME}:WIDTH - 8
            menu-image:COL       = FRAME {&FRAME-NAME}:WIDTH - menu-image:WIDTH   - 12
            boxes:COL            = FRAME {&FRAME-NAME}:WIDTH - boxes:WIDTH
        //    boxes:height         = FRAME {&FRAME-NAME}:height - 2.5
            RECT-2:WIDTH         = FRAME {&FRAME-NAME}:WIDTH - .4
            RECT-8:COL           = FRAME {&FRAME-NAME}:WIDTH - 19
            RECT-9:WIDTH         = FRAME {&FRAME-NAME}:WIDTH - .4
            RECT-10:COL          = FRAME {&FRAME-NAME}:WIDTH - RECT-10:WIDTH      - 11
            RECT-11:COL          = FRAME {&FRAME-NAME}:WIDTH - RECT-11:WIDTH
            RECT-12:COL          = FRAME {&FRAME-NAME}:WIDTH - RECT-12:WIDTH
            Mnemonic:COL         = FRAME {&FRAME-NAME}:WIDTH - 17 
            menuLinkASI:COL      = FRAME {&FRAME-NAME}:WIDTH - menuLinkASI:WIDTH  - 1
            menuLinkZoHo:COL     = FRAME {&FRAME-NAME}:WIDTH - menuLinkZoHo:WIDTH - 1
                        menuLink-1:COL       = FRAME {&FRAME-NAME}:WIDTH - menuLink-1:WIDTH  - 15
            menuLink-2:COL       = menuLink-1:COL - 9
            menuLink-3:COL       = menuLink-2:COL - 9
            menuLink-4:COL       = menuLink-3:COL - 9
            
            menuLink-5:COL       = menuLink-4:COL - 9
            fFollow:col = menuLink-5:COL
            menuLink-6:COL       = menuLink-5:COL - 9
            menuLink-7:COL       = menuLink-6:COL - 9
            menuLink-8:COL       = menuLink-7:COL - 9
            imageSettings:HIDDEN = NO
            menu-image:HIDDEN    = NO
            boxes:HIDDEN         = NO
            Mnemonic:HIDDEN      = NO
            RECT-2:HIDDEN        = NO
            RECT-8:HIDDEN        = NO
            RECT-9:HIDDEN        = NO
           // RECT-10:HIDDEN       = NO
            RECT-11:HIDDEN       = NO
            RECT-12:HIDDEN       = NO
            menuLinkASI:HIDDEN   = NO  
            menuLinkZoHo:HIDDEN  = NO
            menuLink-1:HIDDEN    = NO
            menuLink-2:HIDDEN    = NO
            menuLink-3:HIDDEN    = NO
            menuLink-4:HIDDEN    = NO
            menuLink-5:HIDDEN    = NO
            menuLink-6:HIDDEN    = NO
            menuLink-7:HIDDEN    = NO
            menuLink-8:HIDDEN    = NO 
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW FRAME menuTreeFrame.
    END. /* do with */
    RUN pKeyPress (FRAME menuTreeFrame:HANDLE, 32).
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
    VIEW FRAME searchFrame.

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Comp_Loc MAINMENU 
PROCEDURE Set-Comp_Loc :
/*------------------------------------------------------------------------------
      Purpose:     Set Global and Screen Company/Location Values.
      Parameters:  INPUT company & location values
      Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLoc         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocDscr    AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            company_name:SCREEN-VALUE = ipcCompanyName + " (" + ipcCompany + ")"
            loc_loc:SCREEN-VALUE      = ipcLoc
            company_name
            loc_loc
            g_company                 = ipcCompany
            g_loc                     = ipcLoc
            .
    END.
    RUN spSetSessionParam ("Company", g_company).
    RUN spSetSessionParam ("Location", g_loc).
    DYNAMIC-FUNCTION("sfClearUsage").
    RUN pGetMenuSettings.
/*    RUN sys/ref/nk1look.p (                 */
/*        g_company,"BitMap","DS",NO,NO,"","",*/
/*        OUTPUT cBitMap,OUTPUT lFound        */
/*        ).                                  */
/*    IF lFound AND cBitMap NE "" THEN        */
/*    boxes:LOAD-IMAGE(cBitMap).              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIntVer MAINMENU 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Converts a version string like "16.4.8" or "16.7.12.2" to an integer
    Notes:  In the cases above, these would be 16040800 and 16071202
            Useful for version comparisons
------------------------------------------------------------------------------*/

    DEF VAR cStrVal AS CHAR EXTENT 4 NO-UNDO.
    DEF VAR iIntVal AS INT EXTENT 4 NO-UNDO.
    DEF VAR iIntVer AS INT NO-UNDO.
    ASSIGN
        cStrVal[1] = ENTRY(1,cVerString,".")
        cStrVal[2] = ENTRY(2,cVerString,".")
        cStrVal[3] = IF NUM-ENTRIES(cVerString,".") GT 2 THEN ENTRY(3,cVerString,".") ELSE "0"
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GT 3 THEN ENTRY(4,cVerString,".") ELSE "0"
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

