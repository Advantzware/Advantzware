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
&Scoped-define BGColor 8

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

/* System Constant Values */
{system/sysconst.i}

DEFINE VARIABLE closeMenu         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEulaFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEulaAccepted     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEulaVersion      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUserExit         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSourceMenu       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOK               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hMenuLink         AS HANDLE    NO-UNDO EXTENT 8.
DEFINE VARIABLE iEditorFont       AS INTEGER   NO-UNDO INITIAL ?.
DEFINE VARIABLE iEditorBGColor    AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iEditorFGColor    AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iFrameBGColor     AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iFrameFGColor     AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iRectangleBGColor AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iRectangleFGColor AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE cCEMenu           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBitMap           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSearchOpen       AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lFavorite         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE idx               AS INTEGER   NO-UNDO.
DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSuperAdmin       AS LOGICAL   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS imageSettings imageCompany menuLinkZoHo 
&Scoped-Define DISPLAYED-OBJECTS company_name loc_loc users_user_id ~
Mnemonic 

/* Custom List Definitions                                              */
/* searchFilters,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define searchFilters menuTreeFilter btnMoveDown searchSelections ~
btnMoveUp btnRemove btnFavorite svFavoriteText 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR MAINMENU AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Help 
       MENU-ITEM m_SysCtrl_Usage LABEL "SysCtrl Usage" 
       MENU-ITEM m_Advantzware_Version LABEL "Advantzware Version"
       RULE
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE MENU MENU-BAR-MAINMENU MENUBAR
       SUB-MENU  m_Help         LABEL "Help"          .


/* Definitions of the field level widgets                               */
DEFINE VARIABLE company_name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE loc_loc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Mnemonic AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 5 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE users_user_id AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     FONT 6 NO-UNDO.

DEFINE IMAGE boxes
     FILENAME "Graphics/advantzware_logo.jpg":U
     SIZE 103 BY 18.1.

DEFINE IMAGE imageCompany
     FILENAME "Graphics/32x32/office_building.png":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Change Company/Location".

DEFINE IMAGE imageSettings
     FILENAME "Graphics/32x32/gearwheels.ico":U TRANSPARENT
     SIZE 6.4 BY 1.52 TOOLTIP "Settings".

DEFINE IMAGE menu-image
     FILENAME "Graphics/logo1.bmp":U CONVERT-3D-COLORS
     SIZE 90 BY 4.52.

DEFINE IMAGE menuLink-1
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-2
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-3
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-4
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-5
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-6
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-7
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLink-8
     SIZE 12 BY 2.29.

DEFINE IMAGE menuLinkASI TRANSPARENT
     SIZE 7 BY 1.67 TOOLTIP "Advantzware Link".

DEFINE IMAGE menuLinkZoHo TRANSPARENT
     SIZE 7 BY 1.67 TOOLTIP "Advantzware Help Tickets".

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 92 BY 5.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 10 BY 5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 159.6 BY 2.14
     BGCOLOR 1 FGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 48 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 39 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 40 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 9 BY 1.19
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 159.6 BY .05
     BGCOLOR 0 .

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
     FONT 1.

DEFINE BUTTON btnFavorite 
     IMAGE-UP FILE "Graphics/16x16/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Fav" 
     SIZE 5 BY 1.19.

DEFINE BUTTON BtnFavorites 
     IMAGE-UP FILE "Graphics/16x16/star.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.6 BY 1 TOOLTIP "Search Menu / Edit Favorites".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/16x16/navigate_down.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Favorite Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/16x16/navigate_up.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Favorite Up".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Remove Favorite".

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/16x16/filterwindow.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Search Menu / Edit Favorites".

DEFINE VARIABLE menuTreeFilter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Name Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE svFavoriteText AS CHARACTER FORMAT "X(256)":U INITIAL "Favorites" 
      VIEW-AS TEXT 
     SIZE 10 BY .71
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 45 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE favoritesList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "FAVORITES","FAVORITES" 
     SIZE 45 BY 12.62 NO-UNDO.

DEFINE VARIABLE searchSelections AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 56 BY 11.43
     FONT 1 NO-UNDO.

DEFINE BUTTON btnActivateCueCards 
     LABEL "Activate Inactive Cue Cards" 
     SIZE 29 BY 1.14.

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 9 BY 1.91 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON btnCopyToUser 
     LABEL "Copy From User to Selected User(s)" 
     SIZE 40 BY 1.91 TOOLTIP "Copy From User to Selected User(s)".

DEFINE BUTTON btnLanguage-1  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 1" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnLanguage-2  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 2" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnLanguage-3  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 3" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "OK" 
     SIZE 9 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE BUTTON btnToggle 
     LABEL "Customize Menu" 
     SIZE 35 BY 1.91 TOOLTIP "Customize Menu".

DEFINE VARIABLE copyFromUser AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 40 BY 1 TOOLTIP "Select User"
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/16x16/navigate_right.png":U TRANSPARENT
     SIZE 3.2 BY .76.

DEFINE IMAGE IMAGE-2
     FILENAME "Graphics/24x24/navigate_right.png":U TRANSPARENT
     SIZE 4.8 BY 1.14.

DEFINE IMAGE IMAGE-3
     FILENAME "Graphics/32x32/navigate_right.png":U TRANSPARENT
     SIZE 6.4 BY 1.52.

DEFINE IMAGE IMAGE-4
     FILENAME "Graphics/16x16/calendar_clock.png":U TRANSPARENT
     SIZE 3.2 BY .76.

DEFINE IMAGE IMAGE-5
     FILENAME "Graphics/24x24/calendar_clock.png":U TRANSPARENT
     SIZE 4.8 BY 1.14.

DEFINE IMAGE IMAGE-6
     FILENAME "Graphics/32x32/calendar_clock.ico":U TRANSPARENT
     SIZE 6.4 BY 1.52.

DEFINE VARIABLE cPositionMnemonic AS CHARACTER INITIAL "Begin" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Begin", "Begin",
"End", "End"
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE cShowMnemonic AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "None", "None",
"All", "All",
"Programs Only", "Program"
     SIZE 37 BY 1 TOOLTIP "Show Mnemonic" NO-UNDO.

DEFINE VARIABLE svLanguageList AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Item 1", 1,
"Item 2", 2,
"Item 3", 3
     SIZE 43 BY 4.76 TOOLTIP "Languages" NO-UNDO.

DEFINE VARIABLE svMenuSize AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Small", 1,
"Medium", 2,
"Large", 3
     SIZE 11 BY 5.95 TOOLTIP "Menu Size" NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 58 BY 5.71.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 58 BY 6.43.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 20 BY 2.38.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 37 BY 2.38.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 42 BY 21.67.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 58 BY 2.86.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 31 BY 1.67
     BGCOLOR 14 .

DEFINE VARIABLE copyToUser AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 40 BY 16.91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-USER
     company_name AT ROW 1.71 COL 13 COLON-ALIGNED NO-LABEL
     loc_loc AT ROW 1.71 COL 76 COLON-ALIGNED NO-LABEL
     users_user_id AT ROW 1.71 COL 117 COLON-ALIGNED NO-LABEL
     Mnemonic AT ROW 1.71 COL 141 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "Location:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.71 COL 68
     "Company:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.71 COL 4
     "User ID:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.71 COL 110
     boxes AT ROW 8.62 COL 57
     menu-image AT ROW 3.62 COL 58
     RECT-2 AT ROW 1 COL 1
     RECT-5 AT ROW 1.48 COL 3 WIDGET-ID 38
     RECT-6 AT ROW 1.48 COL 101 WIDGET-ID 40
     RECT-7 AT ROW 1.48 COL 60 WIDGET-ID 42
     RECT-8 AT ROW 1.48 COL 141 WIDGET-ID 44
     RECT-9 AT ROW 3.29 COL 1 WIDGET-ID 46
     RECT-10 AT ROW 3.38 COL 57 WIDGET-ID 48
     imageSettings AT ROW 1.24 COL 152 WIDGET-ID 52
     imageCompany AT ROW 1.24 COL 52 WIDGET-ID 54
     menuLinkASI AT ROW 3.86 COL 152 WIDGET-ID 56
     RECT-11 AT ROW 3.38 COL 150 WIDGET-ID 58
     menuLinkZoHo AT ROW 6 COL 152 WIDGET-ID 64
     menuLink-1 AT ROW 26.95 COL 148 WIDGET-ID 66
     menuLink-2 AT ROW 26.95 COL 135 WIDGET-ID 68
     menuLink-3 AT ROW 26.95 COL 122 WIDGET-ID 70
     menuLink-4 AT ROW 26.95 COL 109 WIDGET-ID 72
     menuLink-5 AT ROW 26.95 COL 96 WIDGET-ID 74
     menuLink-6 AT ROW 26.95 COL 83 WIDGET-ID 76
     menuLink-7 AT ROW 26.95 COL 70 WIDGET-ID 78
     menuLink-8 AT ROW 26.95 COL 57 WIDGET-ID 80
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 .

DEFINE FRAME searchFrame
     BtnFavorites AT ROW 1 COL 1 HELP
          "Search Menu / Edit Favorites" WIDGET-ID 54
     menuTreeFilter AT ROW 1 COL 54 COLON-ALIGNED HELP
          "Enter Search Filter" NO-LABEL WIDGET-ID 2
     btnMoveDown AT ROW 5.76 COL 1 HELP
          "Move Favorite Down" WIDGET-ID 58
     favoritesList AT ROW 2.19 COL 6 NO-LABEL WIDGET-ID 52
     searchSelections AT ROW 2.19 COL 52 NO-LABEL WIDGET-ID 44
     btnMoveUp AT ROW 3.38 COL 1 HELP
          "Move Favorite Up" WIDGET-ID 56
     btnRemove AT ROW 4.57 COL 1 HELP
          "Remove Favorite" WIDGET-ID 26
     btnSearch AT ROW 1 COL 51 HELP
          "Search Menu / Edit Favorites" WIDGET-ID 40
     btnFavorite AT ROW 13.62 COL 52 WIDGET-ID 46
     btnClear AT ROW 13.86 COL 100 HELP
          "Clear Search Filters" WIDGET-ID 42
     svFavoriteText AT ROW 13.86 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     "FAVORITES" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 21 WIDGET-ID 62
          BGCOLOR 15 
     RECT-23 AT ROW 1 COL 6 WIDGET-ID 60
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.38
         SIZE 108 BY 14.05
         FGCOLOR 1  WIDGET-ID 600.

DEFINE FRAME userSettingsFrame
     btnCancel AT ROW 21 COL 12 HELP
          "Cancel" WIDGET-ID 2
     btnLanguage-1 AT ROW 4.81 COL 6 HELP
          "Select this Language" WIDGET-ID 24
     btnLanguage-2 AT ROW 6.48 COL 6 HELP
          "Select this Language" WIDGET-ID 26
     btnLanguage-3 AT ROW 8.14 COL 6 HELP
          "Select this Language" WIDGET-ID 28
     btnOK AT ROW 21 COL 3 HELP
          "Save Changes" WIDGET-ID 4
     btnToggle AT ROW 1.71 COL 14 HELP
          "Customize Menu" WIDGET-ID 80
     copyFromUser AT ROW 1.95 COL 60 COLON-ALIGNED HELP
          "Select User Account ID" NO-LABEL WIDGET-ID 52
     copyToUser AT ROW 3.86 COL 62 NO-LABEL WIDGET-ID 88
     svLanguageList AT ROW 5.05 COL 16 HELP
          "Select Language" NO-LABEL WIDGET-ID 30
     svMenuSize AT ROW 10.76 COL 5 HELP
          "Select Menu Size" NO-LABEL WIDGET-ID 34
     cShowMnemonic AT ROW 18.14 COL 22 HELP
          "Show Mnemonic" NO-LABEL WIDGET-ID 100
     cPositionMnemonic AT ROW 19.33 COL 22 HELP
          "Place Mnemonic at Begin or End of Text" NO-LABEL WIDGET-ID 108
     btnCopyToUser AT ROW 21 COL 62 HELP
          "Copy From User to Selected User(s)" WIDGET-ID 94
     btnActivateCueCards AT ROW 21.48 COL 27 HELP
          "Activate Inactive Cue Cards" WIDGET-ID 116
     " Copy to Selected Users" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.14 COL 64 WIDGET-ID 90
     " HotKey (Mnemonic)" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 17.43 COL 5 WIDGET-ID 106
     "Position:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 19.33 COL 12 WIDGET-ID 114
     "[S] Scheduling" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.52 COL 33 WIDGET-ID 54
          FONT 6
     "Show:" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 18.14 COL 14 WIDGET-ID 112
     " Menu Size" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.29 COL 5 WIDGET-ID 62
     " Language" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 4.1 COL 5 WIDGET-ID 86
     "[S] Scheduling" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.38 COL 29 WIDGET-ID 48
          FONT 6
     "[S] Scheduling" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 11.24 COL 26 WIDGET-ID 42
          FONT 6
     " Copy From User" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.24 COL 64 WIDGET-ID 98
     IMAGE-1 AT ROW 11.24 COL 18 WIDGET-ID 40
     IMAGE-2 AT ROW 13.14 COL 18 WIDGET-ID 44
     IMAGE-3 AT ROW 15.05 COL 18 WIDGET-ID 50
     RECT-16 AT ROW 4.33 COL 2 WIDGET-ID 56
     RECT-17 AT ROW 10.52 COL 2 WIDGET-ID 60
     RECT-18 AT ROW 20.76 COL 2 WIDGET-ID 64
     IMAGE-4 AT ROW 11.24 COL 22 WIDGET-ID 74
     IMAGE-5 AT ROW 13.14 COL 23 WIDGET-ID 76
     IMAGE-6 AT ROW 15.05 COL 25 WIDGET-ID 78
     RECT-19 AT ROW 1.48 COL 13 WIDGET-ID 82
     RECT-20 AT ROW 1.48 COL 61 WIDGET-ID 92
     RECT-21 AT ROW 17.67 COL 2 WIDGET-ID 104
     RECT-22 AT ROW 21.24 COL 26 WIDGET-ID 118
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57 ROW 3.38
         SIZE 103 BY 23.33
         BGCOLOR 15 FGCOLOR 1 
         TITLE "User Settings" WIDGET-ID 200.

DEFINE FRAME menuTreeFrame
     svFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 82
     menuTreeMsg AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 84
     upgradeMsg AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4.57
         SIZE 55 BY 24.91
         BGCOLOR 15  WIDGET-ID 100.


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
       FRAME searchFrame:FRAME = FRAME FRAME-USER:HANDLE
       FRAME userSettingsFrame:FRAME = FRAME FRAME-USER:HANDLE.

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
/* SETTINGS FOR FRAME userSettingsFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME userSettingsFrame:HIDDEN           = TRUE
       FRAME userSettingsFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnCopyToUser IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnLanguage-1 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-1:HIDDEN IN FRAME userSettingsFrame           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-2 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-2:HIDDEN IN FRAME userSettingsFrame           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-3 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-3:HIDDEN IN FRAME userSettingsFrame           = TRUE.

/* SETTINGS FOR BUTTON btnToggle IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnToggle:PRIVATE-DATA IN FRAME userSettingsFrame     = 
                "Customize Menu".

/* SETTINGS FOR COMBO-BOX copyFromUser IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST copyToUser IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-4 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-5 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-6 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-16 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-17 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-18 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-19 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-20 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-21 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET svLanguageList IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET svMenuSize IN FRAME userSettingsFrame
   NO-ENABLE                                                            */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME userSettingsFrame
/* Query rebuild information for FRAME userSettingsFrame
     _Query            is NOT OPENED
*/  /* FRAME userSettingsFrame */
&ANALYZE-RESUME

 



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


&Scoped-define FRAME-NAME userSettingsFrame
&Scoped-define SELF-NAME btnActivateCueCards
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnActivateCueCards MAINMENU
ON CHOOSE OF btnActivateCueCards IN FRAME userSettingsFrame /* Activate Inactive Cue Cards */
DO:
    RUN pActivateCueCards.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel MAINMENU
ON CHOOSE OF btnCancel IN FRAME userSettingsFrame /* Cancel */
DO:
    /* cue card showing, don't close frame */
    IF DYNAMIC-FUNCTION("fCueCardActive") THEN RETURN.
        
    HIDE FRAME userSettingsFrame.
    IF lToggle THEN DO:
        lToggle = NO.
        RUN pReset.
    END. /* if ltoggle */
    VIEW FRAME searchFrame.
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


&Scoped-define FRAME-NAME userSettingsFrame
&Scoped-define SELF-NAME btnCopyToUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyToUser MAINMENU
ON CHOOSE OF btnCopyToUser IN FRAME userSettingsFrame /* Copy From User to Selected User(s) */
DO:
    RUN pCopyToUser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
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


&Scoped-define FRAME-NAME userSettingsFrame
&Scoped-define SELF-NAME btnLanguage-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-1 MAINMENU
ON CHOOSE OF btnLanguage-1 IN FRAME userSettingsFrame /* Lang 1 */
DO:
    svLanguageList:SCREEN-VALUE = "1".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-2 MAINMENU
ON CHOOSE OF btnLanguage-2 IN FRAME userSettingsFrame /* Lang 2 */
DO:
    svLanguageList:SCREEN-VALUE = "2".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-3 MAINMENU
ON CHOOSE OF btnLanguage-3 IN FRAME userSettingsFrame /* Lang 3 */
DO:
    svLanguageList:SCREEN-VALUE = "3".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
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


&Scoped-define FRAME-NAME userSettingsFrame
&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK MAINMENU
ON CHOOSE OF btnOK IN FRAME userSettingsFrame /* OK */
DO:
    /* cue card showing, don't close frame */
    IF DYNAMIC-FUNCTION("fCueCardActive") THEN RETURN.

    ASSIGN
        iLanguage = svLanguageList
        iMenuSize = svMenuSize
        cShowMnemonic
        cPositionMnemonic
        .
    IF lToggle THEN lToggle = NO.
    HIDE FRAME userSettingsFrame.
    cLabelLanguage = ENTRY(iLanguage,cLanguageList).
    RUN pSetUserSettings.
    RUN pReset.
    VIEW FRAME searchFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
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
                FRAME searchFrame:VIRTUAL-HEIGHT = btnSearch:HEIGHT + .1
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


&Scoped-define FRAME-NAME userSettingsFrame
&Scoped-define SELF-NAME btnToggle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToggle MAINMENU
ON CHOOSE OF btnToggle IN FRAME userSettingsFrame /* Customize Menu */
DO:
    ASSIGN
        lToggle = NOT lToggle
        SELF:LABEL = STRING(lToggle,"Save /") + SELF:PRIVATE-DATA
        .
    IF NOT lToggle THEN DO:
        RUN pSaveCustomMenu.
        HIDE FRAME userSettingsFrame.
    END. /* not ltoggle */
    RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME copyFromUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL copyFromUser MAINMENU
ON VALUE-CHANGED OF copyFromUser IN FRAME userSettingsFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cShowMnemonic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cShowMnemonic MAINMENU
ON VALUE-CHANGED OF cShowMnemonic IN FRAME userSettingsFrame
DO:
    cPositionMnemonic:SENSITIVE = cShowMnemonic:SCREEN-VALUE NE "None".
END.

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


&Scoped-define SELF-NAME imageSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL imageSettings MAINMENU
ON MOUSE-SELECT-CLICK OF imageSettings IN FRAME FRAME-USER
DO:
    IF FRAME userSettingsFrame:HIDDEN EQ NO THEN DO:
        APPLY "CHOOSE":U TO btnCancel.
        VIEW FRAME searchFrame.
    END. /* if frame */
    ELSE DO:
        FIND FIRST users NO-LOCK
             WHERE users.user_id EQ USERID("ASI")
             NO-ERROR.
        IF AVAILABLE users THEN DO:
            FIND FIRST userLanguage NO-LOCK
                 WHERE userLanguage.userLanguage EQ users.userLanguage
                 NO-ERROR.
            IF AVAILABLE userLanguage THEN
            iLanguage = userLanguage.languageIdx.
            ASSIGN
                iMenuSize         = LOOKUP(users.menuSize,"Small,Medium,Large")
                svMenuSize        = iMenuSize
                svLanguageList    = iLanguage
                cShowMnemonic     = users.showMnemonic
                cPositionMnemonic = users.positionMnemonic
                .
            DISPLAY svMenuSize svLanguageList cShowMnemonic cPositionMnemonic
                WITH FRAME userSettingsFrame.
        END. /* if avail */
        RUN pGetCopyUsers.
        ASSIGN 
            cCuePrgmName = FRAME userSettingsFrame:NAME 
            hCueFrame    = FRAME userSettingsFrame:HANDLE
            .
        {system/runCueCard.i}
    END. /* else */
    btnToggle:LABEL = btnToggle:PRIVATE-DATA.
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
    OS-COMMAND NO-WAIT START VALUE(menuLinkasi:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME menuLinkZoHo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuLinkZoHo MAINMENU
ON MOUSE-SELECT-CLICK OF menuLinkZoHo IN FRAME FRAME-USER
DO:
    OS-COMMAND NO-WAIT START VALUE(menuLinkZoHo:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME menuTreeFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL menuTreeFilter MAINMENU
ON VALUE-CHANGED OF menuTreeFilter IN FRAME searchFrame
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSearchSelections.
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


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit MAINMENU
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
    APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
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
    btnFavorite:LOAD-IMAGE("Graphics/16x16/navigate_"
        + (IF lFavorite THEN "minus" ELSE "plus")
        + ".gif")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME userSettingsFrame
&Scoped-define SELF-NAME svLanguageList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLanguageList MAINMENU
ON VALUE-CHANGED OF svLanguageList IN FRAME userSettingsFrame
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svMenuSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svMenuSize MAINMENU
ON VALUE-CHANGED OF svMenuSize IN FRAME userSettingsFrame
DO:
    ASSIGN {&SELF-NAME}.
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
    closeMenu = YES.
    IF USERID("ASI") NE "Nosweat" THEN
        MESSAGE 'Exit Advantzware?' VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE closeMenu.
    IF NOT closeMenu THEN RETURN NO-APPLY.
    RUN pSetUserSettings.
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
    DYNAMIC-FUNCTION("sfSetMainMenuHandle", THIS-PROCEDURE).
    RUN pGetUserSettings.
    RUN sys/ref/nk1look.p (
        g_company,"BitMap","DS",NO,NO,"","",
        OUTPUT cBitMap,OUTPUT lFound
        ).
    IF lFound AND cBitMap NE "" THEN
    boxes:LOAD-IMAGE(cBitMap).
    ASSIGN
        {&WINDOW-NAME}:COL = 1
        {&WINDOW-NAME}:ROW = 1
        .
    RUN enable_UI.
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
    RUN pBuildttMenuTree.
    RUN pGetFavorites.
    RUN pLoadFavorites.
    menuTreeMsg:HIDDEN = YES.
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
    {system/runCueCard.i}
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY company_name loc_loc users_user_id Mnemonic 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  ENABLE imageSettings imageCompany menuLinkZoHo 
      WITH FRAME FRAME-USER IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-USER}
  DISPLAY menuTreeFilter favoritesList searchSelections svFavoriteText 
      WITH FRAME searchFrame IN WINDOW MAINMENU.
  ENABLE BtnFavorites menuTreeFilter btnMoveDown favoritesList searchSelections 
         btnMoveUp btnRemove btnSearch btnFavorite 
      WITH FRAME searchFrame IN WINDOW MAINMENU.
  VIEW FRAME searchFrame IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-searchFrame}
  DISPLAY copyFromUser copyToUser svLanguageList svMenuSize cShowMnemonic 
          cPositionMnemonic 
      WITH FRAME userSettingsFrame IN WINDOW MAINMENU.
  ENABLE btnCancel btnOK cShowMnemonic cPositionMnemonic btnActivateCueCards 
      WITH FRAME userSettingsFrame IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-userSettingsFrame}
  DISPLAY svFocus upgradeMsg 
      WITH FRAME menuTreeFrame IN WINDOW MAINMENU.
  ENABLE svFocus 
      WITH FRAME menuTreeFrame IN WINDOW MAINMENU.
  {&OPEN-BROWSERS-IN-QUERY-menuTreeFrame}
  VIEW MAINMENU.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pActivateCueCards MAINMENU 
PROCEDURE pActivateCueCards :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FOR EACH xCueCard EXCLUSIVE-LOCK
        WHERE xCueCard.user_id EQ USERID("ASI")
        :
        DELETE xCueCard.
    END. /* each xcuecard */
    MESSAGE
        "Cue Cards Activated"
    VIEW-AS ALERT-BOX.

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
    
    EMPTY TEMP-TABLE ttMenuTree.
    
    FOR EACH prgrms NO-LOCK
        WHERE prgrms.menu_item EQ YES
          AND prgrms.menuOrder GT 0
          AND prgrms.menuLevel GT 0
          AND prgrms.mnemonic  NE ""
          AND prgrms.securityLevelUser LE users.securityLevel
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
                prgrms.prgtitle,
                prgrms.menuImage[1],
                prgrms.mnemonic,
                cShowMnemonic,
                cPositionMnemonic,
                lActive
                ).
        END. /* if ccemenu */
    END. /* each prgrms */
    /* create an Exit option */
    RUN pCreatettMenuTree (
        FRAME menuTreeFrame:HANDLE,
        9999,                 /* order             */
        1,                    /* level             */
        NO,                   /* is menu           */
        "file",               /* parent            */
        "exit",               /* child             */
        "Exit",               /* text              */
        "navigate_cross.png", /* image             */
        "X",                  /* mnemonic          */
        cShowMnemonic,        /* show mnemonic     */
        cPositionMnemonic,    /* position mnemonic */
        YES                   /* active            */
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyToUser MAINMENU 
PROCEDURE pCopyToUser :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCurrentUser AS LOGICAL NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bxUserMenu FOR xUserMenu.
    
    DO WITH FRAME userSettingsFrame:
        ASSIGN copyFromUser.
        DO idx = 1 TO copyToUser:NUM-ITEMS:
            IF copyToUser:IS-SELECTED(idx) THEN DO:
                /* prevent copy to same from to user */
                IF copyToUser:ENTRY(idx) EQ copyFromUser THEN NEXT.
                /* check if changing current user */
                IF copyToUser:ENTRY(idx) EQ USERID("ASI") THEN
                lCurrentUser = YES.
                /* clear selected user prior exceptions */
                FOR EACH bxUserMenu EXCLUSIVE-LOCK
                    WHERE bxUserMenu.user_id EQ copyToUser:ENTRY(idx)
                    :
                    DELETE bxUserMenu.
                END. /* each bxusermenu */
                /* copy from user exceptions to selected user */
                FOR EACH xUserMenu NO-LOCK
                    WHERE xUserMenu.user_id EQ copyFromUser
                    :
                    CREATE bxUserMenu.
                    ASSIGN
                        bxUserMenu.user_id  = copyToUser:ENTRY(idx)
                        bxUserMenu.prgmname = xUserMenu.prgmname
                        .
                END. /* each xusermenu */
            END. /* if is-selected */
        END. /* do idx */
    END. /* with frame */
    RUN pGetCopyUsers.
    
    /* if current user, need to rebuild menu and redisplay */
    IF lCurrentUser THEN
    RUN pRebuildMenuTree.
    
    MESSAGE 
        "Copy from User" copyFromUser "Complete."
    VIEW-AS ALERT-BOX.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCopyUsers MAINMENU 
PROCEDURE pGetCopyUsers :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME userSettingsFrame:
        ASSIGN
            copyFromUser:LIST-ITEM-PAIRS = ?
            copyToUser:LIST-ITEM-PAIRS   = ?
            .
        FOR EACH xUserMenu NO-LOCK
            BREAK BY xUserMenu.user_id
            :
            IF FIRST-OF(xUserMenu.user_id) THEN DO:
                FIND FIRST users OF xUserMenu NO-LOCK NO-ERROR.
                copyFromUser:ADD-LAST(xUserMenu.user_id
                    + (IF AVAILABLE users THEN
                       " - " + REPLACE(users.user_name,","," ") ELSE ""),xUserMenu.user_id)
                    .
            END. /* first-of */
        END. /* each xusermenu */
        FOR EACH users NO-LOCK:
            copyToUser:ADD-LAST(users.user_id + " - "
                + REPLACE(users.user_name,","," "),users.user_id)
                .
        END. /* each users */
        IF copyFromUser:NUM-ITEMS GT 0 THEN 
        ASSIGN
            copyFromUser:SCREEN-VALUE = copyFromUser:ENTRY(1)
            copyFromUser
            .
        VIEW FRAME userSettingsFrame.
    END. /* else */

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
        svLanguageList:RADIO-BUTTONS IN FRAME userSettingsFrame = ",0"
        .
    DO i = 1 TO NUM-ENTRIES(cLanguageList):
        {system/btnLanguage.i 1}
        {system/btnLanguage.i 2}
        {system/btnLanguage.i 3}
        cList = cList + ENTRY(i,cLanguageList) + ","
                      + STRING(i) + ","
                      .
    END. /* do idx */
    ASSIGN
        cList                        = TRIM(cList,",")
        svLanguageList:RADIO-BUTTONS = cList
        svLanguageList               = iLanguage
        svMenuSize                   = iMenuSize
        .
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE users THEN DO:
        ASSIGN
            iMenuSize         = LOOKUP(users.menuSize,"Small,Medium,Large")
            cShowMnemonic     = users.showMnemonic
            cPositionMnemonic = users.positionMnemonic
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
    ASSIGN
        svMenuSize     = iMenuSize
        svLanguageList = iLanguage
        cLabelLanguage = ENTRY(iLanguage,cLanguageList)
        .
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
         WHERE prgrms.prgmname EQ "usermenu."
         NO-ERROR.
    IF AVAILABLE prgrms THEN DO:
        DO idx = 1 TO NUM-ENTRIES(g_groups):
            IF NOT CAN-DO(prgrms.can_update,ENTRY(idx,g_groups)) THEN NEXT.
            btnToggle:SENSITIVE = YES.
            LEAVE.
        END. /* do idx */
        IF btnToggle:SENSITIVE EQ NO THEN 
        btnToggle:SENSITIVE = CAN-DO(prgrms.can_update,USERID("ASI")).
    END. /* if avail */
    
    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmname EQ "users."
         NO-ERROR.
    IF AVAILABLE prgrms THEN DO:
        DO idx = 1 TO NUM-ENTRIES(g_groups):
            IF NOT CAN-DO(prgrms.can_update,ENTRY(idx,g_groups)) THEN NEXT.
            ASSIGN
                btnLanguage-1:SENSITIVE     = YES
                btnLanguage-2:SENSITIVE     = YES
                btnLanguage-3:SENSITIVE     = YES
                svLanguageList:SENSITIVE    = YES
                svMenuSize:SENSITIVE        = YES
                cShowMnemonic:SENSITIVE     = YES
                cPositionMnemonic:SENSITIVE = YES
                .
            LEAVE.
        END. /* do idx */
        IF svLanguageList:SENSITIVE EQ NO THEN 
        ASSIGN
            btnLanguage-1:SENSITIVE     = CAN-DO(prgrms.can_update,USERID("ASI"))
            btnLanguage-2:SENSITIVE     = CAN-DO(prgrms.can_update,USERID("ASI"))
            btnLanguage-3:SENSITIVE     = CAN-DO(prgrms.can_update,USERID("ASI"))
            svLanguageList:SENSITIVE    = CAN-DO(prgrms.can_update,USERID("ASI"))
            svMenuSize:SENSITIVE        = CAN-DO(prgrms.can_update,USERID("ASI"))
            cShowMnemonic:SENSITIVE     = CAN-DO(prgrms.can_update,USERID("ASI"))
            cPositionMnemonic:SENSITIVE = CAN-DO(prgrms.can_update,USERID("ASI"))
            .
    END. /* if avail */

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
    DEFINE VARIABLE cNK1Value     AS CHARACTER NO-UNDO EXTENT 4.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hWebService   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hSalesSoap    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cVersion      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHelpService  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hPgmMstrSecur AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lAdmin        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cThisVer      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iThisVersion  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastVersion  AS INTEGER   NO-UNDO.
    
    RUN sys/ref/nk1look.p (
        g_company,"CEMenu","C",NO,NO,"","",
        OUTPUT cCEMenu,OUTPUT lFound
        ).

    RUN pMenuSize.
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN sys/ref/nk1look.p (
            g_company,"MENUIMAGE","L",NO,NO,"","",
            OUTPUT lMenuImage,OUTPUT lFound
            ).
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
        END. /* if valid handle */
        IF lAdmin AND USERID("ASI") NE "NoSweat" THEN DO:
            RUN sys/ref/nk1look.p (
                g_company,"AsiHelpService","C",NO,NO,"","",
                OUTPUT cHelpService,OUTPUT lFound
                ).
            CREATE SERVER hWebService.
            hWebService:CONNECT(cHelpService) NO-ERROR.
            IF hWebService:CONNECTED() THEN DO:
                RUN Service1Soap SET hSalesSoap ON hWebService .
                RUN HelpVersion IN hSalesSoap (OUTPUT cVersion).
                ASSIGN
                    cThisVer     = "{&awversion}"
                    iLastVersion = (INTEGER(ENTRY(1,cVersion,".")) * 10000) +
                                   (INTEGER(ENTRY(2,cVersion,".")) * 100) +
                                   (INTEGER(ENTRY(3,cVersion,".")))
                    iThisVersion = (INTEGER(ENTRY(1,cThisVer,".")) * 10000) +
                                   (INTEGER(ENTRY(2,cThisVer,".")) * 100) +
                                   (INTEGER(ENTRY(3,cThisVer,".")))
                                   .
                IF iLastVersion GT iThisVersion THEN DO:
                    RUN sys/ref/nk1look.p (
                        g_company,"MENULINKUPGRADE","C",NO,NO,"","",
                        OUTPUT cNK1Value[1],OUTPUT lFound
                        ).
                    RUN sys/ref/nk1look.p (
                        g_company,"MENULINKUPGRADE","DS",NO,NO,"","",
                        OUTPUT cNK1Value[2],OUTPUT lFound
                        ).
                    RUN sys/ref/nk1look.p (
                        g_company,"MENULINKUPGRADE","I",NO,NO,"","",
                        OUTPUT cNK1Value[3],OUTPUT lFound
                        ).
                    RUN sys/ref/nk1look.p (
                        g_company,"MENULINKUPGRADE","L",NO,NO,"","",
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
                            menuLinkZoHo:TOOLTIP        = "Version " + cVersion + " Upgrade Available"
                            .
                        menuLinkZoHo:LOAD-IMAGE(SEARCH(cNK1Value[1])).
                    END. /* if avail */
                END. /* different version */
            END. /* if connected */
        END. /* if user admin */
    END. /* with frame */
    IF lAdmin THEN
    ASSIGN
        copyFromUser:SENSITIVE IN FRAME userSettingsFrame = YES
        copyToUser:SENSITIVE     = YES
        btncopyToUser:SENSITIVE  = YES
        btnLanguage-1:SENSITIVE  = YES
        btnLanguage-2:SENSITIVE  = YES
        btnLanguage-3:SENSITIVE  = YES
        svLanguageList:SENSITIVE = YES
        svMenuSize:SENSITIVE     = YES
        btnToggle:SENSITIVE      = YES
        .
    
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
    
    OS-COMMAND NO-WAIT START VALUE(hMenuLink[ipiLink]:PRIVATE-DATA).

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
            dObjectHeight = .95
            dObjectWidth  = 4
            iFont         = 32
            .
        WHEN 2 THEN
        ASSIGN
            cImageFolder  = "Graphics/24X24/"
            dObjectHeight = 1.33
            dObjectWidth  = 5.6
            iFont         = 34
            .
        WHEN 3 THEN
        ASSIGN
            cImageFolder  = "Graphics/32X32/"
            dObjectHeight = 1.67
            dObjectWidth  = 7
            iFont         = 36
            .
    END CASE.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    FOR EACH ttMenuTree:
        IF VALID-HANDLE(ttMenuTree.hLevel) THEN DO:
            ASSIGN
                ttMenuTree.hLevel:HIDDEN = YES
                ttMenuTree.hLevel:ROW    = 1
                ttMenuTree.hLevel:WIDTH  = dObjectWidth
                ttMenuTree.hLevel:HEIGHT = dObjectHeight
                .
            ttMenuTree.hLevel:LOAD-IMAGE(SEARCH(cImageFolder + "navigate_right.png")).
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
            ttMenuTree.hEditor:BGCOLOR = ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildMenuTree MAINMENU 
PROCEDURE pRebuildMenuTree :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    RUN pInitMenuTree.
    RUN pBuildttMenuTree.
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
            ttMenuTree.hEditor:BGCOLOR      = ?
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
PROCEDURE pResetByUser :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplMenuChange AS LOGICAL NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    FRAME userSettingsFrame:HIDDEN = YES.
    /* open user settings frame */
    APPLY "MOUSE-SELECT-CLICK":U TO imageSettings IN FRAME {&FRAME-NAME}.
    IF iplMenuChange THEN 
    APPLY "CHOOSE":U TO btnOK IN FRAME userSettingsFrame.
    /* close user settings frame */
    APPLY "MOUSE-SELECT-CLICK":U TO imageSettings IN FRAME {&FRAME-NAME}.

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveCustomMenu MAINMENU 
PROCEDURE pSaveCustomMenu :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* remove xusermenu entry if menu option is active or no longer present */
    FOR EACH xUserMenu
        WHERE xUserMenu.user_id EQ USERID("ASI")
        :
        IF CAN-FIND(FIRST ttMenuTree
                    WHERE ttMenuTree.treeChild EQ xUserMenu.prgmname
                      AND ttMenuTree.isActive  EQ YES) OR
           NOT CAN-FIND(FIRST ttMenuTree
                        WHERE ttMenuTree.treeChild EQ xUserMenu.prgmname) THEN
        DELETE xUserMenu.
    END. /* each xusermenu */

    FOR EACH ttMenuTree
        WHERE ttMenuTree.isActive EQ NO
        :
        IF CAN-FIND(FIRST xUserMenu
                    WHERE xUserMenu.user_id  EQ USERID("ASI")
                      AND xUserMenu.prgmname EQ ttMenuTree.treeChild) THEN
        NEXT.
        /* create entry to hide menu option from user */
        CREATE xUserMenu.
        ASSIGN
            xUserMenu.user_id  = USERID("ASI")
            xUserMenu.prgmname = ttMenuTree.treeChild
            .
    END. /* each ttmenutree */

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
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST users EXCLUSIVE-LOCK
         WHERE users.user_id EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE users THEN DO:
        ASSIGN
            users.menuSize         = ENTRY(iMenuSize,"Small,Medium,Large")
            users.showMnemonic     = cShowMnemonic
            users.positionMnemonic = cPositionMnemonic            
            .
        FIND FIRST userLanguage NO-LOCK
             WHERE userLanguage.languageIdx EQ iLanguage
             NO-ERROR.
        IF AVAILABLE userLanguage THEN
        users.userLanguage = userLanguage.userLanguage.
        FIND CURRENT users NO-LOCK.
    END. /* avail users */

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
            menuLinkASI:HIDDEN   = YES 
            menuLinkZoHo:HIDDEN  = YES
            menuLink-1:HIDDEN    = YES
            menuLink-2:HIDDEN    = YES
            menuLink-3:HIDDEN    = YES
            menuLink-4:HIDDEN    = YES
            menuLink-5:HIDDEN    = YES
            menuLink-6:HIDDEN    = YES
            menuLink-7:HIDDEN    = YES
            menuLink-8:HIDDEN    = YES 
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
            RECT-2:WIDTH         = FRAME {&FRAME-NAME}:WIDTH - .4
            RECT-8:COL           = FRAME {&FRAME-NAME}:WIDTH - 19
            RECT-9:WIDTH         = FRAME {&FRAME-NAME}:WIDTH - .4
            RECT-10:COL          = FRAME {&FRAME-NAME}:WIDTH - RECT-10:WIDTH      - 11
            RECT-11:COL          = FRAME {&FRAME-NAME}:WIDTH - RECT-11:WIDTH
            Mnemonic:COL         = FRAME {&FRAME-NAME}:WIDTH - 17 
            menuLinkASI:COL      = FRAME {&FRAME-NAME}:WIDTH - menuLinkASI:WIDTH  - 1
            menuLinkZoHo:COL     = FRAME {&FRAME-NAME}:WIDTH - menuLinkZoHo:WIDTH - 1
            menuLink-1:COL       = FRAME {&FRAME-NAME}:WIDTH - menuLink-1:WIDTH
            menuLink-2:COL       = menuLink-1:COL - 13
            menuLink-3:COL       = menuLink-2:COL - 13
            menuLink-4:COL       = menuLink-3:COL - 13
            menuLink-5:COL       = menuLink-4:COL - 13
            menuLink-6:COL       = menuLink-5:COL - 13
            menuLink-7:COL       = menuLink-6:COL - 13
            menuLink-8:COL       = menuLink-7:COL - 13
            imageSettings:HIDDEN = NO
            menu-image:HIDDEN    = NO
            boxes:HIDDEN         = NO
            Mnemonic:HIDDEN      = NO
            RECT-2:HIDDEN        = NO
            RECT-8:HIDDEN        = NO
            RECT-9:HIDDEN        = NO
            RECT-10:HIDDEN       = NO
            RECT-11:HIDDEN       = NO
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
    RUN sys/ref/nk1look.p (
        g_company,"BitMap","DS",NO,NO,"","",
        OUTPUT cBitMap,OUTPUT lFound
        ).
    IF lFound AND cBitMap NE "" THEN boxes:LOAD-IMAGE(cBitMap).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

