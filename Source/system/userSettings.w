&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: userSettings.w

  Description: User Settings

  Input Parameters: <none>

  Output Parameters: Rebuild Main Menu on Exit

  Author: Ron Stark

  Created: 11.13.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE OUTPUT PARAMETER oplRebuildMenu AS LOGICAL NO-UNDO.
&ELSE
DEFINE VARIABLE oplRebuildMenu AS LOGICAL NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

&Scoped-define mainMenuBGColor 1
&Scoped-define mainMenuFGColor 15
&Scoped-define FGColor ?
&Scoped-define BGColor 32
&Scoped-define DefaultMenuBGColor 32

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE cActiveUser        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCEMenu            AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEditorFont        AS INTEGER   NO-UNDO INITIAL ?.
DEFINE VARIABLE iEditorBGColor     AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iEditorFGColor     AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iFrameBGColor      AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iFrameFGColor      AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iRectangleBGColor  AS INTEGER   NO-UNDO INITIAL {&BGColor}.
DEFINE VARIABLE iRectangleFGColor  AS INTEGER   NO-UNDO INITIAL {&FGColor}.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER   NO-UNDO.
DEFINE VARIABLE hColorWidget       AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAdmin             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRebuildMenu       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iFGColor           AS INTEGER   NO-UNDO EXTENT 3.
DEFINE VARIABLE iBGColor           AS INTEGER   NO-UNDO EXTENT 3.

&Scoped-define isActive YES
{system/menuTree.i}
{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExit btnSave btnLanguage-1 btnReset ~
btnLanguage-2 btnLanguage-3 colorChoice-0 colorChoice-1 colorChoice-2 ~
colorChoice-3 colorChoice-4 colorChoice-5 colorChoice-6 colorChoice-7 ~
colorChoice-8 colorChoice-9 colorChoice-10 colorChoice-11 colorChoice-12 ~
colorChoice-13 colorChoice-14 colorChoice-15 colorChoice-default FGColor-1 ~
FGColor-2 FGColor-3 BGColor-1 BGColor-2 BGColor-3 cUser svLanguageList ~
svMenuSize svMenuImage cShowMnemonic cPositionMnemonic lShowCueCards 
&Scoped-Define DISPLAYED-OBJECTS cUser copyFromUser copyToUser ~
svLanguageList svMenuSize svMenuImage cShowMnemonic cPositionMnemonic ~
lShowCueCards 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,copyUser,colorPallet                     */
&Scoped-define copyUser copyFromUser copyToUser btnCopyToUser 
&Scoped-define colorPallet colorChoice-0 colorChoice-1 colorChoice-2 ~
colorChoice-3 colorChoice-4 colorChoice-5 colorChoice-6 colorChoice-7 ~
colorChoice-8 colorChoice-9 colorChoice-10 colorChoice-11 colorChoice-12 ~
colorChoice-13 colorChoice-14 colorChoice-15 colorChoice-default FGColor-1 ~
FGColor-2 FGColor-3 BGColor-1 BGColor-2 BGColor-3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCopyToUser 
     LABEL "Copy From User to Selected User(s)" 
     SIZE 40 BY 1.91 TOOLTIP "Copy From User to Selected User(s)".

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 9 BY 1.91 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON btnLanguage-1  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 1" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnLanguage-2  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 2" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnLanguage-3  NO-FOCUS FLAT-BUTTON
     LABEL "Lang 3" 
     SIZE 9 BY 1.67.

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "Reset" 
     SIZE 9 BY 1.91 TOOLTIP "Reset"
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "Save" 
     SIZE 9 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE BUTTON btnToggle 
     LABEL "Customize User's Menu" 
     SIZE 30 BY 1.91 TOOLTIP "Customize User's Menu".

DEFINE VARIABLE copyFromUser AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 40 BY 1 TOOLTIP "Select User"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cUser AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selected User" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 38 BY 1 TOOLTIP "Selected User" NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/16x16/plus.png":U TRANSPARENT
     SIZE 3.2 BY .76.

DEFINE IMAGE IMAGE-2
     FILENAME "Graphics/24x24/plus.png":U TRANSPARENT
     SIZE 4.8 BY 1.14.

DEFINE IMAGE IMAGE-3
     FILENAME "Graphics/32x32/plus.png":U TRANSPARENT
     SIZE 6.4 BY 1.52.

DEFINE IMAGE IMAGE-4
     FILENAME "Graphics/16x16/Scheduling.png":U TRANSPARENT
     SIZE 3.2 BY .76.

DEFINE IMAGE IMAGE-5
     FILENAME "Graphics/24x24/Scheduling.png":U TRANSPARENT
     SIZE 4.8 BY 1.14.

DEFINE IMAGE IMAGE-6
     FILENAME "Graphics/32x32/Scheduling.png":U TRANSPARENT
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
     SIZE 11 BY 5.24 TOOLTIP "Menu Size" NO-UNDO.

DEFINE RECTANGLE BGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE BGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE BGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE colorChoice-0
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 0 .

DEFINE RECTANGLE colorChoice-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 1 .

DEFINE RECTANGLE colorChoice-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 10 .

DEFINE RECTANGLE colorChoice-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 11 .

DEFINE RECTANGLE colorChoice-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 12 .

DEFINE RECTANGLE colorChoice-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 13 .

DEFINE RECTANGLE colorChoice-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 14 .

DEFINE RECTANGLE colorChoice-15
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE colorChoice-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 2 .

DEFINE RECTANGLE colorChoice-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 3 .

DEFINE RECTANGLE colorChoice-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 4 .

DEFINE RECTANGLE colorChoice-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 5 .

DEFINE RECTANGLE colorChoice-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 6 .

DEFINE RECTANGLE colorChoice-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 7 .

DEFINE RECTANGLE colorChoice-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE colorChoice-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 9 .

DEFINE RECTANGLE colorChoice-default
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE cueCardsRect
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 22 BY 1.43
     BGCOLOR 8 .

DEFINE RECTANGLE FGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE FGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE FGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 100 BY 25.95.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 55 BY 5.71.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 56 BY 6.91.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 60 BY 2.38.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 55 BY 1.91.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 42 BY 22.86.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 55 BY 2.86.

DEFINE VARIABLE copyToUser AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 40 BY 18.1 NO-UNDO.

DEFINE VARIABLE lShowCueCards AS LOGICAL INITIAL no 
     LABEL "Show Cue Cards" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .95 NO-UNDO.

DEFINE VARIABLE svMenuImage AS LOGICAL INITIAL no 
     LABEL "Show Menu Images" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE menuTreeMsg AS CHARACTER FORMAT "X(256)":U INITIAL " Initializing Menus, One Moment Please ..." 
      VIEW-AS TEXT 
     SIZE 56 BY 2.14
     BGCOLOR 1 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE svFocus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE .2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnExit AT ROW 27.43 COL 151 HELP
          "Cancel" WIDGET-ID 2
     btnSave AT ROW 27.43 COL 133 HELP
          "Save Changes" WIDGET-ID 4
     btnLanguage-1 AT ROW 4.33 COL 63 HELP
          "Select this Language" WIDGET-ID 24
     btnReset AT ROW 27.43 COL 142 HELP
          "Save Changes" WIDGET-ID 474
     btnLanguage-2 AT ROW 6 COL 63 HELP
          "Select this Language" WIDGET-ID 26
     btnLanguage-3 AT ROW 7.67 COL 63 HELP
          "Select this Language" WIDGET-ID 28
     cUser AT ROW 1.95 COL 76 COLON-ALIGNED WIDGET-ID 476
     copyFromUser AT ROW 1.95 COL 117 COLON-ALIGNED HELP
          "Select User Account ID" NO-LABEL WIDGET-ID 52
     copyToUser AT ROW 3.86 COL 119 NO-LABEL WIDGET-ID 88
     svLanguageList AT ROW 4.33 COL 73 HELP
          "Select Language" NO-LABEL WIDGET-ID 30
     svMenuSize AT ROW 10.29 COL 63 HELP
          "Select Menu Size" NO-LABEL WIDGET-ID 34
     svMenuImage AT ROW 15.76 COL 63 HELP
          "Toggle to Show Menu Images" WIDGET-ID 466
     cShowMnemonic AT ROW 17.91 COL 79 HELP
          "Show Mnemonic" NO-LABEL WIDGET-ID 100
     cPositionMnemonic AT ROW 19.1 COL 79 HELP
          "Place Mnemonic at Begin or End of Text" NO-LABEL WIDGET-ID 108
     btnCopyToUser AT ROW 22.19 COL 119 HELP
          "Copy From User to Selected User(s)" WIDGET-ID 94
     lShowCueCards AT ROW 25.05 COL 133 HELP
          "Toggle to Show/Not Show Cue Cards" WIDGET-ID 470
     btnToggle AT ROW 27.43 COL 102 HELP
          "Customize User's Menu" WIDGET-ID 80
     "[S] Scheduling" VIEW-AS TEXT
          SIZE 28 BY 1.43 AT ROW 13.86 COL 88 WIDGET-ID 54
          FONT 37
     "FG Color:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 21.71 COL 73 WIDGET-ID 454
     " Copy to Selected Users" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.14 COL 121 WIDGET-ID 90
     " Menu Size" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 9.81 COL 65 WIDGET-ID 62
     " Language" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 3.62 COL 65 WIDGET-ID 86
     " HotKey (Mnemonic)" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 17.19 COL 65 WIDGET-ID 106
     "BG Color:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 22.91 COL 73 WIDGET-ID 460
     "Menu Level  1" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 21 COL 73 WIDGET-ID 458
     "Position:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 19.1 COL 69 WIDGET-ID 114
     "[S] Scheduling" VIEW-AS TEXT
          SIZE 34 BY .81 AT ROW 10.76 COL 82 WIDGET-ID 42
          FONT 33
     " Copy From User" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.24 COL 121 WIDGET-ID 98
     "?" VIEW-AS TEXT
          SIZE 2 BY .76 AT ROW 25.29 COL 120 WIDGET-ID 354
          FGCOLOR 0 FONT 6
     "Show:" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 17.91 COL 71 WIDGET-ID 112
     "3" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 21 COL 99 WIDGET-ID 464
     "[S] Scheduling" VIEW-AS TEXT
          SIZE 31 BY .95 AT ROW 12.19 COL 85 WIDGET-ID 48
          FONT 35
     "2" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 21 COL 92 WIDGET-ID 462
     IMAGE-1 AT ROW 10.76 COL 74 WIDGET-ID 40
     IMAGE-2 AT ROW 12.19 COL 74 WIDGET-ID 44
     IMAGE-3 AT ROW 13.86 COL 74 WIDGET-ID 50
     RECT-16 AT ROW 3.86 COL 62 WIDGET-ID 56
     RECT-17 AT ROW 10.05 COL 62 WIDGET-ID 60
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     RECT-18 AT ROW 27.19 COL 101 WIDGET-ID 64
     IMAGE-4 AT ROW 10.76 COL 78 WIDGET-ID 74
     IMAGE-5 AT ROW 12.19 COL 79 WIDGET-ID 76
     IMAGE-6 AT ROW 13.86 COL 81 WIDGET-ID 78
     RECT-19 AT ROW 1.48 COL 62 WIDGET-ID 82
     RECT-20 AT ROW 1.48 COL 118 WIDGET-ID 92
     RECT-21 AT ROW 17.43 COL 62 WIDGET-ID 104
     cueCardsRect AT ROW 24.81 COL 132 WIDGET-ID 118
     colorChoice-0 AT ROW 24.57 COL 62 WIDGET-ID 442
     colorChoice-1 AT ROW 24.57 COL 69 WIDGET-ID 306
     colorChoice-2 AT ROW 24.57 COL 76 WIDGET-ID 308
     colorChoice-3 AT ROW 24.57 COL 83 WIDGET-ID 310
     colorChoice-4 AT ROW 24.57 COL 90 WIDGET-ID 312
     colorChoice-5 AT ROW 24.57 COL 97 WIDGET-ID 314
     colorChoice-6 AT ROW 24.57 COL 104 WIDGET-ID 316
     colorChoice-7 AT ROW 24.57 COL 111 WIDGET-ID 318
     colorChoice-8 AT ROW 25.76 COL 62 WIDGET-ID 320
     colorChoice-9 AT ROW 25.76 COL 69 WIDGET-ID 322
     colorChoice-10 AT ROW 25.76 COL 76 WIDGET-ID 324
     colorChoice-11 AT ROW 25.76 COL 83 WIDGET-ID 326
     colorChoice-12 AT ROW 25.76 COL 90 WIDGET-ID 328
     colorChoice-13 AT ROW 25.76 COL 97 WIDGET-ID 330
     colorChoice-14 AT ROW 25.76 COL 104 WIDGET-ID 332
     colorChoice-15 AT ROW 25.76 COL 111 WIDGET-ID 334
     colorChoice-default AT ROW 25.05 COL 118 WIDGET-ID 352
     FGColor-1 AT ROW 21.71 COL 83 WIDGET-ID 338
     FGColor-2 AT ROW 21.71 COL 90 WIDGET-ID 444
     FGColor-3 AT ROW 21.71 COL 97 WIDGET-ID 446
     BGColor-1 AT ROW 22.91 COL 83 WIDGET-ID 448
     BGColor-2 AT ROW 22.91 COL 90 WIDGET-ID 450
     BGColor-3 AT ROW 22.91 COL 97 WIDGET-ID 452
     RECT-1 AT ROW 1 COL 61 WIDGET-ID 472
     SPACE(0.00) SKIP(2.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 FGCOLOR 1 "User Settings" WIDGET-ID 100.

DEFINE FRAME menuTreeFrame
     svFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 82
     menuTreeMsg AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 88
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 58 BY 28.57
         BGCOLOR 15 
         TITLE "Menu Options" WIDGET-ID 200.


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
/* REPARENT FRAME */
ASSIGN FRAME menuTreeFrame:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR RECTANGLE BGColor-1 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       BGColor-1:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE BGColor-2 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       BGColor-2:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE BGColor-3 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       BGColor-3:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR BUTTON btnCopyToUser IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
ASSIGN 
       btnLanguage-1:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       btnLanguage-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       btnLanguage-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnToggle IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btnToggle:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "Customize User's Menu".

/* SETTINGS FOR RECTANGLE colorChoice-0 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-0:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-1 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-1:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-10 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-10:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-11 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-11:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-12 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-12:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-13 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-13:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-14 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-14:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-15 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-15:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-2 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-2:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-3 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-3:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-4 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-4:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-5 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-5:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-6 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-6:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-7 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-7:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-8 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-8:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-9 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-9:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-default IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       colorChoice-default:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR COMBO-BOX copyFromUser IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR SELECTION-LIST copyToUser IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR RECTANGLE cueCardsRect IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE FGColor-1 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       FGColor-1:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE FGColor-2 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       FGColor-2:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR RECTANGLE FGColor-3 IN FRAME Dialog-Frame
   6                                                                    */
ASSIGN 
       FGColor-3:SELECTABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-21 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME menuTreeFrame
                                                                        */
/* SETTINGS FOR FILL-IN menuTreeMsg IN FRAME menuTreeFrame
   ALIGN-L                                                              */
ASSIGN 
       menuTreeMsg:HIDDEN IN FRAME menuTreeFrame           = TRUE.

/* SETTINGS FOR FILL-IN svFocus IN FRAME menuTreeFrame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME menuTreeFrame
/* Query rebuild information for FRAME menuTreeFrame
     _Query            is NOT OPENED
*/  /* FRAME menuTreeFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* User Settings */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyToUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyToUser Dialog-Frame
ON CHOOSE OF btnCopyToUser IN FRAME Dialog-Frame /* Copy From User to Selected User(s) */
DO:
    RUN pCopyToUser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-1 Dialog-Frame
ON CHOOSE OF btnLanguage-1 IN FRAME Dialog-Frame /* Lang 1 */
DO:
    svLanguageList:SCREEN-VALUE = "1".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-2 Dialog-Frame
ON CHOOSE OF btnLanguage-2 IN FRAME Dialog-Frame /* Lang 2 */
DO:
    svLanguageList:SCREEN-VALUE = "2".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLanguage-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-3 Dialog-Frame
ON CHOOSE OF btnLanguage-3 IN FRAME Dialog-Frame /* Lang 3 */
DO:
    svLanguageList:SCREEN-VALUE = "3".
    APPLY "VALUE-CHANGED":U TO svLanguageList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
    ASSIGN
        lRebuildMenu    = NO
        lToggle         = NO
        btnToggle:LABEL = STRING(lToggle,"SAVE /") + btnToggle:PRIVATE-DATA
        .
    RUN pGetUserSettings.
    RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    ASSIGN
        iLanguage   = svLanguageList
        iMenuSize   = svMenuSize
        iFont       = 30 + iMenuSize * 2
        iFGColor[1] = FGColor-1:BGCOLOR
        iFGColor[2] = FGColor-2:BGCOLOR
        iFGColor[3] = FGColor-3:BGCOLOR
        iBGColor[1] = BGColor-1:BGCOLOR
        iBGColor[2] = BGColor-2:BGCOLOR
        iBGColor[3] = BGColor-3:BGCOLOR
        lMenuImage  = svMenuImage
        cShowMnemonic
        cPositionMnemonic
        cLabelLanguage = ENTRY(iLanguage,cLanguageList)
        .
    RUN pSetUserSettings.
    IF lRebuildMenu THEN DO:
        IF lToggle THEN
        APPLY "CHOOSE":U TO btnToggle.
        ELSE
        RUN pReset.
        ASSIGN
            lRebuildMenu = NO.
            oplRebuildMenu = cActiveUser EQ USERID("ASI") OR oplRebuildMenu
            .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToggle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToggle Dialog-Frame
ON CHOOSE OF btnToggle IN FRAME Dialog-Frame /* Customize User's Menu */
DO:
    ASSIGN
        lToggle = NOT lToggle
        SELF:LABEL = STRING(lToggle,"SAVE /") + SELF:PRIVATE-DATA
        .
    IF NOT lToggle THEN
    RUN pSaveCustomMenu.
    RUN pReset.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorChoice-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorChoice-0 Dialog-Frame
ON SELECTION OF colorChoice-0 IN FRAME Dialog-Frame
,colorChoice-1,colorChoice-2,colorChoice-3,colorChoice-4,colorChoice-5
,colorChoice-6,colorChoice-7,colorChoice-8,colorChoice-9,colorChoice-10
,colorChoice-11,colorChoice-12,colorChoice-13,colorChoice-14,colorChoice-15
,colorChoice-default
DO:
    IF VALID-HANDLE(hColorWidget) THEN
    ASSIGN
        hColorWidget:BGCOLOR = SELF:BGCOLOR
        hColorWidget:FILLED  = TRUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME copyFromUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL copyFromUser Dialog-Frame
ON VALUE-CHANGED OF copyFromUser IN FRAME Dialog-Frame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cPositionMnemonic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cPositionMnemonic Dialog-Frame
ON VALUE-CHANGED OF cPositionMnemonic IN FRAME Dialog-Frame
DO:
    lRebuildMenu = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cShowMnemonic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cShowMnemonic Dialog-Frame
ON VALUE-CHANGED OF cShowMnemonic IN FRAME Dialog-Frame
DO:
    ASSIGN
        cPositionMnemonic:SENSITIVE = cShowMnemonic:SCREEN-VALUE NE "None"
        lRebuildMenu = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUser Dialog-Frame
ON VALUE-CHANGED OF cUser IN FRAME Dialog-Frame /* Selected User */
DO:
    ASSIGN
        {&SELF-NAME}
        cActiveUser = {&SELF-NAME}
        .
    RUN pGetUserSettings.
    RUN pInitMenuTree.
    RUN pBuildttMenuTree("file").
    RUN pMenuSize.
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FGColor-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FGColor-1 Dialog-Frame
ON SELECTION OF FGColor-1 IN FRAME Dialog-Frame
,FGColor-2,FGColor-3,BGColor-1,BGColor-2,BGColor-3
DO:
    hColorWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lShowCueCards
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lShowCueCards Dialog-Frame
ON VALUE-CHANGED OF lShowCueCards IN FRAME Dialog-Frame /* Show Cue Cards */
DO:
    ASSIGN
        {&SELF-NAME}
        cueCardsRect:BGCOLOR = IF {&SELF-NAME} THEN 10 ELSE 12
        {&SELF-NAME}:BGCOLOR = cueCardsRect:BGCOLOR
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLanguageList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLanguageList Dialog-Frame
ON VALUE-CHANGED OF svLanguageList IN FRAME Dialog-Frame
DO:
    ASSIGN
        {&SELF-NAME}
        lRebuildMenu = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svMenuImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svMenuImage Dialog-Frame
ON VALUE-CHANGED OF svMenuImage IN FRAME Dialog-Frame /* Show Menu Images */
DO:
    ASSIGN
        {&SELF-NAME}
        lRebuildMenu = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svMenuSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svMenuSize Dialog-Frame
ON VALUE-CHANGED OF svMenuSize IN FRAME Dialog-Frame
DO:
    ASSIGN
        {&SELF-NAME}
        lRebuildMenu = YES
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


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  FIND FIRST users NO-LOCK
       WHERE users.user_id EQ USERID("ASI")
       NO-ERROR.
  ASSIGN
    iUserSecurityLevel = users.securityLevel
    cActiveUser        = USERID("ASI")
    .
  RUN pInit.
  RUN pGetUserSettings.
  RUN pGetUsers.
  RUN pBuildttMenuTree("file").
  menuTreeMsg:HIDDEN = YES.
  RUN pMenuSize.
  RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).
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
  HIDE FRAME menuTreeFrame.
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
  DISPLAY cUser copyFromUser copyToUser svLanguageList svMenuSize svMenuImage 
          cShowMnemonic cPositionMnemonic lShowCueCards 
      WITH FRAME Dialog-Frame.
  ENABLE btnExit btnSave btnLanguage-1 btnReset btnLanguage-2 btnLanguage-3 
         colorChoice-0 colorChoice-1 colorChoice-2 colorChoice-3 colorChoice-4 
         colorChoice-5 colorChoice-6 colorChoice-7 colorChoice-8 colorChoice-9 
         colorChoice-10 colorChoice-11 colorChoice-12 colorChoice-13 
         colorChoice-14 colorChoice-15 colorChoice-default FGColor-1 FGColor-2 
         FGColor-3 BGColor-1 BGColor-2 BGColor-3 cUser svLanguageList 
         svMenuSize svMenuImage cShowMnemonic cPositionMnemonic lShowCueCards 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  DISPLAY svFocus menuTreeMsg 
      WITH FRAME menuTreeFrame.
  ENABLE svFocus menuTreeMsg 
      WITH FRAME menuTreeFrame.
  {&OPEN-BROWSERS-IN-QUERY-menuTreeFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildttMenuTree Dialog-Frame 
PROCEDURE pBuildttMenuTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcparentname AS CHARACTER.
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
   // EMPTY TEMP-TABLE ttMenuTree.
    
    FOR EACH prgrms NO-LOCK
        WHERE prgrms.menu_item EQ YES
          AND prgrms.menuOrder GT 0
          AND prgrms.menuLevel GT 0
          AND prgrms.mnemonic  NE ""
          AND prgrms.securityLevelUser LE iUserSecurityLevel
        BY prgrms.menuOrder
        :
        IF cCEMenu EQ "Both" OR 
           prgrms.systemType EQ "Both" OR
           prgrms.systemType EQ cCEMenu THEN DO:
            /* xusermenu holds options not active for user */
            lActive = NOT CAN-FIND(FIRST xUserMenu
                                   WHERE xUserMenu.user_id  EQ cActiveUser
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
                lActive,
                ipcparentname
                ).
        END. /* if ccemenu */
    END. /* each prgrms */
    FOR EACH ttMenuTree:
        IF VALID-HANDLE(ttMenuTree.hEditor) THEN
        ttMenuTree.hEditor:FONT = iFont.
    END. /* each ttmenutree */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyToUser Dialog-Frame 
PROCEDURE pCopyToUser :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCurrentUser AS LOGICAL NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bxUserMenu FOR xUserMenu.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN copyFromUser.
        DO idx = 1 TO copyToUser:NUM-ITEMS TRANSACTION:
            IF copyToUser:IS-SELECTED(idx) THEN DO:
                /* prevent copy to same from to user */
                IF copyToUser:ENTRY(idx) EQ copyFromUser THEN NEXT.
                /* check if changing current user */
                IF copyToUser:ENTRY(idx) EQ cActiveUser THEN
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
    MESSAGE 
        "Copy from User" copyFromUser "Complete."
    VIEW-AS ALERT-BOX.
    RUN pGetUsers.
    
    /* if current user, need to rebuild menu and redisplay */
    IF lCurrentUser THEN
    RUN pRebuildMenuTree.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUsers Dialog-Frame 
PROCEDURE pGetUsers :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            copyFromUser:LIST-ITEM-PAIRS = ?
            copyToUser:LIST-ITEM-PAIRS   = ?
            cUser:LIST-ITEM-PAIRS        = ?
            .
        FOR EACH xUserMenu NO-LOCK
            BREAK BY xUserMenu.user_id:
            IF FIRST-OF(xUserMenu.user_id) THEN DO:
                FIND FIRST users OF xUserMenu NO-LOCK NO-ERROR.
                copyFromUser:ADD-LAST(xUserMenu.user_id
                    + (IF AVAILABLE users THEN " - "
                    + REPLACE(users.user_name,","," ") ELSE ""),xUserMenu.user_id)
                    .
            END. /* first-of */
        END. /* each xusermenu */
        FOR EACH users NO-LOCK:
            IF lAdmin OR users.user_id EQ cActiveUser THEN
            cUser:ADD-LAST(users.user_id + " - "
                + REPLACE(users.user_name,","," "),
                users.user_id).
            IF users.securityLevel LE iUserSecurityLevel THEN
            copyToUser:ADD-LAST(users.user_id + " - "
                + REPLACE(users.user_name,","," ")
                + " (" + STRING(users.securityLevel) + ")",
                users.user_id).
        END. /* each users */
        IF copyFromUser:NUM-ITEMS GT 0 THEN 
        ASSIGN
            cUser:SCREEN-VALUE        = cActiveUser
            cUser:INNER-LINES         = cUser:NUM-ITEMS
            copyFromUser:SCREEN-VALUE = copyFromUser:ENTRY(1)
            copyFromUser
            .
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserSettings Dialog-Frame 
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
        svLanguageList:RADIO-BUTTONS IN FRAME {&FRAME-NAME} = ",0"
        lMenuImage  = YES
        iFGColor[1] = 15
        iFGColor[2] = 15
        iFGColor[3] = 15
        iBGColor[1] = 1
        iBGColor[2] = 3
        iBGColor[3] = 1
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
        svMenuImage                  = lMenuImage
        .
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ cActiveUser
         NO-ERROR.
    IF AVAILABLE users THEN DO:
        ASSIGN
            iMenuSize             = LOOKUP(users.menuSize,"Small,Medium,Large")
            cShowMnemonic         = users.showMnemonic
            cPositionMnemonic     = users.positionMnemonic
            lMenuImage            = users.showMenuImages
            iFGColor[1]           = users.menuFGColor[1]
            iFGColor[2]           = users.menuFGColor[2]
            iFGColor[3]           = users.menuFGColor[3]
            iBGColor[1]           = users.menuBGColor[1]
            iBGColor[2]           = users.menuBGColor[2]
            iBGColor[3]           = users.menuBGColor[3]
            FGColor-1:BGCOLOR     = iFGColor[1]
            FGColor-2:BGCOLOR     = iFGColor[2]
            FGColor-3:BGCOLOR     = iFGColor[3]
            BGColor-1:BGCOLOR     = iBGColor[1]
            BGColor-2:BGCOLOR     = iBGColor[2]
            BGColor-3:BGCOLOR     = iBGColor[3]
            lShowCueCards         = users.showCueCards
            cueCardsRect:BGCOLOR  = IF lShowCueCards THEN 10 ELSE 12
            lShowCueCards:BGCOLOR = cueCardsRect:BGCOLOR
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
    
    IF iLanguage LT 1 THEN iLanguage = 1.
    IF iMenuSize LT 1 THEN iMenuSize = 1.
    ASSIGN
        svMenuSize     = iMenuSize
        svLanguageList = iLanguage
        cLabelLanguage = ENTRY(iLanguage,cLanguageList)
        svMenuImage    = lMenuImage
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
        btnToggle:SENSITIVE = CAN-DO(prgrms.can_update,cActiveUser).
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
    DISPLAY
        svMenuSize
        svLanguageList
        cShowMnemonic
        cPositionMnemonic
        svMenuImage
        lShowCueCards
            WITH FRAME {&FRAME-NAME}.
    iFont = 30 + iMenuSize * 2.
    IF lAdmin EQ NO THEN
    DISABLE {&copyUser} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit Dialog-Frame 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hPgmMstrSecur AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (
        g_company,"CEMenu","C",NO,NO,"","",
        OUTPUT cCEMenu,OUTPUT lFound
        ).
    IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
    RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
    IF VALID-HANDLE(hPgmMstrSecur) THEN DO:
        RUN epCanAccess IN hPgmMstrSecur (
            "system/mainMenu.w",
            "CanUpgrade",
            OUTPUT lAdmin 
            ).
    END. /* if valid handle */
    IF lAdmin THEN
    ASSIGN
        copyFromUser:SENSITIVE IN FRAME {&FRAME-NAME} = YES
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMenuSize Dialog-Frame 
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
        IF NOT ttMenuTree.lWidgetExist THEN NEXT.
        IF VALID-HANDLE(ttMenuTree.hLevel) THEN DO:
            ASSIGN
                ttMenuTree.hLevel:HIDDEN = YES
                ttMenuTree.hLevel:ROW    = 1
                ttMenuTree.hLevel:WIDTH  = dObjectWidth
                ttMenuTree.hLevel:HEIGHT = dObjectHeight
                .
            ttMenuTree.hLevel:LOAD-IMAGE(SEARCH(cImageFolder + "plus.png")).
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
            ttMenuTree.hRectangle:WIDTH  = dObjectWidth
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessClick Dialog-Frame 
PROCEDURE pProcessClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* return if in customize menu mode */
    IF lToggle THEN RETURN.
    
    IF AVAILABLE ttMenuTree AND ttMenuTree.isMenu AND NOT ttMenuTree.isOpen THEN
    ASSIGN
        ttMenuTree.hEditor:FONT    = iFont
        ttMenuTree.hEditor:BGCOLOR = {&DefaultMenuBGColor}
        ttMenuTree.hEditor:FGCOLOR = ?
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReset Dialog-Frame 
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
    IF NOT ttMenuTree.lWidgetExist THEN NEXT. 
        ASSIGN
            ttMenuTree.baseText             = fTranslate(ENTRY(1,ttMenuTree.hEditor:PRIVATE-DATA),NO)
            ttMenuTree.hEditor:FONT         = iFont
            ttMenuTree.hEditor:BGCOLOR      = {&DefaultMenuBGColor}
            ttMenuTree.hEditor:FGCOLOR      = ?
            ttMenuTree.hEditor:TOOLTIP      = "HotKey: " + ttMenuTree.mnemonic
            ttMenuTree.hEditor:SCREEN-VALUE = fTreeText(
                                                ttMenuTree.isMenu,
                                                ttMenuTree.baseText,
                                                ttMenuTree.mnemonic,
                                                cShowMnemonic,
                                                cPositionMnemonic
                                                ).
        IF VALID-HANDLE(ttMenuTree.hImage) THEN
        ttMenuTree.hImage:HIDDEN = YES.
        IF VALID-HANDLE(ttMenuTree.hToggle) THEN
        ttMenuTree.hToggle:HIDDEN = YES.
    END. /* each ttmenutree */
    RUN pMenuSize.
    RUN pDisplayMenuTree (FRAME menuTreeFrame:HANDLE, "file", YES, 1).

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveCustomMenu Dialog-Frame 
PROCEDURE pSaveCustomMenu :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* remove xusermenu entry if menu option is active or no longer present */
    DO TRANSACTION:
        FOR EACH xUserMenu EXCLUSIVE-LOCK
            WHERE xUserMenu.user_id EQ cActiveUser
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
                        WHERE xUserMenu.user_id  EQ cActiveUser
                          AND xUserMenu.prgmname EQ ttMenuTree.treeChild) THEN
            NEXT.
            /* create entry to hide menu option from user */
            CREATE xUserMenu.
            ASSIGN
                xUserMenu.user_id  = cActiveUser
                xUserMenu.prgmname = ttMenuTree.treeChild
                .
        END. /* each ttmenutree */
        lRebuildMenu = YES.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetUserSettings Dialog-Frame 
PROCEDURE pSetUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION:
        FIND FIRST users EXCLUSIVE-LOCK
             WHERE users.user_id EQ cActiveUser
             NO-ERROR.
        IF AVAILABLE users THEN DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                users.menuSize         = ENTRY(iMenuSize,"Small,Medium,Large")
                users.showMnemonic     = cShowMnemonic
                users.positionMnemonic = cPositionMnemonic            
                users.showMenuImages   = lMenuImage
                users.menuFGColor[1]   = iFGColor[1]
                users.menuFGColor[2]   = iFGColor[2]
                users.menuFGColor[3]   = iFGColor[3]
                users.menuBGColor[1]   = iBGColor[1]
                users.menuBGColor[2]   = iBGColor[2]
                users.menuBGColor[3]   = iBGColor[3]
                users.showCueCards     = lShowCueCards
                cueCardsRect:BGCOLOR   = IF lShowCueCards THEN 10 ELSE 12
                lShowCueCards:BGCOLOR  = cueCardsRect:BGCOLOR
                .
            CASE users.showCueCard:
                WHEN NO THEN
                RUN spInactivateCueCards ("System", users.user_id).
                WHEN YES THEN
                RUN spActivateCueCards (users.user_id).
            END CASE.
            FIND FIRST userLanguage NO-LOCK
                 WHERE userLanguage.languageIdx EQ iLanguage
                 NO-ERROR.
            IF AVAILABLE userLanguage THEN
            users.userLanguage = userLanguage.userLanguage.
            FIND CURRENT users NO-LOCK.
        END. /* avail users */
    END. /* do trans */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

