&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: system/cueCard.w

  Description: Cue Card Maintenance

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.28.2018

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

&Scoped-define program-id isRunning.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE cFilter      AS CHARACTER NO-UNDO INITIAL "ALL".
DEFINE VARIABLE cSubFilter   AS CHARACTER NO-UNDO INITIAL "ALL".
DEFINE VARIABLE cMode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE hColorWidget AS HANDLE    NO-UNDO.
DEFINE VARIABLE hFontWidget  AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttIsRunning NO-UNDO
    FIELD prgTitle AS CHARACTER 
    FIELD prgmName AS CHARACTER 
    FIELD hWindow  AS HANDLE 
    FIELD hFrame   AS HANDLE
        INDEX prgTitle IS PRIMARY 
            prgTitle
            prgmName
            .
{system/menuTree.i}
{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME cueCardBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cueCard cueCardText

/* Definitions for BROWSE cueCardBrowse                                 */
&Scoped-define FIELDS-IN-QUERY-cueCardBrowse cueCard.cueID ~
cueCardText.cueOrder cueCardText.isActive cueCardText.cueText 
&Scoped-define ENABLED-FIELDS-IN-QUERY-cueCardBrowse 
&Scoped-define QUERY-STRING-cueCardBrowse FOR EACH cueCard ~
      WHERE cueCard.cuePrgmName EQ cSubFilter NO-LOCK, ~
      EACH cueCardText WHERE cueCardText.cueID EQ cueCard.cueID NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-cueCardBrowse OPEN QUERY cueCardBrowse FOR EACH cueCard ~
      WHERE cueCard.cuePrgmName EQ cSubFilter NO-LOCK, ~
      EACH cueCardText WHERE cueCardText.cueID EQ cueCard.cueID NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-cueCardBrowse cueCard cueCardText
&Scoped-define FIRST-TABLE-IN-QUERY-cueCardBrowse cueCard
&Scoped-define SECOND-TABLE-IN-QUERY-cueCardBrowse cueCardText


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-cueCardBrowse}

/* Definitions for FRAME viewFrame                                      */
&Scoped-define FIELDS-IN-QUERY-viewFrame cueCard.cueID cueCard.isActive ~
cueCard.cuePrgmName cueCardText.cueTextID cueCardText.isActive ~
cueCardText.cueOrder cueCardText.cueText 
&Scoped-define QUERY-STRING-viewFrame FOR EACH cueCardText SHARE-LOCK, ~
      EACH cueCard WHERE TRUE /* Join to cueCardText incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-viewFrame OPEN QUERY viewFrame FOR EACH cueCardText SHARE-LOCK, ~
      EACH cueCard WHERE TRUE /* Join to cueCardText incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-viewFrame cueCardText cueCard
&Scoped-define FIRST-TABLE-IN-QUERY-viewFrame cueCardText
&Scoped-define SECOND-TABLE-IN-QUERY-viewFrame cueCard


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnView svFocus cueCardBrowse btnReload ~
btnRunCueCard btnRestoreDefaults 
&Scoped-Define DISPLAYED-OBJECTS svFocus 

/* Custom List Definitions                                              */
/* transPanel,transInit,transUpdate,displayFields,enabledFields,colorFontPalette */
&Scoped-define transPanel btnDown btnUp btnFirst btnLast btnNext btnPrev ~
btnCueCardLayout btnClose btnAdd btnCancel btnCopy btnDelete btnReset ~
btnUpdate 
&Scoped-define transInit btnDown btnUp btnFirst btnLast btnNext btnPrev ~
btnCueCardLayout btnClose btnAdd btnCopy btnDelete btnUpdate 
&Scoped-define transUpdate btnCancel btnReset btnUpdate 
&Scoped-define displayFields cueCard.cueID cueCard.isActive ~
cueCard.cuePrgmName cueCardText.cueTextID cueCardText.isActive ~
cueCardText.cueOrder cueCardText.cueText 
&Scoped-define enabledFields cueCard.isActive cueCardText.isActive ~
cueCardText.cueText 
&Scoped-define colorFontPalette colorChoice-0 colorChoice-1 colorChoice-2 ~
colorChoice-3 colorChoice-4 colorChoice-5 colorChoice-6 colorChoice-7 ~
colorChoice-8 colorChoice-9 colorChoice-10 colorChoice-11 colorChoice-12 ~
colorChoice-13 colorChoice-14 colorChoice-15 fBGColor fFGColor tFGColor ~
tBGColor colorChoice-default fontChoice-0 fontChoice-1 fontChoice-2 ~
fontChoice-3 fontChoice-4 fontChoice-5 fontChoice-6 fontChoice-7 ~
fontChoice-default lGotItFont lDontShowAgainFont cCuetextFont 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnReload 
     IMAGE-UP FILE "Graphics/16x16/elements_tree.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Reload Running Frames" 
     SIZE 4 BY .95 TOOLTIP "Reload Running Frames".

DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnRunCueCard 
     IMAGE-UP FILE "Graphics/16x16/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Test Run" 
     SIZE 4 BY .95 TOOLTIP "Test Run".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/16x16/window_dialog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "View" 
     SIZE 4 BY .95 TOOLTIP "Reload Running Frames".

DEFINE VARIABLE svFocus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE .2 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 4.2 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics\32x32\form_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnCueCardLayout 
     IMAGE-UP FILE "Graphics/32x32/window_size.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/window_size_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cue Card Layout" 
     SIZE 8 BY 1.91 TOOLTIP "Cue Card Layout".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_minus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_minus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnDown 
     IMAGE-UP FILE "Graphics/16x16/down.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/window_size_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Cur Card Down" 
     SIZE 4 BY .95 TOOLTIP "Move Cur Card Down".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_beginning.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_beginning_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_end.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_end_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_right.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_right_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrev 
     IMAGE-UP FILE "Graphics/32x32/navigate_left.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_left_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Previous" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnUp 
     IMAGE-UP FILE "Graphics/16x16/up.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/window_size_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Cur Card Up" 
     SIZE 4 BY .95 TOOLTIP "Move Cur Card Up".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE VARIABLE cCuetextFont AS CHARACTER FORMAT "X(256)":U INITIAL "Cue Card Text (font)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

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

DEFINE RECTANGLE fBGColor
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE fFGColor
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE fontChoice-0
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE fontChoice-default
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 6 BY 1.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 97 BY 1.43.

DEFINE RECTANGLE tBGColor
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE tFGColor
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1.

DEFINE RECTANGLE transPanel
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE transPanel-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 14 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE transPanel-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 34 BY 2.38
     BGCOLOR 15 .

DEFINE VARIABLE lDontShowAgainFont AS LOGICAL INITIAL yes 
     LABEL "Don't Show Again (font)" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE lGotItFont AS LOGICAL INITIAL yes 
     LABEL "Got It (font)" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY cueCardBrowse FOR 
      cueCard, 
      cueCardText SCROLLING.

DEFINE QUERY viewFrame FOR 
      cueCardText, 
      cueCard SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE cueCardBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS cueCardBrowse C-Win _STRUCTURED
  QUERY cueCardBrowse NO-LOCK DISPLAY
      cueCard.cueID FORMAT ">>>>9":U
      cueCardText.cueOrder FORMAT ">>>9":U
      cueCardText.isActive FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      cueCardText.cueText FORMAT "x(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 14.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnView AT ROW 1 COL 70 HELP
          "Access View" WIDGET-ID 48
     svFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 4
     cueCardBrowse AT ROW 1.95 COL 62 WIDGET-ID 500
     btnReload AT ROW 1 COL 66 HELP
          "Reload Running Frames" WIDGET-ID 46
     btnRunCueCard AT ROW 1 COL 74 HELP
          "Test Run" WIDGET-ID 44
     btnRestoreDefaults AT ROW 1 COL 62 HELP
          "Restore Defaults" WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME filterFrame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61 BY 28.57
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 8 "Running Objects" WIDGET-ID 200.

DEFINE FRAME viewFrame
     btnDown AT ROW 12.19 COL 60 HELP
          "Move Cur Card Down" WIDGET-ID 444
     btnUp AT ROW 11.24 COL 60 HELP
          "Move Cur Card Up" WIDGET-ID 442
     btnFirst AT ROW 11.24 COL 66 HELP
          "First" WIDGET-ID 274
     btnLast AT ROW 11.24 COL 90 HELP
          "Last" WIDGET-ID 68
     btnNext AT ROW 11.24 COL 82 HELP
          "Next" WIDGET-ID 276
     btnPrev AT ROW 11.24 COL 74 HELP
          "Previous" WIDGET-ID 278
     btnCueCardLayout AT ROW 11.24 COL 52 HELP
          "Cue Card Layout" WIDGET-ID 34
     btnClose AT ROW 1.48 COL 94 HELP
          "Close" WIDGET-ID 72
     btnAdd AT ROW 11.24 COL 10 HELP
          "Add" WIDGET-ID 20
     btnCancel AT ROW 11.24 COL 42 HELP
          "Cancel" WIDGET-ID 28
     btnCopy AT ROW 11.24 COL 18 HELP
          "Copy" WIDGET-ID 24
     btnDelete AT ROW 11.24 COL 26 HELP
          "Delete" WIDGET-ID 26
     btnReset AT ROW 11.24 COL 34 HELP
          "Reset" WIDGET-ID 22
     btnUpdate AT ROW 11.24 COL 2 HELP
          "Update/Save" WIDGET-ID 18
     cueCard.cueID AT ROW 1.48 COL 16 COLON-ALIGNED WIDGET-ID 300
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     cueCard.isActive AT ROW 1.48 COL 31 WIDGET-ID 304
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     cueCard.cuePrgmName AT ROW 1.48 COL 69 COLON-ALIGNED WIDGET-ID 302
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     cueCardText.cueTextID AT ROW 2.91 COL 16 COLON-ALIGNED WIDGET-ID 288
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
          BGCOLOR 15 
     cueCardText.isActive AT ROW 2.91 COL 31 WIDGET-ID 292
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     cueCardText.cueOrder AT ROW 2.91 COL 83 COLON-ALIGNED WIDGET-ID 284
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 
     cueCardText.cueText AT ROW 4.1 COL 18 NO-LABEL WIDGET-ID 294
          VIEW-AS EDITOR
          SIZE 81 BY 1.67
          BGCOLOR 15 
     lGotItFont AT ROW 8.62 COL 4 HELP
          "Select to Set Font" WIDGET-ID 394
     lDontShowAgainFont AT ROW 9.81 COL 4 HELP
          "Select to Set Font" WIDGET-ID 392
     cCuetextFont AT ROW 9.81 COL 57 COLON-ALIGNED HELP
          "Select Font Size" NO-LABEL WIDGET-ID 396
     "12" VIEW-AS TEXT
          SIZE 3 BY .71 AT ROW 7.33 COL 73.4 WIDGET-ID 434
          BGCOLOR 12 FGCOLOR 15 
     "0" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 46 WIDGET-ID 402
          BGCOLOR 0 FGCOLOR 15 
     "3" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 67 WIDGET-ID 416
          BGCOLOR 3 FGCOLOR 15 
     "Frame FG Color:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 6 COL 2 WIDGET-ID 344
     "BG:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 7.19 COL 26 WIDGET-ID 350
     "2" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 60 WIDGET-ID 414
          BGCOLOR 2 FGCOLOR 15 
     "Cue Card Text:" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 4.1 COL 2 WIDGET-ID 298
     "?" VIEW-AS TEXT
          SIZE 2 BY .76 AT ROW 8.62 COL 39 WIDGET-ID 390
          FGCOLOR 0 FONT 6
     "15" VIEW-AS TEXT
          SIZE 3 BY .71 AT ROW 7.33 COL 94.4 WIDGET-ID 440
          BGCOLOR 15 FGCOLOR 0 
     "14" VIEW-AS TEXT
          SIZE 3 BY .71 AT ROW 7.33 COL 87.4 WIDGET-ID 438
          BGCOLOR 14 FGCOLOR 0 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 62 ROW 16
         SIZE 99 BY 13.57
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     "Nn" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 60 WIDGET-ID 366
          FGCOLOR 0 FONT 2
     "Ee" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 95 WIDGET-ID 386
          FGCOLOR 0 FONT 7
     "1" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 53 WIDGET-ID 412
          BGCOLOR 1 FGCOLOR 15 
     "4" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 74 WIDGET-ID 418
          BGCOLOR 4 FGCOLOR 15 
     "5" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 81 WIDGET-ID 420
          BGCOLOR 5 FGCOLOR 15 
     "8" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 7.33 COL 46 WIDGET-ID 426
          BGCOLOR 8 FGCOLOR 0 
     "?" VIEW-AS TEXT
          SIZE 2 BY .76 AT ROW 6.71 COL 39 WIDGET-ID 354
          FGCOLOR 0 FONT 6
     "Ss" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 74 WIDGET-ID 374
          FGCOLOR 0 FONT 4
     "10" VIEW-AS TEXT
          SIZE 3 BY .71 AT ROW 7.33 COL 59.4 WIDGET-ID 430
          BGCOLOR 10 FGCOLOR 0 
     "13" VIEW-AS TEXT
          SIZE 3 BY .71 AT ROW 7.33 COL 80.4 WIDGET-ID 436
          BGCOLOR 13 FGCOLOR 15 
     "Text FG Color:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 7.19 COL 4 WIDGET-ID 346
     "Oo" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 53 WIDGET-ID 362
          FGCOLOR 0 FONT 1
     "Tt" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 67 WIDGET-ID 370
          FGCOLOR 0 FONT 3
     "BG:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 6 COL 26 WIDGET-ID 348
     "Ff" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 46 WIDGET-ID 358
          FGCOLOR 0 FONT 0
     "Zz" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 88 WIDGET-ID 382
          FGCOLOR 0 FONT 6
     "Ii" VIEW-AS TEXT
          SIZE 3 BY .76 AT ROW 8.62 COL 81 WIDGET-ID 378
          FGCOLOR 0 FONT 5
     "Fonts:" VIEW-AS TEXT
          SIZE 6 BY .71 AT ROW 8.62 COL 30 WIDGET-ID 398
     "9" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 7.33 COL 53 WIDGET-ID 428
          BGCOLOR 9 FGCOLOR 15 
     "7" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 95 WIDGET-ID 424
          BGCOLOR 7 FGCOLOR 15 
     "6" VIEW-AS TEXT
          SIZE 2 BY .71 AT ROW 6.1 COL 88 WIDGET-ID 422
          BGCOLOR 6 FGCOLOR 15 
     "11" VIEW-AS TEXT
          SIZE 3 BY .71 AT ROW 7.33 COL 66.4 WIDGET-ID 432
          BGCOLOR 11 FGCOLOR 0 
     transPanel AT ROW 11 COL 1 WIDGET-ID 16
     transPanel-8 AT ROW 11 COL 65 WIDGET-ID 280
     transPanel-2 AT ROW 11 COL 51 WIDGET-ID 32
     colorChoice-0 AT ROW 6 COL 44 WIDGET-ID 48
     colorChoice-1 AT ROW 6 COL 51 WIDGET-ID 306
     colorChoice-2 AT ROW 6 COL 58 WIDGET-ID 308
     colorChoice-3 AT ROW 6 COL 65 WIDGET-ID 310
     colorChoice-4 AT ROW 6 COL 72 WIDGET-ID 312
     colorChoice-5 AT ROW 6 COL 79 WIDGET-ID 314
     colorChoice-6 AT ROW 6 COL 86 WIDGET-ID 316
     colorChoice-7 AT ROW 6 COL 93 WIDGET-ID 318
     colorChoice-8 AT ROW 7.19 COL 44 WIDGET-ID 320
     colorChoice-9 AT ROW 7.19 COL 51 WIDGET-ID 322
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 62 ROW 16
         SIZE 99 BY 13.57
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     colorChoice-10 AT ROW 7.19 COL 58 WIDGET-ID 324
     colorChoice-11 AT ROW 7.19 COL 65 WIDGET-ID 326
     colorChoice-12 AT ROW 7.19 COL 72 WIDGET-ID 328
     colorChoice-13 AT ROW 7.19 COL 79 WIDGET-ID 330
     colorChoice-14 AT ROW 7.19 COL 86 WIDGET-ID 332
     colorChoice-15 AT ROW 7.19 COL 93 WIDGET-ID 334
     fBGColor AT ROW 6 COL 30 WIDGET-ID 336
     fFGColor AT ROW 6 COL 19 WIDGET-ID 338
     tFGColor AT ROW 7.19 COL 19 WIDGET-ID 340
     tBGColor AT ROW 7.19 COL 30 WIDGET-ID 342
     colorChoice-default AT ROW 6.48 COL 37 WIDGET-ID 352
     fontChoice-0 AT ROW 8.38 COL 44 WIDGET-ID 356
     fontChoice-1 AT ROW 8.38 COL 51 WIDGET-ID 360
     fontChoice-2 AT ROW 8.38 COL 58 WIDGET-ID 364
     fontChoice-3 AT ROW 8.38 COL 65 WIDGET-ID 368
     fontChoice-4 AT ROW 8.38 COL 72 WIDGET-ID 372
     fontChoice-5 AT ROW 8.38 COL 79 WIDGET-ID 376
     fontChoice-6 AT ROW 8.38 COL 86 WIDGET-ID 380
     fontChoice-7 AT ROW 8.38 COL 93 WIDGET-ID 384
     fontChoice-default AT ROW 8.38 COL 37 WIDGET-ID 388
     RECT-2 AT ROW 1.24 COL 2 WIDGET-ID 400
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 62 ROW 16
         SIZE 99 BY 13.57
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 400.


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
         TITLE              = "Cue Card Layput"
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
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME filterFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME viewFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME filterFrame:MOVE-AFTER-TAB-ITEM (svFocus:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME filterFrame:MOVE-BEFORE-TAB-ITEM (cueCardBrowse:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME viewFrame:MOVE-AFTER-TAB-ITEM (cueCardBrowse:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB cueCardBrowse filterFrame DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN svFocus IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME filterFrame
                                                                        */
/* SETTINGS FOR FRAME viewFrame
                                                                        */
ASSIGN 
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCancel IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnClose IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCopy IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCueCardLayout IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDelete IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDown IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnFirst IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnLast IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnNext IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnPrev IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnReset IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnUp IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME viewFrame
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN cCuetextFont IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       cCuetextFont:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-0 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-0:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-1 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-1:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-10 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-10:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-11 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-11:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-12 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-12:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-13 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-13:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-14 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-14:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-15 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-15:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-2 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-2:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-3 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-3:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-4 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-4:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-5 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-5:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-6 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-6:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-7 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-7:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-8 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-8:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-9 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-9:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE colorChoice-default IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       colorChoice-default:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR FILL-IN cueCard.cueID IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN cueCardText.cueOrder IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN cueCard.cuePrgmName IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR EDITOR cueCardText.cueText IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cueCardText.cueTextID IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RECTANGLE fBGColor IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fBGColor:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE fFGColor IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fFGColor:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE fontChoice-0 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-0:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-0:PRIVATE-DATA IN FRAME viewFrame     = 
                "0".

/* SETTINGS FOR RECTANGLE fontChoice-1 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-1:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-1:PRIVATE-DATA IN FRAME viewFrame     = 
                "1".

/* SETTINGS FOR RECTANGLE fontChoice-2 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-2:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-2:PRIVATE-DATA IN FRAME viewFrame     = 
                "2".

/* SETTINGS FOR RECTANGLE fontChoice-3 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-3:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-3:PRIVATE-DATA IN FRAME viewFrame     = 
                "3".

/* SETTINGS FOR RECTANGLE fontChoice-4 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-4:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-4:PRIVATE-DATA IN FRAME viewFrame     = 
                "4".

/* SETTINGS FOR RECTANGLE fontChoice-5 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-5:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-5:PRIVATE-DATA IN FRAME viewFrame     = 
                "5".

/* SETTINGS FOR RECTANGLE fontChoice-6 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-6:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-6:PRIVATE-DATA IN FRAME viewFrame     = 
                "6".

/* SETTINGS FOR RECTANGLE fontChoice-7 IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-7:SELECTABLE IN FRAME viewFrame       = TRUE
       fontChoice-7:PRIVATE-DATA IN FRAME viewFrame     = 
                "7".

/* SETTINGS FOR RECTANGLE fontChoice-default IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       fontChoice-default:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR TOGGLE-BOX cueCardText.isActive IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX cueCard.isActive IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX lDontShowAgainFont IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       lDontShowAgainFont:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR TOGGLE-BOX lGotItFont IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       lGotItFont:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE tBGColor IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       tBGColor:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE tFGColor IN FRAME viewFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       tFGColor:SELECTABLE IN FRAME viewFrame       = TRUE.

/* SETTINGS FOR RECTANGLE transPanel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-8 IN FRAME viewFrame
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE cueCardBrowse
/* Query rebuild information for BROWSE cueCardBrowse
     _TblList          = "ASI.cueCard,ASI.cueCardText WHERE ASI.cueCard ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ","
     _Where[1]         = "cueCard.cuePrgmName EQ cSubFilter"
     _JoinCode[2]      = "cueCardText.cueID EQ cueCard.cueID"
     _FldNameList[1]   = ASI.cueCard.cueID
     _FldNameList[2]   = ASI.cueCardText.cueOrder
     _FldNameList[3]   > ASI.cueCardText.isActive
"cueCardText.isActive" ? ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[4]   > ASI.cueCardText.cueText
"cueCardText.cueText" ? "x(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE cueCardBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME filterFrame
/* Query rebuild information for FRAME filterFrame
     _Query            is NOT OPENED
*/  /* FRAME filterFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _TblList          = "ASI.cueCardText,ASI.cueCard WHERE ASI.cueCardText ..."
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cue Card Layput */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cue Card Layput */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Cue Card Layput */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME viewFrame /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME viewFrame /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME viewFrame /* Close */
DO:
    SELF:MOVE-TO-BOTTOM().
    HIDE FRAME viewFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME viewFrame /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCueCardLayout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCueCardLayout C-Win
ON CHOOSE OF btnCueCardLayout IN FRAME viewFrame /* Cue Card Layout */
DO:
    RUN pCueCardLayout.
    BROWSE cueCardBrowse:REFRESH().
    RUN pDisplay.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME viewFrame /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown C-Win
ON CHOOSE OF btnDown IN FRAME viewFrame /* Move Cur Card Down */
DO:
    RUN pChangeOrder (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst C-Win
ON CHOOSE OF btnFirst IN FRAME viewFrame /* First */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast C-Win
ON CHOOSE OF btnLast IN FRAME viewFrame /* Last */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME viewFrame /* Next */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev C-Win
ON CHOOSE OF btnPrev IN FRAME viewFrame /* Previous */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnReload
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReload C-Win
ON CHOOSE OF btnReload IN FRAME DEFAULT-FRAME /* Reload Running Frames */
DO:
    RUN pInit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME viewFrame /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults C-Win
ON CHOOSE OF btnRestoreDefaults IN FRAME DEFAULT-FRAME /* Defaults */
DO:
    RUN pGetSettings ("_default").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunCueCard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunCueCard C-Win
ON CHOOSE OF btnRunCueCard IN FRAME DEFAULT-FRAME /* Test Run */
DO:
    RUN pRunCueCard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp C-Win
ON CHOOSE OF btnUp IN FRAME viewFrame /* Move Cur Card Up */
DO:
    RUN pChangeOrder (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME viewFrame /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View */
DO:
    VIEW FRAME viewFrame.
    RUN pDisplay.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME cCuetextFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCuetextFont C-Win
ON SELECTION OF cCuetextFont IN FRAME viewFrame
DO:
    hFontWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorChoice-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorChoice-0 C-Win
ON SELECTION OF colorChoice-0 IN FRAME viewFrame
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


&Scoped-define BROWSE-NAME cueCardBrowse
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cueCardBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cueCardBrowse C-Win
ON DEFAULT-ACTION OF cueCardBrowse IN FRAME DEFAULT-FRAME
DO:
    VIEW FRAME viewFrame.
    RUN pDisplay.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cueCardBrowse C-Win
ON VALUE-CHANGED OF cueCardBrowse IN FRAME DEFAULT-FRAME
DO:
    RUN pDisplay.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME fFGColor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fFGColor C-Win
ON SELECTION OF fFGColor IN FRAME viewFrame
,fBGColor,tFGColor,tBGColor
DO:
    hColorWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fontChoice-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fontChoice-0 C-Win
ON SELECTION OF fontChoice-0 IN FRAME viewFrame
,fontChoice-1,fontChoice-2,fontChoice-3,fontChoice-4,fontChoice-5
,fontChoice-6,fontChoice-7,fontChoice-default
DO:
    IF VALID-HANDLE(hFontWidget) THEN
    hFontWidget:FONT = INTEGER(SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lDontShowAgainFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lDontShowAgainFont C-Win
ON SELECTION OF lDontShowAgainFont IN FRAME viewFrame /* Don't Show Again (font) */
DO:
    hFontWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lGotItFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lGotItFont C-Win
ON SELECTION OF lGotItFont IN FRAME viewFrame /* Got It (font) */
DO:
    hFontWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
  hFocus = svFocus:HANDLE.
  RUN pGetSettings (USERID("ASI")).
  RUN pInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY svFocus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnView svFocus cueCardBrowse btnReload btnRunCueCard 
         btnRestoreDefaults 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME filterFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-filterFrame}
  DISPLAY lGotItFont lDontShowAgainFont cCuetextFont 
      WITH FRAME viewFrame IN WINDOW C-Win.
  IF AVAILABLE cueCard THEN 
    DISPLAY cueCard.cueID cueCard.isActive cueCard.cuePrgmName 
      WITH FRAME viewFrame IN WINDOW C-Win.
  IF AVAILABLE cueCardText THEN 
    DISPLAY cueCardText.cueTextID cueCardText.isActive cueCardText.cueOrder 
          cueCardText.cueText 
      WITH FRAME viewFrame IN WINDOW C-Win.
  ENABLE btnDown btnUp btnFirst btnLast btnNext btnPrev btnCueCardLayout 
         btnClose btnAdd btnCopy btnDelete btnUpdate 
      WITH FRAME viewFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-viewFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssign C-Win 
PROCEDURE pAssign :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME viewFrame:
        FIND CURRENT cueCard EXCLUSIVE-LOCK.
        FIND CURRENT cueCardText EXCLUSIVE-LOCK.
        ASSIGN
            cueCard.cueID
            cueCard.cuePrgmName
            cueCardText.cueTextID
            cueCardText.cueOrder
            {&enabledFields}
            cueCardText.frameFGColor      = fFGColor:BGCOLOR
            cueCardText.frameBGColor      = fBGColor:BGCOLOR
            cueCardText.textFGColor       = tFGColor:BGCOLOR
            cueCardText.textBGColor       = tBGColor:BGCOLOR
            cueCardText.textFont          = cCuetextFont:FONT
            cueCardText.dontShowAgainFont = lDontShowAgainFont:FONT
            cueCardText.gotItFont         = lGotItFont:FONT
            .
        FIND CURRENT cueCardText NO-LOCK.
        FIND CURRENT cueCard NO-LOCK.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildMenuTree C-Win 
PROCEDURE pBuildMenuTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrder    AS INTEGER   NO-UNDO.
    
    RUN pInitMenuTree.

    FOR EACH ttIsRunning
        BREAK BY ttIsRunning.prgTitle
              BY ttIsRunning.prgmName
        :
        IF FIRST-OF(ttIsRunning.prgTitle) THEN DO:
            iOrder = iOrder + 1.
            RUN pCreatettMenuTree (
                FRAME filterFrame:HANDLE,
                iOrder,
                1,
                YES,
                "",
                ttIsRunning.prgTitle,
                ttIsRunning.prgTitle,
                "tab_pane.png",
                "",
                "",
                "",
                YES
                ).
        END. /* if first-of prgTitle */
        IF FIRST-OF(ttIsRunning.prgmName) THEN DO:
            iOrder = iOrder + 1.
            RUN pCreatettMenuTree (
                FRAME filterFrame:HANDLE,
                iOrder,
                2,
                NO,
                ttIsRunning.prgTitle,
                ttIsRunning.prgmName,
                ttIsRunning.prgmName,
                "hand_point_right2.png",
                "",
                "",
                "",
                YES
                ).
        END. /* if first-of prgTitle */
    END. /* each ttIsRunning */
    iOrder = iOrder + 1.
    RUN pCreatettMenuTree (
        FRAME filterFrame:HANDLE,
        iOrder,
        1,
        NO,
        "",
        "Exit",
        "Exit",
        "navigate_cross.png",
        "",
        "",
        "",
        YES
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeOrder C-Win 
PROCEDURE pChangeOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiChangeOrder AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo  AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bCueCardText FOR cueCardText.
    
    /* first cue card, can't move up */
    IF cueCardText.cueOrder EQ 1 AND ipiChangeOrder EQ -1 THEN RETURN.
    /* check if at bottom, can't move down */
    FIND LAST bCueCardText NO-LOCK
         WHERE bCueCardText.cueID EQ cueCardText.cueID
         NO-ERROR.
    IF AVAILABLE bCueCardText THEN DO:
         /* check if at bottom, can't move down */
         IF bCueCardText.cueOrder EQ cueCardText.cueOrder AND ipiChangeOrder EQ 1 THEN
        RETURN.
    END. /* if avail */
    ELSE RETURN.
    ASSIGN
        iCurrent = cueCardText.cueOrder
        iMoveTo  = cueCardText.cueOrder + ipiChangeOrder
        .
    FIND FIRST bCueCardText EXCLUSIVE-LOCK
         WHERE bCueCardText.cueID    EQ cueCardText.cueID
           AND bCueCardText.cueOrder EQ iMoveTo
         NO-ERROR.
    IF AVAILABLE bCueCardText THEN DO:
        FIND CURRENT cueCardText EXCLUSIVE-LOCK.
        ASSIGN
            cueCardText.cueOrder  = 0
            bCueCardText.cueOrder = iCurrent
            cueCardText.cueOrder  = iMoveTo
            .
        FIND CURRENT cueCardText NO-LOCK.
    END. /* if avail */
    RUN pReopenBrowse.
/*    BROWSE cueCardBrowse:REFRESH().*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearView C-Win 
PROCEDURE pClearView :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        hWidget = FRAME viewFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE NE "BUTTON" AND
           hWidget:SELECTABLE EQ NO AND 
           hWidget:SENSITIVE THEN
        hWidget:SCREEN-VALUE = if hWidget:TYPE EQ "TOGGLE-BOX" THEN "YES" ELSE "".
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    ASSIGN 
        cueCard.cuePrgmName:SCREEN-VALUE   = cSubFilter
        cueCardText.cueText:SCREEN-VALUE   = "Enter Cue Card Text Here"
        fFGColor:BGCOLOR                   = ?
        fBGColor:BGCOLOR                   = ?
        tFGColor:BGCOLOR                   = ?
        tBGColor:BGCOLOR                   = ?
        cCuetextFont:FONT                  = ?
        lDontShowAgainFont:FONT            = ?
        lGotItFont:FONT                    = ?
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTtIsRunning C-Win 
PROCEDURE pCreateTtIsRunning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame  AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hThisProc AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cName     AS CHARACTER NO-UNDO.

    hThisProc = iphFrame:INSTANTIATING-PROCEDURE.
    IF NOT VALID-HANDLE(hThisProc) THEN RETURN.

    cName = ENTRY(1,hThisProc:NAME,".").
    IF NOT CAN-FIND(FIRST ttIsRunning
                    WHERE ttIsRunning.prgTitle EQ iphWidget:TITLE
                      AND ttIsRunning.prgmName EQ cName) AND
       NOT iphWidget:TITLE BEGINS "Procedure" AND
       NOT cName EQ cCuePrgmName   AND
       NOT cName BEGINS "adecomm/" AND
       NOT cName BEGINS "adeedit/" AND
       NOT cName BEGINS "adm/"      OR
           cName EQ "system/mainMenu" THEN DO:
        /* allows addition of sub-frames in mainmenu */
        IF cName EQ "system/mainMenu" THEN DO:
            IF iphFrame:NAME NE "FRAME-USER" THEN
            cName = iphFrame:NAME. 
        END. /* if mainmenu */
        CREATE ttIsRunning.
        ASSIGN 
            ttIsRunning.prgTitle  = iphWidget:TITLE
            ttIsRunning.prgmName  = cName
            ttIsRunning.hWindow   = iphWidget
            ttIsRunning.hFrame    = iphFrame
             .
    END. /* if not can-find */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD C-Win 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hWidget     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE cSaveParent AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSaveChild  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lContinue   AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bCueCardText FOR cueCardText.
    
    DO WITH FRAME viewFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel}.
                ASSIGN
                    FRAME filterFrame:SENSITIVE    = NO
                    BROWSE cueCardBrowse:SENSITIVE = NO
                    .
                ENABLE {&transUpdate} {&enabledFields} {&colorFontPalette}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Save_As.ico").
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    DISABLE btnReset.
                END. /* add */
                IF iphMode:LABEL EQ "Add" OR iphMode:LABEL EQ "Copy" THEN DO:
                    ASSIGN 
                        cueCardTExt.cueTextID:SCREEN-VALUE = ""
                        cueCardText.cueOrder:SCREEN-VALUE  = "9999"
                        .
                    IF AVAILABLE cueCard THEN DO:
                        MESSAGE 
                            iphMode:LABEL "to New Cue Card Set?"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        UPDATE lAddNewCueCardSet AS LOGICAL.
                        IF lAddNewCueCardSet THEN
                        cueCard.cueID:SCREEN-VALUE = "".
                    END. /* if avail */
                END. /* if add or copy */
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL       = "Save"
                    .                
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        IF INTEGER(cueCard.cueID:SCREEN-VALUE) EQ 0 THEN DO:
                            CREATE cueCard.
                            cueCard.cueID:SCREEN-VALUE = STRING(cueCard.cueID).
                        END. /* if cueid zero */
                        IF cMode EQ "Copy" THEN DO:
                            FIND FIRST bCueCardText NO-LOCK
                                 WHERE ROWID(bCueCardText) EQ ROWID(cueCardText).
                            CREATE cueCardText.
                            BUFFER-COPY bCueCardText EXCEPT cueID cueTextID cueOrder TO cueCardText.
                        END. /* if copy */
                        ELSE DO: /* add */
                            CREATE cueCardText.
                            /* initialize values based on layout of cueCardLayout.w */
                            ASSIGN
                                cueCardText.arrowCol         = 2
                                cueCardText.arrowRow         = 1.24
                                cueCardText.cueOrientation   = 1
                                cueCardText.cuePosition      = 1
                                cueCardText.dontShowAgainCol = 19
                                cueCardText.dontShowAgainRow = 1.24
                                cueCardText.frameCol         = 1
                                cueCardText.frameRow         = 1
                                cueCardText.frameBGColor     = 14
                                cueCardText.frameFGColor     = 1
                                cueCardText.frameHeight      = 7.38
                                cueCardText.frameWidth       = 53
                                cueCardText.gotItCol         = 26
                                cueCardText.gotItRow         = 7.19
                                cueCardText.nextCol          = 48
                                cueCardText.nextRow          = 6.95
                                cueCardText.positionCol      = 9
                                cueCardText.positionRow      = 1.24
                                cueCardText.prevCol          = 9
                                cueCardText.prevRow          = 6.95
                                cueCardText.textCol          = 9
                                cueCardText.textRow          = 2.67
                                cueCardText.textHeight       = 4.29
                                cueCardText.textWidth        = 44
                                fFGColor:BGCOLOR             = cueCardText.frameFGColor
                                fBGColor:BGCOLOR             = cueCardText.frameBGColor
                                tFGColor:BGCOLOR             = cueCardText.textFGColor
                                tBGColor:BGCOLOR             = cueCardText.textBGColor
                                lGotItFont:FONT              = cueCardText.gotItFont
                                lDontShowAgainFont:FONT      = cueCardText.dontShowAgainFont
                                cCuetextFont:FONT            = cueCardText.textFont
                                .
                        END. /* else add */
                        ASSIGN 
                            cueCardText.cueID = cueCard.cueID
                            cueCardText.cueTextID:SCREEN-VALUE = STRING(cueCardText.cueTextID)
                            .
                    END. /* if add/copy */
                    RUN pAssign.
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        RUN pSetCueOrder.
                        ASSIGN
                            cSaveParent = ttMenuTree.treeParent
                            cSaveChild  = ttMenuTree.treeChild
                            .
                        RUN pInit.
                        ASSIGN
                            cFilter    = ""
                            cSubFilter = ""
                            .
                        RUN pReopenBrowse.
                        FIND FIRST ttMenuTree
                             WHERE ttMenuTree.treeParent EQ ""
                               AND ttMenuTree.treeChild  EQ cSaveParent
                             .
                        IF ttMenuTree.isOpen THEN ttMenuTree.isOpen = NO. 
                        RUN pClickMenuTree (ttMenuTree.hEditor).
                        FIND FIRST ttMenuTree
                             WHERE ttMenuTree.treeParent EQ cSaveParent
                               AND ttMenuTree.treeChild  EQ cSaveChild
                             .
                        RUN pClickMenuTree (ttMenuTree.hEditor).
                    END. /* if add */
                    ELSE
                    BROWSE cueCardBrowse:REFRESH().
                END. /* save */
                DISABLE {&transPanel} {&enabledFields} {&colorFontPalette}.
                ENABLE {&transInit}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.ico").
                ASSIGN
                    FRAME viewFrame:TITLE          = "View"
                    btnUpdate:LABEL                = "Update"
                    FRAME filterFrame:SENSITIVE    = YES
                    BROWSE cueCardBrowse:SENSITIVE = YES
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE cueCardBrowse.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE cueCardText THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO:
                        FIND CURRENT cueCardText EXCLUSIVE-LOCK.
                        DELETE cueCardText.
                        /* check if all cue card text records deleted */
                        IF NOT CAN-FIND(FIRST cueCardText
                                        WHERE cueCardText.cueID EQ cueCard.cueID) THEN DO:
                            FIND CURRENT cueCard EXCLUSIVE-LOCK.
                            DELETE cueCard.
                            HIDE FRAME viewFrame.
                        END. /* if not can-find */
                        cMode = iphMode:LABEL.
                        BROWSE cueCardBrowse:DELETE-CURRENT-ROW().
                        RUN pSetCueOrder.
                    END. /* if lcontinue */
                    ELSE
                    BROWSE cueCardBrowse:REFRESH().
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN
            RUN pDisplay.            
        END CASE. /* ipcmode:label */
        IF iphMode:LABEL EQ "Add" THEN
        APPLY "ENTRY":U TO cueCardText.cueText.
        ELSE
        APPLY "ENTRY":U TO BROWSE cueCardBrowse.
        /* save the mode for when logic returns to this procedure */
        cMode = iphMode:LABEL.
    END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCueCardLayout C-Win 
PROCEDURE pCueCardLayout :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FIND FIRST ttIsRunning
         WHERE ttIsRunning.prgmName EQ cSubFilter
         NO-ERROR.
    IF AVAILABLE ttIsRunning THEN DO:
        IF VALID-HANDLE(ttIsRunning.hWindow) THEN
        /* move module to upper left corner so layout has Row & Col reference */
        ASSIGN
            ttIsRunning.hWindow:ROW = 1
            ttIsRunning.hWindow:COL = 1
            .
        ELSE DO:
            MESSAGE
                "Frame Referenced is no longer open." SKIP(1)
                "1. Open Selected Frame." SKIP
                "2. Reload Running Frames."
            VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END. /* else */
    END. /* if avail */
    RUN system/cueCardLayout.w (BUFFER cueCardText, ttIsRunning.hFrame).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplay C-Win 
PROCEDURE pDisplay :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF AVAILABLE cueCardText THEN DO WITH FRAME viewFrame:
        DISPLAY {&displayFields}.
        ASSIGN 
            fFGColor:BGCOLOR        = cueCardText.frameFGColor
            fBGColor:BGCOLOR        = cueCardText.frameBGColor
            tFGColor:BGCOLOR        = cueCardText.textFGColor
            tBGColor:BGCOLOR        = cueCardText.textBGColor
            lGotItFont:FONT         = cueCardText.gotItFont
            lDontShowAgainFont:FONT = cueCardText.dontShowAgainFont
            cCuetextFont:FONT       = cueCardText.textFont
            .
        ENABLE {&transInit}.
    END. /* if avail */
    ELSE DO WITH FRAME viewFrame:
        RUN pClearView.
        ASSIGN 
            cueCard.cueID:SCREEN-VALUE = ""
            cueCardTExt.cueTextID:SCREEN-VALUE = ""
            cueCardText.cueOrder:SCREEN-VALUE  = "9999"
            .
        DISABLE {&transPanel}.
        ENABLE btnAdd btnClose.
    END. /* else */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetIsRunning C-Win 
PROCEDURE pGetIsRunning :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    DEFINE VARIABLE hFrame  AS HANDLE NO-UNDO.
    DEFINE VARIABLE hChild  AS HANDLE NO-UNDO.
    
    EMPTY TEMP-TABLE ttIsRunning.
    
    ASSIGN 
        hWidget = SESSION:HANDLE
        hWidget = hWidget:FIRST-CHILD
        . 
    DO WHILE VALID-HANDLE(hWidget):
        hFrame = hWidget:FIRST-CHILD.
        IF VALID-HANDLE(hFrame) THEN DO:
            IF hFrame:TYPE EQ "FRAME" THEN
            RUN pCreateTtIsRunning (hWidget, hFrame).
        END. /* if valid hframe */

        IF VALID-HANDLE(hFrame) THEN
        hChild = hFrame:FIRST-CHILD.
        
        IF VALID-HANDLE(hChild) THEN  
        hChild = hChild:FIRST-CHILD.
        
        DO WHILE VALID-HANDLE(hChild):
            IF hChild:TYPE EQ "FRAME" THEN
            RUN pCreateTtIsRunning (hWidget, hChild).
            hChild = hChild:NEXT-SIBLING.
        END. /* do while */
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx     AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ g_company
                      AND user-print.program-id EQ "{&program-id}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pGetIsRunning.
    RUN pBuildMenuTree.
    RUN pDisplayMenuTree (FRAME filterFrame:HANDLE, "", YES, 1).
    FIND FIRST ttMenuTree.
    IF AVAILABLE ttMenuTree AND VALID-HANDLE(ttMenuTree.hEditor) THEN
    RUN pClickMenuTree (ttMenuTree.hEditor).
    RUN pSetFocus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavPanel C-Win 
PROCEDURE pNavPanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    CASE iphNavPanel:LABEL:
        WHEN "First" THEN
        APPLY "HOME":U TO BROWSE cueCardBrowse.
        WHEN "Previous" THEN
        BROWSE cueCardBrowse:SELECT-PREV-ROW().
        WHEN "Next" THEN
        BROWSE cueCardBrowse:SELECT-NEXT-ROW().
        WHEN "Last" THEN
        APPLY "END":U TO BROWSE cueCardBrowse.
    END CASE.
    IF AVAILABLE cueCardText THEN
    APPLY "VALUE-CHANGED":U TO BROWSE cueCardBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessClick C-Win 
PROCEDURE pProcessClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    IF AVAILABLE ttMenuTree THEN DO:
        ASSIGN
            cFilter    = ttMenuTree.treeParent
            cSubFilter = ttMenuTree.treeChild
            .
        IF cFilter EQ "" THEN
        ASSIGN
            cFilter    = cSubFilter
            cSubFilter = "ALL"
            .
        RUN pReopenBrowse.
    END. /* if avail not ismenu */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF QUERY cueCardBrowse:NUM-RESULTS EQ 0 THEN
    HIDE FRAME viewFrame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunCueCard C-Win 
PROCEDURE pRunCueCard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF cSubFilter NE "ALL" THEN DO:
        FIND FIRST ttIsRunning
             WHERE ttIsRunning.prgmName EQ cSubFilter
             NO-ERROR.
        IF AVAILABLE ttIsRunning THEN DO:
            IF VALID-HANDLE(ttIsRunning.hWindow) THEN DO:
                ASSIGN
                    cCuePrgmName = cSubFilter
                    hCueWindow   = ttIsRunning.hWindow
                    hCueFrame    = ttIsRunning.hFrame
                    lCueActive   = NO
                    .
                {system/runCueCard.i}
                cCuePrgmName = ENTRY(1,THIS-PROCEDURE:NAME,".").
            END. /* if valid-handle */
            ELSE DO:
                MESSAGE
                    "Frame Referenced is no longer open." SKIP(1)
                    "1. Open Selected Frame." SKIP
                    "2. Reload Running Frames."
                VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END. /* else */
        END. /* if avail */
    END. /* if ne all */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetCueOrder C-Win 
PROCEDURE pSetCueOrder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FOR EACH cueCardText EXCLUSIVE-LOCK 
        WHERE cueCardText.cueID EQ cueCard.cueID
           BY cueCardText.cueOrder 
        :
        ASSIGN 
            idx = idx + 1
            cueCardText.cueOrder = idx
            .
    END. /* each bcuecardtext */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME filterFrame.
        HIDE BROWSE cueCardBrowse.
        HIDE FRAME viewFrame.
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
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            /* filter frame */
            FRAME filterFrame:VIRTUAL-HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            FRAME filterFrame:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
            /* browse frame */
            BROWSE cueCardBrowse:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
                                        - BROWSE cueCardBrowse:ROW + 1
            BROWSE cueCardBrowse:WIDTH  = FRAME {&FRAME-NAME}:WIDTH
                                        - FRAME filterFrame:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW FRAME filterFrame.
        VIEW BROWSE cueCardBrowse.
    END. /* do with */

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

