&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
DEF VAR hDynQuery AS HANDLE.
DEF VAR hDynBrowse AS HANDLE.
DEF VAR hBuffer AS HANDLE.
DEF VAR hColumnHandle AS HANDLE EXTENT 500.
DEF VAR iCtr AS INT.
DEF VAR hMe AS HANDLE.
DEF VAR cMe AS CHAR.
DEF VAR hFillIn AS HANDLE EXTENT 500.
DEF VAR hLabel AS HANDLE EXTENT 500.
DEF VAR hFillIn2 AS HANDLE EXTENT 500.
DEF VAR hLabel2 AS HANDLE EXTENT 500.
DEF VAR vDisp1 AS CHAR NO-UNDO EXTENT 50.
DEF VAR vDisp2 AS CHAR NO-UNDO EXTENT 50.
DEF VAR xWin AS HANDLE NO-UNDO EXTENT 20.
DEF VAR hPgmHandle AS HANDLE NO-UNDO EXTENT 20.
DEF VAR xFrame AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iWinCtr AS INTEGER NO-UNDO.
DEF VAR iCtrX AS INTEGER NO-UNDO.
DEF VAR iRow AS INT INITIAL 1.
DEF VAR iMaxScreenWidth AS INT NO-UNDO.
DEF VAR cWhereClause AS CHAR NO-UNDO.
DEF VAR cSortByPhrase AS CHAR NO-UNDO.
DEF VAR cStatus AS CHAR NO-UNDO.
DEF VAR cColList AS CHAR NO-UNDO.
DEF SHARED VAR h_SDO AS HANDLE NO-UNDO.
DEF STREAM dumpstream.

DEF VAR cFileName AS CHAR NO-UNDO INITIAL "customer".
DEF VAR cSortBy AS CHAR NO-UNDO INITIAL "cust-no".

{n:\repository\devtools\shared.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bChangeSort cbFileName cbSortList cbLockCol ~
bZoom bClear bCancel bChangeFilter bChangeHide 
&Scoped-Define DISPLAYED-OBJECTS cbFileName cbSortList fiFiltered fiHidden ~
cbLockCol 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Spreadsheet 
       MENU-ITEM m_Range        LABEL "Landscape"     
       MENU-ITEM m_Single       LABEL "Portrait"      .

DEFINE SUB-MENU m_Print 
       MENU-ITEM m_Range2       LABEL "Landscape"     
       MENU-ITEM m_Single2      LABEL "Portrait"      .

DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Sort         LABEL "Sort"          
       RULE
       MENU-ITEM m_Find         LABEL "Find"          
       MENU-ITEM m_Zoom         LABEL "Zoom"          
       MENU-ITEM m_Hide         LABEL "Hide"          
       SUB-MENU  m_Spreadsheet  LABEL "Excel"         
       RULE
       MENU-ITEM m_Printer_Setup LABEL "Printer Setup..."
       SUB-MENU  m_Print        LABEL "Print"         
       MENU-ITEM m_Dump         LABEL "Dump"          
       RULE
       MENU-ITEM m_Clear        LABEL "Clear"         
       MENU-ITEM m_Exit         LABEL "Exit"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bCancelAdvFilter 
     LABEL "Cancel" 
     SIZE 9.6 BY .95.

DEFINE BUTTON bClearAdvFilter AUTO-GO 
     LABEL "Clear" 
     SIZE 10 BY .95 TOOLTIP "Choose to completely clear the where clause".

DEFINE BUTTON bOKAdvFilter 
     LABEL "OK" 
     SIZE 9.6 BY .95.

DEFINE VARIABLE eWhere AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 61.4 BY 5.52 NO-UNDO.

DEFINE VARIABLE fiEach AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 62 BY 1.05 NO-UNDO.

DEFINE BUTTON bCancelBasicFilter 
     LABEL "Cancel" 
     SIZE 12 BY .95.

DEFINE BUTTON bClearFilter AUTO-GO 
     LABEL "Clear" 
     SIZE 12 BY .95 TOOLTIP "Choose to clear all previously selected filter values".

DEFINE BUTTON bEdit 
     LABEL "Advanced" 
     SIZE 12 BY .95 TOOLTIP "Choose to open the Advanced WhereClause editor (requires programming)".

DEFINE BUTTON bOKBasicFilter 
     LABEL "OK" 
     SIZE 12 BY .95.

DEFINE VARIABLE cbAnd1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cOperator1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "=",">",">=","<","<=","<>","Rng" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cOperator2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "=",">",">=","<","<=","<>","Rng" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cOperator3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "=",">",">=","<","<=","<>","Rng" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cOperator4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "=",">",">=","<","<=","<>","Rng" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cOperator5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "=",">",">=","<","<=","<>","Rng" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cOperator6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "=",">",">=","<","<=","<>","Rng" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere1Value AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere1Value2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere2Value AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere2Value2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere3Value AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere3Value2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere4Value AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere4Value2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere5Value AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere5Value2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere6Value AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE cWhere6Value2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiDoThis AS CHARACTER FORMAT "X(256)":U INITIAL "Find all **<table>** records using:" 
      VIEW-AS TEXT 
     SIZE 44 BY 1.05 NO-UNDO.

DEFINE VARIABLE fiFrom AS CHARACTER FORMAT "X(256)":U INITIAL "Range:" 
      VIEW-AS TEXT 
     SIZE 7 BY 1.05 NO-UNDO.

DEFINE VARIABLE tFilterAll AS LOGICAL INITIAL no 
     LABEL "All Fields" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1.05 TOOLTIP "Use ALL fields or use only key (fast) fields" NO-UNDO.

DEFINE BUTTON bCancelHide 
     LABEL "Cancel" 
     SIZE 9.6 BY .95.

DEFINE BUTTON bClearHide AUTO-GO 
     LABEL "Clear" 
     SIZE 10 BY .95 TOOLTIP "Choose to re-displayALL fields in the browse".

DEFINE BUTTON bOKHide 
     LABEL "OK" 
     SIZE 9.6 BY .95.

DEFINE VARIABLE slHide AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 6.57 TOOLTIP "Select the columns to HIDEin the browse display" NO-UNDO.

DEFINE BUTTON bCancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 9 BY .95 TOOLTIP "Click to exitthis function".

DEFINE BUTTON bChangeFilter 
     IMAGE-UP FILE "img\filter.bmp":U NO-FOCUS
     LABEL "Change" 
     SIZE 4 BY 1.05 TOOLTIP "Change Filter".

DEFINE BUTTON bChangeHide 
     IMAGE-UP FILE "img/hide.bmp":U NO-FOCUS
     LABEL "Change" 
     SIZE 4 BY 1.05 TOOLTIP "Change Hidden Columns".

DEFINE BUTTON bChangeSort 
     IMAGE-UP FILE "img/sort.bmp":U NO-FOCUS
     LABEL "&Sort" 
     SIZE 4 BY 1.05 TOOLTIP "Change Sort Order".

DEFINE BUTTON bClear AUTO-GO 
     LABEL "&Clear All" 
     SIZE 10 BY .95 TOOLTIP "Click to remove allfilter/find criteria anddisplay all columns".

DEFINE BUTTON bZoom AUTO-GO 
     LABEL "&Zoom" 
     SIZE 8 BY .95 TOOLTIP "Click to display alist of 'zoomable' files".

DEFINE VARIABLE cbFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "File" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "Select the file to browse" NO-UNDO.

DEFINE VARIABLE cbLockCol AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Lock Columns" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "0","1","2","3","4","5","6","7","8","9" 
     DROP-DOWN-LIST
     SIZE 6 BY 1 TOOLTIP "Select the number of columnsto 'lock' (prevent from scrolling)" NO-UNDO.

DEFINE VARIABLE cbSortList AS CHARACTER FORMAT "X(256)":U INITIAL "by (default)" 
     LABEL "Sort" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "by (default)" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "Click the Sort (A/Z) button to change the sort order" NO-UNDO.

DEFINE VARIABLE fiFiltered AS CHARACTER FORMAT "X(256)":U INITIAL "NOT filtered" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13 BY 1.05 TOOLTIP "Click the Filter (funnel) button to change the filter/find criteria" NO-UNDO.

DEFINE VARIABLE fiHidden AS CHARACTER FORMAT "X(256)":U INITIAL "0 hidden" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12 BY 1.05 TOOLTIP "Click the Hide (stopsign) button to change the hidden columns" NO-UNDO.

DEFINE BUTTON bCancelSort 
     LABEL "Cancel" 
     SIZE 9.6 BY .95.

DEFINE BUTTON bOKSort 
     LABEL "OK" 
     SIZE 9.6 BY .95.

DEFINE VARIABLE cbSortBy AS CHARACTER FORMAT "X(256)":U INITIAL "(none)" 
     LABEL "Sort By" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "(none)" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Select the first (primary) sort field, or '(none)'" NO-UNDO.

DEFINE VARIABLE cbSortBy2 AS CHARACTER FORMAT "X(256)":U INITIAL "(none)" 
     LABEL "Then By" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "(none)" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Select the second sort field, or '(none)'" NO-UNDO.

DEFINE VARIABLE cbSortBy3 AS CHARACTER FORMAT "X(256)":U INITIAL "(none)" 
     LABEL "Then By" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "(none)" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Select the third sort field, or '(none)'" NO-UNDO.

DEFINE VARIABLE cbSortBy4 AS CHARACTER FORMAT "X(256)":U INITIAL "(none)" 
     LABEL "Then By" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "(none)" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Select the fourth sort field, or '(none)'" NO-UNDO.

DEFINE VARIABLE cbSortBy5 AS CHARACTER FORMAT "X(256)":U INITIAL "(none)" 
     LABEL "Then By" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "(none)" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Select the fifth sort field, or '(none)'" NO-UNDO.

DEFINE VARIABLE tSortAll AS LOGICAL INITIAL no 
     LABEL "All Fields" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .91 TOOLTIP "Use ALL fields or use only key (fast) fields" NO-UNDO.

DEFINE BUTTON bCancelZoom 
     LABEL "Cancel" 
     SIZE 9.6 BY .95.

DEFINE BUTTON bOKZoom 
     LABEL "OK" 
     SIZE 9.6 BY .95.

DEFINE VARIABLE slZoom AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 36 BY 7.62 TOOLTIP "Select a file to jump to,then press OK to execute" NO-UNDO.

DEFINE VARIABLE tUseFilter AS LOGICAL INITIAL no 
     LABEL "Use Filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .91 TOOLTIP "Check to use current filtervalues in Zoomed browse" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     bChangeSort AT ROW 1 COL 60.2
     cbFileName AT ROW 1 COL 4 COLON-ALIGNED
     cbSortList AT ROW 1 COL 34 COLON-ALIGNED
     fiFiltered AT ROW 1 COL 64 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiHidden AT ROW 1 COL 85 NO-LABEL NO-TAB-STOP 
     cbLockCol AT ROW 1 COL 115 COLON-ALIGNED
     bZoom AT ROW 1 COL 124
     bClear AT ROW 1 COL 133
     bCancel AT ROW 1 COL 144
     bChangeFilter AT ROW 1 COL 79
     bChangeHide AT ROW 1 COL 97
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152.4 BY 22.32.

DEFINE FRAME fZoom
     slZoom AT ROW 1 COL 1 NO-LABEL
     tUseFilter AT ROW 8.91 COL 2
     bOKZoom AT ROW 8.91 COL 16
     bCancelZoom AT ROW 8.91 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 104 ROW 12.84
         SIZE 37 BY 10.26
         TITLE "Zoom To File".

DEFINE FRAME fBasicFilter
     cbWhere1 AT ROW 2.1 COL 1 COLON-ALIGNED NO-LABEL
     cOperator1 AT ROW 2.05 COL 25 COLON-ALIGNED NO-LABEL
     cWhere1Value AT ROW 2.05 COL 35 COLON-ALIGNED NO-LABEL
     cWhere1Value2 AT ROW 2.05 COL 55 COLON-ALIGNED NO-LABEL
     cbAnd1 AT ROW 2.05 COL 76 NO-LABEL
     cbWhere2 AT ROW 3.38 COL 1 COLON-ALIGNED NO-LABEL
     cOperator2 AT ROW 3.38 COL 25 COLON-ALIGNED NO-LABEL
     cWhere2Value AT ROW 3.38 COL 35 COLON-ALIGNED NO-LABEL
     cWhere2Value2 AT ROW 3.38 COL 55 COLON-ALIGNED NO-LABEL
     cbAnd2 AT ROW 3.38 COL 76 NO-LABEL
     cbWhere3 AT ROW 4.67 COL 1 COLON-ALIGNED NO-LABEL
     cOperator3 AT ROW 4.67 COL 25 COLON-ALIGNED NO-LABEL
     cWhere3Value AT ROW 4.67 COL 35 COLON-ALIGNED NO-LABEL
     cWhere3Value2 AT ROW 4.67 COL 55 COLON-ALIGNED NO-LABEL
     cbAnd3 AT ROW 4.67 COL 76 NO-LABEL
     cbWhere4 AT ROW 6 COL 1 COLON-ALIGNED NO-LABEL
     cOperator4 AT ROW 6 COL 25 COLON-ALIGNED NO-LABEL
     cWhere4Value AT ROW 6 COL 35 COLON-ALIGNED NO-LABEL
     cWhere4Value2 AT ROW 6 COL 55 COLON-ALIGNED NO-LABEL
     cbAnd4 AT ROW 6 COL 76 NO-LABEL
     cbWhere5 AT ROW 7.33 COL 1 COLON-ALIGNED NO-LABEL
     cOperator5 AT ROW 7.33 COL 25 COLON-ALIGNED NO-LABEL
     cWhere5Value AT ROW 7.33 COL 35 COLON-ALIGNED NO-LABEL
     cWhere5Value2 AT ROW 7.33 COL 55 COLON-ALIGNED NO-LABEL
     cbAnd5 AT ROW 7.33 COL 76 NO-LABEL
     cbWhere6 AT ROW 8.62 COL 1 COLON-ALIGNED NO-LABEL
     cOperator6 AT ROW 8.62 COL 25 COLON-ALIGNED NO-LABEL
     cWhere6Value AT ROW 8.62 COL 35 COLON-ALIGNED NO-LABEL
     cWhere6Value2 AT ROW 8.62 COL 55 COLON-ALIGNED NO-LABEL
     bEdit AT ROW 4.67 COL 88
     bClearFilter AT ROW 6 COL 88
     bOKBasicFilter AT ROW 7.33 COL 88
     bCancelBasicFilter AT ROW 8.62 COL 88
     tFilterAll AT ROW 1 COL 88
     fiFrom AT ROW 1 COL 51 COLON-ALIGNED NO-LABEL
     fiDoThis AT ROW 1 COL 2 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 12.84
         SIZE 101 BY 10.26
         TITLE "Basic Filter".

DEFINE FRAME fHide
     slHide AT ROW 1 COL 1 NO-LABEL
     bClearHide AT ROW 7.86 COL 3
     bOKHide AT ROW 7.86 COL 15
     bCancelHide AT ROW 7.86 COL 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 104 ROW 3.37
         SIZE 37 BY 9.21
         TITLE "Hide Columns".

DEFINE FRAME fSort
     cbSortBy AT ROW 1.24 COL 9 COLON-ALIGNED
     cbSortBy2 AT ROW 2.57 COL 9 COLON-ALIGNED
     cbSortBy3 AT ROW 3.86 COL 9 COLON-ALIGNED
     cbSortBy4 AT ROW 5.14 COL 9 COLON-ALIGNED
     cbSortBy5 AT ROW 6.48 COL 9 COLON-ALIGNED
     tSortAll AT ROW 7.86 COL 2
     bOKSort AT ROW 7.86 COL 15
     bCancelSort AT ROW 7.86 COL 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.37
         SIZE 37 BY 9.21
         TITLE "Sort By".

DEFINE FRAME fAdvFilter
     fiEach AT ROW 1 COL 1 NO-LABEL
     eWhere AT ROW 2.05 COL 1.6 NO-LABEL
     bClearAdvFilter AT ROW 7.86 COL 30
     bOKAdvFilter AT ROW 7.86 COL 42
     bCancelAdvFilter AT ROW 7.86 COL 53
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 40 ROW 3.37
         SIZE 63 BY 9.21
         TITLE "Advanced Filter Editor".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "MXP Dynamic Browser"
         HEIGHT             = 22.33
         WIDTH              = 152.4
         MAX-HEIGHT         = 51.76
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 51.76
         VIRTUAL-WIDTH      = 256
         ALWAYS-ON-TOP      = yes
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("adeicon/rbuild%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/rbuild%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME fAdvFilter:FRAME = FRAME fMain:HANDLE
       FRAME fBasicFilter:FRAME = FRAME fMain:HANDLE
       FRAME fHide:FRAME = FRAME fMain:HANDLE
       FRAME fSort:FRAME = FRAME fMain:HANDLE
       FRAME fZoom:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fAdvFilter
   NOT-VISIBLE                                                          */
ASSIGN 
       bOKAdvFilter:HIDDEN IN FRAME fAdvFilter           = TRUE.

/* SETTINGS FOR FILL-IN fiEach IN FRAME fAdvFilter
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME fBasicFilter
   Custom                                                               */
ASSIGN 
       bOKBasicFilter:HIDDEN IN FRAME fBasicFilter           = TRUE.

/* SETTINGS FOR COMBO-BOX cbAnd1 IN FRAME fBasicFilter
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cbAnd2 IN FRAME fBasicFilter
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cbAnd3 IN FRAME fBasicFilter
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cbAnd4 IN FRAME fBasicFilter
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cbAnd5 IN FRAME fBasicFilter
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiDoThis IN FRAME fBasicFilter
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiFrom IN FRAME fBasicFilter
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fHide
                                                                        */
ASSIGN 
       bOKHide:HIDDEN IN FRAME fHide           = TRUE.

/* SETTINGS FOR FRAME fMain
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fSort:MOVE-AFTER-TAB-ITEM (bCancel:HANDLE IN FRAME fMain)
       XXTABVALXX = FRAME fBasicFilter:MOVE-BEFORE-TAB-ITEM (FRAME fZoom:HANDLE)
       XXTABVALXX = FRAME fHide:MOVE-BEFORE-TAB-ITEM (FRAME fBasicFilter:HANDLE)
       XXTABVALXX = FRAME fAdvFilter:MOVE-BEFORE-TAB-ITEM (FRAME fHide:HANDLE)
       XXTABVALXX = FRAME fSort:MOVE-BEFORE-TAB-ITEM (FRAME fAdvFilter:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

/* SETTINGS FOR FILL-IN fiFiltered IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHidden IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME fSort
                                                                        */
ASSIGN 
       bOKSort:HIDDEN IN FRAME fSort           = TRUE.

/* SETTINGS FOR FRAME fZoom
                                                                        */
ASSIGN 
       bOKZoom:HIDDEN IN FRAME fZoom           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* MXP Dynamic Browser */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* MXP Dynamic Browser */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* MXP Dynamic Browser */
DO:
  ASSIGN
    FRAME fMain:VISIBLE = FALSE.
    
  IF VALID-HANDLE(hDynBrowse) THEN ASSIGN
    hDynBrowse:VISIBLE = FALSE.
    
  ASSIGN
    wWin:WIDTH = MAXIMUM(wWin:WIDTH,152.4)
    wWin:HEIGHT = MAXIMUM(wWin:HEIGHT,22.32)
    FRAME fMain:WIDTH = wWin:WIDTH
    FRAME fMain:HEIGHT = wWin:HEIGHT - 0.03
    FRAME fMain:SCROLLABLE = FALSE.
    
  IF VALID-HANDLE(hDynBrowse) THEN ASSIGN
    hDynBrowse:WIDTH = FRAME fMain:WIDTH
    hDynBrowse:HEIGHT = FRAME fMain:HEIGHT - 1.6.
  
  ASSIGN
    FRAME fMain:VISIBLE = TRUE.
    
  IF VALID-HANDLE(hDynBrowse) THEN ASSIGN
    hDynBrowse:VISIBLE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain wWin
ON ALT-F OF FRAME fMain
ANYWHERE
DO:
  APPLY 'choose' TO bChangeFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain wWin
ON ALT-H OF FRAME fMain
ANYWHERE
DO:
  APPLY 'choose' TO bChangeHide.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain wWin
ON ALT-S OF FRAME fMain
ANYWHERE
DO:
  APPLY 'choose' TO bChangeSort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain wWin
ON ALT-X OF FRAME fMain
ANYWHERE
DO:
  APPLY 'choose' TO bCancel IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain wWin
ON END-ERROR OF FRAME fMain
ANYWHERE
DO:
  IF FRAME fSort:VISIBLE = TRUE THEN DO:
    ASSIGN FRAME fSort:VISIBLE = FALSE.
    RETURN NO-APPLY.
  END.
  IF FRAME fAdvFilter:VISIBLE = TRUE THEN DO:
    ASSIGN FRAME fAdvFilter:VISIBLE = FALSE.
    RETURN NO-APPLY.
  END.
  IF FRAME fHide:VISIBLE = TRUE THEN DO:
    ASSIGN FRAME fHide:VISIBLE = FALSE.
    RETURN NO-APPLY.
  END.
  IF FRAME fZoom:VISIBLE = TRUE THEN DO:
    ASSIGN FRAME fZoom:VISIBLE = FALSE.
    RETURN NO-APPLY.
  END.
  IF FRAME fBasicFilter:VISIBLE = TRUE THEN DO: 
    ASSIGN FRAME fBasicFilter:VISIBLE = FALSE.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancel wWin
ON CHOOSE OF bCancel IN FRAME fMain /* Cancel */
DO:
  APPLY 'window-close' TO wWin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fAdvFilter
&Scoped-define SELF-NAME bCancelAdvFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancelAdvFilter wWin
ON CHOOSE OF bCancelAdvFilter IN FRAME fAdvFilter /* Cancel */
DO:
    FRAME fAdvFilter:VISIBLE = FALSE.

    IF cWhereClause <> "" THEN ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = "FILTERED".
    ELSE ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = "NOT filtered".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME bCancelBasicFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancelBasicFilter wWin
ON CHOOSE OF bCancelBasicFilter IN FRAME fBasicFilter /* Cancel */
DO:
    FRAME fBasicFilter:VISIBLE = FALSE.

    IF cWhereClause <> "" THEN ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = "FILTERED".
    ELSE ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = "NOT filtered".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fHide
&Scoped-define SELF-NAME bCancelHide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancelHide wWin
ON CHOOSE OF bCancelHide IN FRAME fHide /* Cancel */
DO:
    FRAME fHide:VISIBLE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fSort
&Scoped-define SELF-NAME bCancelSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancelSort wWin
ON CHOOSE OF bCancelSort IN FRAME fSort /* Cancel */
DO:
    FRAME fSort:VISIBLE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fZoom
&Scoped-define SELF-NAME bCancelZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancelZoom wWin
ON CHOOSE OF bCancelZoom IN FRAME fZoom /* Cancel */
DO:
    FRAME fZoom:VISIBLE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME bChangeFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bChangeFilter wWin
ON CHOOSE OF bChangeFilter IN FRAME fMain /* Change */
DO:
  ASSIGN
    FRAME fBasicFilter:ROW = (wWin:HEIGHT / 2) - 5.13
    FRAME fBasicFilter:COLUMN = (wWin:WIDTH / 2) - 50.5
    FRAME fBasicFilter:VISIBLE = TRUE.
    FRAME fBasicFilter:MOVE-TO-TOP().
    
  ASSIGN
    fiDoThis:SCREEN-VALUE = "Find all **" + UPPER(cbFileName:SCREEN-VALUE IN FRAME fMain) +
                            "** records using:".
    
  APPLY 'value-changed' TO cbAnd1 IN FRAME fBasicFilter.
  APPLY 'value-changed' TO cOperator1 IN FRAME fBasicFilter.
  APPLY 'entry' TO cbWhere1 IN FRAME fBasicFilter.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bChangeHide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bChangeHide wWin
ON CHOOSE OF bChangeHide IN FRAME fMain /* Change */
DO:
  ASSIGN
    FRAME fHide:ROW = (wWin:HEIGHT / 2) - 4.61
    FRAME fHide:COLUMN = (wWin:WIDTH / 2) - 18.5
    FRAME fHide:VISIBLE = TRUE.
    FRAME fHide:MOVE-TO-TOP().
    
  APPLY 'entry' TO slHide IN FRAME fHide.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bChangeSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bChangeSort wWin
ON CHOOSE OF bChangeSort IN FRAME fMain /* Sort */
DO:
    ASSIGN
        FRAME fSort:ROW = (wWin:HEIGHT / 2) - 4.61
        FRAME fSort:COLUMN = (wWin:WIDTH / 2) - 18.5
        FRAME fSort:VISIBLE = TRUE.
        FRAME fSort:MOVE-TO-TOP().
    
    APPLY 'entry' TO cbSortBy IN FRAME fSort.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bClear wWin
ON CHOOSE OF bClear IN FRAME fMain /* Clear All */
DO:
  APPLY 'value-changed' TO cbFileName IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fAdvFilter
&Scoped-define SELF-NAME bClearAdvFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bClearAdvFilter wWin
ON CHOOSE OF bClearAdvFilter IN FRAME fAdvFilter /* Clear */
DO:
    ASSIGN 
        eWhere:SCREEN-VALUE IN FRAME fAdvFilter = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME bClearFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bClearFilter wWin
ON CHOOSE OF bClearFilter IN FRAME fBasicFilter /* Clear */
DO:
    ASSIGN 
        cbWhere1:SCREEN-VALUE = ""
        cbWhere2:SCREEN-VALUE = ""
        cbWhere3:SCREEN-VALUE = ""
        cbWhere4:SCREEN-VALUE = ""
        cbWhere5:SCREEN-VALUE = ""
        cbWhere6:SCREEN-VALUE = ""
        cOperator1:SCREEN-VALUE = ""
        cOperator2:SCREEN-VALUE = ""
        cOperator3:SCREEN-VALUE = ""
        cOperator4:SCREEN-VALUE = ""
        cOperator5:SCREEN-VALUE = ""
        cOperator6:SCREEN-VALUE = ""
        cWhere1Value:SCREEN-VALUE = ""
        cWhere2Value:SCREEN-VALUE = ""
        cWhere3Value:SCREEN-VALUE = ""
        cWhere4Value:SCREEN-VALUE = ""
        cWhere5Value:SCREEN-VALUE = ""
        cWhere6Value:SCREEN-VALUE = ""
        cWhere1Value2:SCREEN-VALUE = ""
        cWhere2Value2:SCREEN-VALUE = ""
        cWhere3Value2:SCREEN-VALUE = ""
        cWhere4Value2:SCREEN-VALUE = ""
        cWhere5Value2:SCREEN-VALUE = ""
        cWhere6Value2:SCREEN-VALUE = ""
        cbAnd1:SCREEN-VALUE = ""
        cbAnd2:SCREEN-VALUE = ""
        cbAnd3:SCREEN-VALUE = ""
        cbAnd4:SCREEN-VALUE = ""
        cbAnd5:SCREEN-VALUE = ""
        .
    
    FRAME fAdvFilter:VISIBLE = FALSE.
    
    RUN ipApplyFilter IN THIS-PROCEDURE.    
    
    IF cWhereClause <> "" THEN ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = " FILTERED".
    ELSE ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = "NOT filtered".
        
    ENABLE 
        cbFileName
    WITH FRAME fMain.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fHide
&Scoped-define SELF-NAME bClearHide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bClearHide wWin
ON CHOOSE OF bClearHide IN FRAME fHide /* Clear */
DO:
  slHide:LIST-ITEMS IN FRAME fHide = "".
    
    FIND FIRST _file WHERE _file._file-name = hBuffer:TABLE NO-LOCK NO-ERROR.
    FOR EACH _field OF _file:
        IF slHide:LOOKUP(_field._field-name) <> ? and
        slHide:LOOKUP(_field._field-name) = 0 THEN slHide:ADD-LAST(_field._field-name).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME bEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bEdit wWin
ON CHOOSE OF bEdit IN FRAME fBasicFilter /* Advanced */
DO:
  ASSIGN
    FRAME fBasicFilter:VISIBLE = FALSE
    FRAME fAdvFilter:ROW = (wWin:HEIGHT / 2) - 4.61
    FRAME fAdvFilter:COLUMN = (wWin:WIDTH / 2) - 31.5
    FRAME fAdvFilter:VISIBLE = TRUE.
    FRAME fAdvFilter:MOVE-TO-TOP().
    
  ASSIGN
    fiEach:SCREEN-VALUE IN FRAME fAdvFilter = " FOR EACH " + cbFileName:SCREEN-VALUE IN FRAME fMain +
                                                " WHERE"
    eWhere:SCREEN-VALUE IN FRAME fAdvFilter = cWhereClause.
                                                
  APPLY 'entry' TO eWhere IN FRAME fAdvFilter.
                                                
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fAdvFilter
&Scoped-define SELF-NAME bOKAdvFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOKAdvFilter wWin
ON CHOOSE OF bOKAdvFilter IN FRAME fAdvFilter /* OK */
DO:
    FRAME fAdvFilter:VISIBLE = FALSE.
    
    RUN ipCreateFilter IN THIS-PROCEDURE (INPUT "Advanced").
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME bOKBasicFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOKBasicFilter wWin
ON CHOOSE OF bOKBasicFilter IN FRAME fBasicFilter /* OK */
DO:
    FRAME fBasicFilter:VISIBLE = FALSE.

    RUN ipCreateFilter IN THIS-PROCEDURE (INPUT "Basic").
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fHide
&Scoped-define SELF-NAME bOKHide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOKHide wWin
ON CHOOSE OF bOKHide IN FRAME fHide /* OK */
DO:
    FRAME fHide:VISIBLE = FALSE.
    
    ASSIGN
        iCtr = IF NUM-ENTRIES(slHide:SCREEN-VALUE IN FRAME fHide) <> ? THEN
                NUM-ENTRIES(slHide:SCREEN-VALUE IN FRAME fHide) ELSE 0
        fiHidden:SCREEN-VALUE IN FRAME fMain = STRING(iCtr) + " hidden".
    
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fSort
&Scoped-define SELF-NAME bOKSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOKSort wWin
ON CHOOSE OF bOKSort IN FRAME fSort /* OK */
DO:
    FRAME fSort:VISIBLE = FALSE.
    
    RUN ipCreateSort IN THIS-PROCEDURE.
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fZoom
&Scoped-define SELF-NAME bOKZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOKZoom wWin
ON CHOOSE OF bOKZoom IN FRAME fZoom /* OK */
OR 'mouse-select-dblclick' OF slZoom
DO:
    ASSIGN cbFileName:SCREEN-VALUE IN FRAME fMain = slZoom:SCREEN-VALUE.
    
    FRAME fZoom:VISIBLE = FALSE.
    
    APPLY 'value-changed' TO cbFileName IN FRAME fMain.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME bZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bZoom wWin
ON CHOOSE OF bZoom IN FRAME fMain /* Zoom */
DO:
  ASSIGN
    FRAME fZoom:ROW = (wWin:HEIGHT / 2) - 5.13
    FRAME fZoom:COLUMN = (wWin:WIDTH / 2) - 18.5
    FRAME fZoom:VISIBLE = TRUE.
    FRAME fZoom:MOVE-TO-TOP().
    tUseFilter:CHECKED IN FRAME fZoom = FALSE.

    APPLY 'entry' TO slZoom IN FRAME fZoom.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME cbAnd1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd1 wWin
ON VALUE-CHANGED OF cbAnd1 IN FRAME fBasicFilter
DO:
  IF SELF:SCREEN-VALUE = "AND" OR
     SELF:SCREEN-VALUE = "OR" THEN ASSIGN
        cbWhere2:VISIBLE = TRUE
        cOperator2:VISIBLE = TRUE
        cWhere2Value:VISIBLE = TRUE
        cbAnd2:VISIBLE = TRUE.
   ELSE ASSIGN
        cbWhere2:VISIBLE = FALSE
        cOperator2:VISIBLE = FALSE
        cWhere2Value:VISIBLE = FALSE
        cbAnd2:VISIBLE = FALSE.
        
   APPLY 'value-changed' TO cOperator2.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd2 wWin
ON VALUE-CHANGED OF cbAnd2 IN FRAME fBasicFilter
DO:
  IF SELF:SCREEN-VALUE = "AND" OR
     SELF:SCREEN-VALUE = "OR" THEN ASSIGN
        cbWhere3:VISIBLE = TRUE
        cOperator3:VISIBLE = TRUE
        cWhere3Value:VISIBLE = TRUE
        cbAnd3:VISIBLE = TRUE.
   ELSE ASSIGN
        cbWhere3:VISIBLE = FALSE
        cOperator3:VISIBLE = FALSE
        cWhere3Value:VISIBLE = FALSE
        cbAnd3:VISIBLE = FALSE.
        
   APPLY 'value-changed' TO cOperator3.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd3 wWin
ON VALUE-CHANGED OF cbAnd3 IN FRAME fBasicFilter
DO:
  IF SELF:SCREEN-VALUE = "AND" OR
     SELF:SCREEN-VALUE = "OR" THEN ASSIGN
        cbWhere4:VISIBLE = TRUE
        cOperator4:VISIBLE = TRUE
        cWhere4Value:VISIBLE = TRUE
        cbAnd4:VISIBLE = TRUE.
   ELSE ASSIGN
        cbWhere4:VISIBLE = FALSE
        cOperator4:VISIBLE = FALSE
        cWhere4Value:VISIBLE = FALSE
        cbAnd4:VISIBLE = FALSE.
        
   APPLY 'value-changed' TO cOperator4.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd4 wWin
ON VALUE-CHANGED OF cbAnd4 IN FRAME fBasicFilter
DO:
  IF SELF:SCREEN-VALUE = "AND" OR
     SELF:SCREEN-VALUE = "OR" THEN ASSIGN
        cbWhere5:VISIBLE = TRUE
        cOperator5:VISIBLE = TRUE
        cWhere5Value:VISIBLE = TRUE
        cbAnd5:VISIBLE = TRUE.
   ELSE ASSIGN
        cbWhere5:VISIBLE = FALSE
        cOperator5:VISIBLE = FALSE
        cWhere5Value:VISIBLE = FALSE
        cbAnd5:VISIBLE = FALSE.
        
   APPLY 'value-changed' TO cOperator5.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd5 wWin
ON VALUE-CHANGED OF cbAnd5 IN FRAME fBasicFilter
DO:
  IF SELF:SCREEN-VALUE = "AND" OR
     SELF:SCREEN-VALUE = "OR" THEN ASSIGN
        cbWhere6:VISIBLE = TRUE
        cOperator6:VISIBLE = TRUE
        cWhere6Value:VISIBLE = TRUE.
   ELSE ASSIGN
        cbWhere6:VISIBLE = FALSE
        cOperator6:VISIBLE = FALSE
        cWhere6Value:VISIBLE = FALSE.
        
   APPLY 'value-changed' TO cOperator6.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME cbFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFileName wWin
ON VALUE-CHANGED OF cbFileName IN FRAME fMain /* File */
DO:
    
    ASSIGN
        cbSortBy:LIST-ITEMS IN FRAME fSort = "(none)"
        cbSortBy2:LIST-ITEMS IN FRAME fSort = "(none)"
        cbSortBy3:LIST-ITEMS IN FRAME fSort = "(none)"
        cbSortBy4:LIST-ITEMS IN FRAME fSort = "(none)"
        cbSortBy5:LIST-ITEMS IN FRAME fSort = "(none)"
        cbSortList:LIST-ITEMS IN FRAME fMain = "(default)"
        cbSortBy:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy2:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy3:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy4:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy5:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortList:SCREEN-VALUE IN FRAME fMain = "(default)"
        cbWhere1:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbWhere2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbWhere3:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbWhere4:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbWhere5:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbWhere6:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cOperator1:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cOperator2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cOperator3:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cOperator4:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cOperator5:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cOperator6:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere1Value:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere2Value:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere3Value:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere4Value:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere5Value:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere6Value:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere1Value2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere2Value2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere3Value2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere4Value2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere5Value2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cWhere6Value2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbAnd1:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbAnd2:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbAnd3:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbAnd4:SCREEN-VALUE IN FRAME fBasicFilter = ""
        cbAnd5:SCREEN-VALUE IN FRAME fBasicFilter = ""
        slHide:LIST-ITEMS IN FRAME fHide = ""
        cColList = ""
        cWhereClause = ""
        cSortByPhrase = "".

    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLockCol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLockCol wWin
ON VALUE-CHANGED OF cbLockCol IN FRAME fMain /* Lock Columns */
DO:
  ASSIGN
    hDynBrowse:NUM-LOCKED-COLUMNS = INTEGER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fSort
&Scoped-define SELF-NAME cbSortBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSortBy wWin
ON VALUE-CHANGED OF cbSortBy IN FRAME fSort /* Sort By */
DO:
  IF SELF:SCREEN-VALUE = "(none)" THEN ASSIGN
    cbSortBy2:SCREEN-VALUE = "(none)"
    cbSortBy3:SCREEN-VALUE = "(none)"
    cbSortBy4:SCREEN-VALUE = "(none)"
    cbSortBy5:SCREEN-VALUE = "(none)".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSortBy2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSortBy2 wWin
ON VALUE-CHANGED OF cbSortBy2 IN FRAME fSort /* Then By */
DO:
  IF SELF:SCREEN-VALUE = "(none)" THEN ASSIGN
    cbSortBy3:SCREEN-VALUE = "(none)"
    cbSortBy4:SCREEN-VALUE = "(none)"
    cbSortBy5:SCREEN-VALUE = "(none)".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSortBy3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSortBy3 wWin
ON VALUE-CHANGED OF cbSortBy3 IN FRAME fSort /* Then By */
DO:
  IF SELF:SCREEN-VALUE = "(none)" THEN ASSIGN
    cbSortBy4:SCREEN-VALUE = "(none)"
    cbSortBy5:SCREEN-VALUE = "(none)".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSortBy4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSortBy4 wWin
ON VALUE-CHANGED OF cbSortBy4 IN FRAME fSort /* Then By */
DO:
  IF SELF:SCREEN-VALUE = "(none)" THEN ASSIGN
    cbSortBy5:SCREEN-VALUE = "(none)".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME cOperator1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cOperator1 wWin
ON VALUE-CHANGED OF cOperator1 IN FRAME fBasicFilter
OR 'value-changed' OF cOperator2
OR 'value-changed' OF cOperator3
OR 'value-changed' OF cOperator4
OR 'value-changed' OF cOperator5
OR 'value-changed' OF cOperator6
DO:
    IF cOperator1:SCREEN-VALUE = "Rng"
    OR cOperator2:SCREEN-VALUE = "Rng"
    OR cOperator3:SCREEN-VALUE = "Rng"
    OR cOperator4:SCREEN-VALUE = "Rng"
    OR cOperator5:SCREEN-VALUE = "Rng"
    OR cOperator6:SCREEN-VALUE = "Rng"
    THEN ASSIGN
        fiFrom:VISIBLE = TRUE.
    ELSE ASSIGN
        fiFrom:VISIBLE = FALSE.
        
    ASSIGN
        cWhere1Value2:VISIBLE = (cWhere1Value:VISIBLE = TRUE AND cOperator1:SCREEN-VALUE = "Rng")
        cWhere2Value2:VISIBLE = (cWhere2Value:VISIBLE = TRUE AND cOperator2:SCREEN-VALUE = "Rng")
        cWhere3Value2:VISIBLE = (cWhere3Value:VISIBLE = TRUE AND cOperator3:SCREEN-VALUE = "Rng")
        cWhere4Value2:VISIBLE = (cWhere4Value:VISIBLE = TRUE AND cOperator4:SCREEN-VALUE = "Rng")
        cWhere5Value2:VISIBLE = (cWhere5Value:VISIBLE = TRUE AND cOperator5:SCREEN-VALUE = "Rng")
        cWhere6Value2:VISIBLE = (cWhere6Value:VISIBLE = TRUE AND cOperator6:SCREEN-VALUE = "Rng").
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cWhere1Value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cWhere1Value wWin
ON RETURN OF cWhere1Value IN FRAME fBasicFilter
OR RETURN OF cWhere2Value
OR RETURN OF cWhere3Value
OR RETURN OF cWhere4Value
OR RETURN OF cWhere5Value
OR RETURN OF cWhere6Value
DO:
  APPLY 'choose' TO bOKBasicFilter IN FRAME fBasicFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cWhere1Value2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cWhere1Value2 wWin
ON RETURN OF cWhere1Value2 IN FRAME fBasicFilter
OR RETURN OF cWhere2Value
OR RETURN OF cWhere3Value
OR RETURN OF cWhere4Value
DO:
  APPLY 'choose' TO bOKBasicFilter IN FRAME fBasicFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fAdvFilter
&Scoped-define SELF-NAME eWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eWhere wWin
ON RETURN OF eWhere IN FRAME fAdvFilter
DO:
  APPLY 'choose' TO bOKAdvFilter IN FRAME fAdvFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Clear wWin
ON CHOOSE OF MENU-ITEM m_Clear /* Clear */
DO:
  hDynBrowse:DESELECT-ROWS().
  slHide:LIST-ITEMS IN FRAME fHide = "".
    
    FIND FIRST _file WHERE _file._file-name = hBuffer:TABLE NO-LOCK NO-ERROR.
    FOR EACH _field OF _file:
        IF slHide:LOOKUP(_field._field-name) <> ? and
        slHide:LOOKUP(_field._field-name) = 0 THEN slHide:ADD-LAST(_field._field-name).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Dump
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Dump wWin
ON CHOOSE OF MENU-ITEM m_Dump /* Dump */
DO:
  DEF VAR cDumpName AS CHAR NO-UNDO.
  DEF VAR iCtr3 AS INT NO-UNDO.
  DEF VAR iCtr2 AS INT NO-UNDO.
  DEF VAR hBF AS HANDLE NO-UNDO.
  
  FIND _file WHERE 
    _file._file-name = cbFileName:SCREEN-VALUE IN FRAME fMain
    NO-LOCK NO-ERROR.
    
  IF AVAIL _file THEN ASSIGN
    cDumpName = _file._dump-name + ".d".
    
  SYSTEM-DIALOG GET-FILE cDumpName
  TITLE "Choose Location for Dump File"
  FILTERS "Data Files (*.d)"  "*.d"
  CREATE-TEST-FILE
  USE-FILENAME
  DEFAULT-EXTENSION ".d".

  OUTPUT STREAM dumpstream TO VALUE(cDumpName) NO-ECHO NO-MAP NO-CONVERT.
  
  IF hDynBrowse:NUM-SELECTED-ROWS = 0 THEN hDynBrowse:SELECT-ALL().

  DO iCtr3 = 1 TO hDynBrowse:NUM-SELECTED-ROWS:
    hDynBrowse:FETCH-SELECTED-ROW(iCtr3).
    DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
        hBF = hBuffer:BUFFER-FIELD(iCtr2).
        IF hBF:DATA-TYPE = "character" 
        AND hBF:BUFFER-VALUE <> ? THEN PUT STREAM dumpstream UNFORMATTED '"'.
        PUT STREAM dumpstream UNFORMATTED hBF:BUFFER-VALUE.
        IF hBF:DATA-TYPE = "character" 
        AND hBF:BUFFER-VALUE <> ? THEN PUT STREAM dumpstream UNFORMATTED '"'.
        IF iCtr2 < hBuffer:NUM-FIELDS THEN PUT STREAM dumpstream UNFORMATTED ' '.
    END.
    PUT STREAM dumpstream SKIP.
  END.
  
  OUTPUT STREAM dumpstream CLOSE.
  
  MESSAGE "File: " + cbFileName:SCREEN-VALUE IN FRAME fMain +
          " dump to: " + cDumpName + " complete. (" + 
          STRING(hDynBrowse:NUM-SELECTED-ROWS) + " records dumped)"
          VIEW-AS ALERT-BOX.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit wWin
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY 'choose' TO bCancel IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Find wWin
ON CHOOSE OF MENU-ITEM m_Find /* Find */
DO:
  APPLY 'choose' TO bChangeFilter IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Hide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Hide wWin
ON CHOOSE OF MENU-ITEM m_Hide /* Hide */
DO:
  APPLY 'choose' TO bChangeHide IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Print wWin
ON CHOOSE OF MENU m_Print /* Print */
DO:
    RUN ipExcel (INPUT "Print").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Printer_Setup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Printer_Setup wWin
ON CHOOSE OF MENU-ITEM m_Printer_Setup /* Printer Setup... */
DO:
    SYSTEM-DIALOG PRINTER-SETUP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Range
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Range wWin
ON CHOOSE OF MENU-ITEM m_Range /* Landscape */
DO:
  RUN ipExcel (INPUT "Sheet", INPUT "Rows").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Range2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Range2 wWin
ON CHOOSE OF MENU-ITEM m_Range2 /* Landscape */
DO:
  RUN ipExcel IN THIS-PROCEDURE (INPUT "Print", INPUT "Rows").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Single
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Single wWin
ON CHOOSE OF MENU-ITEM m_Single /* Portrait */
DO:
  RUN ipExcel IN THIS-PROCEDURE (INPUT "Sheet", INPUT "Columns").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Single2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Single2 wWin
ON CHOOSE OF MENU-ITEM m_Single2 /* Portrait */
DO:
  RUN ipExcel IN THIS-PROCEDURE (INPUT "Print", INPUT "Columns").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sort wWin
ON CHOOSE OF MENU-ITEM m_Sort /* Sort */
DO:
  APPLY 'choose' TO bChangeSort IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Zoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Zoom wWin
ON CHOOSE OF MENU-ITEM m_Zoom /* Zoom */
DO:
  APPLY 'choose' TO bZoom IN FRAME fMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fBasicFilter
&Scoped-define SELF-NAME tFilterAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tFilterAll wWin
ON VALUE-CHANGED OF tFilterAll IN FRAME fBasicFilter /* All Fields */
DO:
    DEF VAR hField AS HANDLE NO-UNDO.
    
    IF SELF:CHECKED THEN DO:
        MESSAGE
            "Using this option can significantly slow the filter" SKIP
            "process, and any further operations taken on this" SKIP
            "browse.  Are you sure you want to use this option?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
            UPDATE lChoice AS LOGICAL.
        
        IF lChoice THEN DO:
            ASSIGN 
                cbWhere1:LIST-ITEMS IN FRAME fBasicFilter = ""
                cbWhere1:SCREEN-VALUE IN FRAME fBasicFilter = ?.
        
            DO iCtr = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN 
                    hField = hBuffer:BUFFER-FIELD(iCtr).
                IF (cbWhere1:LIST-ITEMS = ? OR NOT CAN-DO(cbWhere1:LIST-ITEMS,hField:NAME)) AND
                    hField:EXTENT < 2 THEN 
                    cbWhere1:ADD-LAST(hField:NAME).
            END.
        END.
    END.
    ELSE DO:
            ASSIGN 
                cbWhere1:LIST-ITEMS IN FRAME fBasicFilter = ""
                cbWhere1:SCREEN-VALUE IN FRAME fBasicFilter = ?.
            DO iCtr = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN 
                    hField = hBuffer:BUFFER-FIELD(iCtr).
                FOR EACH _index OF _file, EACH _index-field OF _index WHERE, EACH _field OF _index-field:
                    IF (cbWhere1:LIST-ITEMS = ? OR NOT CAN-DO(cbWhere1:LIST-ITEMS,_field._field-name)) AND
                        hField:EXTENT < 2 THEN 
                        cbWhere1:ADD-LAST(_field._field-name).
                END.
            END.
    END.
    
    ASSIGN            
        cbWhere2:LIST-ITEMS IN FRAME fBasicFilter = cbWhere1:LIST-ITEMS IN FRAME fBasicFilter
        cbWhere3:LIST-ITEMS IN FRAME fBasicFilter = cbWhere1:LIST-ITEMS IN FRAME fBasicFilter
        cbWhere4:LIST-ITEMS IN FRAME fBasicFilter = cbWhere1:LIST-ITEMS IN FRAME fBasicFilter
        cbWhere5:LIST-ITEMS IN FRAME fBasicFilter = cbWhere1:LIST-ITEMS IN FRAME fBasicFilter.
    
    APPLY 'entry' TO cbWhere1 IN FRAME fBasicFilter.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fSort
&Scoped-define SELF-NAME tSortAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tSortAll wWin
ON VALUE-CHANGED OF tSortAll IN FRAME fSort /* All Fields */
DO:
    DEF VAR hField AS HANDLE NO-UNDO.
    
    IF SELF:CHECKED THEN DO:
        MESSAGE
            "Using this option can significantly slow the sorting" SKIP
            "process, and any further operations taken on this" SKIP
            "browse.  Are you sure you want to use this option?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
            UPDATE lChoice AS LOGICAL.
        
        IF lChoice THEN DO:
            ASSIGN 
                cbSortBy:LIST-ITEMS IN FRAME fSort = "(none)"
                cbSortBy:SCREEN-VALUE IN FRAME fSort = "(none)".
        
            DO iCtr = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN 
                    hField = hBuffer:BUFFER-FIELD(iCtr).
                IF (cbSortBy:LIST-ITEMS = ? OR NOT CAN-DO(cbSortBy:LIST-ITEMS,hField:NAME)) AND
                    hField:EXTENT < 2 THEN 
                    cbSortBy:ADD-LAST(hField:NAME).
            END.
        END.
    END.
    ELSE DO:
            ASSIGN 
                cbSortBy:LIST-ITEMS IN FRAME fSort = "(none)"
                cbSortBy:SCREEN-VALUE IN FRAME fSort = "(none)".
        
            DO iCtr = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN 
                    hField = hBuffer:BUFFER-FIELD(iCtr).
                FOR EACH _index OF _file, EACH _index-field OF _index WHERE, EACH _field OF _index-field:
                    IF (cbSortBy:LIST-ITEMS = ? OR NOT CAN-DO(cbSortBy:LIST-ITEMS,_field._field-name)) AND
                        hField:EXTENT < 2 THEN 
                        cbSortBy:ADD-LAST(_field._field-name).
                END.
            END.
    END.
    
    ASSIGN            
        cbSortBy2:LIST-ITEMS IN FRAME fSort = cbSortBy:LIST-ITEMS IN FRAME fSort
        cbSortBy3:LIST-ITEMS IN FRAME fSort = cbSortBy:LIST-ITEMS IN FRAME fSort
        cbSortBy4:LIST-ITEMS IN FRAME fSort = cbSortBy:LIST-ITEMS IN FRAME fSort
        cbSortBy5:LIST-ITEMS IN FRAME fSort = cbSortBy:LIST-ITEMS IN FRAME fSort
        cbSortBy2:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy3:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy4:SCREEN-VALUE IN FRAME fSort = "(none)"
        cbSortBy5:SCREEN-VALUE IN FRAME fSort = "(none)".
    
    APPLY 'entry' TO cbSortBy IN FRAME fSort.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY cbFileName cbSortList fiFiltered fiHidden cbLockCol 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE bChangeSort cbFileName cbSortList cbLockCol bZoom bClear bCancel 
         bChangeFilter bChangeHide 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY cbSortBy cbSortBy2 cbSortBy3 cbSortBy4 cbSortBy5 tSortAll 
      WITH FRAME fSort IN WINDOW wWin.
  ENABLE cbSortBy cbSortBy2 cbSortBy3 cbSortBy4 cbSortBy5 tSortAll bOKSort 
         bCancelSort 
      WITH FRAME fSort IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fSort}
  DISPLAY fiEach eWhere 
      WITH FRAME fAdvFilter IN WINDOW wWin.
  ENABLE eWhere bClearAdvFilter bOKAdvFilter bCancelAdvFilter 
      WITH FRAME fAdvFilter IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fAdvFilter}
  DISPLAY slHide 
      WITH FRAME fHide IN WINDOW wWin.
  ENABLE slHide bClearHide bOKHide bCancelHide 
      WITH FRAME fHide IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fHide}
  DISPLAY cbWhere1 cOperator1 cWhere1Value cWhere1Value2 cbAnd1 cbWhere2 
          cOperator2 cWhere2Value cWhere2Value2 cbAnd2 cbWhere3 cOperator3 
          cWhere3Value cWhere3Value2 cbAnd3 cbWhere4 cOperator4 cWhere4Value 
          cWhere4Value2 cbAnd4 cbWhere5 cOperator5 cWhere5Value cWhere5Value2 
          cbAnd5 cbWhere6 cOperator6 cWhere6Value cWhere6Value2 tFilterAll 
          fiFrom fiDoThis 
      WITH FRAME fBasicFilter IN WINDOW wWin.
  ENABLE cbWhere1 cOperator1 cWhere1Value cWhere1Value2 cbAnd1 cbWhere2 
         cOperator2 cWhere2Value cWhere2Value2 cbAnd2 cbWhere3 cOperator3 
         cWhere3Value cWhere3Value2 cbAnd3 cbWhere4 cOperator4 cWhere4Value 
         cWhere4Value2 cbAnd4 cbWhere5 cOperator5 cWhere5Value cWhere5Value2 
         cbAnd5 cbWhere6 cOperator6 cWhere6Value cWhere6Value2 bEdit 
         bClearFilter bOKBasicFilter bCancelBasicFilter tFilterAll 
      WITH FRAME fBasicFilter IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fBasicFilter}
  DISPLAY slZoom tUseFilter 
      WITH FRAME fZoom IN WINDOW wWin.
  ENABLE slZoom tUseFilter bOKZoom bCancelZoom 
      WITH FRAME fZoom IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fZoom}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME fMain:
    
    ASSIGN      
        FRAME fSort:VISIBLE = FALSE
        FRAME fAdvFilter:VISIBLE = FALSE
        FRAME fHide:VISIBLE = FALSE
        FRAME fZoom:VISIBLE = FALSE
        FRAME fBasicFilter:VISIBLE = FALSE.
        
    FRAME fSort:MOVE-TO-TOP().
    FRAME fAdvFilter:MOVE-TO-TOP().
    FRAME fHide:MOVE-TO-TOP().
    FRAME fZoom:MOVE-TO-TOP().
    FRAME fBasicFilter:MOVE-TO-TOP().
  END.
  
    APPLY 'value-changed' TO cbAnd1 IN FRAME fBasicFilter.
    APPLY 'value-changed' TO cbAnd2 IN FRAME fBasicFilter.
    APPLY 'value-changed' TO cbAnd3 IN FRAME fBasicFilter.
    APPLY 'value-changed' TO cbAnd4 IN FRAME fBasicFilter.
    APPLY 'value-changed' TO cbAnd5 IN FRAME fBasicFilter.
  
  FOR EACH _file WHERE index(_file._file-name,"_") <> 1
    AND INDEX(_file._file-name,"SYS") <> 1:
    cbFileName:ADD-LAST(_file._file-name).
  END.

  ASSIGN cbFileName:SCREEN-VALUE = cbFileName:ENTRY(1).

  ASSIGN
    cbFileName:SCREEN-VALUE = IF file-to-do <> "" THEN ENTRY(1,file-to-do) 
                                ELSE cbFileName:SCREEN-VALUE.
  
  IF cbFileName:SCREEN-VALUE = ? 
  OR cbFileName:SCREEN-VALUE = "" THEN 
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT "customer").
  ELSE
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE).

  APPLY 'value-changed' TO cbFileName IN FRAME fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipApplyFilter wWin 
PROCEDURE ipApplyFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME fBasicFilter:
  ASSIGN cWhereClause =
    (IF cbWhere1:SCREEN-VALUE <> ? THEN cbWhere1:SCREEN-VALUE + " " + 
        cOperator1:SCREEN-VALUE + " '" + 
        cWhere1Value:SCREEN-VALUE + "'" ELSE "") +
    (IF cbWhere2:SCREEN-VALUE <> ? THEN " " + cbAnd1:SCREEN-VALUE + " " + cbWhere2:SCREEN-VALUE + " " + 
        cOperator2:SCREEN-VALUE + " '" + 
        cWhere2Value:SCREEN-VALUE + "'" ELSE "") +
    (IF cbWhere3:SCREEN-VALUE <> ? THEN " " + cbAnd2:SCREEN-VALUE + " " + cbWhere3:SCREEN-VALUE + " " + 
        cOperator3:SCREEN-VALUE + " '" + 
        cWhere3Value:SCREEN-VALUE + "'" ELSE "") +
    (IF cbWhere4:SCREEN-VALUE <> ? THEN " " + cbAnd3:SCREEN-VALUE + " " + cbWhere4:SCREEN-VALUE + " " + 
        cOperator4:SCREEN-VALUE + " '" + 
        cWhere4Value:SCREEN-VALUE + "'" ELSE "") +
    "".
  END.

  DO WITH FRAME fSort:
  IF cbSortBy3 <> ? THEN
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
  ELSE IF cbSortBy2 <> ? THEN
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
  ELSE IF cbSortBy <> ? THEN
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE IN FRAME fMain).
  ELSE
    RUN ipCreateQuery IN THIS-PROCEDURE (INPUT cbFileName:SCREEN-VALUE). 
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateFilter wWin 
PROCEDURE ipCreateFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER cFrom AS CHAR NO-UNDO.
DEF VAR cWhereSeg AS CHAR EXTENT 6 NO-UNDO INITIAL "".
DEF VAR iCtr2 AS INT NO-UNDO.
DEF VAR hBufField AS HANDLE NO-UNDO.
DEF VAR lChar AS LOG NO-UNDO.
  
  DO WITH FRAME fBasicFilter:
    IF cFrom = "Advanced" THEN ASSIGN
        cWhereClause = replace(eWhere:SCREEN-VALUE IN FRAME fAdvFilter,'"',"'").
    ELSE DO:
        IF cbWhere1:SCREEN-VALUE <> "" THEN DO:
            test:
            DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN
                    hBufField = hBuffer:BUFFER-FIELD(iCtr2).
                IF hBufField:NAME = cbWhere1:SCREEN-VALUE THEN DO:
                    IF hBufField:DATA-TYPE = "character" THEN ASSIGN
                        lChar = TRUE.
                    ELSE ASSIGN
                        lChar = FALSE.
                    LEAVE test.
                END.
            END.
            IF cOperator1:SCREEN-VALUE <> "Rng" THEN DO:
                ASSIGN
                    cWhereSeg[1] = 
                        cbWhere1:SCREEN-VALUE + " " +
                        cOperator1:SCREEN-VALUE + " " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere1Value:SCREEN-VALUE +
                        (IF lChar THEN "'" ELSE ""). 
            END.
            ELSE DO:
                ASSIGN
                    cWhereSeg[1] =
                        "(" + cbWhere1:SCREEN-VALUE + " >= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere1Value:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        " AND " + 
                        cbWhere1:SCREEN-VALUE + " <= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere1Value2:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        ") ".
            END.
        END.
        
        IF cbWhere2:SCREEN-VALUE <> "" THEN DO:
            test:
            DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN
                    hBufField = hBuffer:BUFFER-FIELD(iCtr2).
                IF hBufField:NAME = cbWhere2:SCREEN-VALUE THEN DO:
                    IF hBufField:DATA-TYPE = "character" THEN ASSIGN
                        lChar = TRUE.
                    ELSE ASSIGN
                        lChar = FALSE.
                    LEAVE test.
                END.
            END.
            IF cbAnd1:SCREEN-VALUE <> ? AND
            cbAnd1:SCREEN-VALUE <> "" THEN ASSIGN
                cWhereSeg[2] = " " + cbAnd1:SCREEN-VALUE + " ".
            IF cOperator2:SCREEN-VALUE <> "Rng" THEN DO:
                ASSIGN
                    cWhereSeg[2] =  cWhereSeg[2] +
                        cbWhere2:SCREEN-VALUE + " " +
                        cOperator2:SCREEN-VALUE + " " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere2Value:SCREEN-VALUE +
                        (IF lChar THEN "'" ELSE ""). 
            END.
            ELSE DO:
                ASSIGN
                    cWhereSeg[2] =  cWhereSeg[2] +
                        "(" + cbWhere2:SCREEN-VALUE + " >= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere2Value:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        " AND " + 
                        cbWhere2:SCREEN-VALUE + " <= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere2Value2:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        ") ".
            END.
        END.
        
        IF cbWhere3:SCREEN-VALUE <> "" THEN DO:
            test:
            DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN
                    hBufField = hBuffer:BUFFER-FIELD(iCtr2).
                IF hBufField:NAME = cbWhere3:SCREEN-VALUE THEN DO:
                    IF hBufField:DATA-TYPE = "character" THEN ASSIGN
                        lChar = TRUE.
                    ELSE ASSIGN
                        lChar = FALSE.
                    LEAVE test.
                END.
            END.
            IF cbAnd2:SCREEN-VALUE <> ? AND
            cbAnd2:SCREEN-VALUE <> "" THEN ASSIGN
                cWhereSeg[3] = " " + cbAnd2:SCREEN-VALUE + " ".
            IF cOperator3:SCREEN-VALUE <> "Rng" THEN DO:
                ASSIGN
                    cWhereSeg[3] =  cWhereSeg[3] +
                        cbWhere3:SCREEN-VALUE + " " +
                        cOperator3:SCREEN-VALUE + " " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere3Value:SCREEN-VALUE +
                        (IF lChar THEN "'" ELSE ""). 
            END.
            ELSE DO:
                ASSIGN
                    cWhereSeg[3] =  cWhereSeg[3] +
                        "(" + cbWhere3:SCREEN-VALUE + " >= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere3Value:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        " AND " + 
                        cbWhere3:SCREEN-VALUE + " <= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere3Value2:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        ") ".
            END.
        END.
        
        IF cbWhere4:SCREEN-VALUE <> "" THEN DO:
            test:
            DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN
                    hBufField = hBuffer:BUFFER-FIELD(iCtr2).
                IF hBufField:NAME = cbWhere4:SCREEN-VALUE THEN DO:
                    IF hBufField:DATA-TYPE = "character" THEN ASSIGN
                        lChar = TRUE.
                    ELSE ASSIGN
                        lChar = FALSE.
                    LEAVE test.
                END.
            END.
            IF cbAnd3:SCREEN-VALUE <> ? AND
            cbAnd3:SCREEN-VALUE <> "" THEN ASSIGN
                cWhereSeg[4] = " " + cbAnd3:SCREEN-VALUE + " ".
            IF cOperator4:SCREEN-VALUE <> "Rng" THEN DO:
                ASSIGN
                    cWhereSeg[4] =  cWhereSeg[4] +
                        cbWhere4:SCREEN-VALUE + " " +
                        cOperator4:SCREEN-VALUE + " " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere4Value:SCREEN-VALUE +
                        (IF lChar THEN "'" ELSE ""). 
            END.
            ELSE DO:
                ASSIGN
                    cWhereSeg[4] =  cWhereSeg[4] +
                        "(" + cbWhere4:SCREEN-VALUE + " >= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere4Value:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        " AND " + 
                        cbWhere4:SCREEN-VALUE + " <= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere4Value2:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        ") ".
            END.
        END.
        
        IF cbWhere5:SCREEN-VALUE <> "" THEN DO:
            test:
            DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN
                    hBufField = hBuffer:BUFFER-FIELD(iCtr2).
                IF hBufField:NAME = cbWhere5:SCREEN-VALUE THEN DO:
                    IF hBufField:DATA-TYPE = "character" THEN ASSIGN
                        lChar = TRUE.
                    ELSE ASSIGN
                        lChar = FALSE.
                    LEAVE test.
                END.
            END.
            IF cbAnd4:SCREEN-VALUE <> ? AND
            cbAnd4:SCREEN-VALUE <> "" THEN ASSIGN
                cWhereSeg[5] = " " + cbAnd4:SCREEN-VALUE + " ".
            IF cOperator5:SCREEN-VALUE <> "Rng" THEN DO:
                ASSIGN
                    cWhereSeg[5] =  cWhereSeg[5] +
                        cbWhere5:SCREEN-VALUE + " " +
                        cOperator5:SCREEN-VALUE + " " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere5Value:SCREEN-VALUE +
                        (IF lChar THEN "'" ELSE ""). 
            END.
            ELSE DO:
                ASSIGN
                    cWhereSeg[5] =  cWhereSeg[5] +
                        "(" + cbWhere5:SCREEN-VALUE + " >= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere5Value:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        " AND " + 
                        cbWhere5:SCREEN-VALUE + " <= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere5Value2:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        ") ".
            END.
        END.
        
        IF cbWhere6:SCREEN-VALUE <> "" THEN DO:
            test:
            DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
                ASSIGN
                    hBufField = hBuffer:BUFFER-FIELD(iCtr2).
                IF hBufField:NAME = cbWhere6:SCREEN-VALUE THEN DO:
                    IF hBufField:DATA-TYPE = "character" THEN ASSIGN
                        lChar = TRUE.
                    ELSE ASSIGN
                        lChar = FALSE.
                    LEAVE test.
                END.
            END.
            IF cbAnd5:SCREEN-VALUE <> ? AND
            cbAnd5:SCREEN-VALUE <> "" THEN ASSIGN
                cWhereSeg[6] = " " + cbAnd5:SCREEN-VALUE + " ".
            IF cOperator6:SCREEN-VALUE <> "Rng" THEN DO:
                ASSIGN
                    cWhereSeg[6] =  cWhereSeg[6] +
                        cbWhere6:SCREEN-VALUE + " " +
                        cOperator6:SCREEN-VALUE + " " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere6Value:SCREEN-VALUE +
                        (IF lChar THEN "'" ELSE ""). 
            END.
            ELSE DO:
                ASSIGN
                    cWhereSeg[6] =  cWhereSeg[6] +
                        "(" + cbWhere6:SCREEN-VALUE + " >= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere6Value:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        " AND " + 
                        cbWhere6:SCREEN-VALUE + " <= " +
                        (IF lChar THEN "'" ELSE "") +
                        cWhere6Value2:SCREEN-VALUE + 
                        (IF lChar THEN "'" ELSE "") + 
                        ") ".
            END.
        END.
        
        ASSIGN cWhereClause = 
            (IF cWhereSeg[1] <> ? THEN cWhereSeg[1] ELSE "") + 
            (IF cWhereSeg[2] <> ? THEN cWhereSeg[2] ELSE "") + 
            (IF cWhereSeg[3] <> ? THEN cWhereSeg[3] ELSE "") + 
            (IF cWhereSeg[4] <> ? THEN cWhereSeg[4] ELSE "") + 
            (IF cWhereSeg[5] <> ? THEN cWhereSeg[5] ELSE "") + 
            (IF cWhereSeg[6] <> ? THEN cWhereSeg[6] ELSE "").
            
    END.
  END.

    IF cWhereClause <> "" THEN ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = " FILTERED".
    ELSE ASSIGN
        fiFiltered:SCREEN-VALUE IN FRAME fMain = "NOT filtered".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateQuery wWin 
PROCEDURE ipCreateQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcFileName AS CHAR.
DEF VAR iNumIndex AS INT NO-UNDO.
DEF VAR hColNumber AS HANDLE NO-UNDO.
DEF VAR lOK AS LOG NO-UNDO.
DEF VAR hThisColumn AS HANDLE NO-UNDO.
DEF VAR lAllRecs AS LOG NO-UNDO.
DEF VAR iDispRecs AS INT NO-UNDO.
DEF VAR iCtr2 AS INT NO-UNDO.
DEF VAR cHideList AS CHAR NO-UNDO.

IF CAN-FIND(FIRST _file WHERE _file._file-name = ipcFileName) THEN DO:
IF VALID-HANDLE(hDynBrowse) AND 
   valid-handle(hDynBrowse:CURRENT-COLUMN) THEN ASSIGN 
        hMe = hDynBrowse:CURRENT-COLUMN
        cMe = hMe:NAME.

    /* build a sorted list of columns in any existing browser */
    IF cColList <> "" THEN DO:
        cColList = "". 
        DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
            ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
            ASSIGN cColList = cColList + "," + hThisColumn:NAME.
        END.
    END.
    ASSIGN cColList = SUBSTRING(cColList,2).
    
    IF VALID-HANDLE(hDynBrowse) THEN DELETE WIDGET hDynBrowse.
    IF VALID-HANDLE(hDynQuery) THEN DELETE WIDGET hDynQuery.
    IF VALID-HANDLE(hBuffer) THEN DELETE WIDGET hBuffer.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName.

    CREATE QUERY hDynQuery.
    hDynQuery:SET-BUFFERS(hBuffer). 
    hDynQuery:QUERY-PREPARE("FOR EACH " + ipcFileName + " where " + string(cWhereClause) + cSortByPhrase).
    hDynQuery:QUERY-OPEN.
    
    CREATE BROWSE hDynBrowse
        ASSIGN 
        FRAME = FRAME fMain:HANDLE
        SENSITIVE = TRUE
        SEPARATORS = TRUE
        ROW-MARKERS = FALSE
        COLUMN-RESIZABLE = TRUE
        COLUMN-MOVABLE = TRUE 
        ALLOW-COLUMN-SEARCHING = TRUE
        MULTIPLE = TRUE 
        TRIGGERS:
            ON mouse-select-click PERSISTENT RUN ipFindRelated IN THIS-PROCEDURE.
            ON mouse-select-dblclick PERSISTENT RUN ipSelectValue IN THIS-PROCEDURE.
            ON CTRL-A PERSISTENT RUN ipSelectAll IN THIS-PROCEDURE.
        END TRIGGERS.
        ASSIGN 
        hDynBrowse:QUERY = hDynQuery
        hDynBrowse:FRAME = FRAME fMain:HANDLE
        hDynBrowse:COL = 1
        hDynBrowse:ROW = 2.5
        hDynBrowse:WIDTH = FRAME fMain:WIDTH
        hDynBrowse:HEIGHT = FRAME fMain:HEIGHT - 1.6
        hDynBrowse:VISIBLE = TRUE
        NO-ERROR.
    /*
    FIND FIRST z_file WHERE z_file.FILE-NAME = ipcFileName NO-LOCK NO-ERROR.
    IF AVAIL z_file THEN ASSIGN hDynBrowse:MAX-DATA-GUESS = z_file.rec-count.
    */
    FIND FIRST _file WHERE _file._file-name = ipcFileName NO-LOCK NO-ERROR.
    FOR EACH _field OF _file BY _field._order:
        IF slHide:LOOKUP(_field._field-name) IN FRAME fHide<> ? and
        slHide:LOOKUP(_field._field-name) IN FRAME fHide = 0 THEN slHide:ADD-LAST(_field._field-name) IN FRAME fHide.
    END.
    DO iCtr = 1 TO slHide:NUM-ITEMS:
        IF slHide:IS-SELECTED(iCtr) THEN ASSIGN
            cHideList = cHideList + "," + ENTRY(iCtr,slHide:LIST-ITEMS).
    END.
    cHideList = SUBSTRING(cHideList,2).
    
    /* modify the column list by removing any items in the hide list */
    DO iCtr = 1 TO NUM-ENTRIES(cHideList):
        cColList = REPLACE(cColList,ENTRY(iCtr,cHideList),"").
    END.
    cColList = REPLACE(cColList,",,",",").
    
    hDynBrowse:ADD-COLUMNS-FROM(hBuffer,cHideList) NO-ERROR.
    DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:                                               
        ASSIGN
            hColumnHandle[iCtr] = hDynBrowse:GET-BROWSE-COLUMN(iCtr)
            hColumnHandle[iCtr]:READ-ONLY = FALSE.
        IF AVAIL _file THEN DO:
            checkindex:
            FOR EACH _index OF _file, EACH _index-field OF _index WHERE /* _index-field._index-seq = 1 */, EACH _field OF _index-field:
                IF _field._field-name = hColumnHandle[iCtr]:NAME THEN do:
                    ASSIGN
                    hColumnHandle[iCtr]:READ-ONLY = TRUE 
                    hColumnHandle[iCtr]:LABEL-BGCOLOR = 14
                    iNumIndex = iNumIndex + 1.
                    hDynBrowse:MOVE-COLUMN(iCtr,iNumIndex).
                    DO WITH FRAME fSort:
                    IF cbSortBy:LIST-ITEMS = ? OR NOT CAN-DO(cbSortBy:LIST-ITEMS,hColumnHandle[iCtr]:NAME) THEN DO:
                        cbSortBy:ADD-LAST(hColumnHandle[iCtr]:NAME).
                        cbSortBy2:ADD-LAST(hColumnHandle[iCtr]:NAME).
                        cbSortBy3:ADD-LAST(hColumnHandle[iCtr]:NAME).
                        cbSortBy4:ADD-LAST(hColumnHandle[iCtr]:NAME).
                        cbSortBy5:ADD-LAST(hColumnHandle[iCtr]:NAME).
                    END.
                    END.
                    LEAVE checkindex.
                END.
            END.            
        END.
        hColumnHandle[iCtr]:READ-ONLY = FALSE.
    END.
    
    ASSIGN
        hDynBrowse:NUM-LOCKED-COLUMNS = INTEGER(cbLockCol:SCREEN-VALUE IN FRAME fMain).
    
    /* if there is not a previously created sorted column list, build one now */
    IF NUM-ENTRIES(cColList) = 0 THEN DO:
        DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
            ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
            ASSIGN cColList = cColList + "," + hThisColumn:NAME.
        END.
        ASSIGN cColList = SUBSTRING(cColList,2).

        DO WITH FRAME fSort:
            DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
                ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
                IF hThisColumn:NAME = cbSortBy:SCREEN-VALUE THEN DO:
                    hDynBrowse:MOVE-COLUMN(iCtr,1).
                END.
            END.
            DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
                ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
                IF hThisColumn:NAME = cbSortBy2:SCREEN-VALUE THEN DO:
                    hDynBrowse:MOVE-COLUMN(iCtr,2).
                END.
            END.
            DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
                ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
                IF hThisColumn:NAME = cbSortBy3:SCREEN-VALUE THEN DO:
                    hDynBrowse:MOVE-COLUMN(iCtr,3).
                END.
            END.
            DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
                ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
                IF hThisColumn:NAME = cbSortBy4:SCREEN-VALUE THEN DO:
                    hDynBrowse:MOVE-COLUMN(iCtr,4).
                END.
            END.
            DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
                ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
                IF hThisColumn:NAME = cbSortBy5:SCREEN-VALUE THEN DO:
                    hDynBrowse:MOVE-COLUMN(iCtr,5).
                END.
            END.
        END.
    END.
    ELSE DO:
        DO iCtr = 1 TO NUM-ENTRIES(cColList):
            nestblock:
            DO iCtr2 = 1 TO hDynBrowse:NUM-COLUMNS:
                ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr2).
                IF hThisColumn:NAME = ENTRY(iCtr,cColList) THEN DO:
                    hDynBrowse:MOVE-COLUMN(iCtr,iCtr2).
                    LEAVE nestblock.
                END.
            END.
        END.
    END.

    RUN ipWhereOptions.
        
    APPLY 'value-changed' TO hDynBrowse.
    
    
    IF hDynBrowse:MAX-DATA-GUESS > hDynQuery:NUM-RESULTS THEN ASSIGN
        lAllRecs = NO
        iDispRecs = hDynBrowse:MAX-DATA-GUESS.
    ELSE ASSIGN
        lAllRecs = YES 
        iDispRecs = hDynQuery:NUM-RESULTS.
                        
    cStatus =  "Browsing file: " + ipcFileName +
        (IF lAllRecs THEN " (Containing " ELSE " (Approx. ") + 
        STRING(iDispRecs) +
        (IF iDispRecs = 1 THEN " record)" ELSE " records)").
        
    STATUS DEFAULT cStatus.
    STATUS INPUT cStatus.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateSort wWin 
PROCEDURE ipCreateSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fSort:
    
    cbSortList:LIST-ITEMS IN FRAME fMain = "".
    
    IF cbSortBy:SCREEN-VALUE = "(none)" THEN ASSIGN
        cbSortList:LIST-ITEMS IN FRAME fMain = " by (default)"
        cSortByPhrase = "".
    ELSE IF cbSortBy2:SCREEN-VALUE = "(none)" THEN DO:    
        cbSortList:ADD-LAST("by " + cbSortBy:SCREEN-VALUE) IN FRAME fMain.
        cSortByPhrase = "by " + cbSortBy:SCREEN-VALUE.    
    END.
    ELSE IF cbSortBy3:SCREEN-VALUE = "(none)" THEN DO:    
        cbSortList:ADD-LAST("by " + cbSortBy:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy2:SCREEN-VALUE) IN FRAME fMain.
        cSortByPhrase = "by " + cbSortBy:SCREEN-VALUE + 
                        " by " + cbSortBy2:SCREEN-VALUE.    
    END.
    ELSE IF cbSortBy4:SCREEN-VALUE = "(none)" THEN DO:    
        cbSortList:ADD-LAST("by " + cbSortBy:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy2:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy3:SCREEN-VALUE) IN FRAME fMain.
        cSortByPhrase = "by " + cbSortBy:SCREEN-VALUE +
                        " by " + cbSortBy2:SCREEN-VALUE +
                        " by " + cbSortBy3:SCREEN-VALUE.    
    END.
    ELSE IF cbSortBy5:SCREEN-VALUE = "(none)" THEN DO:    
        cbSortList:ADD-LAST("by " + cbSortBy:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy2:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy3:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy4:SCREEN-VALUE) IN FRAME fMain.
        cSortByPhrase = "by " + cbSortBy:SCREEN-VALUE +
                        " by " + cbSortBy2:SCREEN-VALUE +
                        " by " + cbSortBy3:SCREEN-VALUE +
                        " by " + cbSortBy4:SCREEN-VALUE.    
    END.
    ELSE DO:    
        cbSortList:ADD-LAST("by " + cbSortBy:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy2:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy3:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy4:SCREEN-VALUE + " then") IN FRAME fMain.
        cbSortList:ADD-LAST("by " + cbSortBy5:SCREEN-VALUE) IN FRAME fMain.
        cSortByPhrase = "by " + cbSortBy:SCREEN-VALUE +
                        " by " + cbSortBy2:SCREEN-VALUE +
                        " by " + cbSortBy3:SCREEN-VALUE +
                        " by " + cbSortBy4:SCREEN-VALUE +
                        " by " + cbSortBy5:SCREEN-VALUE.    
    END.
        
        
    cbSortList:SCREEN-VALUE IN FRAME fMain = ENTRY(1,cbSortList:LIST-ITEMS IN FRAME fMain).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExcel wWin 
PROCEDURE ipExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER cWhatToDo AS CHAR NO-UNDO.
DEF INPUT PARAMETER cMode AS CHAR NO-UNDO.

DEF VAR cTable              AS CHARACTER    NO-UNDO.
DEF VAR cFields             AS CHARACTER    NO-UNDO.
DEF VAR hQuery              AS HANDLE       NO-UNDO.
DEF VAR hBuffer2            AS HANDLE       NO-UNDO.
DEF VAR hField              AS HANDLE       NO-UNDO.
DEF VAR iFields             AS INTEGER      NO-UNDO.
DEF VAR iRow                AS INTEGER      NO-UNDO.
DEF VAR cRow                AS CHARACTER    NO-UNDO INITIAL "A".
DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR hThisField          AS HANDLE       NO-UNDO.
DEF VAR hThisColumn         AS HANDLE       NO-UNDO.
DEF VAR iCtr2               AS INT          NO-UNDO.
DEF VAR iCtr3               AS INT          NO-UNDO.
DEF VAR cColList            AS CHAR         NO-UNDO.
DEF VAR iColumn             AS INT          NO-UNDO.
DEF VAR lExtentCleared      AS LOG          NO-UNDO.
DEF VAR cLastField          AS CHAR         NO-UNDO.
DEF VAR iBrowseCount        AS INT          NO-UNDO.
DEF VAR tWidth              AS INT          NO-UNDO.

IF hDynBrowse:NUM-SELECTED-ROWS = 0 THEN hDynBrowse:SELECT-ALL().

STATUS DEFAULT "Exporting data. Please wait...".

SESSION:SET-WAIT-STATE("general").

CREATE "Excel.Application" chExcelApplication.

ASSIGN 
    chWorkbook                 = chExcelApplication:Workbooks:Add()
    chWorkSheet                = chExcelApplication:Sheets:Item(1)
    cTable                     = hBuffer:TABLE
    chWorkSheet:NAME           = cTable
    cColList                   = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,"
    cColList                   = cColList + "AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,"
    cColList                   = cColList + "BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,"
    cColList                   = cColList + "CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ,"
    cColList                   = cColList + "DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM,DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ,"
    cColList                   = cColList + "EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM,EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ,"
    cColList                   = cColList + "FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM,FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ,"
    cColList                   = cColList + "GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM,GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ"
    .
       

IF cMode = "Rows" THEN DO:
    ASSIGN iRow = 1.
    DO iFields = 1 TO hDynBrowse:NUM-COLUMNS.
        ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iFields).
        chWorkSheet:range(ENTRY(iFields,cColList) + STRING(iRow)):Value = (hThisColumn:NAME).
        chWorkSheet:range(ENTRY(iFields,cColList) + STRING(iRow)):Font:Bold = TRUE.
    END.
END.
ELSE IF cMode = "Columns" THEN DO: 
    ASSIGN iRow = 1.
    DO iFields = 1 TO hDynBrowse:NUM-COLUMNS.
        ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iFields).
        chWorkSheet:range(ENTRY(1,cColList) + STRING(iRow)):Value = (hThisColumn:NAME).
        chWorkSheet:range(ENTRY(1,cColList) + STRING(iRow)):Font:Bold = TRUE.
        chWorkSheet:COLUMNS(ENTRY(1,cColList)):ColumnWidth = 20.
        iRow = iRow + 1.
    END.
    iRow = 1.
END.

IF hDynQuery:GET-FIRST() = FALSE THEN DO:
    chExcelApplication:QUIT().
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chWorkBook         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
    RETURN.
END.

IF cMode = "Rows" THEN DO iCtr3 = 1 TO hDynBrowse:NUM-SELECTED-ROWS:
  hDynBrowse:FETCH-SELECTED-ROW(iCtr3).
  DO:
    ASSIGN 
        iRow = iRow + 1
        iColumn = 1.
    IF iCtr3 MODULO 100 = 0 THEN
        STATUS DEFAULT "Exporting data. Please wait... (" + string(iCtr3) +
            " of " + STRING(hDynBrowse:NUM-SELECTED-ROWS) + " records exported)". .
    DO iFields = 1 TO hDynBrowse:NUM-COLUMNS:
        ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iFields).
        IF hThisColumn:NAME = cLastField THEN DO:
            lExtentCleared = FALSE.
            NEXT.
        END.
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            hField = hBuffer:BUFFER-FIELD(iCtr).
            IF iRow = 2 THEN DO:
                    chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):ColumnWidth = IF hField:DATA-TYPE = "integer" THEN 6
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN 20
                                                                               ELSE hThisColumn:WIDTH-CHARS.
                    chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#####0":U
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN "###,###,##0.00":U
                                                                               ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                               ELSE "@":U.
                    tWidth = tWidth + chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):ColumnWidth.
            END.
            IF hThisColumn:NAME = hField:NAME THEN DO:
                IF hField:EXTENT < 2 THEN DO:
                    chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):Value = hField:BUFFER-VALUE.
                    iColumn = iColumn + 1.
                    cLastField = hThisColumn:NAME.
                END.
                ELSE DO iCtr2 = 1 TO hField:EXTENT:
                    chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):Value = hField:BUFFER-VALUE(iCtr2).
                    iColumn = iColumn + 1.
                    lExtentCleared = YES.
                    cLastField = hThisColumn:NAME.
                END.
            END.
        END.
    END.
  END.
END.
ELSE IF cMode = "Columns" THEN DO iCtr3 = 1 TO hDynBrowse:NUM-SELECTED-ROWS:
  hDynBrowse:FETCH-SELECTED-ROW(iCtr3).
  DO:
    ASSIGN 
        iRow = iRow + 1
        iColumn = 1.
    IF iCtr3 MODULO 100 = 0 THEN
        STATUS DEFAULT "Exporting data. Please wait... (" + string(iCtr3) +
            " of " + STRING(hDynBrowse:NUM-SELECTED-ROWS) + " records exported)". .
    DO iFields = 1 TO hDynBrowse:NUM-COLUMNS:
        ASSIGN hThisColumn = hDynBrowse:GET-BROWSE-COLUMN(iFields).
        IF hThisColumn:NAME = cLastField THEN DO:
            lExtentCleared = FALSE.
            NEXT.
        END.
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            hField = hBuffer:BUFFER-FIELD(iCtr).
            IF hThisColumn:NAME = hField:NAME THEN DO:
                IF hField:EXTENT < 2 THEN DO:
                    chWorkSheet:range(ENTRY(iRow,cColList) + STRING(iColumn)):Value = hField:BUFFER-VALUE.
                    chWorkSheet:COLUMNS(ENTRY(iRow,cColList)):ColumnWidth = MAX(tWidth,
                                                                                (IF hField:DATA-TYPE = "integer" THEN 6
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN 20
                                                                               ELSE hThisColumn:WIDTH-CHARS)).
                    chWorkSheet:range(ENTRY(iRow,cColList) + STRING(iColumn)):HorizontalAlignment = -4131.
                    tWidth = MAX(tWidth,chWorkSheet:COLUMNS(ENTRY(iRow,cColList)):ColumnWidth).
                    chWorkSheet:range(ENTRY(iRow,cColList) + STRING(iColumn)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#####0":U
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN "###,###,##0.00":U
                                                                               ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                               ELSE "@":U.
                    iColumn = iColumn + 1.
                    cLastField = hThisColumn:NAME.
                END.
                ELSE DO iCtr2 = 1 TO hField:EXTENT:
                    chWorkSheet:range(ENTRY(iRow,cColList) + STRING(iColumn)):Value = hField:BUFFER-VALUE(iCtr2).
                    chWorkSheet:COLUMNS(ENTRY(iRow,cColList)):ColumnWidth = MAX(tWidth,
                                                                                (IF hField:DATA-TYPE = "integer" THEN 6
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN 20
                                                                               ELSE hThisColumn:WIDTH-CHARS)).
                    chWorkSheet:range(ENTRY(iRow,cColList) + STRING(iColumn)):HorizontalAlignment = -4131.
                    tWidth = MAX(tWidth,chWorkSheet:COLUMNS(ENTRY(iRow,cColList)):ColumnWidth).
                    chWorkSheet:range(ENTRY(iRow,cColList) + STRING(iColumn)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#####0":U
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN "###,###,##0.00":U
                                                                               ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                               ELSE "@":U.
                    iColumn = iColumn + 1.
                    lExtentCleared = YES.
                    cLastField = hThisColumn:NAME.
                END.
            END.
        END.
    END.
  END.
END.

IF cWhatToDo = "Sheet" THEN DO:
    DO:
        chWorksheet:PageSetup:LeftHeader = "MXP Dynamic Browser" + chr(10) + "UserId: " + 
                            entry(1,userid(LDBNAME(1)),"@") + " | DB: " + DBNAME.
        chWorksheet:PageSetup:LeftFooter = "fsBrowse.w - (c) 2006, Foresight Software, Inc.".                    
        chWorksheet:PageSetup:RightHeader = "Table: " + hBuffer:TABLE.
        chWorksheet:PageSetup:FirstPageNumber = 1.
        chWorksheet:PageSetup:RightFooter = "Print Date: " +
                                                STRING(MONTH(TODAY),"99") + "/" +
                                                STRING(DAY(TODAY),"99") + "/" +
                                                STRING(YEAR(TODAY),"9999") + CHR(10) +
                                            "Page: &P".
        chWorksheet:PageSetup:PrintGridlines = TRUE.
        chWorksheet:PageSetup:ORIENTATION = IF cMode = "Rows" THEN (IF tWidth < 550 THEN 1 ELSE 2) ELSE 1.
    END.
    ASSIGN    
        chExcelApplication:Visible = TRUE.
END.
ELSE IF cWhatToDo = "Print" THEN DO:
    DO:
        chWorksheet:PageSetup:LeftHeader = "MXP Dynamic Browser" + chr(10) + "UserId: " + 
                            entry(1,userid(LDBNAME(1)),"@") + " | DB: " + DBNAME.
        chWorksheet:PageSetup:LeftFooter = "fsBrowse.w - (c) 2006, Foresight Software, Inc.".                    
        chWorksheet:PageSetup:RightHeader = "Table: " + hBuffer:TABLE.
        chWorksheet:PageSetup:FirstPageNumber = 1.
        chWorksheet:PageSetup:RightFooter = "Print Date: " +
                                                STRING(MONTH(TODAY),"99") + "/" +
                                                STRING(DAY(TODAY),"99") + "/" +
                                                STRING(YEAR(TODAY),"9999") + CHR(10) +
                                            "Page: &P".
        chWorksheet:PageSetup:PrintGridlines = TRUE.
        chWorksheet:PageSetup:ORIENTATION = IF cMode = "Rows" THEN (IF tWidth < 550 THEN 1 ELSE 2) ELSE 1.
        chWorksheet:PrintOut(,,,,SESSION:PRINTER-NAME,,).
    END.
END.

SESSION:SET-WAIT-STATE("").
STATUS DEFAULT cStatus.

RELEASE OBJECT chWorkSheet        NO-ERROR.
RELEASE OBJECT chWorkBook         NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFindRelated wWin 
PROCEDURE ipFindRelated :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE.

IF VALID-HANDLE(hDynBrowse) THEN DO:
    DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:
        ASSIGN hColumn=hDynBrowse:GET-BROWSE-COLUMN(iCtr).
        IF hColumn:X > LAST-EVENT:X THEN LEAVE.
    END.
    ASSIGN
        slZoom:LIST-ITEMS IN FRAME fZoom = ""
        hColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr - 1).
        
    FOR EACH _field WHERE _field._field-name = hColumn:NAME:
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF AVAIL _file THEN
            slZoom:ADD-LAST(_file._file-name) IN FRAME fZoom.
    END.
    
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSelectAll wWin 
PROCEDURE ipSelectAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hDynBrowse:SELECT-ALL().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSelectValue wWin 
PROCEDURE ipSelectValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurColumn AS HANDLE.
DEF VAR hBufField AS HANDLE.
DEF VAR cColumnName AS CHAR.
IF fr-field <> "" THEN DO:
    hDynBrowse:FETCH-SELECTED-ROW(1).
    DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:                                               
        ASSIGN
            hCurColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr)
            cColumnName = hCurColumn:NAME.
        IF cColumnName = fr-field THEN DO:
            hBufField = hCurColumn:BUFFER-FIELD.
            fr-value = hBufField:BUFFER-VALUE.
            IF VALID-HANDLE(xfocus#)
            AND xfocus#:type = "fill-in":U THEN do:            
                xfocus#:SCREEN-VALUE = fr-value.
                apply "VALUE-CHANGED":U to xfocus#.
             END.
            ASSIGN
                result = STRING(hBuffer:ROWID).
            APPLY 'choose' TO bCancel IN FRAME fMain.
        END.
    END.
END.
ELSE DO:
    hDynBrowse:FETCH-SELECTED-ROW(1).
    DO iCtr = 1 TO 1:                                               
        ASSIGN
            hCurColumn = hDynBrowse:GET-BROWSE-COLUMN(iCtr)
            cColumnName = hCurColumn:NAME.
        hBufField = hCurColumn:BUFFER-FIELD.
        fr-value = hBufField:BUFFER-VALUE.
        IF VALID-HANDLE(xfocus#)
            AND xfocus#:type = "fill-in":U THEN do:            
                xfocus#:SCREEN-VALUE = fr-value.
                apply "VALUE-CHANGED":U to xfocus#.
             END.
            ASSIGN
                result = STRING(hBuffer:ROWID).
            APPLY 'choose' TO bCancel IN FRAME fMain.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipWhereOptions wWin 
PROCEDURE ipWhereOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLimit AS CHAR NO-UNDO.
DEF VAR hThisField AS HANDLE NO-UNDO.
DEF VAR cOldData AS CHAR EXTENT 6 NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR iCtr2 AS INT NO-UNDO.

DO WITH FRAME fMain:
IF tFilterAll:CHECKED IN FRAME fBasicFilter THEN ASSIGN
    cLimit = "Full".
ELSE ASSIGN
    cLimit = "Limit".
    
ASSIGN
    cOldData[1] = cbWhere1:SCREEN-VALUE IN FRAME fBasicFilter
    cOldData[2] = cbWhere2:SCREEN-VALUE IN FRAME fBasicFilter
    cOldData[3] = cbWhere3:SCREEN-VALUE IN FRAME fBasicFilter
    cOldData[4] = cbWhere4:SCREEN-VALUE IN FRAME fBasicFilter
    cOldData[5] = cbWhere5:SCREEN-VALUE IN FRAME fBasicFilter
    cOldData[6] = cbWhere6:SCREEN-VALUE IN FRAME fBasicFilter
    cbWhere1:LIST-ITEMS IN FRAME fBasicFilter = ""
    cbWhere2:LIST-ITEMS IN FRAME fBasicFilter = ""
    cbWhere3:LIST-ITEMS IN FRAME fBasicFilter = ""
    cbWhere4:LIST-ITEMS IN FRAME fBasicFilter = ""
    cbWhere5:LIST-ITEMS IN FRAME fBasicFilter = ""
    cbWhere6:LIST-ITEMS IN FRAME fBasicFilter = "".
        

IF cLimit = "Full" THEN DO: 
    
    main:
    DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:                                               
        ASSIGN
            hThisField = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
        
        DO iCtr2 = 1 TO hBuffer:NUM-FIELDS:
            hField = hBuffer:BUFFER-FIELD(iCtr2).
            IF hThisField:NAME = hField:NAME THEN DO:
                IF hField:EXTENT > 0 THEN NEXT main.
            END.
        END.
        
        cbWhere1:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
        cbWhere2:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
        cbWhere3:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
        cbWhere4:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
        cbWhere5:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
        cbWhere6:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
    END.
END.
ELSE DO iCtr = 1 TO hDynBrowse:NUM-COLUMNS:   
    ASSIGN
        hThisField = hDynBrowse:GET-BROWSE-COLUMN(iCtr).
    FIND FIRST _file WHERE _file._file-name = hBuffer:TABLE NO-LOCK NO-ERROR.
    IF AVAIL _file THEN DO:
        checkindex:
        FOR EACH _index OF _file, EACH _index-field OF _index WHERE /* _index-field._index-seq = 1 */, EACH _field OF _index-field:
            IF _field._field-name = hThisField:NAME THEN do:
                IF cbWhere1:LIST-ITEMS  IN FRAME fBasicFilter= ? OR NOT CAN-DO(cbWhere1:LIST-ITEMS IN FRAME fBasicFilter,hThisField:NAME) THEN DO:
                    cbWhere1:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
                    cbWhere2:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
                    cbWhere3:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
                    cbWhere4:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
                    cbWhere5:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
                    cbWhere6:ADD-LAST(hThisField:NAME) IN FRAME fBasicFilter.
                END.
                LEAVE checkindex.
            END.
        END.            
    END.
END.
END.

ASSIGN
    cbWhere1:SCREEN-VALUE IN FRAME fBasicFilter = IF CAN-DO(cbWhere1:LIST-ITEMS IN FRAME fBasicFilter,cOldData[1]) THEN cOldData[1] ELSE "" 
    cbWhere2:SCREEN-VALUE IN FRAME fBasicFilter = IF CAN-DO(cbWhere2:LIST-ITEMS IN FRAME fBasicFilter,cOldData[2]) THEN cOldData[2] ELSE ""  
    cbWhere3:SCREEN-VALUE IN FRAME fBasicFilter = IF CAN-DO(cbWhere3:LIST-ITEMS IN FRAME fBasicFilter,cOldData[3]) THEN cOldData[3] ELSE ""  
    cbWhere4:SCREEN-VALUE IN FRAME fBasicFilter = IF CAN-DO(cbWhere4:LIST-ITEMS IN FRAME fBasicFilter,cOldData[4]) THEN cOldData[4] ELSE "" 
    cbWhere5:SCREEN-VALUE IN FRAME fBasicFilter = IF CAN-DO(cbWhere5:LIST-ITEMS IN FRAME fBasicFilter,cOldData[5]) THEN cOldData[5] ELSE "" 
    cbWhere6:SCREEN-VALUE IN FRAME fBasicFilter = IF CAN-DO(cbWhere6:LIST-ITEMS IN FRAME fBasicFilter,cOldData[6]) THEN cOldData[6] ELSE "" .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

