&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: est/d-panelDetails.w

  Description: Dialog box to display panel sizes

  Input Parameters: 
      <none>

  Output Parameters:
      <none>

  Author: Mithun Porandla

  Created: 12/31/2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{system/FormulaProcs.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipriInput   AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFluteID  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcScoreSet AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cEstimateNo        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFormNo            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBlankNo           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPOId              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPOLine            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cStyleID           AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdFormulaProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentPanelType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentSizeFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCurrentSizeFactor AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDecimalFlag       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFormulaLength     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormulaWidth      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPanelTypeWidth    AS CHARACTER NO-UNDO INITIAL "W".
DEFINE VARIABLE cPanelTypeLength   AS CHARACTER NO-UNDO INITIAL "L".
DEFINE VARIABLE cSizeFormat16ths   AS CHARACTER NO-UNDO INITIAL "16th's".
DEFINE VARIABLE cSizeFormat32nds   AS CHARACTER NO-UNDO INITIAL "32nd's".
DEFINE VARIABLE cSizeFormatDecimal AS CHARACTER NO-UNDO INITIAL "Decimal".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rdSizeFormat fiPanel-1 cbType-1 ~
fiPanel-2 cbType-2 fiPanel-3 cbType-3 fiPanel-4 cbType-4 fiPanel-5 cbType-5 ~
fiPanel-6 cbType-6 fiPanel-7 cbType-7 fiPanel-8 cbType-8 fiPanel-9 cbType-9 ~
fiPanel-10 cbType-10 fiPanel-11 cbType-11 fiPanel-12 cbType-12 fiPanel-13 ~
cbType-13 fiPanel-14 cbType-14 fiPanel-15 cbType-15 fiPanel-16 cbType-16 ~
fiPanel-17 cbType-17 fiPanel-18 cbType-18 fiPanel-19 cbType-19 cbType-20 ~
fiPanel-20 btSave btnCancel rdPanelType 
&Scoped-Define DISPLAYED-OBJECTS rdSizeFormat fiCustomField1 fiCustomField2 ~
fiStyleFormula fiPanelLabel cbTypeLabel fiFormulaLabel ~
fiScoreAllowanceLabel fiPanel-1 cbType-1 fiFormula-1 fiScoreAllowance-1 ~
fiPanel-2 cbType-2 fiFormula-2 fiScoreAllowance-2 fiPanel-3 cbType-3 ~
fiFormula-3 fiScoreAllowance-3 fiPanel-4 cbType-4 fiFormula-4 ~
fiScoreAllowance-4 fiPanel-5 cbType-5 fiFormula-5 fiScoreAllowance-5 ~
fiPanel-6 cbType-6 fiFormula-6 fiScoreAllowance-6 fiPanel-7 cbType-7 ~
fiFormula-7 fiScoreAllowance-7 fiPanel-8 cbType-8 fiFormula-8 ~
fiScoreAllowance-8 fiPanel-9 cbType-9 fiFormula-9 fiScoreAllowance-9 ~
fiPanel-10 cbType-10 fiFormula-10 fiScoreAllowance-10 fiPanel-11 cbType-11 ~
fiFormula-11 fiScoreAllowance-11 fiPanel-12 cbType-12 fiFormula-12 ~
fiScoreAllowance-12 fiPanel-13 cbType-13 fiFormula-13 fiScoreAllowance-13 ~
fiPanel-14 cbType-14 fiFormula-14 fiScoreAllowance-14 fiPanel-15 cbType-15 ~
fiFormula-15 fiScoreAllowance-15 fiPanel-16 cbType-16 fiFormula-16 ~
fiScoreAllowance-16 fiPanel-17 cbType-17 fiFormula-17 fiScoreAllowance-17 ~
fiPanel-18 cbType-18 fiFormula-18 fiScoreAllowance-18 fiPanel-19 cbType-19 ~
fiFormula-19 fiScoreAllowance-19 cbType-20 fiFormula-20 fiScoreAllowance-20 ~
fiPanel-20 rdPanelType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U
     LABEL "Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cbType-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1"," Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbType-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbTypeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Type - Description" 
     VIEW-AS FILL-IN 
     SIZE 33.2 BY .81 NO-UNDO.

DEFINE VARIABLE fiCustomField1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 19.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustomField2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Style Code" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiFormula-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormula-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormulaLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Formula" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE fiPanel-1 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-10 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-11 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-12 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-13 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-14 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-15 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-16 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-17 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-18 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-19 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-2 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-20 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-3 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-4 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-5 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-6 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-7 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-8 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanel-9 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPanelLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Panel Size" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-1 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-10 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-11 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-12 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-13 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-14 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-15 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-16 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-17 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-18 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-19 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-2 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-20 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-3 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-4 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-5 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-6 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-7 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-8 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowance-9 AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiScoreAllowanceLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Allowance" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE fiStyleFormula AS CHARACTER FORMAT "X(256)":U 
     LABEL "Formula" 
     VIEW-AS FILL-IN 
     SIZE 49.8 BY 1 NO-UNDO.

DEFINE VARIABLE rdPanelType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Show Widths", "1",
"Show Lengths", "2"
     SIZE 38 BY 1.38 NO-UNDO.

DEFINE VARIABLE rdSizeFormat AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Show 16th's", "1",
"Show 32nd's", "2",
"Decimal", "3"
     SIZE 19.4 BY 2.43 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67.8 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rdSizeFormat AT ROW 1.24 COL 82.4 NO-LABEL WIDGET-ID 186
     fiCustomField1 AT ROW 1.33 COL 16.2 COLON-ALIGNED WIDGET-ID 172
     fiCustomField2 AT ROW 1.33 COL 51.8 COLON-ALIGNED WIDGET-ID 174
     fiStyleFormula AT ROW 2.43 COL 16.2 COLON-ALIGNED WIDGET-ID 194
     fiPanelLabel AT ROW 4.33 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 256
     cbTypeLabel AT ROW 4.33 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 258
     fiFormulaLabel AT ROW 4.33 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 260
     fiScoreAllowanceLabel AT ROW 4.33 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 262
     fiPanel-1 AT ROW 5.19 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     cbType-1 AT ROW 5.19 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiFormula-1 AT ROW 5.19 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiScoreAllowance-1 AT ROW 5.19 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiPanel-2 AT ROW 6.38 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     cbType-2 AT ROW 6.38 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiFormula-2 AT ROW 6.38 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fiScoreAllowance-2 AT ROW 6.38 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fiPanel-3 AT ROW 7.57 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     cbType-3 AT ROW 7.57 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fiFormula-3 AT ROW 7.57 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiScoreAllowance-3 AT ROW 7.57 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiPanel-4 AT ROW 8.71 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     cbType-4 AT ROW 8.71 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     fiFormula-4 AT ROW 8.71 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     fiScoreAllowance-4 AT ROW 8.71 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fiPanel-5 AT ROW 9.91 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     cbType-5 AT ROW 9.91 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fiFormula-5 AT ROW 9.91 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiScoreAllowance-5 AT ROW 9.91 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fiPanel-6 AT ROW 11.1 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     cbType-6 AT ROW 11.1 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     fiFormula-6 AT ROW 11.1 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     fiScoreAllowance-6 AT ROW 11.1 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fiPanel-7 AT ROW 12.29 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cbType-7 AT ROW 12.29 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fiFormula-7 AT ROW 12.29 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fiScoreAllowance-7 AT ROW 12.29 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fiPanel-8 AT ROW 13.48 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     cbType-8 AT ROW 13.48 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     fiFormula-8 AT ROW 13.48 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fiScoreAllowance-8 AT ROW 13.48 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     fiPanel-9 AT ROW 14.67 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     cbType-9 AT ROW 14.67 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     fiFormula-9 AT ROW 14.67 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     fiScoreAllowance-9 AT ROW 14.67 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 72
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     fiPanel-10 AT ROW 15.86 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     cbType-10 AT ROW 15.86 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fiFormula-10 AT ROW 15.86 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiScoreAllowance-10 AT ROW 15.86 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     fiPanel-11 AT ROW 17.1 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     cbType-11 AT ROW 17.1 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     fiFormula-11 AT ROW 17.1 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     fiScoreAllowance-11 AT ROW 17.1 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fiPanel-12 AT ROW 18.29 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     cbType-12 AT ROW 18.29 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     fiFormula-12 AT ROW 18.29 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     fiScoreAllowance-12 AT ROW 18.29 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     fiPanel-13 AT ROW 19.48 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     cbType-13 AT ROW 19.48 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fiFormula-13 AT ROW 19.48 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     fiScoreAllowance-13 AT ROW 19.48 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiPanel-14 AT ROW 20.67 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     cbType-14 AT ROW 20.67 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiFormula-14 AT ROW 20.67 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiScoreAllowance-14 AT ROW 20.67 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     fiPanel-15 AT ROW 21.86 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     cbType-15 AT ROW 21.86 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     fiFormula-15 AT ROW 21.86 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fiScoreAllowance-15 AT ROW 21.86 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fiPanel-16 AT ROW 23.05 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     cbType-16 AT ROW 23.05 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     fiFormula-16 AT ROW 23.05 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     fiScoreAllowance-16 AT ROW 23.05 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     fiPanel-17 AT ROW 24.24 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     cbType-17 AT ROW 24.24 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     fiFormula-17 AT ROW 24.24 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 132
     fiScoreAllowance-17 AT ROW 24.24 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     fiPanel-18 AT ROW 25.43 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     cbType-18 AT ROW 25.43 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 138
     fiFormula-18 AT ROW 25.43 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 140
     fiScoreAllowance-18 AT ROW 25.43 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 144
     fiPanel-19 AT ROW 26.62 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     cbType-19 AT ROW 26.62 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     fiFormula-19 AT ROW 26.62 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     fiScoreAllowance-19 AT ROW 26.62 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 152
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     cbType-20 AT ROW 27.76 COL 27.8 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     fiFormula-20 AT ROW 27.76 COL 62.6 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     fiScoreAllowance-20 AT ROW 27.76 COL 91.6 COLON-ALIGNED NO-LABEL WIDGET-ID 160
     fiPanel-20 AT ROW 27.81 COL 12.2 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     btSave AT ROW 29.29 COL 53.8 NO-TAB-STOP 
     btnCancel AT ROW 29.29 COL 62.8 NO-TAB-STOP 
     rdPanelType AT ROW 29.67 COL 3 NO-LABEL WIDGET-ID 190
     "Panel 13:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 19.62 COL 3.2 WIDGET-ID 240
     "Panel 15:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 22 COL 3.2 WIDGET-ID 244
     "Panel 16:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 23.19 COL 3.2 WIDGET-ID 246
     "Panel 17:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 24.38 COL 3.2 WIDGET-ID 248
     "Panel 18:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 25.57 COL 3.2 WIDGET-ID 250
     "Panel 19:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 26.76 COL 3.2 WIDGET-ID 252
     "Panel 20:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 27.95 COL 3.2 WIDGET-ID 254
     "Panel 3:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 7.67 COL 4.6 WIDGET-ID 200
     "Panel 2:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 6.52 COL 4.6 WIDGET-ID 198
     "Panel 7:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 12.43 COL 4.6 WIDGET-ID 208
     "Panel 6:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 11.29 COL 4.6 WIDGET-ID 206
     "Panel 5:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 10.1 COL 4.6 WIDGET-ID 204
     "Panel 4:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.86 COL 4.6 WIDGET-ID 202
     "Panel 1:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 5.29 COL 4.6 WIDGET-ID 196
     "Panel 9:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 14.86 COL 4.6 WIDGET-ID 212
     "Panel 14:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 20.81 COL 3.2 WIDGET-ID 242
     "Panel 8:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 13.67 COL 4.6 WIDGET-ID 210
     "Panel 10:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 16 COL 3.2 WIDGET-ID 214
     "Panel 11:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 17.24 COL 3.2 WIDGET-ID 236
     "Panel 12:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 18.43 COL 3.2 WIDGET-ID 238
     RECT-1 AT ROW 1.05 COL 2.2 WIDGET-ID 264
     SPACE(39.19) SKIP(27.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FONT 6
         TITLE "Panel Sizes"
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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cbTypeLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustomField1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCustomField2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormula-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormulaLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPanelLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowance-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScoreAllowanceLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStyleFormula IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Panel Sizes */
DO:
    IF VALID-HANDLE(hdFormulaProcs) THEN
        DELETE PROCEDURE hdFormulaProcs.
        
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave Dialog-Frame
ON CHOOSE OF btSave IN FRAME Dialog-Frame /* Save */
DO:
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    RUN pSavePanels.

    IF cCurrentSizeFormat NE cSizeFormatDecimal THEN
        RUN SwitchPanelSizeFormatForttPanel IN hdFormulaProcs (
            INPUT        cCurrentSizeFormat,
            INPUT        cSizeFormatDecimal,
            INPUT-OUTPUT TABLE ttPanel
            ).

    IF ipcType EQ "eb" THEN
        RUN UpdatePanelDetailsForEstimate IN hdFormulaProcs (
            INPUT cCompany,
            INPUT cEstimateNo,
            INPUT iFormNo,
            INPUT iBlankNo,
            INPUT TABLE ttPanel
            ).
    ELSE IF ipcType EQ "po-ordl" THEN DO:                
        DO TRANSACTION: 
            FIND FIRST bf-po-ordl EXCLUSIVE-LOCK
                WHERE ROWID(bf-po-ordl) EQ ipriInput
                NO-ERROR.
            IF AVAILABLE bf-po-ordl THEN
                bf-po-ordl.spare-char-1 = IF rdPanelType:SCREEN-VALUE EQ "1" THEN
                    "WIDTH"
                    ELSE
                    "LENGTH".
        END.           
        RELEASE bf-po-ordl.
        
        RUN UpdatePanelDetailsForPO IN hdFormulaProcs (
            INPUT cCompany,
            INPUT iPOId,
            INPUT iPOLine,
            INPUT TABLE ttPanel
            ).

    END.
    ELSE IF ipcType EQ "style" THEN
        RUN UpdatePanelDetailsForStyle IN hdFormulaProcs (
            INPUT cCompany,
            INPUT cStyleID,
            INPUT ipcFluteID,
            INPUT ipcScoreSet,
            INPUT TABLE ttPanel
            ).
                                
    IF cCurrentSizeFormat NE cSizeFormatDecimal THEN
        RUN SwitchPanelSizeFormatForttPanel IN hdFormulaProcs (
            INPUT        cSizeFormatDecimal,
            INPUT        cCurrentSizeFormat,
            INPUT-OUTPUT TABLE ttPanel
            ).
            
    MESSAGE "Data saved successfully"
        VIEW-AS ALERT-BOX INFORMATION.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-1 Dialog-Frame
ON LEAVE OF fiPanel-1 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-10 Dialog-Frame
ON LEAVE OF fiPanel-10 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-11 Dialog-Frame
ON LEAVE OF fiPanel-11 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-12 Dialog-Frame
ON LEAVE OF fiPanel-12 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-13 Dialog-Frame
ON LEAVE OF fiPanel-13 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-14 Dialog-Frame
ON LEAVE OF fiPanel-14 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-15 Dialog-Frame
ON LEAVE OF fiPanel-15 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-16 Dialog-Frame
ON LEAVE OF fiPanel-16 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-17 Dialog-Frame
ON LEAVE OF fiPanel-17 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-18 Dialog-Frame
ON LEAVE OF fiPanel-18 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-19 Dialog-Frame
ON LEAVE OF fiPanel-19 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-2 Dialog-Frame
ON LEAVE OF fiPanel-2 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-20 Dialog-Frame
ON LEAVE OF fiPanel-20 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-3 Dialog-Frame
ON LEAVE OF fiPanel-3 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-4 Dialog-Frame
ON LEAVE OF fiPanel-4 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-5 Dialog-Frame
ON LEAVE OF fiPanel-5 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-6 Dialog-Frame
ON LEAVE OF fiPanel-6 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-7 Dialog-Frame
ON LEAVE OF fiPanel-7 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-8 Dialog-Frame
ON LEAVE OF fiPanel-8 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPanel-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPanel-9 Dialog-Frame
ON LEAVE OF fiPanel-9 IN FRAME Dialog-Frame
DO:
    RUN ValidatePanelSize IN hdFormulaProcs (
        INPUT  DECIMAL(SELF:SCREEN-VALUE),
        INPUT  cCurrentSizeFormat,
        INPUT  lDecimalFlag,
        OUTPUT cMessage,
        OUTPUT lSuccess
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rdPanelType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rdPanelType Dialog-Frame
ON VALUE-CHANGED OF rdPanelType IN FRAME Dialog-Frame
DO:
    RUN pSavePanels.
    
    IF SELF:SCREEN-VALUE EQ "1" THEN
        ASSIGN
            fiStyleFormula:SCREEN-VALUE = cFormulaWidth
            cCurrentPanelType           = cPanelTypeWidth
            .
    ELSE
        ASSIGN
            fiStyleFormula:SCREEN-VALUE = cFormulaLength       
            cCurrentPanelType           = cPanelTypeLength
            .

    RUN pLoadlengthWidth (
        INPUT cCurrentPanelType
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rdSizeFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rdSizeFormat Dialog-Frame
ON VALUE-CHANGED OF rdSizeFormat IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE cSizeFormat AS CHARACTER NO-UNDO.
    
    IF SELF:SCREEN-VALUE EQ "1" THEN
        cSizeFormat = cSizeFormat16ths.
    ELSE IF SELF:SCREEN-VALUE EQ "2" THEN
        cSizeFormat = cSizeFormat32nds.
    ELSE IF SELF:SCREEN-VALUE EQ "3" THEN
        cSizeFormat = cSizeFormatDecimal.
        
    RUN SwitchPanelSizeFormatForttPanel IN hdFormulaProcs (
        INPUT        cCurrentSizeFormat,
        INPUT        cSizeFormat,
        INPUT-OUTPUT TABLE ttPanel
        ).  
    
    RUN pLoadLengthWidth (
        INPUT cCurrentPanelType
        ).
    
    cCurrentSizeFormat = cSizeFormat.
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
    
    RUN pInit.
    
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
  DISPLAY rdSizeFormat fiCustomField1 fiCustomField2 fiStyleFormula fiPanelLabel 
          cbTypeLabel fiFormulaLabel fiScoreAllowanceLabel fiPanel-1 cbType-1 
          fiFormula-1 fiScoreAllowance-1 fiPanel-2 cbType-2 fiFormula-2 
          fiScoreAllowance-2 fiPanel-3 cbType-3 fiFormula-3 fiScoreAllowance-3 
          fiPanel-4 cbType-4 fiFormula-4 fiScoreAllowance-4 fiPanel-5 cbType-5 
          fiFormula-5 fiScoreAllowance-5 fiPanel-6 cbType-6 fiFormula-6 
          fiScoreAllowance-6 fiPanel-7 cbType-7 fiFormula-7 fiScoreAllowance-7 
          fiPanel-8 cbType-8 fiFormula-8 fiScoreAllowance-8 fiPanel-9 cbType-9 
          fiFormula-9 fiScoreAllowance-9 fiPanel-10 cbType-10 fiFormula-10 
          fiScoreAllowance-10 fiPanel-11 cbType-11 fiFormula-11 
          fiScoreAllowance-11 fiPanel-12 cbType-12 fiFormula-12 
          fiScoreAllowance-12 fiPanel-13 cbType-13 fiFormula-13 
          fiScoreAllowance-13 fiPanel-14 cbType-14 fiFormula-14 
          fiScoreAllowance-14 fiPanel-15 cbType-15 fiFormula-15 
          fiScoreAllowance-15 fiPanel-16 cbType-16 fiFormula-16 
          fiScoreAllowance-16 fiPanel-17 cbType-17 fiFormula-17 
          fiScoreAllowance-17 fiPanel-18 cbType-18 fiFormula-18 
          fiScoreAllowance-18 fiPanel-19 cbType-19 fiFormula-19 
          fiScoreAllowance-19 cbType-20 fiFormula-20 fiScoreAllowance-20 
          fiPanel-20 rdPanelType 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 rdSizeFormat fiPanel-1 cbType-1 fiPanel-2 cbType-2 fiPanel-3 
         cbType-3 fiPanel-4 cbType-4 fiPanel-5 cbType-5 fiPanel-6 cbType-6 
         fiPanel-7 cbType-7 fiPanel-8 cbType-8 fiPanel-9 cbType-9 fiPanel-10 
         cbType-10 fiPanel-11 cbType-11 fiPanel-12 cbType-12 fiPanel-13 
         cbType-13 fiPanel-14 cbType-14 fiPanel-15 cbType-15 fiPanel-16 
         cbType-16 fiPanel-17 cbType-17 fiPanel-18 cbType-18 fiPanel-19 
         cbType-19 cbType-20 fiPanel-20 btSave btnCancel rdPanelType 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPanelValues Dialog-Frame 
PROCEDURE pGetPanelValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiPanelNumber    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPanelSize      AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormula        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScoreAllowance AS DECIMAL   NO-UNDO.     
    
    DEFINE VARIABLE hdWidget AS HANDLE    NO-UNDO.

    hdWidget = FRAME DIALOG-FRAME:FIRST-CHILD:FIRST-CHILD.
    DO WHILE VALID-HANDLE(hdWidget):
        IF hdWidget:NAME = "fiPanel-" + STRING(ipiPanelNumber) THEN
            opdPanelSize = DECIMAL(hdWidget:SCREEN-VALUE).
        ELSE IF hdWidget:NAME = "cbType-" + STRING(ipiPanelNumber) THEN
            opcType = IF hdWidget:SCREEN-VALUE EQ ? THEN "" ELSE hdWidget:SCREEN-VALUE.
        ELSE IF hdWidget:NAME = "fiFormula-" + STRING(ipiPanelNumber) THEN
            opcFormula = hdWidget:SCREEN-VALUE.
        ELSE IF hdWidget:NAME = "fiScoreAllowance-" + STRING(ipiPanelNumber) THEN
            opdScoreAllowance = DECIMAL(hdWidget:SCREEN-VALUE).        
  
        hdWidget = hdWidget:NEXT-SIBLING.
    END.
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
    DEFINE BUFFER bf-eb      FOR eb.
    DEFINE BUFFER bf-style   FOR style.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
    
    RUN spGetSessionParam (
        INPUT  "Company",
        OUTPUT cCompany
        ).
        
    cCurrentPanelType = cPaneltypeWidth.

    RUN pUpdateComboBoxes.   

    IF ipcType EQ "style" THEN DO:
        FIND FIRST bf-style NO-LOCK
             WHERE ROWID(bf-style) EQ ipriInput
             NO-ERROR.
        IF NOT AVAILABLE bf-style THEN
            RETURN.

        RUN pSetWidgetProperties (
            INPUT  "fiPanel",
            INPUT  TRUE,  /* Hidden */
            INPUT  FALSE, /* Sensitive */
            INPUT  -1
            ).

        RUN pSetWidgetProperties (
            INPUT  "fiFormula",
            INPUT  TRUE,  /* Hidden */
            INPUT  FALSE, /* Sensitive */
            INPUT  -1
            ).

        RUN pSetWidgetProperties (
            INPUT  "fiScoreAllowance",
            INPUT  FALSE, /* Hidden */
            INPUT  TRUE,  /* Sensitive */
            INPUT  14
            ).
                        
        RUN pSetWidgetProperties (
            INPUT  "cbType",
            INPUT  FALSE,  /* Hidden */
            INPUT  TRUE,   /* Sensitive */
            INPUT  32
            ).
        
        rdPanelType:HIDDEN = TRUE.
    END.        
    ELSE IF ipcType EQ "po-ordl" THEN DO:
        FIND FIRST bf-po-ordl NO-LOCK
             WHERE ROWID(bf-po-ordl) EQ ipriInput
             NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            RETURN.            
    END.    
    ELSE IF ipcType EQ "eb" THEN DO:            
        FIND FIRST bf-eb NO-LOCK 
             WHERE ROWID(bf-eb) EQ ipriInput
             NO-ERROR.
        IF NOT AVAILABLE bf-eb THEN
            RETURN.        
    END.

    IF AVAILABLE bf-eb THEN
        FIND FIRST bf-style NO-LOCK 
             WHERE bf-style.company EQ bf-eb.company
               AND bf-style.style   EQ bf-eb.style
             NO-ERROR.
    
    IF AVAILABLE bf-po-ordl THEN
        ASSIGN
            iPOId   = bf-po-ordl.po-no
            iPOLine = bf-po-ordl.line
            .

    IF AVAILABLE bf-eb THEN
        ASSIGN
            cEstimateNo = bf-eb.est-no
            iFormNo     = bf-eb.form-no
            iBlankNo    = bf-eb.blank-no
            .

    IF AVAILABLE bf-style THEN            
        ASSIGN
            cStyleID       = bf-style.style
            cFormulaLength = bf-style.formula[2]
            cFormulaWidth  = IF ipcScoreSet EQ "POBlankWidth" THEN 
                                 bf-style.formula[20]
                             ELSE IF bf-style.formula[20] NE "" THEN
                                 bf-style.formula[20]
                             ELSE 
                                 bf-style.formula[1]
            .
    
    IF ipcType EQ "eb" THEN
        ASSIGN
            fiCustomField1:SCREEN-VALUE = cEstimateNo
            fiCustomField2:SCREEN-VALUE = cStyleID
            .
    ELSE IF ipcType EQ "po-ordl" THEN
        ASSIGN
            fiCustomField1:LABEL        = "PO #"
            fiCustomField2:LABEL        = "Line #"
            fiCustomField1:SCREEN-VALUE = STRING(iPOId)
            fiCustomField2:SCREEN-VALUE = STRING(iPOLine)
            .
    ELSE IF ipcType EQ "style" THEN
        ASSIGN
            fiCustomField1:LABEL        = "Style"
            fiCustomField2:LABEL        = "Flute"
            fiCustomField1:SCREEN-VALUE = cStyleID
            fiCustomField2:SCREEN-VALUE = ipcFluteID
            .
        
    EMPTY TEMP-TABLE ttPanel.

    RUN GetSizeFactor IN hdFormulaProcs (
        INPUT  cCompany,
        OUTPUT dCurrentSizeFactor,
        OUTPUT cCurrentSizeFormat,
        OUTPUT lDecimalFlag
        ).

    IF ipcType EQ "style" THEN        
        RUN GetPanelDetailsForStyle IN hdFormulaProcs (
            INPUT  cCompany,
            INPUT  cStyleID,
            INPUT  ipcFluteID,
            INPUT  ipcScoreSet,
            OUTPUT TABLE ttPanel
            ).
    ELSE IF ipcType EQ "eb" THEN DO:        
        RUN Formula_BuildAndSavePanelDetailsForEstimate IN hdFormulaProcs (
            INPUT  ipriInput
            ).
        RUN Formula_GetPanelDetailsForPOScores IN hdFormulaProcs (
            INPUT  cCompany,
            INPUT  cEstimateNo,
            INPUT  iFormNo,
            INPUT  iBlankNo,
            OUTPUT TABLE ttPanel
            ).        
    END.
    ELSE IF ipcType EQ "po-ordl" THEN DO:
        RUN Formula_BuildAndSavePanelDetailsForPO IN hdFormulaProcs (
            INPUT  ipriInput
            ).     
        RUN GetPanelDetailsForPO IN hdFormulaProcs (
            INPUT  cCompany,
            INPUT  iPOID,
            INPUT  iPOLine,
            OUTPUT TABLE ttPanel
            ).     
        
        RUN Formula_GetFormulaFromttPanel IN hdFormulaProcs (
            INPUT  TABLE ttPanel BY-REFERENCE,
            OUTPUT cFormulaLength,
            OUTPUT cFormulaWidth
            ).               
    END.

    IF cCurrentSizeFormat NE cSizeFormatDecimal THEN DO:
        RUN SwitchPanelSizeFormatForttPanel IN hdFormulaProcs (
            INPUT        cSizeFormatDecimal,
            INPUT        cCurrentSizeFormat,
            INPUT-OUTPUT TABLE ttPanel
            ).
    END.
        
    RUN pUpdateComboBoxes.
            
    IF ipcType EQ "style" THEN
        cCurrentPanelType = "".
    
    ASSIGN
        rdPanelType:SCREEN-VALUE    = "1"
        fiStyleFormula:SCREEN-VALUE = cFormulaWidth
        .

    IF ipcType EQ "po-ordl" THEN DO:
        IF bf-po-ordl.spare-char-1 EQ "LENGTH" THEN
            ASSIGN
                cCurrentPanelType           = cPanelTypeLength
                rdPanelType:SCREEN-VALUE    = "2"
                fiStyleFormula:SCREEN-VALUE = cFormulaLength
                .
        ELSE
            ASSIGN
                cCurrentPanelType           = cPanelTypeWidth
                rdPanelType:SCREEN-VALUE    = "1"
                fiStyleFormula:SCREEN-VALUE = cFormulaWidth
                .
    END.        
    
    IF cCurrentSizeFormat EQ cSizeFormat16ths THEN
        rdSizeFormat:SCREEN-VALUE = "1".
    ELSE IF cCurrentSizeFormat EQ cSizeFormat32nds THEN
        rdSizeFormat:SCREEN-VALUE = "2".
    ELSE IF cCurrentSizeFormat EQ cSizeFormatDecimal THEN
        rdSizeFormat:SCREEN-VALUE = "3".

    RUN pLoadLengthWidth (
        INPUT cCurrentPanelType
        ).

    RELEASE bf-eb.
    RELEASE bf-style.    
    RELEASE bf-po-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadLengthWidth Dialog-Frame 
PROCEDURE pLoadLengthWidth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPanelType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF ipcType EQ "style" THEN
        ipcPanelType = "".
    
    DO iIndex = 1 TO 20:
        RUN pUpdatePanel (
            INPUT iIndex,
            INPUT 0,
            INPUT " ", /* Make sure to send space instead of empty. Combo-boxes once set to a non-empty value will not set back to a empty value */
            INPUT "",
            INPUT 0
            ).
    END.
    
    iIndex = 0.
    DO iIndex = 1 TO 20:
        FIND FIRST ttPanel
             WHERE ttPanel.cPanelType = ipcPanelType
               AND ttPanel.iPanelNum  = iIndex
             NO-ERROR.
        IF AVAILABLE ttPanel THEN DO:
            RUN pUpdatePanel (
                INPUT ttPanel.iPanelNum,
                INPUT ttPanel.dPanelSize,
                INPUT ttPanel.cScoreType,
                INPUT ttPanel.cPanelFormula,
                INPUT ttPanel.dScoringAllowance
                ).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSavePanels Dialog-Frame 
PROCEDURE pSavePanels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dPanelSize        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cScoreType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPanelFormula     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dScoringAllowance AS DECIMAL   NO-UNDO.

    DO iIndex = 1 TO 20:
        RUN pGetPanelValues (
            INPUT  iIndex,
            OUTPUT dPanelSize,
            OUTPUT cScoreType,
            OUTPUT cPanelFormula,
            OUTPUT dScoringAllowance
            ).

        FIND FIRST ttPanel
             WHERE ttPanel.cPanelType = cCurrentpanelType
               AND ttPanel.iPanelNum  = iIndex
             NO-ERROR.
        IF AVAILABLE ttPanel THEN
            ASSIGN
                ttPanel.cPanelFormula         = cPanelFormula
                ttPanel.dScoringAllowance     = dScoringAllowance
                ttPanel.cScoreType            = cScoreType
                ttPanel.dPanelSize            = dPanelSize
                .
        ELSE DO:
            /* Validate cScoreType with a space instead of an empty value. Combo-boxes although displayed empty is actually a space */
            IF dPanelSize NE 0 OR cScoreType NE " " OR dScoringAllowance NE 0 THEN DO:
                CREATE ttPanel.
                ASSIGN
                    ttPanel.cPanelType            = cCurrentPanelType
                    ttPanel.iPanelNum             = iIndex
                    ttPanel.cPanelFormula         = cPanelFormula
                    ttPanel.dScoringAllowance     = dScoringAllowance
                    ttPanel.cScoreType            = cScoreType
                    ttPanel.dPanelSize            = dPanelSize
                    ttPanel.dPanelSizeFromFormula = dPanelSize
                    .                    
            END.      
        END.
    END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetWidgetProperties Dialog-Frame 
PROCEDURE pSetWidgetProperties PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcWidgetType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplHidden     AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive  AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdColumn     AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE hdWidget     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iPanelNumber AS INTEGER NO-UNDO.
    
    hdWidget = FRAME DIALOG-FRAME:FIRST-CHILD:FIRST-CHILD.
    DO WHILE VALID-HANDLE(hdWidget):        
        IF hdWidget:NAME BEGINS ipcWidgetType THEN
            ASSIGN
                hdWidget:SENSITIVE = iplSensitive
                hdWidget:HIDDEN    = iplHidden
                hdWidget:COLUMN    = IF ipdColumn EQ -1 THEN
                                         hdWidget:COLUMN
                                     ELSE
                                         ipdColumn
                .      
        
        IF hdWidget:NAME BEGINS ipcWidgetType + "Label" THEN
            hdWidget:SENSITIVE = FALSE.
            
        hdWidget = hdWidget:NEXT-SIBLING.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateComboBoxes Dialog-Frame 
PROCEDURE pUpdateComboBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdWidget   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cScoreType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-scoreType FOR scoreType.
    
    FOR EACH scoreType NO-LOCK 
        WHERE scoreType.company EQ cCompany:
        cScoreType = cScoreType
                   + scoreType.scoreType + " - "
                   + scoreType.description + ","
                   + scoreType.scoreType + ","
                   .
    END.
    
    /* Make sure to add a space instead of empty value to the list-items of combo-boxes. 
       As combo-boxes once set to a non-empty value will not set back to a empty value */
    IF cScoreType NE "" THEN
        cScoreType = ", ," + TRIM(cScoreType,",").
    ELSE
        cScoreType = ",".
        
    hdWidget = FRAME DIALOG-FRAME:FIRST-CHILD:FIRST-CHILD.
    
    DO WHILE VALID-HANDLE(hdWidget):
        IF hdWidget:NAME BEGINS "cbType-" THEN
            hdWidget:LIST-ITEM-PAIRS = cScoreType NO-ERROR.

        hdWidget = hdWidget:NEXT-SIBLING.
    END.
    
    RELEASE bf-scoreType.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdatePanel Dialog-Frame 
PROCEDURE pUpdatePanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiPanelNumber    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdPanelSize      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPanelType      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormula        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdScoreAllowance AS DECIMAL   NO-UNDO.     
    
    DEFINE VARIABLE hdWidget AS HANDLE    NO-UNDO.
    
    hdWidget = FRAME DIALOG-FRAME:FIRST-CHILD:FIRST-CHILD.
    DO WHILE VALID-HANDLE(hdWidget):
        IF hdWidget:NAME = "fiPanel-" + STRING(ipiPanelNumber) THEN
            hdWidget:SCREEN-VALUE = STRING(ipdPanelSize).
        ELSE IF hdWidget:NAME = "cbType-" + STRING(ipiPanelNumber) THEN
            hdWidget:SCREEN-VALUE = STRING(ipcPanelType) NO-ERROR.        
        ELSE IF hdWidget:NAME = "fiFormula-" + STRING(ipiPanelNumber) THEN
            hdWidget:SCREEN-VALUE = STRING(ipcFormula).        
        ELSE IF hdWidget:NAME = "fiScoreAllowance-" + STRING(ipiPanelNumber) THEN
            hdWidget:SCREEN-VALUE = STRING(ipdScoreAllowance).        
  
        hdWidget = hdWidget:NEXT-SIBLING.
    END.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

