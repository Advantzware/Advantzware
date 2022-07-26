&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est/ttEstimateQuantity.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ip-eb-recid  AS RECID.
DEFINE INPUT  PARAMETER iplCorrware AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstimateQuantity.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ld-msf AS DECIMAL NO-UNDO.
DEFINE VARIABLE hdEstimateProcs AS HANDLE.
DEFINE VARIABLE hdVendorCostProcs AS HANDLE NO-UNDO.

RUN est/EstimateProcs.p PERSISTENT SET hdEstimateProcs.
RUN system/VendorCostProcs.p PERSISTENT SET hdVendorCostProcs.

{system/VendorCostProcs.i}
DEFINE TEMP-TABLE tt-report LIKE report.

DEFINE TEMP-TABLE tt-count 
        FIELD iIndex AS INTEGER 
        FIELD iQuantity AS INTEGER .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-qty1 lv-rel-1 tg_RunShip-1 lv-qty2 ~
lv-rel-2 tg_RunShip-2 lv-qty3 lv-rel-3 tg_RunShip-3 lv-qty4 lv-rel-4 ~
tg_RunShip-4 lv-qty5 lv-rel-5 tg_RunShip-5 lv-qty6 lv-rel-6 tg_RunShip-6 ~
lv-qty7 lv-rel-7 tg_RunShip-7 lv-qty8 lv-rel-8 tg_RunShip-8 lv-qty9 ~
lv-rel-9 tg_RunShip-9 lv-qty10 lv-rel-10 tg_RunShip-10 lv-qty11 lv-rel-11 ~
tg_RunShip-11 lv-qty12 lv-rel-12 tg_RunShip-12 lv-qty13 lv-rel-13 ~
tg_RunShip-13 lv-qty14 lv-rel-14 tg_RunShip-14 lv-qty15 lv-rel-15 ~
tg_RunShip-15 lv-qty16 lv-rel-16 tg_RunShip-16 lv-qty17 lv-rel-17 ~
tg_RunShip-17 lv-qty18 lv-rel-18 tg_RunShip-18 lv-qty19 lv-rel-19 ~
tg_RunShip-19 lv-qty20 lv-rel-20 tg_RunShip-20 Btn_OK Btn_Cancel RECT-6 ~
rd_vendor Btn_Add-Break Btn_Clear 
&Scoped-Define DISPLAYED-OBJECTS lv-qty1 lv-rel-1 tg_RunShip-1 lv-msf-1 ~
lv-qty2 lv-rel-2 tg_RunShip-2 lv-msf-2 lv-qty3 lv-rel-3 tg_RunShip-3 ~
lv-msf-3 lv-qty4 lv-rel-4 tg_RunShip-4 lv-msf-4 lv-qty5 lv-rel-5 ~
tg_RunShip-5 lv-msf-5 lv-qty6 lv-rel-6 tg_RunShip-6 lv-msf-6 lv-qty7 ~
lv-rel-7 tg_RunShip-7 lv-msf-7 lv-qty8 lv-rel-8 tg_RunShip-8 lv-msf-8 ~
lv-msf-9 lv-qty9 lv-rel-9 tg_RunShip-9 lv-qty10 lv-rel-10 tg_RunShip-10 ~
lv-msf-10 lv-qty11 lv-rel-11 tg_RunShip-11 lv-msf-11 lv-qty12 lv-rel-12 ~
tg_RunShip-12 lv-msf-12 lv-qty13 lv-rel-13 tg_RunShip-13 lv-msf-13 lv-qty14 ~
lv-rel-14 tg_RunShip-14 lv-msf-14 lv-qty15 lv-rel-15 tg_RunShip-15 ~
lv-msf-15 lv-qty16 lv-rel-16 tg_RunShip-16 lv-msf-16 lv-qty17 lv-rel-17 ~
tg_RunShip-17 lv-msf-17 lv-qty18 lv-rel-18 tg_RunShip-18 lv-msf-18 lv-qty19 ~
lv-rel-19 tg_RunShip-19 lv-msf-19 lv-qty20 lv-rel-20 tg_RunShip-20 ~
lv-msf-20 lv-next-qty1 lv-next-qty2 lv-next-qty17 lv-next-qty16 ~
lv-next-qty15 lv-next-qty14 lv-next-qty13 lv-next-qty12 lv-next-qty11 ~
lv-next-qty10 lv-next-qty9 lv-next-qty8 lv-next-qty7 lv-next-qty6 ~
lv-next-qty5 lv-next-qty4 lv-next-qty3 lv-next-qty18 lv-next-qty19 ~
lv-next-qty20 rd_vendor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUseVendItemCost Dialog-Frame
FUNCTION fUseVendItemCost RETURNS LOGICAL PRIVATE
  (ipcCompany AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add-Break  
     LABEL "Add Break Quantities" 
     SIZE 23.6 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Clear  
     LABEL "Clear Quantities" 
     SIZE 23.6 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK  
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE rd_vendor AS CHARACTER FORMAT "x(15)":U INITIAL "1" 
     LABEL "Vendor" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 24 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-msf-1 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-10 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-11 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-12 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-13 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-14 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-15 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-16 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-17 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-18 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-19 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-2 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-20 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-3 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-4 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-5 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-6 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-7 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-8 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-9 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-next-qty1 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty10 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty11 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty12 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty13 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty14 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty15 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty16 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty17 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty18 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty19 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty2 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty20 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty3 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty4 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty5 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty6 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty7 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty8 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-next-qty9 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty1 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty10 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty11 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty12 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty13 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty14 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty15 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty16 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty17 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty18 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty19 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty2 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty20 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty3 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty4 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty5 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty6 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty7 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.1.

DEFINE VARIABLE lv-qty8 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty9 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-1 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-10 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-11 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-12 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-13 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-14 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-15 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-16 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-17 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-18 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-19 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-2 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-20 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-3 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-4 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-5 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-6 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-7 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.1.

DEFINE VARIABLE lv-rel-8 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-9 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 23.1.

DEFINE VARIABLE tg_RunShip-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-16 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-18 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-19 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-20 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE tg_RunShip-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-qty1 AT ROW 2.43 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-1 AT ROW 2.43 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-1 AT ROW 2.57 COL 43 WIDGET-ID 4
     lv-msf-1 AT ROW 2.43 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty2 AT ROW 3.48 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-2 AT ROW 3.48 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-2 AT ROW 3.57 COL 43 WIDGET-ID 6
     lv-msf-2 AT ROW 3.48 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty3 AT ROW 4.48 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-3 AT ROW 4.48 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-3 AT ROW 4.57 COL 43 WIDGET-ID 8
     lv-msf-3 AT ROW 4.48 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty4 AT ROW 5.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-4 AT ROW 5.33 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-4 AT ROW 5.52 COL 43 WIDGET-ID 10
     lv-msf-4 AT ROW 5.33 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty5 AT ROW 6.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-5 AT ROW 6.29 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-5 AT ROW 6.48 COL 43 WIDGET-ID 12
     lv-msf-5 AT ROW 6.29 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty6 AT ROW 7.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-6 AT ROW 7.29 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-6 AT ROW 7.43 COL 43 WIDGET-ID 14
     lv-msf-6 AT ROW 7.29 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty7 AT ROW 8.24 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-7 AT ROW 8.24 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-7 AT ROW 8.38 COL 43 WIDGET-ID 20
     lv-msf-7 AT ROW 8.24 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty8 AT ROW 9.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-8 AT ROW 9.33 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-8 AT ROW 9.33 COL 43 WIDGET-ID 18
     lv-msf-8 AT ROW 9.33 COL 51 COLON-ALIGNED NO-LABEL
     lv-msf-9 AT ROW 10.29 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty9 AT ROW 10.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-9 AT ROW 10.33 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-9 AT ROW 10.29 COL 43 WIDGET-ID 22
     lv-qty10 AT ROW 11.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-10 AT ROW 11.29 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-10 AT ROW 11.24 COL 43 WIDGET-ID 24
     lv-msf-10 AT ROW 11.24 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty11 AT ROW 12.19 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-11 AT ROW 12.19 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-11 AT ROW 12.19 COL 43 WIDGET-ID 26
     lv-msf-11 AT ROW 12.19 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty12 AT ROW 13.14 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-12 AT ROW 13.14 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-12 AT ROW 13.14 COL 43 WIDGET-ID 28
     lv-msf-12 AT ROW 13.14 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty13 AT ROW 14.1 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-13 AT ROW 14.1 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-13 AT ROW 14.1 COL 43 WIDGET-ID 30
     lv-msf-13 AT ROW 14.1 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty14 AT ROW 15.05 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-14 AT ROW 15.05 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-14 AT ROW 15.14 COL 43 WIDGET-ID 32
     lv-msf-14 AT ROW 15.05 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty15 AT ROW 16 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-15 AT ROW 16 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-15 AT ROW 16.19 COL 43 WIDGET-ID 34
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     lv-msf-15 AT ROW 16 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty16 AT ROW 16.95 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-16 AT ROW 16.95 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-16 AT ROW 17.14 COL 43 WIDGET-ID 36
     lv-msf-16 AT ROW 16.95 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty17 AT ROW 17.91 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-17 AT ROW 17.91 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-17 AT ROW 18.1 COL 43 WIDGET-ID 38
     lv-msf-17 AT ROW 17.91 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty18 AT ROW 18.86 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-18 AT ROW 18.86 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-18 AT ROW 19 COL 43 WIDGET-ID 40
     lv-msf-18 AT ROW 18.86 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty19 AT ROW 19.81 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-19 AT ROW 19.81 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-19 AT ROW 19.95 COL 43 WIDGET-ID 42
     lv-msf-19 AT ROW 19.81 COL 51 COLON-ALIGNED NO-LABEL
     lv-qty20 AT ROW 20.76 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-20 AT ROW 20.76 COL 20 COLON-ALIGNED NO-LABEL
     tg_RunShip-20 AT ROW 20.86 COL 43 WIDGET-ID 44
     lv-msf-20 AT ROW 20.76 COL 51 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 22.43 COL 13
     Btn_Cancel AT ROW 22.43 COL 29
     lv-next-qty1 AT ROW 2.43 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     lv-next-qty2 AT ROW 3.43 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     lv-next-qty17 AT ROW 18 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     lv-next-qty16 AT ROW 17 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     lv-next-qty15 AT ROW 16 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     lv-next-qty14 AT ROW 15.05 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     lv-next-qty13 AT ROW 14.1 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     lv-next-qty12 AT ROW 13.14 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     lv-next-qty11 AT ROW 12.19 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     lv-next-qty10 AT ROW 11.24 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     lv-next-qty9 AT ROW 10.24 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     lv-next-qty8 AT ROW 9.29 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     lv-next-qty7 AT ROW 8.33 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     lv-next-qty6 AT ROW 7.33 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     lv-next-qty5 AT ROW 6.33 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     lv-next-qty4 AT ROW 5.33 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     lv-next-qty3 AT ROW 4.43 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     lv-next-qty18 AT ROW 19 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     lv-next-qty19 AT ROW 20 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     lv-next-qty20 AT ROW 20.86 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     rd_vendor AT ROW 1.29 COL 97 COLON-ALIGNED WIDGET-ID 258
     Btn_Add-Break AT ROW 22.43 COL 68.6 WIDGET-ID 260
     Btn_Clear AT ROW 22.43 COL 95.4 WIDGET-ID 262
     "Run Ship" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.48 COL 40 WIDGET-ID 2
          FONT 6
     "Releases" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.48 COL 25
          FONT 6
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     "Qty MSF" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.48 COL 54
          FONT 6
     "Quantity" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 6
          FONT 6
     "Next Qty" VIEW-AS TEXT
          SIZE 10.6 BY .62 AT ROW 1.48 COL 73.8 WIDGET-ID 46
          FONT 6
     RECT-6 AT ROW 1 COL 2
     SPACE(19.99) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Quantity Detail Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-next-qty9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Quantity Detail Information */
DO:
  oplError = YES.  
   IF VALID-HANDLE(hdEstimateProcs) THEN
   DELETE OBJECT hdEstimateProcs.
   IF VALID-HANDLE(hdVendorCostProcs) THEN
   DELETE OBJECT hdVendorCostProcs.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add-Break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add-Break Dialog-Frame
ON CHOOSE OF Btn_Add-Break IN FRAME Dialog-Frame /* Add Break Quantities */
DO:
    oplError = NO.
    
    RUN pAddBreak.      
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   oplError = YES.
   IF VALID-HANDLE(hdEstimateProcs) THEN
   DELETE OBJECT hdEstimateProcs.
   IF VALID-HANDLE(hdVendorCostProcs) THEN
   DELETE OBJECT hdVendorCostProcs.
   APPLY "CLOSE":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear Dialog-Frame
ON CHOOSE OF Btn_Clear IN FRAME Dialog-Frame /* Clear Quantities */
DO:
    ASSIGN lv-qty1:SCREEN-VALUE = "0"
           lv-qty2:SCREEN-VALUE = "0"
           lv-qty3:SCREEN-VALUE = "0"
           lv-qty4:SCREEN-VALUE = "0"
           lv-qty5:SCREEN-VALUE = "0"
           lv-qty6:SCREEN-VALUE = "0"
           lv-qty7:SCREEN-VALUE = "0" 
           lv-qty8:SCREEN-VALUE = "0"
           lv-qty9:SCREEN-VALUE = "0" 
           lv-qty10:SCREEN-VALUE = "0"           
           lv-qty11:SCREEN-VALUE = "0" 
           lv-qty12:SCREEN-VALUE = "0"
           lv-qty13:SCREEN-VALUE = "0" 
           lv-qty14:SCREEN-VALUE = "0"
           lv-qty15:SCREEN-VALUE = "0" 
           lv-qty16:SCREEN-VALUE = "0"
           lv-qty17:SCREEN-VALUE = "0" 
           lv-qty18:SCREEN-VALUE = "0"
           lv-qty19:SCREEN-VALUE = "0" 
           lv-qty20:SCREEN-VALUE = "0"
           .      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    oplError = NO.
        
    ASSIGN lv-qty1 lv-qty2
           lv-qty3 lv-qty4
           lv-qty5 lv-qty6
           lv-qty7 lv-qty8
           lv-qty9 lv-qty10
           lv-rel-1 lv-rel-2
           lv-rel-3 lv-rel-4
           lv-rel-5 lv-rel-6
           lv-rel-7 lv-rel-8
           lv-rel-9 lv-rel-10.
    ASSIGN lv-qty11 lv-qty12
           lv-qty13 lv-qty14
           lv-qty15 lv-qty16
           lv-qty17 lv-qty18
           lv-qty19 lv-qty20
           lv-rel-11 lv-rel-12
           lv-rel-13 lv-rel-14
           lv-rel-15 lv-rel-16
           lv-rel-17 lv-rel-18
           lv-rel-19 lv-rel-20.
           
    IF lv-qty1 EQ 0 AND lv-qty2 EQ 0 AND lv-qty3 EQ 0 AND lv-qty4 EQ 0 AND lv-qty5 EQ 0 AND lv-qty6 EQ 0 AND lv-qty7 EQ 0 AND lv-qty8 EQ 0 AND lv-qty9 EQ 0 AND lv-qty10 EQ 0 AND 
       lv-qty11 EQ 0 AND lv-qty12 EQ 0 AND lv-qty13 EQ 0 AND lv-qty14 EQ 0 AND lv-qty15 EQ 0 AND lv-qty16 EQ 0 AND lv-qty17 EQ 0 AND lv-qty18 EQ 0 AND lv-qty19 EQ 0 AND lv-qty20 EQ 0 THEN
    DO:
        MESSAGE "Please enter at least one quantity ..." 
                VIEW-AS ALERT-BOX INFO .
        APPLY "entry" TO lv-qty1.         
        RETURN.        
    END.       
           
    ASSIGN tg_RunShip-1 tg_RunShip-2
            tg_RunShip-3 tg_RunShip-4
            tg_RunShip-5 tg_RunShip-6
            tg_RunShip-7 tg_RunShip-8
            tg_RunShip-9 tg_RunShip-10
            tg_RunShip-11 tg_RunShip-12
            tg_RunShip-13 tg_RunShip-14
            tg_RunShip-15 tg_RunShip-16
            tg_RunShip-17 tg_RunShip-18
            tg_RunShip-19 tg_RunShip-20
            .
    IF AVAILABLE ttEstimateQuantity THEN 
            ASSIGN ttEstimateQuantity.EstQuantity[1] = lv-qty1 
                   ttEstimateQuantity.EstRelease[1]  = lv-rel-1
                   ttEstimateQuantity.EstRunship[1]  = tg_runship-1
                   ttEstimateQuantity.EstQuantity[2] = lv-qty2 
                   ttEstimateQuantity.EstRelease[2]  = lv-rel-2
                   ttEstimateQuantity.EstRunship[2]  = tg_runship-2
                   ttEstimateQuantity.EstQuantity[3] = lv-qty3 
                   ttEstimateQuantity.EstRelease[3]  = lv-rel-3
                   ttEstimateQuantity.EstRunship[3]  = tg_runship-3
                   ttEstimateQuantity.EstQuantity[4] = lv-qty4 
                   ttEstimateQuantity.EstRelease[4]  = lv-rel-4
                   ttEstimateQuantity.EstRunship[4]  = tg_runship-4
                   ttEstimateQuantity.EstQuantity[5] = lv-qty5 
                   ttEstimateQuantity.EstRelease[5]  = lv-rel-5
                   ttEstimateQuantity.EstRunship[5]  = tg_runship-5
                   ttEstimateQuantity.EstQuantity[6] = lv-qty6 
                   ttEstimateQuantity.EstRelease[6]  = lv-rel-6
                   ttEstimateQuantity.EstRunship[6]  = tg_runship-6
                   ttEstimateQuantity.EstQuantity[7] = lv-qty7 
                   ttEstimateQuantity.EstRelease[7]  = lv-rel-7
                   ttEstimateQuantity.EstRunship[7]  = tg_runship-7
                   ttEstimateQuantity.EstQuantity[8] = lv-qty8 
                   ttEstimateQuantity.EstRelease[8]  = lv-rel-8
                   ttEstimateQuantity.EstRunship[8]  = tg_runship-8
                   ttEstimateQuantity.EstQuantity[9] = lv-qty9 
                   ttEstimateQuantity.EstRelease[9]  = lv-rel-9
                   ttEstimateQuantity.EstRunship[9]  = tg_runship-9
                   ttEstimateQuantity.EstQuantity[10] = lv-qty10 
                   ttEstimateQuantity.EstRelease[10]  = lv-rel-10
                   ttEstimateQuantity.EstRunship[10]  = tg_runship-10
                   ttEstimateQuantity.EstQuantity[11] = lv-qty11 
                   ttEstimateQuantity.EstRelease[11]  = lv-rel-11
                   ttEstimateQuantity.EstRunship[11]  = tg_runship-11
                   ttEstimateQuantity.EstQuantity[12] = lv-qty12 
                   ttEstimateQuantity.EstRelease[12]  = lv-rel-12
                   ttEstimateQuantity.EstRunship[12]  = tg_runship-12
                   ttEstimateQuantity.EstQuantity[13] = lv-qty13 
                   ttEstimateQuantity.EstRelease[13]  = lv-rel-13
                   ttEstimateQuantity.EstRunship[13]  = tg_runship-13
                   ttEstimateQuantity.EstQuantity[14] = lv-qty14 
                   ttEstimateQuantity.EstRelease[14]  = lv-rel-14
                   ttEstimateQuantity.EstRunship[14]  = tg_runship-14
                   ttEstimateQuantity.EstQuantity[15] = lv-qty15 
                   ttEstimateQuantity.EstRelease[15]  = lv-rel-15
                   ttEstimateQuantity.EstRunship[15]  = tg_runship-15
                   ttEstimateQuantity.EstQuantity[16] = lv-qty16 
                   ttEstimateQuantity.EstRelease[16]  = lv-rel-16
                   ttEstimateQuantity.EstRunship[16]  = tg_runship-16
                   ttEstimateQuantity.EstQuantity[17] = lv-qty17 
                   ttEstimateQuantity.EstRelease[17]  = lv-rel-17
                   ttEstimateQuantity.EstRunship[17]  = tg_runship-17
                   ttEstimateQuantity.EstQuantity[18] = lv-qty18 
                   ttEstimateQuantity.EstRelease[18]  = lv-rel-18
                   ttEstimateQuantity.EstRunship[18]  = tg_runship-18
                   ttEstimateQuantity.EstQuantity[19] = lv-qty19 
                   ttEstimateQuantity.EstRelease[19]  = lv-rel-19
                   ttEstimateQuantity.EstRunship[19]  = tg_runship-19
                   ttEstimateQuantity.EstQuantity[20] = lv-qty20 
                   ttEstimateQuantity.EstRelease[20]  = lv-rel-20
                   ttEstimateQuantity.EstRunship[20]  = tg_runship-20.
                   
    APPLY "go" TO FRAME {&FRAME-NAME}.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty1 Dialog-Frame
ON VALUE-CHANGED OF lv-qty1 IN FRAME Dialog-Frame
DO:  
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-1:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty10 Dialog-Frame
ON VALUE-CHANGED OF lv-qty10 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-10:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty11 Dialog-Frame
ON VALUE-CHANGED OF lv-qty11 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-11:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty12 Dialog-Frame
ON VALUE-CHANGED OF lv-qty12 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-12:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty13 Dialog-Frame
ON VALUE-CHANGED OF lv-qty13 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-13:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty14 Dialog-Frame
ON VALUE-CHANGED OF lv-qty14 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-14:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty15 Dialog-Frame
ON VALUE-CHANGED OF lv-qty15 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-15:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty16 Dialog-Frame
ON VALUE-CHANGED OF lv-qty16 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-16:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty17 Dialog-Frame
ON VALUE-CHANGED OF lv-qty17 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-17:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty18 Dialog-Frame
ON VALUE-CHANGED OF lv-qty18 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-18:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty19 Dialog-Frame
ON VALUE-CHANGED OF lv-qty19 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-19:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty2 Dialog-Frame
ON VALUE-CHANGED OF lv-qty2 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-2:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty20 Dialog-Frame
ON VALUE-CHANGED OF lv-qty20 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-20:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty3 Dialog-Frame
ON VALUE-CHANGED OF lv-qty3 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-3:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty4 Dialog-Frame
ON VALUE-CHANGED OF lv-qty4 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-4:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty5 Dialog-Frame
ON VALUE-CHANGED OF lv-qty5 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-5:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty6 Dialog-Frame
ON VALUE-CHANGED OF lv-qty6 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-6:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty7 Dialog-Frame
ON VALUE-CHANGED OF lv-qty7 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-7:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty8 Dialog-Frame
ON VALUE-CHANGED OF lv-qty8 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-8:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty9 Dialog-Frame
ON VALUE-CHANGED OF lv-qty9 IN FRAME Dialog-Frame
DO:
  RUN Estimate_GetMSF IN hdEstimateProcs(ip-eb-recid, iplCorrware, DEC(FOCUS:SCREEN-VALUE), OUTPUT ld-msf).
  lv-msf-9:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rd_vendor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_vendor Dialog-Frame
ON VALUE-CHANGED OF rd_vendor IN FRAME Dialog-Frame
DO:  
    rd_vendor = rd_vendor:SCREEN-VALUE.
    RUN pGetNextQty.
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
       
      FIND eb NO-LOCK
         WHERE RECID(eb) EQ ip-eb-recid NO-ERROR.
      
      FIND FIRST ttEstimateQuantity NO-ERROR.
      IF AVAILABLE ttEstimateQuantity THEN
      DO:                 
          ASSIGN lv-qty1 = ttEstimateQuantity.EstQuantity[1]
                 lv-qty2 = ttEstimateQuantity.EstQuantity[2]
                 lv-qty3 = ttEstimateQuantity.EstQuantity[3]
                 lv-qty4 = ttEstimateQuantity.EstQuantity[4]
                 lv-qty5 = ttEstimateQuantity.EstQuantity[5]
                 lv-qty6 = ttEstimateQuantity.EstQuantity[6]
                 lv-qty7 = ttEstimateQuantity.EstQuantity[7]
                 lv-qty8 = ttEstimateQuantity.EstQuantity[8]
                 lv-qty9 = ttEstimateQuantity.EstQuantity[9]
                 lv-qty10 = ttEstimateQuantity.EstQuantity[10]
                 lv-qty11 = ttEstimateQuantity.EstQuantity[11]
                 lv-qty12 = ttEstimateQuantity.EstQuantity[12]
                 lv-qty13 = ttEstimateQuantity.EstQuantity[13]
                 lv-qty14 = ttEstimateQuantity.EstQuantity[14]
                 lv-qty15 = ttEstimateQuantity.EstQuantity[15]
                 lv-qty16 = ttEstimateQuantity.EstQuantity[16]
                 lv-qty17 = ttEstimateQuantity.EstQuantity[17]
                 lv-qty18 = ttEstimateQuantity.EstQuantity[18]
                 lv-qty19 = ttEstimateQuantity.EstQuantity[19]
                 lv-qty20 = ttEstimateQuantity.EstQuantity[20].
                     
          ASSIGN lv-rel-1  = ttEstimateQuantity.EstRelease[1]
                 lv-rel-2  = ttEstimateQuantity.EstRelease[2]
                 lv-rel-3  = ttEstimateQuantity.EstRelease[3]
                 lv-rel-4  = ttEstimateQuantity.EstRelease[4]
                 lv-rel-5  = ttEstimateQuantity.EstRelease[5]
                 lv-rel-6  = ttEstimateQuantity.EstRelease[6]
                 lv-rel-7  = ttEstimateQuantity.EstRelease[7]
                 lv-rel-8  = ttEstimateQuantity.EstRelease[8]
                 lv-rel-9  = ttEstimateQuantity.EstRelease[9]
                 lv-rel-10 = ttEstimateQuantity.EstRelease[10]
                 lv-rel-11 = ttEstimateQuantity.EstRelease[11]
                 lv-rel-12 = ttEstimateQuantity.EstRelease[12]
                 lv-rel-13 = ttEstimateQuantity.EstRelease[13]
                 lv-rel-14 = ttEstimateQuantity.EstRelease[14]
                 lv-rel-15 = ttEstimateQuantity.EstRelease[15]
                 lv-rel-16 = ttEstimateQuantity.EstRelease[16]
                 lv-rel-17 = ttEstimateQuantity.EstRelease[17]
                 lv-rel-18 = ttEstimateQuantity.EstRelease[18]
                 lv-rel-19 = ttEstimateQuantity.EstRelease[19]
                 lv-rel-20 = ttEstimateQuantity.EstRelease[20].
                 
          ASSIGN tg_RunShip-1 = ttEstimateQuantity.EstRunship[1] 
                 tg_RunShip-2 = ttEstimateQuantity.EstRunship[2] 
                 tg_RunShip-3 = ttEstimateQuantity.EstRunship[3]
                 tg_RunShip-4 = ttEstimateQuantity.EstRunship[4]
                 tg_RunShip-5 = ttEstimateQuantity.EstRunship[5]
                 tg_RunShip-6 = ttEstimateQuantity.EstRunship[6]
                 tg_RunShip-7 = ttEstimateQuantity.EstRunship[7]
                 tg_RunShip-8 = ttEstimateQuantity.EstRunship[8]
                 tg_RunShip-9 = ttEstimateQuantity.EstRunship[9]
                 tg_RunShip-10 = ttEstimateQuantity.EstRunship[10]
                 tg_RunShip-11 = ttEstimateQuantity.EstRunship[11]
                 tg_RunShip-12 = ttEstimateQuantity.EstRunship[12]
                 tg_RunShip-13 = ttEstimateQuantity.EstRunship[13]
                 tg_RunShip-14 = ttEstimateQuantity.EstRunship[14]
                 tg_RunShip-15 = ttEstimateQuantity.EstRunship[15]
                 tg_RunShip-16 = ttEstimateQuantity.EstRunship[16]
                 tg_RunShip-17 = ttEstimateQuantity.EstRunship[17]
                 tg_RunShip-18 = ttEstimateQuantity.EstRunship[18]
                 tg_RunShip-19 = ttEstimateQuantity.EstRunship[19]
                 tg_RunShip-20 = ttEstimateQuantity.EstRunship[20].
                 
          ASSIGN lv-msf-1 = ttEstimateQuantity.EstMSF[1] 
                 lv-msf-2 = ttEstimateQuantity.EstMSF[2] 
                 lv-msf-3 = ttEstimateQuantity.EstMSF[3]
                 lv-msf-4 = ttEstimateQuantity.EstMSF[4]
                 lv-msf-5 = ttEstimateQuantity.EstMSF[5]
                 lv-msf-6 = ttEstimateQuantity.EstMSF[6]
                 lv-msf-7 = ttEstimateQuantity.EstMSF[7]
                 lv-msf-8 = ttEstimateQuantity.EstMSF[8]
                 lv-msf-9 = ttEstimateQuantity.EstMSF[9]
                 lv-msf-10 = ttEstimateQuantity.EstMSF[10]
                 lv-msf-11 = ttEstimateQuantity.EstMSF[11]
                 lv-msf-12 = ttEstimateQuantity.EstMSF[12]
                 lv-msf-13 = ttEstimateQuantity.EstMSF[13]
                 lv-msf-14 = ttEstimateQuantity.EstMSF[14]
                 lv-msf-15 = ttEstimateQuantity.EstMSF[15]
                 lv-msf-16 = ttEstimateQuantity.EstMSF[16]
                 lv-msf-17 = ttEstimateQuantity.EstMSF[17]
                 lv-msf-18 = ttEstimateQuantity.EstMSF[18]
                 lv-msf-19 = ttEstimateQuantity.EstMSF[19]
                 lv-msf-20 = ttEstimateQuantity.EstMSF[20]. 
                 
          ASSIGN lv-next-qty1 = ttEstimateQuantity.EstNextQuantity[1] 
                 lv-next-qty2 = ttEstimateQuantity.EstNextQuantity[2] 
                 lv-next-qty3 = ttEstimateQuantity.EstNextQuantity[3]
                 lv-next-qty4 = ttEstimateQuantity.EstNextQuantity[4]
                 lv-next-qty5 = ttEstimateQuantity.EstNextQuantity[5]
                 lv-next-qty6 = ttEstimateQuantity.EstNextQuantity[6]
                 lv-next-qty7 = ttEstimateQuantity.EstNextQuantity[7]
                 lv-next-qty8 = ttEstimateQuantity.EstNextQuantity[8]
                 lv-next-qty9 = ttEstimateQuantity.EstNextQuantity[9]
                 lv-next-qty10 = ttEstimateQuantity.EstNextQuantity[10]
                 lv-next-qty11 = ttEstimateQuantity.EstNextQuantity[11]
                 lv-next-qty12 = ttEstimateQuantity.EstNextQuantity[12]
                 lv-next-qty13 = ttEstimateQuantity.EstNextQuantity[13]
                 lv-next-qty14 = ttEstimateQuantity.EstNextQuantity[14]
                 lv-next-qty15 = ttEstimateQuantity.EstNextQuantity[15]
                 lv-next-qty16 = ttEstimateQuantity.EstNextQuantity[16]
                 lv-next-qty17 = ttEstimateQuantity.EstNextQuantity[17]
                 lv-next-qty18 = ttEstimateQuantity.EstNextQuantity[18]
                 lv-next-qty19 = ttEstimateQuantity.EstNextQuantity[19]
                 lv-next-qty20 = ttEstimateQuantity.EstNextQuantity[20]. 
          RUN pGetVendorList.                               
      END.
      
   RUN enable_UI. 

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
  DISPLAY lv-qty1 lv-rel-1 tg_RunShip-1 lv-msf-1 lv-qty2 lv-rel-2 tg_RunShip-2 
          lv-msf-2 lv-qty3 lv-rel-3 tg_RunShip-3 lv-msf-3 lv-qty4 lv-rel-4 
          tg_RunShip-4 lv-msf-4 lv-qty5 lv-rel-5 tg_RunShip-5 lv-msf-5 lv-qty6 
          lv-rel-6 tg_RunShip-6 lv-msf-6 lv-qty7 lv-rel-7 tg_RunShip-7 lv-msf-7 
          lv-qty8 lv-rel-8 tg_RunShip-8 lv-msf-8 lv-msf-9 lv-qty9 lv-rel-9 
          tg_RunShip-9 lv-qty10 lv-rel-10 tg_RunShip-10 lv-msf-10 lv-qty11 
          lv-rel-11 tg_RunShip-11 lv-msf-11 lv-qty12 lv-rel-12 tg_RunShip-12 
          lv-msf-12 lv-qty13 lv-rel-13 tg_RunShip-13 lv-msf-13 lv-qty14 
          lv-rel-14 tg_RunShip-14 lv-msf-14 lv-qty15 lv-rel-15 tg_RunShip-15 
          lv-msf-15 lv-qty16 lv-rel-16 tg_RunShip-16 lv-msf-16 lv-qty17 
          lv-rel-17 tg_RunShip-17 lv-msf-17 lv-qty18 lv-rel-18 tg_RunShip-18 
          lv-msf-18 lv-qty19 lv-rel-19 tg_RunShip-19 lv-msf-19 lv-qty20 
          lv-rel-20 tg_RunShip-20 lv-msf-20 lv-next-qty1 lv-next-qty2 
          lv-next-qty17 lv-next-qty16 lv-next-qty15 lv-next-qty14 lv-next-qty13 
          lv-next-qty12 lv-next-qty11 lv-next-qty10 lv-next-qty9 lv-next-qty8 
          lv-next-qty7 lv-next-qty6 lv-next-qty5 lv-next-qty4 lv-next-qty3 
          lv-next-qty18 lv-next-qty19 lv-next-qty20 rd_vendor 
      WITH FRAME Dialog-Frame.
  ENABLE lv-qty1 lv-rel-1 tg_RunShip-1 lv-qty2 lv-rel-2 tg_RunShip-2 lv-qty3 
         lv-rel-3 tg_RunShip-3 lv-qty4 lv-rel-4 tg_RunShip-4 lv-qty5 lv-rel-5 
         tg_RunShip-5 lv-qty6 lv-rel-6 tg_RunShip-6 lv-qty7 lv-rel-7 
         tg_RunShip-7 lv-qty8 lv-rel-8 tg_RunShip-8 lv-qty9 lv-rel-9 
         tg_RunShip-9 lv-qty10 lv-rel-10 tg_RunShip-10 lv-qty11 lv-rel-11 
         tg_RunShip-11 lv-qty12 lv-rel-12 tg_RunShip-12 lv-qty13 lv-rel-13 
         tg_RunShip-13 lv-qty14 lv-rel-14 tg_RunShip-14 lv-qty15 lv-rel-15 
         tg_RunShip-15 lv-qty16 lv-rel-16 tg_RunShip-16 lv-qty17 lv-rel-17 
         tg_RunShip-17 lv-qty18 lv-rel-18 tg_RunShip-18 lv-qty19 lv-rel-19 
         tg_RunShip-19 lv-qty20 lv-rel-20 tg_RunShip-20 Btn_OK Btn_Cancel 
         RECT-6 rd_vendor Btn_Add-Break Btn_Clear 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddBreak Dialog-Frame 
PROCEDURE pAddBreak :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
    DEFINE VARIABLE dListQty AS DECIMAL EXTENT 40 NO-UNDO.
    DEFINE VARIABLE dQuantity AS DECIMAL EXTENT 40 NO-UNDO.   
    
    EMPTY TEMP-TABLE tt-count.
        
    DO WITH FRAME {&FRAME-NAME}:  
       ASSIGN dQuantity[1] =  integer(ttEstimateQuantity.EstQuantity[1])
           dQuantity[2] =  integer(ttEstimateQuantity.EstQuantity[2])
           dQuantity[3] =  integer(ttEstimateQuantity.EstQuantity[3])
           dQuantity[4] =  integer(ttEstimateQuantity.EstQuantity[4])
           dQuantity[5] =  integer(ttEstimateQuantity.EstQuantity[5])
           dQuantity[6] =  integer(ttEstimateQuantity.EstQuantity[6])
           dQuantity[7] =  integer(ttEstimateQuantity.EstQuantity[7])
           dQuantity[8] =  integer(ttEstimateQuantity.EstQuantity[8])
           dQuantity[9] =  integer(ttEstimateQuantity.EstQuantity[9])
           dQuantity[10] =  integer(ttEstimateQuantity.EstQuantity[10])
           dQuantity[11] =  integer(ttEstimateQuantity.EstQuantity[11])
           dQuantity[12] =  integer(ttEstimateQuantity.EstQuantity[12])
           dQuantity[13] =  integer(ttEstimateQuantity.EstQuantity[13])
           dQuantity[14] =  integer(ttEstimateQuantity.EstQuantity[14])
           dQuantity[15] =  integer(ttEstimateQuantity.EstQuantity[15])
           dQuantity[16] =  integer(ttEstimateQuantity.EstQuantity[16])
           dQuantity[17] =  integer(ttEstimateQuantity.EstQuantity[17])
           dQuantity[18] =  integer(ttEstimateQuantity.EstQuantity[18])
           dQuantity[19] =  integer(ttEstimateQuantity.EstQuantity[19])
           dQuantity[20] =  integer(ttEstimateQuantity.EstQuantity[20])
           . 
    END.   
    
    iCount = 1.
    j = 1.
    DO i = 1 TO 20:
     IF dQuantity[i] NE ttEstimateQuantity.EstNextQuantity[j] AND ttEstimateQuantity.EstNextQuantity[j] NE 0 THEN
      DO:           
         CREATE tt-count.
         ASSIGN
            tt-count.iIndex = iCount 
            tt-count.iQuantity = dQuantity[i].
         CREATE tt-count.
         ASSIGN
            tt-count.iIndex = iCount + 1
            tt-count.iQuantity = ttEstimateQuantity.EstNextQuantity[j].   
            
        ASSIGN
         iCount = iCount + 2
         j = j + 1 .
     END.    
     ELSE IF dQuantity[i] EQ ttEstimateQuantity.EstNextQuantity[j] AND dQuantity[i] NE 0 THEN
     DO:
        CREATE tt-count.
         ASSIGN
            tt-count.iIndex = iCount 
            tt-count.iQuantity = dQuantity[i].
        ASSIGN                   
         j = j + 1
         iCount = iCount + 1. 
     END.
     ELSE IF dQuantity[i] NE 0 THEN
     DO:  
        CREATE tt-count.
         ASSIGN
            tt-count.iIndex = iCount 
            tt-count.iQuantity = dQuantity[i].
        ASSIGN                 
         iCount = iCount + 1.  
     END.       
    END.
    RELEASE tt-count.
    
    i = 1.
    FOR EACH tt-count NO-LOCK
          BY tt-count.iQuantity:
       dListQty[i] = tt-count.iQuantity. 
       i = i + 1.
    END.
      
    DO WITH FRAME {&FRAME-NAME}:  
       ASSIGN 
         lv-qty1:SCREEN-VALUE = string(dListQty[1])
         lv-qty2:SCREEN-VALUE = string(dListQty[2])
         lv-qty3:SCREEN-VALUE = string(dListQty[3])
         lv-qty4:SCREEN-VALUE = string(dListQty[4])
         lv-qty5:SCREEN-VALUE = string(dListQty[5])
         lv-qty6:SCREEN-VALUE = string(dListQty[6])
         lv-qty7:SCREEN-VALUE = string(dListQty[7])
         lv-qty8:SCREEN-VALUE = string(dListQty[8])
         lv-qty9:SCREEN-VALUE = string(dListQty[9])
         lv-qty10:SCREEN-VALUE = string(dListQty[10])
         lv-qty11:SCREEN-VALUE = string(dListQty[11])
         lv-qty12:SCREEN-VALUE = string(dListQty[12])
         lv-qty13:SCREEN-VALUE = string(dListQty[13])
         lv-qty14:SCREEN-VALUE = string(dListQty[14])
         lv-qty15:SCREEN-VALUE = string(dListQty[15])
         lv-qty16:SCREEN-VALUE = string(dListQty[16])
         lv-qty17:SCREEN-VALUE = string(dListQty[17])
         lv-qty18:SCREEN-VALUE = string(dListQty[18])
         lv-qty19:SCREEN-VALUE = string(dListQty[19])
         lv-qty20:SCREEN-VALUE = string(dListQty[20]).
    END.  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetVendorList Dialog-Frame 
PROCEDURE pGetVendorList :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-forms  AS INTEGER   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE li       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lUseVIC  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScope   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lLogic   AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    rd_vendor:LIST-ITEMS IN FRAME {&frame-name} = "".
    
    cScope = DYNAMIC-FUNCTION("VendCost_GetValidScopes","Est-RM-Over").
    
    FIND FIRST est NO-LOCK
        WHERE est.company EQ eb.company
        AND est.est-no EQ eb.est-no NO-ERROR.
       
    IF AVAILABLE est THEN
    DO:          
        lUseVIC = fUseVendItemCost(est.company).
    
        FOR EACH ef
            WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no        
            NO-LOCK:             
                     
            IF lUseVIC THEN 
            DO:
                FIND FIRST est-qty NO-LOCK 
                    WHERE est-qty.company EQ est.company
                    AND est-qty.est-no EQ est.est-no
                    NO-ERROR.
                IF AVAILABLE est-qty THEN dQty = est-qty.eqty.
                RUN BuildVendItemCosts(est.company, ef.board, "RM", cScope, YES,
                    ef.est-no,ef.form-no,0,
                    dQty, "EA", 
                    ef.gsh-len, ef.gsh-wid, ef.gsh-dep, "IN",
                    ef.weight, "LBS/MSF", 
                    OUTPUT TABLE ttVendItemCost,
                    OUTPUT lError, OUTPUT cMessage).
                FOR EACH ttVendItemCost, 
                    FIRST vend NO-LOCK 
                    WHERE vend.company EQ ttVendItemCost.company
                    AND vend.vend-no EQ ttVendItemCost.vendorID:
                    CREATE tt-report.
                    ASSIGN 
                        tt-report.key-01 = vend.vend-no
                        tt-report.key-02 = vend.name
                        tt-report.key-03 = ef.board
                        .
                END.
                EMPTY TEMP-TABLE ttVendItemCost.
            END.
            ELSE 
            DO: 
                FOR EACH e-item-vend
                    WHERE e-item-vend.company EQ ef.company
                    AND e-item-vend.i-no    EQ ef.board
                    AND e-item-vend.vend-no NE ""         
                    AND ef.gsh-wid          GE e-item-vend.roll-w[27]
                    AND ef.gsh-wid          LE e-item-vend.roll-w[28]
                    AND ef.gsh-len          GE e-item-vend.roll-w[29]
                    AND ef.gsh-len          LE e-item-vend.roll-w[30]
                    NO-LOCK:
    
                    FIND FIRST vend
                        WHERE vend.company EQ e-item-vend.company
                        AND vend.vend-no EQ e-item-vend.vend-no
                        NO-LOCK NO-ERROR.
    
                    CREATE tt-report.
                    ASSIGN
                        tt-report.key-01 = e-item-vend.vend-no
                        tt-report.key-02 = IF AVAILABLE vend THEN vend.name ELSE ""
                        tt-report.rec-id = RECID(e-item-vend)
                        tt-report.key-03 = ef.board.
                END.
            END.
        END.
    
    END.
   
    lLogic = rd_vendor:ADD-LAST ("") IN FRAME {&frame-name} NO-ERROR.
    FOR EACH tt-report NO-LOCK:   
        lLogic = rd_vendor:ADD-LAST (STRING(tt-report.key-01)) IN FRAME {&frame-name} NO-ERROR.
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetNextQty Dialog-Frame 
PROCEDURE pGetNextQty :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.
    DEFINE VARIABLE cBaseQty AS INTEGER EXTENT 20 NO-UNDO.
    DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cVendorID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opdCostNextPriceBreak AS DECIMAL NO-UNDO.
    
        
    DO WITH FRAME {&FRAME-NAME}:
      cVendorID = rd_vendor:SCREEN-VALUE.
      IF cVendorID EQ ? THEN
      cVendorID = "".
    END.                  
     
    FIND FIRST est NO-LOCK
        WHERE est.company EQ eb.company
        AND est.est-no EQ eb.est-no NO-ERROR.
        
    FIND FIRST ef NO-LOCK 
            WHERE ef.company EQ eb.company 
            AND ef.est-no EQ eb.est-no
            AND ef.form-no EQ eb.form-no NO-ERROR. 
                
    IF eb.pur-man THEN 
    DO:
    RUN GetNextPriceBreak IN hdVendorCostProcs (INPUT-OUTPUT TABLE ttEstimateQuantity BY-REFERENCE, eb.company, "FG", eb.stock-no, eb.est-no, eb.form-no, eb.blank-no, ef.gsh-len, ef.gsh-wid, ef.gsh-dep, ROWID(ef), eb.num-up, cVendorID, OUTPUT opdCostNextPriceBreak).                                .
    END.
    ELSE 
    DO:
    RUN GetNextPriceBreak IN hdVendorCostProcs (INPUT-OUTPUT TABLE ttEstimateQuantity BY-REFERENCE,  eb.company, "RM", ef.board, eb.est-no, eb.form-no, eb.blank-no, ef.gsh-len, ef.gsh-wid, ef.gsh-dep, ROWID(ef), eb.num-up, cVendorID, OUTPUT opdCostNextPriceBreak).
    END.        
              
    DO WITH FRAME {&FRAME-NAME}:                         
     ASSIGN lv-next-qty1:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[1]) 
         lv-next-qty2:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[2]) 
         lv-next-qty3:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[3])
         lv-next-qty4:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[4])
         lv-next-qty5:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[5])
         lv-next-qty6:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[6])
         lv-next-qty7:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[7])
         lv-next-qty8:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[8])
         lv-next-qty9:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[9])
         lv-next-qty10:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[10])
         lv-next-qty11:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[11])
         lv-next-qty12:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[12])
         lv-next-qty13:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[13])
         lv-next-qty14:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[14])
         lv-next-qty15:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[15])
         lv-next-qty16:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[16])
         lv-next-qty17:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[17])
         lv-next-qty18:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[18])
         lv-next-qty19:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[19])
         lv-next-qty20:SCREEN-VALUE = STRING(ttEstimateQuantity.EstNextQuantity[20]).          
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUseVendItemCost Dialog-Frame
FUNCTION fUseVendItemCost RETURNS LOGICAL PRIVATE
  (ipcCompany AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns NK1 setting to activate VendItemCost as source of vendor costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "VendItemCost", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    
    RETURN lFound AND cReturn EQ "Yes".	

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
