&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/dAddSetEst.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */


DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.  /* poup in edit or add mode */
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO .
DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO .
DEFINE OUTPUT PARAMETER opcJobNo AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER NO-UNDO.
DEFINE VARIABLE v-count      AS INTEGER NO-UNDO.
DEFINE VARIABLE k_frac       AS DECIMAL INIT 6.25 NO-UNDO.
DEFINE VARIABLE lCreateNewFG AS LOGICAL NO-UNDO .

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{est/ttInputEst.i NEW}  
{sys/inc/var.i NEW shared}
{fgrep/ttFGReorder.i}
{jc/ttMultiSelectItem.i}
{custom/gcompany.i}  
ASSIGN
cocode = g_company
locode = g_loc
gcompany = cocode.
   
{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
    DEFINE TEMP-TABLE tt-64-dec NO-UNDO
        FIELD DEC AS DECIMAL DECIMALS 6.

    DO v-count = 0 TO 63:
        CREATE tt-64-dec.
        tt-64-dec.DEC = v-count / 64.0.
        RELEASE tt-64-dec.
    END.
END.  

DEFINE VARIABLE lv-copy-qty      AS INTEGER   EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-copy-rel      AS INTEGER   EXTENT 20 NO-UNDO.
DEFINE VARIABLE cLogicalRunShip  AS CHARACTER EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-crt-est-rowid AS ROWID     NO-UNDO.
DEFINE VARIABLE cStackCode       AS CHARACTER NO-UNDO .
DEFINE VARIABLE iOldQty          AS INTEGER   NO-UNDO .
DEFINE VARIABLE lShowMessage     AS LOGICAL   NO-UNDO .
DEFINE VARIABLE dMachBlankSqFt   AS DECIMAL   NO-UNDO.


DEFINE BUFFER bf-eb FOR eb.
DEF NEW SHARED BUFFER xest           FOR est.
DEF NEW SHARED BUFFER xef            FOR ef.
DEF NEW SHARED BUFFER xeb            FOR eb.
DEF NEW SHARED BUFFER xqty           FOR est-qty.

DEFINE TEMP-TABLE ttCompareEst NO-UNDO
       FIELD est-no AS CHARACTER
       FIELD stock-no AS CHARACTER
       FIELD num-len AS DECIMAL .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttInputEst

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttInputEst.cStockNo ttInputEst.cPartName ttInputEst.iQuantityYield ttInputEst.iMolds ttInputEst.cFgEstNo ttInputEst.dSqFt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttInputEst WHERE ttInputEst.cCompany = cocode ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttInputEst WHERE ttInputEst.cCompany = cocode ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttInputEst
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttInputEst


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 cMachCode cBoard iTargetCyl ~
dtDueDate btnCalendar-1 btn-add btn-copy btn-update btn-delete btn-viewjob ~
btn-add-multiple btn-imp-bal btn-sel-head tb_auto Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cMachCode cBoard iTargetCyl cJobNo ~
cLineDscr cBoardDscr dtDueDate dtCreatedDate cUserID dtStartDate cStatus ~
dtEstCom cEstNo iItem dTotSqFt iMolds dUtilization tb_auto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
     LABEL "Add " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-add-multiple 
     LABEL "Add Multiple" 
     SIZE 27.8 BY 1.14.

DEFINE BUTTON btn-copy 
     LABEL "Copy " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete 
     LABEL "Delete " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-imp-bal 
     LABEL "Import Remaining Balances" 
     SIZE 35 BY 1.14.

DEFINE BUTTON btn-sel-head 
     LABEL "Import From Selected Head" 
     SIZE 34.2 BY 1.14.

DEFINE BUTTON btn-update 
     LABEL "Update " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-viewjob 
     LABEL "View Job Queue" 
     SIZE 23.2 BY 1.14.

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Create Head/Job" 
     SIZE 24 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE cBoard AS CHARACTER FORMAT "X(10)":U 
     LABEL "Furnish" 
     VIEW-AS FILL-IN 
     SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE cBoardDscr AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cEstNo AS CHARACTER FORMAT "X(10)":U 
     LABEL "Estimate ID" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cJobNo AS CHARACTER FORMAT "X(10)":U 
     LABEL "Head ID" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cLineDscr AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 39.0 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMachCode AS CHARACTER FORMAT "X(8)":U 
     LABEL "Line" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cStatus AS CHARACTER FORMAT "X(10)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cUserID AS CHARACTER FORMAT "X(10)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dtCreatedDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Created Date" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dtDueDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Due Date" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dtEstCom AS DATE FORMAT "99/99/9999":U 
     LABEL "Est. Completion" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dTotSqFt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Sq Ft" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dtStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dUtilization AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Utilization" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iItem AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iMolds AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Molds" 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iTargetCyl AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Target Cycles" 
     VIEW-AS FILL-IN 
     SIZE 23.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 154 BY 24.05
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 62.4 BY 2.71
     BGCOLOR 15 .

DEFINE VARIABLE tb_auto AS LOGICAL INITIAL yes 
     LABEL "Auto Schedule" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.8 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttInputEst SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttInputEst.cStockNo LABEL "FG Item" WIDTH 21 LABEL-BGCOLOR 14 FORMAT "x(15)"
    ttInputEst.cPartName LABEL "Item Name" WIDTH 38 LABEL-BGCOLOR 14 FORMAT "x(30)"
    ttInputEst.iQuantityYield LABEL "Job Quantity" FORMAT ">>>,>>>,>>9" WIDTH 24 LABEL-BGCOLOR 14
    ttInputEst.iMolds LABEL "Molds" FORMAT ">>>>>>9" WIDTH 15 LABEL-BGCOLOR 14
    ttInputEst.cFgEstNo LABEL "Estimate" FORMAT "x(8)" WIDTH 18 LABEL-BGCOLOR 14
    ttInputEst.dSqFt LABEL "Total Sq Ft" FORMAT "->>,>>>,>>9.99" WIDTH 20 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 151.6 BY 13.05
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-1 AT ROW 7.91 COL 3.4
     cMachCode AT ROW 2 COL 19.6 COLON-ALIGNED WIDGET-ID 176
     cBoard AT ROW 4.29 COL 19.4 COLON-ALIGNED WIDGET-ID 88
     iTargetCyl AT ROW 5.43 COL 19.4 COLON-ALIGNED WIDGET-ID 42
     cJobNo AT ROW 2 COL 91.2 COLON-ALIGNED WIDGET-ID 196
     cLineDscr AT ROW 2 COL 37.4 COLON-ALIGNED NO-LABEL WIDGET-ID 202
     cBoardDscr AT ROW 4.29 COL 43.4 COLON-ALIGNED NO-LABEL
     dtDueDate AT ROW 5.43 COL 130.8 COLON-ALIGNED WIDGET-ID 280
     btnCalendar-1 AT ROW 5.43 COL 147.6
     btn-add AT ROW 21.19 COL 4.2 WIDGET-ID 16
     btn-copy AT ROW 21.19 COL 19.8 WIDGET-ID 252
     btn-update AT ROW 21.19 COL 35.4 WIDGET-ID 256
     btn-delete AT ROW 21.19 COL 51.4 WIDGET-ID 254
     btn-viewjob AT ROW 3.05 COL 21.8 WIDGET-ID 266
     dtCreatedDate AT ROW 2 COL 130.8 COLON-ALIGNED WIDGET-ID 268
     cUserID AT ROW 3.1 COL 91.2 COLON-ALIGNED WIDGET-ID 270
     dtStartDate AT ROW 3.1 COL 130.8 COLON-ALIGNED WIDGET-ID 272
     cStatus AT ROW 4.29 COL 91.2 COLON-ALIGNED WIDGET-ID 274
     dtEstCom AT ROW 4.29 COL 130.8 COLON-ALIGNED WIDGET-ID 276
     cEstNo AT ROW 5.43 COL 91.2 COLON-ALIGNED WIDGET-ID 278
     iItem AT ROW 22.95 COL 14.2 COLON-ALIGNED WIDGET-ID 282
     dTotSqFt AT ROW 22.95 COL 45.2 COLON-ALIGNED WIDGET-ID 286
     iMolds AT ROW 24.05 COL 14.2 COLON-ALIGNED WIDGET-ID 284
     dUtilization AT ROW 24.05 COL 45.2 COLON-ALIGNED WIDGET-ID 288
     btn-add-multiple AT ROW 6.57 COL 4.2 WIDGET-ID 292
     btn-imp-bal AT ROW 6.57 COL 32.6 WIDGET-ID 296
     btn-sel-head AT ROW 6.57 COL 68.2 WIDGET-ID 294
     tb_auto AT ROW 24.05 COL 81.6 WIDGET-ID 260
     Btn_OK AT ROW 23.91 COL 110.4
     Btn_Cancel AT ROW 23.91 COL 135.4
     " Head Analysis" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 22.33 COL 6 WIDGET-ID 206
     "%" VIEW-AS TEXT
          SIZE 2.8 BY .62 AT ROW 24.24 COL 62.2 WIDGET-ID 298
     RECT-4 AT ROW 1.48 COL 2 WIDGET-ID 236
     RECT-6 AT ROW 22.52 COL 3.6 WIDGET-ID 290
     SPACE(90.00) SKIP(0.83)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Head ID Creator"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME D-Dialog
   3                                                                    */
/* SETTINGS FOR FILL-IN cBoardDscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cEstNo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cJobNo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cLineDscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cStatus IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cUserID IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dtCreatedDate IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dtEstCom IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dTotSqFt IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dtStartDate IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dUtilization IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iItem IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iMolds IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       tb_auto:PRIVATE-DATA IN FRAME D-Dialog     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttInputEst WHERE ttInputEst.cCompany = cocode ~
        ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 D-Dialog
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME D-Dialog /* BROWSE-1 */
DO:
        APPLY "CHOOSE" TO btn-update .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Head ID Creator */
DO:             
        DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
        IF ipType EQ "" THEN 
        DO:
            MESSAGE "Cancelling will not create the estimate and you will lose the components that you entered."
                SKIP 
                "Are you sure you want to continue?" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lCheck .
        END.
        ELSE lCheck = TRUE.
               
        IF lCheck THEN 
        DO:   
            EMPTY TEMP-TABLE ttInputEst .
            
        
            APPLY "END-ERROR":U TO SELF.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add D-Dialog
ON CHOOSE OF btn-add IN FRAME D-Dialog /* Add  */
DO:
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
        DEFINE BUFFER bff-ttInputEst FOR ttInputEst .
        
       RUN valid-mach(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
            
        IF INTEGER(iTargetCyl:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Target Cycles must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO iTargetCyl .
            RETURN NO-APPLY.
        END.          
    
        RUN jc/dAddEditMoldItem.w (?,
                               "Add", 
                               int(iTargetCyl:SCREEN-VALUE), 
                               0,
                               OUTPUT lv-rowid) . 
        
            RUN repo-query (lv-rowid). 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add-multiple
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add-multiple D-Dialog
ON CHOOSE OF btn-add-multiple IN FRAME D-Dialog /* Add Multiple */
DO:
       DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
       DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
       DEFINE BUFFER bf-ttInputEst FOR ttInputEst.
       
       RUN valid-mach(OUTPUT lError) NO-ERROR.
       IF lError THEN RETURN NO-APPLY.
            
       IF INTEGER(iTargetCyl:SCREEN-VALUE) LE 0 THEN 
       DO:
           MESSAGE "Target Cycles must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
           APPLY "entry" TO iTargetCyl .
           RETURN NO-APPLY.
       END.            
       
       RUN jc/dMultiSelectItem.w (OUTPUT TABLE ttMultiSelectItem) . 
       
       FOR EACH ttMultiSelectItem NO-LOCK
           WHERE ttMultiSelectItem.isSelect:
           CREATE bf-ttInputEst.
            ASSIGN
                bf-ttInputEst.cEstType = "MoldTandem"
                bf-ttInputEst.cSetType = "MoldEstTandem"
                bf-ttInputEst.cCompany = cocode 
                bf-ttInputEst.cStockNo = ttMultiSelectItem.itemID
                bf-ttInputEst.iMolds   = ttMultiSelectItem.multiplier 
                bf-ttInputEst.iQuantityYield = ttMultiSelectItem.quantityToOrder
                lv-rowid               = ROWID(bf-ttInputEst).
                FIND FIRST itemfg NO-LOCK 
                     WHERE itemfg.company EQ cocode
                     AND itemfg.i-no EQ ttMultiSelectItem.itemID NO-ERROR .
                IF AVAILABLE itemfg THEN
                DO:
                  ASSIGN
                      bf-ttInputEst.cPartName = itemfg.i-name 
                      bf-ttInputEst.cFgEstNo  = itemfg.est-no
                      bf-ttInputEst.dSqFt = itemfg.t-sqft * bf-ttInputEst.iMolds
                      .               
                END.          
       END.
            
       RUN repo-query (lv-rowid).               

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy D-Dialog
ON CHOOSE OF btn-copy IN FRAME D-Dialog /* Copy  */
DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
            
        IF AVAILABLE ttInputEst THEN
        DO:   
            
            RUN jc/dAddEditMoldItem.w (RECID(ttInputEst),
                                   "Copy",
                                   iTargetCyl:SCREEN-VALUE, 
                                   dMachBlankSqFt,
                                   OUTPUT lv-rowid) .            
            
            RUN repo-query (lv-rowid).            
        END.      
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete D-Dialog
ON CHOOSE OF btn-delete IN FRAME D-Dialog /* Delete  */
DO:
        DEFINE VARIABLE hftp     AS HANDLE NO-UNDO.
        DEFINE VARIABLE lv-rowid AS ROWID  NO-UNDO.
        IF AVAILABLE ttInputEst THEN 
        DO:
            MESSAGE "Are you sure you want to delete this Component?" 
                VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN RETURN NO-APPLY.  
         
            DELETE ttInputEst .
            RUN repo-query (lv-rowid).
        
        END.                                             
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-imp-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-imp-bal D-Dialog
ON CHOOSE OF btn-imp-bal IN FRAME D-Dialog /* Import Remaining Balances */
DO:
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-sel-head
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-sel-head D-Dialog
ON CHOOSE OF btn-sel-head IN FRAME D-Dialog /* Import From Selected Head */
DO:
                                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update D-Dialog
ON CHOOSE OF btn-update IN FRAME D-Dialog /* Update  */
DO:
        DEFINE VARIABLE lv-rowid  AS ROWID NO-UNDO.
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
        
        RUN valid-mach(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
            
        IF INTEGER(iTargetCyl:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Target Cycles must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO iTargetCyl .
            RETURN NO-APPLY.
        END.
        
        IF AVAILABLE ttInputEst THEN 
        DO:
            
            RUN jc/dAddEditMoldItem.w (RECID(ttInputEst),
                                   "Update",
                                   iTargetCyl:SCREEN-VALUE, 
                                   dMachBlankSqFt,
                                   OUTPUT lv-rowid) . 
                                   
            RUN repo-query (ROWID(ttInputEst)).
        END. 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-viewjob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-viewjob D-Dialog
ON CHOOSE OF btn-viewjob IN FRAME D-Dialog /* View Job Queue */
DO:
                                               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 D-Dialog
ON CHOOSE OF btnCalendar-1 IN FRAME D-Dialog
DO:
    {methods/btnCalendar.i dtDueDate }
        APPLY "entry" TO dtDueDate .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
        DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
        IF ipType EQ "" THEN 
        DO:
            MESSAGE "Cancelling will not create the estimate and you will lose the components that you entered."
                SKIP 
                "Are you sure you want to continue?" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lCheck .
        END.
        ELSE  lCheck = TRUE.
        IF lCheck THEN 
        DO:
            EMPTY TEMP-TABLE ttInputEst .
           
            APPLY "END-ERROR":U TO SELF.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Create Head/Job */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        DEFINE VARIABLE riEb AS ROWID NO-UNDO .
        DEFINE VARIABLE riJob AS ROWID NO-UNDO.
        DEFINE VARIABLE cKeyItem AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lEstimateCreate AS LOGICAL NO-UNDO.
        
        DEFINE BUFFER bff-eb FOR eb.
        DEFINE BUFFER bf-job FOR job.
        DEFINE BUFFER bf-ttInputEst FOR ttInputEst.
        
        iCount = 0.
        FOR EACH ttInputEst NO-LOCK :
            iCount = iCount + 1.
        END.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.   
        
         RUN valid-mach(OUTPUT lError) NO-ERROR.
         IF lError THEN RETURN NO-APPLY.
         
         RUN valid-board(OUTPUT lError) NO-ERROR.
         IF lError THEN RETURN NO-APPLY.
            
        IF INTEGER(iTargetCyl:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Target Cycles must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO iTargetCyl .
            RETURN NO-APPLY.
        END.
                                       
                         
        SESSION:SET-WAIT-STATE("general").
  
        RUN create-ttfrmout.
        
        RUN pCheckEstimate(INPUT iCount, OUTPUT lEstimateCreate, OUTPUT riEb).
              
        IF NOT lEstimateCreate THEN
        RUN est/BuildEstimate.p ("F", OUTPUT riEb).
         

        FIND FIRST bff-eb NO-LOCK
             WHERE bff-eb.company EQ cocode
             AND ROWID(bff-eb) EQ riEb NO-ERROR .             
        IF AVAIL bff-eb THEN
        DO:
          RUN jc/CrtEstopForMold.p(ROWID(bff-eb), cMachCode).
          cEstNo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = bff-eb.est-no.  
          ipType = "created".
        END.
        
        FOR EACH bf-ttInputEst NO-LOCK
             WHERE bf-ttInputEst.lKeyItem:
             cKeyItem = bf-ttInputEst.cStockNo .
             LEAVE.
        END.
        
        RUN jc/MoldJobProcs.p(INPUT ROWID(bff-eb),INPUT dtDueDate, INPUT cKeyItem, OUTPUT riJob).
        
        FIND FIRST bf-job NO-LOCK
             WHERE ROWID(bf-job) EQ riJob NO-ERROR .
        IF AVAIL bf-job THEN
        DO:
         ASSIGN
           cJobNo:SCREEN-VALUE         =  STRING(bf-job.job-no) + "-" + STRING(bf-job.job-no2,"99")
           dtCreatedDate:SCREEN-VALUE  =  STRING(bf-job.create-date)
           cUserID:SCREEN-VALUE        = STRING(bf-job.user-id)
           cStatus:SCREEN-VALUE        = STRING(bf-job.stat)  
           Btn_OK:SENSITIVE            = NO 
           oplCreated                  = YES
           opcJobNo                    = bf-job.job-no. 
        END.
         
         MESSAGE "Process complete." VIEW-AS ALERT-BOX INFO. 
         
         APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cBoard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cBoard D-Dialog
ON HELP OF cBoard IN FRAME D-Dialog /* Furnish */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-rowid AS ROWID NO-UNDO.    
        
        RUN windows/l-board1.w (cocode,"",cBoard:SCREEN-VALUE, OUTPUT look-rowid).
        FIND FIRST ITEM WHERE ROWID(item) EQ look-rowid NO-LOCK NO-ERROR.
         IF AVAIL ITEM AND ITEM.i-no NE cBoard:SCREEN-VALUE THEN DO:
            ASSIGN
                cBoard:screen-value      = item.i-no
                cBoardDscr:SCREEN-VALUE  = ITEM.i-name
                .
                END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cBoard D-Dialog
ON LEAVE OF cBoard IN FRAME D-Dialog /* Furnish */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-board(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY. 
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMachCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMachCode D-Dialog
ON HELP OF cMachCode IN FRAME D-Dialog /* Line */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        
        RUN windows/l-mach.w (cocode, locode, cMachCode:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND cMachCode:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                cMachCode:screen-value      = ENTRY(1,char-val)                
                .     
        APPLY "value-changed" TO cMachCode IN FRAME {&FRAME-NAME}.
        APPLY "entry" TO cMachCode IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMachCode D-Dialog
ON LEAVE OF cMachCode IN FRAME D-Dialog /* Line */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
   
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-mach(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.          
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMachCode D-Dialog
ON VALUE-CHANGED OF cMachCode IN FRAME D-Dialog /* Line */
DO:     
        IF cMachCode:SCREEN-VALUE NE "" THEN 
        DO:
            RUN pNewMachine.
        END. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dtDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dtDueDate D-Dialog
ON HELP OF dtDueDate IN FRAME D-Dialog /* Due Date */
DO:
  {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /*{src/adm/template/dialogmn.i}*/
    RUN enable_UI.
    {methods/nowait.i}     
    DO WITH FRAME {&frame-name}:  
        
        DISABLE btn-viewjob btn-imp-bal btn-sel-head tb_auto.
        ASSIGN
         cJobNo:HIDDEN = YES
         dtCreatedDate:HIDDEN = YES 
         cUserID:HIDDEN = YES
         cStatus:HIDDEN = YES
         dtEstCom:HIDDEN = YES
         dtStartDate:HIDDEN = YES
         cEstNo:HIDDEN = YES
         .
        APPLY "entry" TO cMachCode.    
    END.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.           
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ttfrmout D-Dialog 
PROCEDURE create-ttfrmout :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ttInputEst FOR ttInputEst. 
    DEFINE VARIABLE iBlank AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantity AS INTEGER NO-UNDO.
                     
    iBlank = 1.                 
    FOR EACH  bf-ttInputEst EXCLUSIVE-LOCK:
       
       FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ bf-ttInputEst.cStockNo NO-ERROR .
       FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode 
            AND cust.cust-no = itemfg.cust-no NO-ERROR.
       IF NOT AVAIL cust THEN     
       FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode 
            AND cust.active = "X" NO-ERROR.
            
        ASSIGN    
            bf-ttInputEst.iFormNo          = 1
            bf-ttInputEst.iBlankNo         = iBlank            
            bf-ttInputEst.cBoard           = cBoard             
            bf-ttInputEst.cStyle           = itemfg.style  
            bf-ttInputEst.cPartID          = itemfg.part-no             
            bf-ttInputEst.cPartName        = itemfg.i-name
            bf-ttInputEst.cPartDescription = itemfg.part-dscr1
            bf-ttInputEst.dLength          = itemfg.l-score[50]
            bf-ttInputEst.dWidth           = itemfg.w-score[50]            
            bf-ttInputEst.dDepth           = itemfg.d-score[50]   
            bf-ttInputEst.cCategory        = itemfg.procat .
       IF AVAIL cust THEN
          ASSIGN
            bf-ttInputEst.cCustomer = cust.cust-no
            bf-ttInputEst.cShipTo   = cust.cust-no .
            
       IF iBlank EQ 1 THEN
       iQuantity = bf-ttInputEst.iQuantityYield.
       ASSIGN
           bf-ttInputEst.iQuantity = iQuantity.
       
       FIND FIRST ITEM NO-LOCK 
            WHERE item.company = cocode
            AND item.i-no = cBoard NO-ERROR.
       IF AVAIL ITEM THEN
       DO:
          ASSIGN
              bf-ttInputEst.cFlute = item.flute
              bf-ttInputEst.cTest  = item.reg-no.           
       END.
        
        ASSIGN 
            iBlank = iBlank + 1.                        
       
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY cMachCode cBoard iTargetCyl cJobNo cLineDscr cBoardDscr dtDueDate 
          dtCreatedDate cUserID dtStartDate cStatus dtEstCom cEstNo iItem 
          dTotSqFt iMolds dUtilization tb_auto 
      WITH FRAME D-Dialog.
  ENABLE BROWSE-1 cMachCode cBoard iTargetCyl dtDueDate btnCalendar-1 btn-add 
         btn-copy btn-update btn-delete btn-viewjob btn-add-multiple 
         btn-imp-bal btn-sel-head tb_auto Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckEstimate D-Dialog 
PROCEDURE pCheckEstimate :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplEstimateCreate AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opriRowid AS ROWID NO-UNDO.
    DEFINE BUFFER bf-eb FOR eb.
    DEFINE BUFFER bff-ttInputEst FOR ttInputEst .
    DEFINE VARIABLE lCheckEb AS LOGICAL NO-UNDO.
    

    FOR EACH bff-ttInputEst NO-LOCK:                   
     FOR EACH bf-eb  NO-LOCK
         WHERE bf-eb.company EQ cocode 
         AND bf-eb.est-type EQ 4
         AND bf-eb.stock-no EQ bff-ttInputEst.cStockNo
         AND bf-eb.num-len EQ  bff-ttInputEst.iMolds :               
                              
         CREATE ttCompareEst.
            ASSIGN
            ttCompareEst.est-no   = bf-eb.est-no
            ttCompareEst.stock-no = bf-eb.stock-no
            ttCompareEst.num-len  = bf-eb.num-len
            .
     END.
    END.
    
    MAIN-COMPARE:    
    FOR EACH ttCompareEst:
        j = 0.
        FOR EACH bf-eb  NO-LOCK
            WHERE bf-eb.company EQ cocode 
            AND bf-eb.est-type EQ 4
            AND bf-eb.est-no EQ ttCompareEst.est-no:               
            FIND FIRST bff-ttInputEst NO-LOCK
                 WHERE bff-ttInputEst.cStockNo EQ  bf-eb.stock-no 
                 AND bff-ttInputEst.iMolds EQ  bf-eb.num-len NO-ERROR.
             IF NOT AVAIL bff-ttInputEst THEN NEXT MAIN-COMPARE. 
             j = j + 1 .  
             opriRowid = ROWID(bf-eb) .
        END.
        IF  ipiCount EQ j THEN
        DO:                  
            oplEstimateCreate = YES.
            LEAVE MAIN-COMPARE.
        END.     
    END.      
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNewMachine D-Dialog 
PROCEDURE pNewMachine :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ cMachCode:screen-value 
        NO-LOCK NO-ERROR.

        IF AVAILABLE mach THEN 
        DO:
            dMachBlankSqFt = mach.max-len  * mach.max-wid / 144 .
            cLineDscr:SCREEN-VALUE = "L:" + STRING(mach.max-len / 12) + " ft " + "W:" + STRING(mach.max-wid / 12) + " ft - " + STRING(dMachBlankSqFt) + " Sq Ft" . 
            
        END.         
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query D-Dialog 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    DEFINE VARIABLE iMoldsCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dTotSqFtCount AS DECIMAL NO-UNDO.     
    DEFINE BUFFER bff-ttInputEst FOR ttInputEst .     

    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY BROWSE-1 FOR EACH ttInputEst
            NO-LOCK BY ttInputEst.cStockNo.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.  
        i = 0 .
        iMoldsCount = 0.
        FOR EACH bff-ttInputEst:
           i = i + 1.
           iMoldsCount = iMoldsCount + bff-ttInputEst.iMolds .
           dTotSqFtCount = dTotSqFtCount + bff-ttInputEst.dSqFt  .          
        END.
        ASSIGN
          iItem:SCREEN-VALUE = string(i) 
          iMolds:SCREEN-VALUE = STRING(iMoldsCount)
          dTotSqFt:SCREEN-VALUE = STRING(dTotSqFtCount)
          dUtilization:SCREEN-VALUE = string((dTotSqFtCount / dMachBlankSqFt) * 100).                  
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttInputEst"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec D-Dialog 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-dec AS DECIMAL DECIMALS 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER op-dec AS DECIMAL DECIMALS 6 NO-UNDO.
    
    FIND FIRST tt-64-dec WHERE
        SUBSTRING(STRING(tt-64-dec.DEC),1,3) EQ substring(STRING(ip-dec),1,3) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-64-dec  THEN
        op-error = YES.
    ELSE  op-dec = tt-64-dec.DEC .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-board D-Dialog 
PROCEDURE valid-board :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
    
    FIND item WHERE item.company = cocode
                AND item.i-no = cBoard:SCREEN-VALUE 
                    NO-LOCK NO-ERROR.
                    
    IF NOT AVAIL item THEN DO:
      MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX INFO.
     APPLY "entry" TO cBoard .
                    oplOutError = YES .
    END.
    IF AVAIL ITEM THEN
    cBoardDscr:SCREEN-VALUE = ITEM.i-name.      
      
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mach D-Dialog 
PROCEDURE valid-mach :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST mach
            {sys/look/machW.i}
            AND mach.m-code EQ cMachCode:screen-value 
        NO-LOCK NO-ERROR.

        IF NOT AVAILABLE mach THEN 
        DO:
            MESSAGE "Must enter a valid Machine Code, try help"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cMachCode .
            oplOutError = YES .
        END.   

        IF NOT oplOutError AND mach.obsolete THEN 
        DO:
            RUN displayMessage("27").
            APPLY "entry" TO cMachCode .
            oplOutError = YES .
        END.            
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

