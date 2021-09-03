&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: oe/dAddOrder.w
  
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
{oe\ttInputOrd.i}

DEFINE INPUT PARAMETER ipcSourceType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSourceValue AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustomerPo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttEstItem.
DEFINE OUTPUT PARAMETER oplBack AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplCancel AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc AS CHARACTER NO-UNDO.
RUN spGetSessionParam ("Company", OUTPUT cCompany).
RUN spGetSessionParam ("Location", OUTPUT cLoc).

DEF VAR v-duelist AS cha INIT "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,NCUST,NITEM,NCNI,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" NO-UNDO. /* Task 04081403 */
DEF VAR ll-valid-po-no AS LOG NO-UNDO.
DEF VARIABLE lOeprompt AS LOGICAL NO-UNDO.
DEFIN VARIABLE cRtnChar AS LOGICAL NO-UNDO.
DEFIN VARIABLE lRecFound AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cCompany, "OEPROMPT", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lOeprompt = LOGICAL(cRtnChar) NO-ERROR.
    
DEFINE VARIABLE hOrderEntryProcs AS HANDLE NO-UNDO. 
RUN oe/OrderEntryProcs.p PERSISTENT SET hOrderEntryProcs.

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
&Scoped-define INTERNAL-TABLES ttInputOrdLine

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttInputOrdLine.line ttInputOrdLine.cItemType ttInputOrdLine.est-no ttInputOrdLine.i-no ttInputOrdLine.part-no ttInputOrdLine.i-name ttInputOrdLine.qty ttInputOrdLine.cQtyUom ttInputOrdLine.price ttInputOrdLine.pr-uom ttInputOrdLine.po-no ttInputOrdLine.t-price ttInputOrdLine.tax ttInputOrdLine.e-num  ttInputOrdLine.lCreateRel ttInputOrdLine.lCreateJob ttInputOrdLine.lCreatePo
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttInputOrdLine WHERE ttInputOrdLine.Company = cCompany ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttInputOrdLine WHERE ttInputOrdLine.Company = cCompany ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttInputOrdLine
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttInputOrdLine


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cCustNo sold-to Btn_OK Btn_Cancel BROWSE-1 ~
btn-add btn-copy btn-update btn-delete cEstNo iQuoteNo overRun underRun ship-to ~
cCustPo cDue dtDueDate btnCalendar-1 Btn_Back Btn_Advanced
&Scoped-Define DISPLAYED-OBJECTS cCustNo sold-to cEstNo iQuoteNo overRun underRun ~
ship-to cCustPo cDue dtDueDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btn-add 
     LABEL "Add " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-copy 
     LABEL "Copy " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete 
     LABEL "Delete " 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-update 
     LABEL "Update " 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 18 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Create Order" 
     SIZE 18 BY 1.29
     BGCOLOR 8 .
     
DEFINE BUTTON Btn_Back AUTO-GO 
    LABEL "&Back" 
    SIZE 15 BY 1.00
    BGCOLOR 8 . 
        
DEFINE BUTTON Btn_Advanced  
    LABEL "&Advanced" 
    SIZE 15 BY 1.00
    BGCOLOR 8 .    

DEFINE VARIABLE cCustNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer ID#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPo AS CHARACTER FORMAT "X(15)":U 
     LABEL "Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 20.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cDue AS CHARACTER FORMAT "X(5)":U 
     LABEL "Due Date" 
     VIEW-AS FILL-IN 
     SIZE 7.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cEstNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE overRun AS INTEGER FORMAT ">>9%":U 
     LABEL "Over/Under" 
     VIEW-AS FILL-IN 
     SIZE 7.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.
     
DEFINE VARIABLE underRun AS INTEGER FORMAT ">>9%":U 
     LABEL "Over/Under"  
     VIEW-AS FILL-IN 
     SIZE 7.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.          

DEFINE VARIABLE dtDueDate AS DATE FORMAT "99/99/9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 13.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iQuoteNo AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Quote #" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sold-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Sold To" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 159 BY 4.05
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 256.9 BY 17.14
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttInputOrdLine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttInputOrdLine.line LABEL "Lin" WIDTH 5 LABEL-BGCOLOR 14 FORMAT ">9"
    ttInputOrdLine.cItemType LABEL "Item Type"  LABEL-BGCOLOR 14 FORMAT "x(10)"
    ttInputOrdLine.est-no LABEL "Estimate" FORMAT "x(8)" WIDTH 12 LABEL-BGCOLOR 14
    ttInputOrdLine.i-no LABEL "Item Id/Misc" FORMAT "x(15)" WIDTH 22 LABEL-BGCOLOR 14
    ttInputOrdLine.part-no LABEL "Customer Part" FORMAT "x(15)" WIDTH 22 LABEL-BGCOLOR 14
    ttInputOrdLine.i-name LABEL "Description" FORMAT "x(30)" WIDTH 30 LABEL-BGCOLOR 14
    ttInputOrdLine.qty LABEL "Qty" FORMAT ">>>,>>>,>>9" WIDTH 14 LABEL-BGCOLOR 14
    ttInputOrdLine.cQtyUom LABEL "Uom" WIDTH 6 LABEL-BGCOLOR 14
    ttInputOrdLine.price LABEL "Price" WIDTH 12 LABEL-BGCOLOR 14
    ttInputOrdLine.pr-uom LABEL "Uom" FORMAT "x(3)" WIDTH 6 LABEL-BGCOLOR 14
    ttInputOrdLine.po-no LABEL "PO #" FORMAT "x(15)" WIDTH 18 LABEL-BGCOLOR 14
    ttInputOrdLine.t-price LABEL "Total" FORMAT "->>>,>>>,>>9.99" LABEL-BGCOLOR 14
    ttInputOrdLine.tax LABEL "Tax" WIDTH 6 FORMAT "Yes/No" LABEL-BGCOLOR 14
    ttInputOrdLine.e-num LABEL "POLine" WIDTH 6 FORMAT ">9" LABEL-BGCOLOR 14
    ttInputOrdLine.lCreateRel LABEL "Create Release " WIDTH 14 FORMAT "Yes/No" LABEL-BGCOLOR 14
    ttInputOrdLine.lCreateJob LABEL "Create Job " WIDTH 12 FORMAT "Yes/No" LABEL-BGCOLOR 14
    ttInputOrdLine.lCreatePo LABEL "Create Po " WIDTH 12 FORMAT "Yes/No" LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 255.4 BY 14.52
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cEstNo AT ROW 2.24 COL 19.6 COLON-ALIGNED WIDGET-ID 266
     iQuoteNo AT ROW 3.33 COL 19.6 COLON-ALIGNED WIDGET-ID 268
     cCustNo AT ROW 2.24 COL 56.8 COLON-ALIGNED WIDGET-ID 176
     sold-to AT ROW 3.33 COL 56.8 COLON-ALIGNED WIDGET-ID 178
     ship-to AT ROW 2.24 COL 92 COLON-ALIGNED WIDGET-ID 272
     overRun AT ROW 3.33 COL 92 COLON-ALIGNED WIDGET-ID 270
     underRun AT ROW 3.33 COL 102 COLON-ALIGNED NO-LABEL
     cCustPo AT ROW 2.24 COL 132.2 COLON-ALIGNED WIDGET-ID 274
     cDue AT ROW 3.33 COL 124 COLON-ALIGNED WIDGET-ID 276
     dtDueDate AT ROW 3.33 COL 132.2 COLON-ALIGNED NO-LABEL WIDGET-ID 278 
     btnCalendar-1 AT ROW 3.33 COL 146.6
     Btn_Back AT ROW 4.36 COL 5.4
     Btn_Advanced AT ROW 4.36 COL 140.4
     BROWSE-1 AT ROW 6.76 COL 5.6
     btn-add AT ROW 21.86 COL 5.4 WIDGET-ID 16
     btn-copy AT ROW 21.86 COL 21 WIDGET-ID 252
     btn-update AT ROW 21.86 COL 36.6 WIDGET-ID 256
     btn-delete AT ROW 21.86 COL 52.6 WIDGET-ID 254 
     
     Btn_OK AT ROW 23.67 COL 224.8
     Btn_Cancel AT ROW 23.67 COL 243.8
     "Order Header" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 1.19 COL 5 WIDGET-ID 206
     "Order Line" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 5.86 COL 7.4 WIDGET-ID 264
     "/" VIEW-AS TEXT
          SIZE 1 BY .71 AT ROW 3.33 COL 102  
     RECT-4 AT ROW 1.48 COL 2 WIDGET-ID 236
     RECT-5 AT ROW 6.24 COL 4.4 WIDGET-ID 262
     SPACE(1.99) SKIP(2.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add Order"
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
/* BROWSE-TAB BROWSE-1 Btn_Cancel D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME  Dialog-Frame
   3                                                                    */   
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttInputOrdLine WHERE ttInputOrdLine.Company = cCompany ~
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

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Set Estimate */
DO:             
      EMPTY TEMP-TABLE ttInputOrdLine .
      DELETE OBJECT hOrderEntryProcs.
      oplBack = NO.
      oplCancel = YES.
      APPLY "END-ERROR":U TO SELF.
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add D-Dialog
ON CHOOSE OF btn-add IN FRAME D-Dialog /* Add  */
DO:
        DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.
        DEFINE VARIABLE lError   AS LOGICAL NO-UNDO.
        DEFINE BUFFER bff-ttInputOrdLine FOR ttInputOrdLine .
                           
        RUN oe/dAddEditOrdLine.w (?,ROWID(eb),"Add","",
            "",
            "",
            "",
            NO, OUTPUT lv-rowid) . 
        FIND FIRST bff-ttInputOrdLine NO-LOCK
            WHERE bff-ttInputOrdLine.Company EQ cCompany
            AND ROWID(bff-ttInputOrdLine) EQ lv-rowid NO-ERROR .
        IF AVAILABLE bff-ttInputOrdLine THEN
            RUN repo-query (lv-rowid). 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy D-Dialog
ON CHOOSE OF btn-copy IN FRAME D-Dialog /* Copy  */
DO:
        /*DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        DEFINE BUFFER bff-ttInputOrdLine FOR ttInputOrdLine.         
    
        IF AVAILABLE ttInputOrdLine THEN
        DO:   
            BUFFER-COPY ttInputOrdLine  TO bff-ttInputOrdLine .
            lv-rowid = IF AVAILABLE eb THEN ROWID(eb) ELSE ?.
            RUN est/dAddEditComp.w (RECID(ttInputOrdLine),lv-rowid,"Copy",cCustPart:SCREEN-VALUE IN FRAME {&frame-name},
                item-name:SCREEN-VALUE IN FRAME {&frame-name},
                cCustPart:SCREEN-VALUE IN FRAME {&frame-name},
                fg-cat:SCREEN-VALUE IN FRAME {&frame-name},
                LOGICAL(tb_auto:SCREEN-VALUE IN FRAME {&frame-name}), OUTPUT lv-rowid) . 
            
            RUN repo-query (lv-rowid).            
        END. */     
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete D-Dialog
ON CHOOSE OF btn-delete IN FRAME D-Dialog /* Delete  */
DO:
       /* DEFINE VARIABLE hftp     AS HANDLE NO-UNDO.
        DEFINE VARIABLE lv-rowid AS ROWID  NO-UNDO.
        IF AVAILABLE ttInputOrdLine THEN 
        DO:
            MESSAGE "Are you sure you want to delete this Component?" 
                VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN RETURN NO-APPLY.  
         
            DELETE ttInputOrdLine .
            RUN repo-query (lv-rowid).
        
        END. */                                            
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update D-Dialog
ON CHOOSE OF btn-update IN FRAME D-Dialog /* Update  */
DO:
        /*DEFINE VARIABLE lv-rowid  AS ROWID NO-UNDO. 
        DEFINE VARIABLE rwRowidEb AS ROWID NO-UNDO. 
        IF AVAILABLE ttInputOrdLine THEN 
        DO:
            rwRowidEb = IF AVAILABLE eb THEN ROWID(eb) ELSE ?.
            RUN est/dAddEditComp.w (RECID(ttInputOrdLine),rwRowidEb,"Update",cCustPart:SCREEN-VALUE IN FRAME {&frame-name},
                item-name:SCREEN-VALUE IN FRAME {&frame-name},
                cCustPart:SCREEN-VALUE IN FRAME {&frame-name},
                fg-cat:SCREEN-VALUE IN FRAME {&frame-name},
                LOGICAL(tb_auto:SCREEN-VALUE IN FRAME {&frame-name}),OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(ttInputOrdLine)).
        END. */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
      DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
      
      EMPTY TEMP-TABLE ttInputOrdLine .
      
      oplBack = NO.
      oplCancel = YES.     
      APPLY "END-ERROR":U TO SELF.
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Save */
DO:
        /*DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        iCount = 0.
        FOR EACH ttInputOrdLine NO-LOCK :
            iCount = iCount + 1.
        END.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        
        RUN valid-due-code(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
        
        RUN valid-est-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
        
        RUN valid-sold-id(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
        
        RUN valid-po-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.

        IF INTEGER(quantity:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Quantity must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO quantity .
            RETURN NO-APPLY.
        END.         

        RUN valid-cust-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .

        RUN valid-fgitem(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.

        RUN valid-part-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.

        RUN valid-procat(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .   

        RUN valid-ship-id(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY . 
        
        IF iCount LE 0 THEN
        DO:
            MESSAGE "Please add atleast one component..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO quantity .
            RETURN NO-APPLY.
        END.
                         
        SESSION:SET-WAIT-STATE("general").
  
        RUN create-ttfrmout.
         
        IF ipType EQ "Edit" THEN 
        DO:
            RUN est/UpdSetEst.p(INPUT  ipriRowid ) .
        END.
                 
        SESSION:SET-WAIT-STATE("").
  
        APPLY "close" TO THIS-PROCEDURE.*/
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Back D-Dialog
ON CHOOSE OF Btn_Back IN FRAME D-Dialog /* Back */
    DO:
        
        oplBack = YES.
        
        APPLY "close" TO THIS-PROCEDURE.        
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Advanced
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Advanced D-Dialog
ON CHOOSE OF Btn_Advanced IN FRAME D-Dialog /* Advanced */
    DO:
        
               
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustNo D-Dialog
ON HELP OF cCustNo IN FRAME D-Dialog /* Customer ID# */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-custact.w (cCompany,"", OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)                
                .     
        APPLY "value-changed" TO cCustNo IN FRAME {&FRAME-NAME}.
        APPLY "entry" TO cCustNo IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustNo D-Dialog
ON LEAVE OF cCustNo IN FRAME D-Dialog /* Customer ID# */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
   
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-cust-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST cust WHERE cust.company = cCompany
                AND cust.cust-no EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR . 
                
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cCompany
                AND shipto.cust-no EQ cCustNo:SCREEN-VALUE
                AND shipto.ship-id EQ cCustNo:SCREEN-VALUE  NO-ERROR.
             
            IF AVAILABLE shipto THEN
                ASSIGN ship-to:SCREEN-VALUE   = shipto.ship-id .    
                
            FIND FIRST soldto NO-LOCK
                 WHERE soldto.company EQ cCompany
                 AND soldto.cust-no EQ cCustNo:SCREEN-VALUE
                 AND soldto.sold-id BEGINS trim(cCustNo:SCREEN-VALUE)
                 USE-INDEX sold-id NO-ERROR.
                 
            IF AVAIL soldto THEN
                ASSIGN              
                   sold-to:SCREEN-VALUE      = soldto.sold-id  .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME cCustPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPo D-Dialog
ON HELP OF cCustPo IN FRAME D-Dialog /* Customer PO# */
DO:
                                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPo D-Dialog
ON LEAVE OF cCustPo IN FRAME D-Dialog /* Customer PO# */
DO:
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
    IF LASTKEY NE -1 THEN 
    DO:
               
    END.   
                                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDue D-Dialog
ON HELP OF cDue IN FRAME D-Dialog /* Due Date */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-dcode.w (v-duelist, OUTPUT char-val).
   
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)               
                .                                    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDue D-Dialog
ON LEAVE OF cDue IN FRAME D-Dialog /* Due Date */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:              
            
        END.                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cEstNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cEstNo D-Dialog
ON HELP OF cEstNo IN FRAME D-Dialog /* Estimate # */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-est.w (cCompany,cLoc,cEstNo:screen-value, OUTPUT char-val).
        
        IF char-val <> ""  THEN 
        DO:
           FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
           IF AVAIL eb THEN DO:
            ASSIGN
                SELF:screen-value      = eb.est-no
                cCustNo:SCREEN-VALUE   = eb.cust-no.                
                RUN pGetQuote.
           END.
        END.        
              
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cEstNo D-Dialog
ON LEAVE OF cEstNo IN FRAME D-Dialog /* Estimate # */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
   
        IF LASTKEY NE -1 THEN 
        DO:             
           RUN pNewEstimate.
           RUN pGetQuote.
        END. 
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iQuoteNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQuoteNo D-Dialog
ON HELP OF iQuoteNo IN FRAME D-Dialog /* Quote # */
DO:
                                  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQuoteNo D-Dialog
ON LEAVE OF iQuoteNo IN FRAME D-Dialog /* Quote # */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
                      
        END.                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-to D-Dialog
ON HELP OF ship-to IN FRAME D-Dialog /* Ship To */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-shipto.w (cCompany,"",cCustNo:SCREEN-VALUE,"", OUTPUT char-val).
   
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)                  
                .                                    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-to D-Dialog
ON LEAVE OF ship-to IN FRAME D-Dialog /* Ship To */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
                        
        END.                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sold-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sold-to D-Dialog
ON HELP OF sold-to IN FRAME D-Dialog /* Sold To */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-soldto.w (cCompany, cCustno:screen-value,sold-to:screen-value, OUTPUT char-val).
   
        IF char-val <> "" AND SELF:screen-value <> entry(2,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(2,char-val)                
                .                                    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sold-to D-Dialog
ON LEAVE OF sold-to IN FRAME D-Dialog /* Sold To */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
                         
        END.                                
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
    
    RUN enable_UI.
    {methods/nowait.i}  
    
    RUN pNewEstimate. 
    
    DO WITH FRAME {&frame-name}:  
        IF ipcSourceType EQ "Estimate" THEN 
        DO:             
            APPLY "entry" TO cEstNo IN FRAME {&FRAME-NAME}.            
        END.    
                
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
   /* DEFINE BUFFER bf-ttInputOrdLine FOR ttInputOrdLine.     
    CREATE tt-eb-set .
    ASSIGN 
        tt-eb-set.est-type         = 6
        tt-eb-set.Company          = cCompany
        tt-eb-set.loc              = locode
        tt-eb-set.form-no          = 0
        tt-eb-set.blank-no         = 0 
        tt-eb-set.part-no          = cCustPart
        tt-eb-set.stock-no         = fg-no
        tt-eb-set.part-dscr1       = item-name
        tt-eb-set.part-dscr2       = item-dscr 
        tt-eb-set.cust-no          = cCustNo 
        tt-eb-set.ship-id          = ship-to         
        tt-eb-set.len              = len 
        tt-eb-set.wid              = wid 
        tt-eb-set.dep              = dep        
        tt-eb-set.procat           = fg-cat
        tt-eb-set.eqty             = quantity.
        IF cType EQ "No" THEN
        ASSIGN 
          tt-eb-set.set-is-assembled = FALSE
          tt-eb-set.pur-man          = TRUE .
        ELSE IF cType EQ "Auto" THEN
        ASSIGN 
          tt-eb-set.set-is-assembled = TRUE
          tt-eb-set.pur-man          = TRUE .
        ELSE IF cType EQ "Yes" THEN
        ASSIGN 
          tt-eb-set.set-is-assembled = FALSE
          tt-eb-set.pur-man          = FALSE .
        ELSE IF cType EQ "Q" THEN
        ASSIGN 
          tt-eb-set.set-is-assembled = TRUE
          tt-eb-set.pur-man          = FALSE .
                  
                     
    FOR EACH  bf-ttInputOrdLine EXCLUSIVE-LOCK:
        ASSIGN
            bf-ttInputOrdLine.iQuantity = quantity
            bf-ttInputOrdLine.cCustomer = cCustNo
            bf-ttInputOrdLine.cShipTo   = ship-to
            .
        
        ASSIGN 
            bf-ttInputOrdLine.copy-qty[2]  = lv-copy-qty[2] 
            bf-ttInputOrdLine.copy-qty[3]  = lv-copy-qty[3] 
            bf-ttInputOrdLine.copy-qty[4]  = lv-copy-qty[4] 
            bf-ttInputOrdLine.copy-qty[5]  = lv-copy-qty[5] 
            bf-ttInputOrdLine.copy-qty[6]  = lv-copy-qty[6] 
            bf-ttInputOrdLine.copy-qty[7]  = lv-copy-qty[7] 
            bf-ttInputOrdLine.copy-qty[8]  = lv-copy-qty[8] 
            bf-ttInputOrdLine.copy-qty[9]  = lv-copy-qty[9] 
            bf-ttInputOrdLine.copy-qty[10] = lv-copy-qty[10]
         
            bf-ttInputOrdLine.copy-qty[11] = lv-copy-qty[11] 
            bf-ttInputOrdLine.copy-qty[12] = lv-copy-qty[12] 
            bf-ttInputOrdLine.copy-qty[13] = lv-copy-qty[13] 
            bf-ttInputOrdLine.copy-qty[14] = lv-copy-qty[14] 
            bf-ttInputOrdLine.copy-qty[15] = lv-copy-qty[15] 
            bf-ttInputOrdLine.copy-qty[16] = lv-copy-qty[16] 
            bf-ttInputOrdLine.copy-qty[17] = lv-copy-qty[17] 
            bf-ttInputOrdLine.copy-qty[18] = lv-copy-qty[18] 
            bf-ttInputOrdLine.copy-qty[19] = lv-copy-qty[19] 
            bf-ttInputOrdLine.copy-qty[20] = lv-copy-qty[20] 

            bf-ttInputOrdLine.copy-rel[1]  = lv-copy-rel[1]
            bf-ttInputOrdLine.copy-rel[2]  = lv-copy-rel[2] 
            bf-ttInputOrdLine.copy-rel[3]  = lv-copy-rel[3] 
            bf-ttInputOrdLine.copy-rel[4]  = lv-copy-rel[4] 
            bf-ttInputOrdLine.copy-rel[5]  = lv-copy-rel[5] 
            bf-ttInputOrdLine.copy-rel[6]  = lv-copy-rel[6] 
            bf-ttInputOrdLine.copy-rel[7]  = lv-copy-rel[7] 
            bf-ttInputOrdLine.copy-rel[8]  = lv-copy-rel[8] 
            bf-ttInputOrdLine.copy-rel[9]  = lv-copy-rel[9] 
            bf-ttInputOrdLine.copy-rel[10] = lv-copy-rel[10]
         
            bf-ttInputOrdLine.copy-rel[11] = lv-copy-rel[11] 
            bf-ttInputOrdLine.copy-rel[12] = lv-copy-rel[12] 
            bf-ttInputOrdLine.copy-rel[13] = lv-copy-rel[13] 
            bf-ttInputOrdLine.copy-rel[14] = lv-copy-rel[14] 
            bf-ttInputOrdLine.copy-rel[15] = lv-copy-rel[15] 
            bf-ttInputOrdLine.copy-rel[16] = lv-copy-rel[16] 
            bf-ttInputOrdLine.copy-rel[17] = lv-copy-rel[17] 
            bf-ttInputOrdLine.copy-rel[18] = lv-copy-rel[18] 
            bf-ttInputOrdLine.copy-rel[19] = lv-copy-rel[19] 
            bf-ttInputOrdLine.copy-rel[20] = lv-copy-rel[20] .    
     
        IF cLogicalRunShip[1] NE "" THEN
            ASSIGN
                bf-ttInputOrdLine.copy-runship[1]  = cLogicalRunShip[1]
                bf-ttInputOrdLine.copy-runship[2]  = cLogicalRunShip[2]
                bf-ttInputOrdLine.copy-runship[3]  = cLogicalRunShip[3]
                bf-ttInputOrdLine.copy-runship[4]  = cLogicalRunShip[4]
                bf-ttInputOrdLine.copy-runship[5]  = cLogicalRunShip[5]
                bf-ttInputOrdLine.copy-runship[6]  = cLogicalRunShip[6]
                bf-ttInputOrdLine.copy-runship[7]  = cLogicalRunShip[7]
                bf-ttInputOrdLine.copy-runship[8]  = cLogicalRunShip[8]
                bf-ttInputOrdLine.copy-runship[9]  = cLogicalRunShip[9]
                bf-ttInputOrdLine.copy-runship[10] = cLogicalRunShip[10]
                bf-ttInputOrdLine.copy-runship[11] = cLogicalRunShip[11]
                bf-ttInputOrdLine.copy-runship[12] = cLogicalRunShip[12]
                bf-ttInputOrdLine.copy-runship[13] = cLogicalRunShip[13]
                bf-ttInputOrdLine.copy-runship[14] = cLogicalRunShip[14]
                bf-ttInputOrdLine.copy-runship[15] = cLogicalRunShip[15]
                bf-ttInputOrdLine.copy-runship[16] = cLogicalRunShip[16]
                bf-ttInputOrdLine.copy-runship[17] = cLogicalRunShip[17]
                bf-ttInputOrdLine.copy-runship[18] = cLogicalRunShip[18]
                bf-ttInputOrdLine.copy-runship[19] = cLogicalRunShip[19]
                bf-ttInputOrdLine.copy-runship[20] = cLogicalRunShip[20].
        ELSE 
        DO:  
            FIND FIRST eb NO-LOCK 
                WHERE eb.company EQ cCompany
                AND ROWID(eb) EQ ipriRowid NO-ERROR .    
            IF AVAILABLE eb  THEN 
            DO:
                FIND est-qty NO-LOCK
                    WHERE est-qty.company EQ eb.company
                    AND est-qty.est-no EQ eb.est-no
                    AND est-qty.eqty EQ eb.eqty 
                    NO-ERROR.
                IF AVAILABLE est-qty THEN 
                DO:   
                    ASSIGN
                        bf-ttInputOrdLine.copy-runship[1]  = STRING(est-qty.whsed[1])
                        bf-ttInputOrdLine.copy-runship[2]  = STRING(est-qty.whsed[2])
                        bf-ttInputOrdLine.copy-runship[3]  = STRING(est-qty.whsed[3])
                        bf-ttInputOrdLine.copy-runship[4]  = STRING(est-qty.whsed[4])
                        bf-ttInputOrdLine.copy-runship[5]  = STRING(est-qty.whsed[5])
                        bf-ttInputOrdLine.copy-runship[6]  = STRING(est-qty.whsed[6])
                        bf-ttInputOrdLine.copy-runship[7]  = STRING(est-qty.whsed[7])
                        bf-ttInputOrdLine.copy-runship[8]  = STRING(est-qty.whsed[8])
                        bf-ttInputOrdLine.copy-runship[9]  = STRING(est-qty.whsed[9])
                        bf-ttInputOrdLine.copy-runship[10] = STRING(est-qty.whsed[10])
                        bf-ttInputOrdLine.copy-runship[11] = STRING(est-qty.whsed[11])
                        bf-ttInputOrdLine.copy-runship[12] = STRING(est-qty.whsed[12])
                        bf-ttInputOrdLine.copy-runship[13] = STRING(est-qty.whsed[13])
                        bf-ttInputOrdLine.copy-runship[14] = STRING(est-qty.whsed[14])
                        bf-ttInputOrdLine.copy-runship[15] = STRING(est-qty.whsed[15])
                        bf-ttInputOrdLine.copy-runship[16] = STRING(est-qty.whsed[16])
                        bf-ttInputOrdLine.copy-runship[17] = STRING(est-qty.whsed[17])
                        bf-ttInputOrdLine.copy-runship[18] = STRING(est-qty.whsed[18])
                        bf-ttInputOrdLine.copy-runship[19] = STRING(est-qty.whsed[19])
                        bf-ttInputOrdLine.copy-runship[20] = STRING(est-qty.whsed[20]).                
                END.
            END.  
        END.
              
       
    END.
    RELEASE tt-eb-set.*/
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
  DISPLAY cCustNo sold-to cEstNo iQuoteNo overRun underRun ship-to cCustPo cDue 
          dtDueDate 
      WITH FRAME D-Dialog.
  ENABLE cCustNo sold-to Btn_OK Btn_Cancel BROWSE-1 btn-add btn-copy btn-update 
         btn-delete cEstNo iQuoteNo overRun underRun ship-to cCustPo cDue dtDueDate 
         btnCalendar-1 Btn_Back Btn_Advanced
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNewEstimate D-Dialog 
PROCEDURE pNewEstimate :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLine AS INTEGER NO-UNDO.
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:  
      cEstNo:SCREEN-VALUE = ipcSourceValue.
      cCustPo:SCREEN-VALUE = ipcCustomerPo.
      
      RUN OrderEntry_GetEstDetail IN hOrderEntryProcs(INPUT TABLE ttEstItem, INPUT cCompany, INPUT ipcSourceValue, OUTPUT TABLE ttInputOrdLine, OUTPUT TABLE ttInputOrd ).
            
      {&open-query-{&browse-name}}  
      
      FIND FIRST ttInputOrd NO-LOCK NO-ERROR.
      IF AVAIL ttInputOrd THEN 
      ASSIGN
          cCustNo:SCREEN-VALUE = ttInputOrd.cust-no                   
          ship-to:SCREEN-VALUE = ttInputOrd.ship-id
          sold-to:SCREEN-VALUE = ttInputOrd.cust-no                  
          cDue:SCREEN-VALUE =  "On"
          dtDueDate:SCREEN-VALUE = string(ttInputOrd.due-date).
      
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
    
    

    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY BROWSE-1 FOR EACH ttInputOrdLine
            NO-LOCK BY ttInputOrdLine.line.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetOverUnderPct D-Dialog 
PROCEDURE pGetOverUnderPct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dOverPer AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dUnderPer AS DECIMAL NO-UNDO.
   DEFINE VARIABLE cTagDesc AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lAvailable AS LOGICAL NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:          
    RUN oe/GetOverUnderPct.p(cCompany,
                           cCustNo:SCREEN-VALUE ,
                           TRIM(ship-to:SCREEN-VALUE),
                           "", /* FG Item*/
                           0,
                           OUTPUT dOverPer , OUTPUT dUnderPer, OUTPUT cTagDesc ) .  
      overRun:SCREEN-VALUE = STRING(dOverPer).
      underRun:SCREEN-VALUE = STRING(dUnderPer). 
      //RUN pAddTag ("Over Percentage",cTagDesc ).
      //RUN pAddTag ("Under Percentage",cTagDesc ).
  END.
  //  deAutoOverRun = dOverPer.
   // deAutoUnderRun = dUnderPer.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no D-Dialog 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST cust
            WHERE cust.company  EQ cCompany
            AND cust.cust-no   EQ cCustNo:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Customer, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cCustNo .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetQuote D-Dialog 
PROCEDURE pGetQuote :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:  
       IF cEstNo:SCREEN-VALUE NE "" THEN DO:
       FIND FIRST est NO-LOCK
          WHERE est.company EQ cCompany
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(cEstNo:SCREEN-VALUE))) + TRIM(cEstNo:SCREEN-VALUE)
          NO-ERROR.
      IF NOT AVAIL est THEN RETURN.
      
      FOR EACH quotehd
            WHERE quotehd.company EQ est.company
              AND quotehd.loc     EQ est.loc
              AND quotehd.est-no  EQ est.est-no
             NO-LOCK,

             EACH quoteitm OF quotehd NO-LOCK,

             EACH quoteqty OF quoteitm NO-LOCK:
            iQuoteNo:SCREEN-VALUE = string(quotehd.q-no) . 
          LEAVE.
        END.      
     END. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fgitem D-Dialog 
PROCEDURE valid-fgitem :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
     
       /* IF fg-no:SCREEN-VALUE  NE "" AND NOT lCreateNewFG THEN 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company  EQ cCompany
                AND itemfg.i-no    EQ fg-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE itemfg  THEN 
            DO:
                MESSAGE "This item does not exist, would you like to add it?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll-ans AS LOG.
                IF ll-ans THEN
                    ASSIGN lCreateNewFG = TRUE .
                IF NOT ll-ans THEN 
                DO:
                    APPLY "entry" TO fg-no .
                    oplOutError = YES .
                END.
            END.
        END.*/
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-item-dscr D-Dialog 
PROCEDURE valid-item-dscr :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
       /* IF item-dscr:SCREEN-VALUE EQ "" THEN 
        DO:
            MESSAGE "Description must not be blank. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO item-dscr.
            oplOutError = YES .
        END. */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-item-name D-Dialog 
PROCEDURE valid-item-name :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
       /* IF item-name:SCREEN-VALUE EQ "" THEN 
        DO:
            MESSAGE "Item Name must not be blank. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO item-name.
            oplOutError = YES .
        END.*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no D-Dialog 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        /*IF cCustPart:SCREEN-VALUE  EQ "" THEN 
        DO:
            MESSAGE "Cust Part# must be enter..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO cCustPart .
            oplOutError = YES .
        END.   */
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat D-Dialog 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
       /* fg-cat:SCREEN-VALUE  = CAPS(fg-cat:SCREEN-VALUE).

        IF NOT CAN-FIND(FIRST fgcat
            WHERE fgcat.company EQ cCompany
            AND fgcat.procat  EQ fg-cat:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Invalid FG Category, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-cat .
            oplOutError = YES .
        END.  */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id D-Dialog 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF ship-to:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST shipto
            WHERE shipto.company EQ cCompany
            AND shipto.cust-no EQ cCustNo:SCREEN-VALUE
            AND shipto.ship-id EQ ship-to:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Ship To, try help...             "  VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ship-to .
            oplOutError = YES .
      
        END.  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sold-id D-Dialog 
PROCEDURE valid-sold-id :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF sold-to:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST shipto
            WHERE soldto.company EQ cCompany
            AND soldto.cust-no EQ cCustNo:SCREEN-VALUE
            AND soldto.sold-id EQ sold-to:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Sold To, try help...             "  VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO sold-to .
            oplOutError = YES .         
        END.  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no D-Dialog 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEF BUFFER b-oe-ordl FOR oe-ordl.
    
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no EQ cCustNo:SCREEN-VALUE
          AND cust.po-mandatory
        NO-ERROR.
        
        IF AVAIL cust AND TRIM(cCustPO:SCREEN-VALUE) EQ "" THEN DO:
          MESSAGE "PO# is mandatory for this Customer..."
              VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO cCustPO.
           oplOutError = YES . 
        END.
        
     IF NOT ll-valid-po-no AND lOeprompt AND cCustPO:SCREEN-VALUE NE "" THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ cCompany
          AND b-oe-ordl.po-no   EQ cCustPO:SCREEN-VALUE
          AND b-oe-ordl.cust-no EQ cCustNo:SCREEN-VALUE            
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN DO:
      MESSAGE "Customer PO already exists for Order/Item - " + 
              TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
              TRIM(b-oe-ordl.i-no) " ." SKIP
              "Do you want to continue?"
          VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
      IF NOT ll-ans THEN DO:
        APPLY "entry" TO cCustPO.
        oplOutError = YES.
      END.
      ELSE ll-valid-po-no = YES.
    END.   
        
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-due-code D-Dialog 
PROCEDURE valid-due-code :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    {&methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
       IF cDue:screen-value <> "" AND
           lookup(cDue:screen-value,v-duelist) = 0 THEN 
        DO:
           MESSAGE "Invalid Due Code. Try help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cDue .
            oplOutError = YES .
        END.           
    END.
     {&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
