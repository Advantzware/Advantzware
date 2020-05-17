&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/dNewMiscEst.w
  
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
DEFINE VARIABLE opCADCAM AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE v-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE k_frac     AS DECIMAL INIT 6.25 NO-UNDO.
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

/*{est/frmotvar.i "shared"}*/
{est\ttInputEst.i}
{sys/inc/var.i shared}
{custom/gcompany.i}  

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

DEFINE VARIABLE iFormNumber      AS INTEGER NO-UNDO.
DEFINE VARIABLE iBlankNumber     AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumofCADForm    AS INTEGER NO-UNDO.
DEFINE VARIABLE iProjectCount    AS INTEGER INIT 50 NO-UNDO.
DEFINE VARIABLE lv-copy-qty      AS INTEGER EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-copy-rel      AS INTEGER EXTENT 20 NO-UNDO.
DEFINE VARIABLE cLogicalRunShip  AS CHARACTER EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-crt-est-rowid AS ROWID   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO .
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEFINE VARIABLE cCEMiscDefaultStyle AS CHARACTER NO-UNDO .
DEFINE VARIABLE cCEMiscDefaultBoard AS CHARACTER NO-UNDO .
DEFINE VARIABLE cCEMiscDefaultStackCode AS CHARACTER NO-UNDO .
DEFINE VARIABLE cStackCode AS CHARACTER NO-UNDO .
DEFINE VARIABLE iOldQty AS INTEGER NO-UNDO .
DEFINE VARIABLE lShowMessage AS LOGICAL NO-UNDO .
DEFINE VARIABLE lMiscEstimateSource AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMiscEstimateSource AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMiscEstimateSource AS INTEGER NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.

RUN sys/ref/nk1look.p (INPUT cocode, "CEMiscDefaultStyle", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  cCEMiscDefaultStyle = cRtnChar NO-ERROR .

RUN sys/ref/nk1look.p (INPUT cocode, "CEMiscDefaultBoard", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  cCEMiscDefaultBoard = cRtnChar NO-ERROR .

RUN sys/ref/nk1look.p (INPUT cocode, "CEMiscDefaultStackCode", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  cCEMiscDefaultStackCode = cRtnChar NO-ERROR .
  
RUN sys/ref/nk1look.p (INPUT cocode, "MiscEstimateSource", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  lMiscEstimateSource = logical(cRtnChar) NO-ERROR . 
  
RUN sys/ref/nk1look.p (INPUT cocode, "MiscEstimateSource", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  cMiscEstimateSource = STRING(cRtnChar) NO-ERROR .
  
RUN sys/ref/nk1look.p (INPUT cocode, "MiscEstimateSource", "I" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound). 
  iMiscEstimateSource = INTEGER(cRtnChar) NO-ERROR .  

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS quantity cCustNo ship-to cCustPart fg-no ~
item-name item-dscr len wid dep style-cod board fg-cat sub-unit iUnitCount ~
iPerPallet iPartial pallet dWeightPerM iStackHeight Btn_OK Btn_Cancel cSEst 
&Scoped-Define DISPLAYED-OBJECTS quantity cCustNo ship-to cCustPart fg-no ~
item-name item-dscr len wid dep style-cod style-dscr board fg-cat sub-unit ~
sub-Unit-dscr iUnitCount iPerPallet iPartial pallet pallet-dscr ~
tot-iUnitCount dWeightPerM iStackHeight cust-name ship-name board-dscr ~
cat-dscr cSEst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Next" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE iStackHeight AS INTEGER FORMAT ">":U INITIAL 1 
     LABEL "Stack Height" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "1",1,
                     "2",2,
                     "3",3,
                     "4",4
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE board AS CHARACTER FORMAT "X(12)":U 
     LABEL "Board" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE board-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cat-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPart AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cSEst AS CHARACTER FORMAT "X(8)":U 
     LABEL "Source Est" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-name AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dep AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Depth" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dWeightPerM AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Override Weight Per M" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "FG Category" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fg-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item Code" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iPartial AS INTEGER FORMAT "->>,>>9":U INITIAL 0 
     LABEL "Partial" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iPerPallet AS INTEGER FORMAT "->>,>>9":U INITIAL 0 
     LABEL "Per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-dscr AS CHARACTER FORMAT "X(30)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iUnitCount AS INTEGER FORMAT "->>,>>9":U INITIAL 0 
     LABEL "Pack Code Unit" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE len AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pallet AS CHARACTER FORMAT "X(10)":U 
     LABEL "Pallet" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pallet-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE quantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-name AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-cod AS CHARACTER FORMAT "X(8)":U 
     LABEL "Style Code" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sub-unit AS CHARACTER FORMAT "X(10)":U 
     LABEL "Packing Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sub-Unit-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE tot-iUnitCount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Unit/Pallet Count" 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE wid AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 122.2 BY 3
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 63.2 BY 9.81
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 58.6 BY 9.81
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 124.2 BY 15.48
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cSEst AT ROW 2.14 COL 16.8 COLON-ALIGNED WIDGET-ID 250
     quantity AT ROW 2.19 COL 49.2 COLON-ALIGNED WIDGET-ID 198
     cCustNo AT ROW 3.33 COL 16.4 COLON-ALIGNED WIDGET-ID 176
     ship-to AT ROW 3.33 COL 74.8 COLON-ALIGNED WIDGET-ID 178
     cCustPart AT ROW 5.76 COL 19.2 COLON-ALIGNED WIDGET-ID 88
     fg-no AT ROW 6.86 COL 19.2 COLON-ALIGNED WIDGET-ID 42
     item-name AT ROW 7.95 COL 19.2 COLON-ALIGNED WIDGET-ID 208
     item-dscr AT ROW 9.1 COL 19.2 COLON-ALIGNED WIDGET-ID 210
     len AT ROW 10.29 COL 12 COLON-ALIGNED WIDGET-ID 190
     wid AT ROW 10.29 COL 32.2 COLON-ALIGNED WIDGET-ID 194
     dep AT ROW 10.29 COL 52.4 COLON-ALIGNED WIDGET-ID 192
     style-cod AT ROW 11.62 COL 19.2 COLON-ALIGNED WIDGET-ID 180
     style-dscr AT ROW 11.62 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     board AT ROW 12.71 COL 19.2 COLON-ALIGNED WIDGET-ID 174
     fg-cat AT ROW 13.81 COL 19.2 COLON-ALIGNED WIDGET-ID 196
     sub-unit AT ROW 5.76 COL 82.6 COLON-ALIGNED WIDGET-ID 216
     sub-Unit-dscr AT ROW 5.76 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 218
     iUnitCount AT ROW 6.91 COL 84.6 COLON-ALIGNED WIDGET-ID 220
     iPerPallet AT ROW 6.91 COL 112.6 COLON-ALIGNED WIDGET-ID 222
     iPartial AT ROW 8.1 COL 84.6 COLON-ALIGNED
     pallet AT ROW 9.29 COL 78.4 COLON-ALIGNED WIDGET-ID 224
     pallet-dscr AT ROW 9.29 COL 93 COLON-ALIGNED NO-LABEL WIDGET-ID 226
     tot-iUnitCount AT ROW 10.71 COL 93 COLON-ALIGNED WIDGET-ID 228
     dWeightPerM AT ROW 12.1 COL 93 COLON-ALIGNED WIDGET-ID 238
     iStackHeight AT ROW 13.52 COL 93 COLON-ALIGNED WIDGET-ID 248
     Btn_OK AT ROW 15.29 COL 51
     Btn_Cancel AT ROW 15.29 COL 67
     cust-name AT ROW 3.33 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 202
     ship-name AT ROW 3.33 COL 92.8 COLON-ALIGNED NO-LABEL WIDGET-ID 204
     board-dscr AT ROW 12.76 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 212
     cat-dscr AT ROW 13.86 COL 34.2 COLON-ALIGNED NO-LABEL WIDGET-ID 214     
     " Product Input" VIEW-AS TEXT
          SIZE 16.8 BY .86 AT ROW 4.81 COL 5 WIDGET-ID 166
     " Main Input" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1.48 COL 5 WIDGET-ID 206
     " Packing" VIEW-AS TEXT
          SIZE 11 BY .86 AT ROW 4.81 COL 69 WIDGET-ID 234
     RECT-1 AT ROW 1.71 COL 3 WIDGET-ID 82
     RECT-2 AT ROW 5.24 COL 3 WIDGET-ID 230
     RECT-3 AT ROW 5.24 COL 66.6 WIDGET-ID 232
     RECT-4 AT ROW 1.24 COL 2 WIDGET-ID 236
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Miscellaneous Product Estimate"
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
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN board-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cat-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pallet-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sub-Unit-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tot-iUnitCount IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Miscellaneous Product Estimate */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        EMPTY TEMP-TABLE ttInputEst .
        
        APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board D-Dialog
ON HELP OF board IN FRAME D-Dialog /* Board */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-board.w (gcompany,"",FOCUS:SCREEN-VALUE,OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value = ENTRY(1,char-val)
                .                                    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board D-Dialog
ON LEAVE OF board IN FRAME D-Dialog /* Board */
DO:
    IF LASTKEY NE -1 THEN DO:
        IF NOT CAN-FIND(item WHERE item.company = gcompany
            AND item.i-no = board:screen-value
            AND LOOKUP(ITEM.mat-type,"P,R,B,F" ) GT 0)
            THEN 
        DO:
            MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO board.
            RETURN NO-APPLY.
        END.
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST item WHERE item.company = cocode
                AND item.i-no EQ board:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE ITEM THEN
                ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
        END.
                                                
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
        EMPTY TEMP-TABLE ttInputEst .
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Next */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        IF INTEGER(quantity:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Quantity must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO quantity .
            RETURN NO-APPLY.
        END.

        RUN pCheckRelQty(YES) .

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

        RUN valid-style(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .   

        RUN valid-board(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .   

        RUN valid-pack(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY . 

        RUN valid-pallet(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY . 

        RUN valid-sub-UnitCount(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .

        RUN valid-PerPallet(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .
         
        SESSION:SET-WAIT-STATE("general").
  
        RUN create-ttfrmout.
        
        IF ipType EQ "Edit" THEN DO:
            RUN est/UpdMiscEst.p(INPUT  ipriRowid) .
        END.
        
        
        SESSION:SET-WAIT-STATE("").
  
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustNo D-Dialog
ON HELP OF cCustNo IN FRAME D-Dialog /* Cust# */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-custact.w (gcompany,"", OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)
                cust-name:screen-value = ENTRY(2,char-val)
                .     
         APPLY "value-changed" TO cCustNo IN FRAME {&FRAME-NAME}.
         APPLY "entry" TO cCustNo IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustNo D-Dialog
ON LEAVE OF cCustNo IN FRAME D-Dialog /* Cust# */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
   
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-cust-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST cust WHERE cust.company = cocode
                AND cust.cust-no EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .
             
            IF AVAILABLE cust THEN
                ASSIGN cust-name:SCREEN-VALUE = cust.NAME .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustNo D-Dialog
ON VALUE-CHANGED OF cCustNo IN FRAME D-Dialog /* Cust# */
DO:     
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ cCustNo:SCREEN-VALUE
                AND shipto.ship-id EQ cCustNo:SCREEN-VALUE  NO-ERROR.
             
            IF AVAILABLE shipto THEN
                ASSIGN ship-to:SCREEN-VALUE = shipto.ship-id
                       ship-name:SCREEN-VALUE = shipto.ship-name .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustPart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPart D-Dialog
ON HELP OF cCustPart IN FRAME D-Dialog /* Cust Part# */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-cstprt.w (gcompany, "", FOCUS:SCREEN-VALUE, "", OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)
                item-name:screen-value = ENTRY(2,char-val)
                item-dscr:screen-value = ENTRY(3,char-val)
                fg-no:screen-value     = ENTRY(4,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPart D-Dialog
ON LEAVE OF cCustPart IN FRAME D-Dialog /* Cust Part# */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-part-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSEst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSEst D-Dialog
ON HELP OF cSEst IN FRAME D-Dialog /* Source Est */
DO:
        DEFINE VARIABLE char-val   AS CHARACTER   NO-UNDO.
                
        RUN windows/l-esttyp.w (g_company,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN cSEst:SCREEN-VALUE = ENTRY(1,char-val).
        
        APPLY "entry" TO cSEst IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSEst D-Dialog
ON LEAVE OF cSEst IN FRAME D-Dialog /* Source Est */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO.
        
        IF LASTKEY NE -1 THEN 
        DO:
            cEstNo = cSEst:SCREEN-VALUE.
            RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
            cSEst:SCREEN-VALUE = cEstNo.
            RUN valid-est-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            RUN pGetEstDetail .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSEst D-Dialog
ON VALUE-CHANGED OF cSEst IN FRAME D-Dialog /* Source Est */
DO:     
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
           /*RUN pGetEstDetail . */             
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dep D-Dialog
ON HELP OF dep IN FRAME D-Dialog /* Depth */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dep D-Dialog
ON LEAVE OF dep IN FRAME D-Dialog /* Depth */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-cat D-Dialog
ON HELP OF fg-cat IN FRAME D-Dialog /* FG Category */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-fgcat.w (gcompany,fg-cat:SCREEN-VALUE,OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value     = ENTRY(1,char-val)
                cat-dscr:screen-value = ENTRY(2,char-val) .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-cat D-Dialog
ON LEAVE OF fg-cat IN FRAME D-Dialog /* FG Category */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-procat(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST fgcat WHERE fgcat.company = cocode
                    AND fgcat.procat EQ fg-cat:SCREEN-VALUE NO-LOCK NO-ERROR .
             
                IF AVAILABLE fgcat THEN
                    ASSIGN cat-dscr:SCREEN-VALUE = fgcat.dscr .
            END.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-no D-Dialog
ON HELP OF fg-no IN FRAME D-Dialog /* FG Item Code */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-itemfa.w (gcompany, "", FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value = ENTRY(1,char-val) .
        FIND FIRST itemfg WHERE RECID(itemfg) = look-recid NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                cCustPart:SCREEN-VALUE = itemfg.part-no
                item-name:SCREEN-VALUE = itemfg.i-name
                item-dscr:SCREEN-VALUE = itemfg.part-dscr1 .
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-no D-Dialog
ON LEAVE OF fg-no IN FRAME D-Dialog /* FG Item Code */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-fgitem (OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-no D-Dialog
ON VALUE-CHANGED OF fg-no IN FRAME D-Dialog /* FG Item Code */
DO:
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company = cocode
            AND itemfg.i-no EQ fg-no:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                cCustPart:SCREEN-VALUE = itemfg.part-no
                item-name:SCREEN-VALUE = itemfg.i-name
                item-dscr:SCREEN-VALUE = itemfg.part-dscr1 .
         ASSIGN lCreateNewFG = FALSE .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iPartial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iPartial D-Dialog
ON VALUE-CHANGED OF iPartial IN FRAME D-Dialog /* Partial */
DO:
        tot-iUnitCount:SCREEN-VALUE = STRING(INTEGER(iUnitCount:SCREEN-VALUE) * INTEGER(iPerPallet:SCREEN-VALUE) + INTEGER(iPartial:SCREEN-VALUE)) .  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iPerPallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iPerPallet D-Dialog
ON LEAVE OF iPerPallet IN FRAME D-Dialog /* Per Pallet */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:

            RUN valid-PerPallet(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY .
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iPerPallet D-Dialog
ON VALUE-CHANGED OF iPerPallet IN FRAME D-Dialog /* Per Pallet */
DO:
        tot-iUnitCount:SCREEN-VALUE = STRING(INTEGER(iUnitCount:SCREEN-VALUE) * INTEGER(iPerPallet:SCREEN-VALUE) + INTEGER(iPartial:SCREEN-VALUE)) .  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-dscr D-Dialog
ON LEAVE OF item-dscr IN FRAME D-Dialog /* Description */
DO:
       DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            
        END.
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-name D-Dialog
ON LEAVE OF item-name IN FRAME D-Dialog /* Item Name */
DO:
       DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iUnitCount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iUnitCount D-Dialog
ON LEAVE OF iUnitCount IN FRAME D-Dialog /* Pack Code Unit */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:

            RUN valid-sub-UnitCount(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY .
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iUnitCount D-Dialog
ON VALUE-CHANGED OF iUnitCount IN FRAME D-Dialog /* Pack Code Unit */
DO:
        tot-iUnitCount:SCREEN-VALUE = STRING(INTEGER(iUnitCount:SCREEN-VALUE) * INTEGER(iPerPallet:SCREEN-VALUE) + INTEGER(iPartial:SCREEN-VALUE)) .  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL len D-Dialog
ON LEAVE OF len IN FRAME D-Dialog /* Length */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pallet D-Dialog
ON HELP OF pallet IN FRAME D-Dialog /* Pallet */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
   
        RUN windows/l-item.w (gcompany, "", "D", FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value        = ENTRY(1,char-val)
                pallet-dscr:screen-value = ENTRY(2,char-val)
                .    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pallet D-Dialog
ON LEAVE OF pallet IN FRAME D-Dialog /* Pallet */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
       
    
        RUN valid-pallet(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY. 
    

        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST ITEM WHERE ITEM.company = cocode
                AND ITEM.i-no EQ pallet:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE ITEM THEN
                ASSIGN pallet-dscr:SCREEN-VALUE = ITEM.i-name .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quantity D-Dialog
ON HELP OF quantity IN FRAME D-Dialog /* Quantity */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        DEFINE VARIABLE char-val2  AS cha   NO-UNDO.        
        DEFINE VARIABLE date-val   AS cha   NO-UNDO.
        DEFINE VARIABLE date-val2  AS cha   NO-UNDO.
        DEFINE VARIABLE cLogicalVal  AS CHARACTER NO-UNDO.

    
        run est/estqtyfr.w (len:screen-value,wid:SCREEN-VALUE, quantity:SCREEN-VALUE,ipriRowid, output char-val, output char-val2, output date-val, output date-val2, OUTPUT cLogicalVal) .
        if char-val <> "?" 
           then assign quantity:screen-value = entry(1,char-val)
                       lv-copy-qty[1] = integer(entry(1,char-val))
                       lv-copy-qty[2] = integer(entry(2,char-val))
                       lv-copy-qty[3] = integer(entry(3,char-val))
                       lv-copy-qty[4] = integer(entry(4,char-val))
                       lv-copy-qty[5] = integer(entry(5,char-val))
                       lv-copy-qty[6] = integer(entry(6,char-val))
                       lv-copy-qty[7] = integer(entry(7,char-val))
                       lv-copy-qty[8] = integer(entry(8,char-val))
                       lv-copy-qty[9] = integer(entry(9,char-val))
                       lv-copy-qty[10] = integer(entry(10,char-val))
                       lv-copy-rel[1] = INTEGER(entry(11,char-val))
                       lv-copy-rel[2] = integer(entry(12,char-val))
                       lv-copy-rel[3] = integer(entry(13,char-val))
                       lv-copy-rel[4] = integer(entry(14,char-val))
                       lv-copy-rel[5] = integer(entry(15,char-val))
                       lv-copy-rel[6] = integer(entry(16,char-val))
                       lv-copy-rel[7] = integer(entry(17,char-val))
                       lv-copy-rel[8] = integer(entry(18,char-val))
                       lv-copy-rel[9] = integer(entry(19,char-val))
                       lv-copy-rel[10] = integer(entry(20,char-val)).
        if char-val2 <> "?" 
           then assign lv-copy-qty[11] = integer(entry(1,char-val2))
                       lv-copy-qty[12] = integer(entry(2,char-val2))
                       lv-copy-qty[13] = integer(entry(3,char-val2))
                       lv-copy-qty[14] = integer(entry(4,char-val2))
                       lv-copy-qty[15] = integer(entry(5,char-val2))
                       lv-copy-qty[16] = integer(entry(6,char-val2))
                       lv-copy-qty[17] = integer(entry(7,char-val2))
                       lv-copy-qty[18] = integer(entry(8,char-val2))
                       lv-copy-qty[19] = integer(entry(9,char-val2))
                       lv-copy-qty[20] = integer(entry(10,char-val2))
                       lv-copy-rel[11] =  integer(entry(11,char-val2))
                       lv-copy-rel[12] = integer(entry(12,char-val2))  
                       lv-copy-rel[13] = integer(entry(13,char-val2))  
                       lv-copy-rel[14] = integer(entry(14,char-val2))  
                       lv-copy-rel[15] = integer(entry(15,char-val2))  
                       lv-copy-rel[16] = integer(entry(16,char-val2))  
                       lv-copy-rel[17] = integer(entry(17,char-val2))  
                       lv-copy-rel[18] = integer(entry(18,char-val2))  
                       lv-copy-rel[19] = integer(entry(19,char-val2))  
                       lv-copy-rel[20] = integer(entry(20,char-val2)) 
            .  
          IF cLogicalVal NE "?"   THEN   
             ASSIGN                   
               cLogicalRunShip[1] = STRING(entry(1,cLogicalVal))
               cLogicalRunShip[2] = STRING(entry(2,cLogicalVal))
               cLogicalRunShip[3] = STRING(entry(3,cLogicalVal))
               cLogicalRunShip[4] = STRING(entry(4,cLogicalVal))
               cLogicalRunShip[5] = STRING(entry(5,cLogicalVal))
               cLogicalRunShip[6] = STRING(entry(6,cLogicalVal))
               cLogicalRunShip[7] = STRING(entry(7,cLogicalVal))
               cLogicalRunShip[8] = STRING(entry(8,cLogicalVal))
               cLogicalRunShip[9] = STRING(entry(9,cLogicalVal))
               cLogicalRunShip[10] = STRING(entry(10,cLogicalVal))
               cLogicalRunShip[11] = STRING(entry(11,cLogicalVal))
               cLogicalRunShip[12] = STRING(entry(12,cLogicalVal))  
               cLogicalRunShip[13] = STRING(entry(13,cLogicalVal))  
               cLogicalRunShip[14] = STRING(entry(14,cLogicalVal))  
               cLogicalRunShip[15] = STRING(entry(15,cLogicalVal))  
               cLogicalRunShip[16] = STRING(entry(16,cLogicalVal))  
               cLogicalRunShip[17] = STRING(entry(17,cLogicalVal))  
               cLogicalRunShip[18] = STRING(entry(18,cLogicalVal))  
               cLogicalRunShip[19] = STRING(entry(19,cLogicalVal))  
               cLogicalRunShip[20] = STRING(entry(20,cLogicalVal)) . 
            ELSE cLogicalRunShip[1] = "?" .   
                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quantity D-Dialog
ON LEAVE OF quantity IN FRAME D-Dialog /* Quantity */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF INTEGER(quantity:SCREEN-VALUE) LE 0 THEN 
            DO:
                MESSAGE "Quantity must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
                APPLY "entry" TO quantity .
                RETURN NO-APPLY.
            END.
            RUN pCheckRelQty(NO) .
        END.                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quantity D-Dialog
ON VALUE-CHANGED OF quantity IN FRAME D-Dialog /* Quantity */
DO:
    lShowMessage = NO .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-to D-Dialog
ON HELP OF ship-to IN FRAME D-Dialog /* Ship To */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-shipto.w (gcompany,"",cCustNo:SCREEN-VALUE,"", OUTPUT char-val).
   
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)
                ship-name:screen-value = ENTRY(2,char-val)
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
            IF ship-to:SCREEN-VALUE NE "" THEN 
            DO:
                RUN valid-ship-id(OUTPUT lError) NO-ERROR.
                IF lError THEN RETURN NO-APPLY.
            END.
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST shipto WHERE shipto.company = cocode
                    AND shipto.cust-no EQ cCustNo:SCREEN-VALUE
                    AND shipto.ship-id EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .
             
                IF AVAILABLE shipto THEN
                    ASSIGN ship-name:SCREEN-VALUE = shipto.ship-name .
            END.
        END.                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod D-Dialog
ON HELP OF style-cod IN FRAME D-Dialog /* Style Code */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-stylec.w (gcompany,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value = ENTRY(1,char-val)
                .    
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST style WHERE style.company = cocode
                AND style.style EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE style THEN do:
                ASSIGN style-dscr:SCREEN-VALUE = style.dscr 
                       sub-unit:SCREEN-VALUE   = style.spare-char-5.                        
                       RUN pGetBoardFromStyle(style.style).
                       RUN pDefaultPackInfo .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod D-Dialog
ON LEAVE OF style-cod IN FRAME D-Dialog /* Style Code */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .    
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            RUN valid-style(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY. 
        END.

        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST style WHERE style.company = cocode
                AND style.style EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE style THEN
                ASSIGN style-dscr:SCREEN-VALUE = style.dscr .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod D-Dialog
ON VALUE-CHANGED OF style-cod IN FRAME D-Dialog /* Style Code */
DO:     
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST style NO-LOCK WHERE style.company = cocode
                AND style.style EQ SELF:SCREEN-VALUE  NO-ERROR .
                         
            IF AVAILABLE style THEN do:
                ASSIGN style-dscr:SCREEN-VALUE = style.dscr 
                       sub-unit:SCREEN-VALUE   = style.spare-char-5 .
                RUN pGetBoardFromStyle(style.style).                          
                RUN pDefaultPackInfo .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sub-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sub-unit D-Dialog
ON HELP OF sub-unit IN FRAME D-Dialog /* Packing Code */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-item.w (cocode, "", "C", FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value          = ENTRY(1,char-val)
                sub-unit-dscr:screen-value = ENTRY(2,char-val)
                .    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sub-unit D-Dialog
ON LEAVE OF sub-unit IN FRAME D-Dialog /* Packing Code */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:

            RUN valid-pack(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY .

            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST ITEM WHERE ITEM.company = cocode
                    AND ITEM.i-no EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

                IF AVAILABLE ITEM THEN
                    ASSIGN 
                        sub-unit-dscr:SCREEN-VALUE = ITEM.i-name
                        iUnitCount:SCREEN-VALUE = STRING(item.box-case)
                        iPerPallet:SCREEN-VALUE = STRING(item.case-pall)
                        .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wid D-Dialog
ON LEAVE OF wid IN FRAME D-Dialog /* Width */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
        IF ipType EQ "Edit" THEN do:
            RUN pDisplayValue.
            RUN pDisplayQty.
            DISABLE cSEst .
            APPLY "entry" TO quantity IN FRAME {&FRAME-NAME}.
        END.    
        ELSE do:
            RUN pDefaultValue. 
            IF NOT lMiscEstimateSource THEN do:
              DISABLE cSEst .
              APPLY "entry" TO quantity IN FRAME {&FRAME-NAME}.
            END.
            ELSE
            APPLY "entry" TO cSEst IN FRAME {&FRAME-NAME}.
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
  
    ASSIGN
        iFormNumber  = 1
        iBlankNumber = 1             .
  
    CREATE ttInputEst.
    ASSIGN 
        ttInputEst.cCompany         = cocode
        ttInputEst.cPartID          = cCustPart
        ttInputEst.cStockNo         = fg-no
        ttInputEst.cPartName        = item-name
        ttInputEst.cPartDescription = item-dscr
        
        ttInputEst.cBndlCode        = sub-unit
        ttInputEst.iUnitCount       = iUnitCount
        ttInputEst.iPerPallet       = iPerPallet
        ttInputEst.iPartial         = iPartial
        ttInputEst.cPallet          = pallet
        ttInputEst.iFormNo          = iFormNumber
        ttInputEst.iBlankNo         = iBlankNumber 

        ttInputEst.cCustomer        = cCustNo 
        ttInputEst.cShipTo          = ship-to 
        ttInputEst.cStyle           = style-cod 
        ttInputEst.dLength          = len 
        ttInputEst.dWidth           = wid 
        ttInputEst.dDepth           = dep 
        ttInputEst.cBoard           = board  
        ttInputEst.iQuantity        = quantity 
        ttInputEst.cCategory        = fg-cat 
        ttInputEst.dWeightPerM      = dWeightPerM
        ttInputEst.iStackHeight     = iStackHeight
        ttInputEst.iStackCode       = cStackCode 
        ttInputEst.cEstType         = "MiscEstimate"
        ttInputEst.cSourceEst       = cSEst 
        .
     ASSIGN 
         ttInputEst.copy-qty[2] = lv-copy-qty[2] 
         ttInputEst.copy-qty[3] = lv-copy-qty[3] 
         ttInputEst.copy-qty[4] = lv-copy-qty[4] 
         ttInputEst.copy-qty[5] = lv-copy-qty[5] 
         ttInputEst.copy-qty[6] = lv-copy-qty[6] 
         ttInputEst.copy-qty[7] = lv-copy-qty[7] 
         ttInputEst.copy-qty[8] = lv-copy-qty[8] 
         ttInputEst.copy-qty[9] = lv-copy-qty[9] 
         ttInputEst.copy-qty[10] = lv-copy-qty[10]
         
         ttInputEst.copy-qty[11] = lv-copy-qty[11] 
         ttInputEst.copy-qty[12] = lv-copy-qty[12] 
         ttInputEst.copy-qty[13] = lv-copy-qty[13] 
         ttInputEst.copy-qty[14] = lv-copy-qty[14] 
         ttInputEst.copy-qty[15] = lv-copy-qty[15] 
         ttInputEst.copy-qty[16] = lv-copy-qty[16] 
         ttInputEst.copy-qty[17] = lv-copy-qty[17] 
         ttInputEst.copy-qty[18] = lv-copy-qty[18] 
         ttInputEst.copy-qty[19] = lv-copy-qty[19] 
         ttInputEst.copy-qty[20] = lv-copy-qty[20] 

         ttInputEst.copy-rel[1] = lv-copy-rel[1]
         ttInputEst.copy-rel[2] = lv-copy-rel[2] 
         ttInputEst.copy-rel[3] = lv-copy-rel[3] 
         ttInputEst.copy-rel[4] = lv-copy-rel[4] 
         ttInputEst.copy-rel[5] = lv-copy-rel[5] 
         ttInputEst.copy-rel[6] = lv-copy-rel[6] 
         ttInputEst.copy-rel[7] = lv-copy-rel[7] 
         ttInputEst.copy-rel[8] = lv-copy-rel[8] 
         ttInputEst.copy-rel[9] = lv-copy-rel[9] 
         ttInputEst.copy-rel[10] = lv-copy-rel[10]
         
         ttInputEst.copy-rel[11] = lv-copy-rel[11] 
         ttInputEst.copy-rel[12] = lv-copy-rel[12] 
         ttInputEst.copy-rel[13] = lv-copy-rel[13] 
         ttInputEst.copy-rel[14] = lv-copy-rel[14] 
         ttInputEst.copy-rel[15] = lv-copy-rel[15] 
         ttInputEst.copy-rel[16] = lv-copy-rel[16] 
         ttInputEst.copy-rel[17] = lv-copy-rel[17] 
         ttInputEst.copy-rel[18] = lv-copy-rel[18] 
         ttInputEst.copy-rel[19] = lv-copy-rel[19] 
         ttInputEst.copy-rel[20] = lv-copy-rel[20] .    
     
     IF cLogicalRunShip[1] NE "" THEN
        ASSIGN
         ttInputEst.copy-runship[1] = cLogicalRunShip[1]
         ttInputEst.copy-runship[2] = cLogicalRunShip[2]
         ttInputEst.copy-runship[3] = cLogicalRunShip[3]
         ttInputEst.copy-runship[4] = cLogicalRunShip[4]
         ttInputEst.copy-runship[5] = cLogicalRunShip[5]
         ttInputEst.copy-runship[6] = cLogicalRunShip[6]
         ttInputEst.copy-runship[7] = cLogicalRunShip[7]
         ttInputEst.copy-runship[8] = cLogicalRunShip[8]
         ttInputEst.copy-runship[9] = cLogicalRunShip[9]
         ttInputEst.copy-runship[10] = cLogicalRunShip[10]
         ttInputEst.copy-runship[11] = cLogicalRunShip[11]
         ttInputEst.copy-runship[12] = cLogicalRunShip[12]
         ttInputEst.copy-runship[13] = cLogicalRunShip[13]
         ttInputEst.copy-runship[14] = cLogicalRunShip[14]
         ttInputEst.copy-runship[15] = cLogicalRunShip[15]
         ttInputEst.copy-runship[16] = cLogicalRunShip[16]
         ttInputEst.copy-runship[17] = cLogicalRunShip[17]
         ttInputEst.copy-runship[18] = cLogicalRunShip[18]
         ttInputEst.copy-runship[19] = cLogicalRunShip[19]
         ttInputEst.copy-runship[20] = cLogicalRunShip[20].
       ELSE do:  
         FIND FIRST eb NO-LOCK 
              WHERE eb.company EQ cocode
               AND ROWID(eb) EQ ipriRowid NO-ERROR .    
          IF AVAIL eb  THEN do:
             FIND est-qty NO-LOCK
              WHERE est-qty.company EQ eb.company
                AND est-qty.est-no EQ eb.est-no
                AND est-qty.eqty EQ eb.eqty 
                 NO-ERROR.
           IF AVAIL est-qty THEN DO:   
              ASSIGN
                ttInputEst.copy-runship[1] = string(est-qty.whsed[1])
                ttInputEst.copy-runship[2] = string(est-qty.whsed[2])
                ttInputEst.copy-runship[3] = string(est-qty.whsed[3])
                ttInputEst.copy-runship[4] = string(est-qty.whsed[4])
                ttInputEst.copy-runship[5] = string(est-qty.whsed[5])
                ttInputEst.copy-runship[6] = string(est-qty.whsed[6])
                ttInputEst.copy-runship[7] = string(est-qty.whsed[7])
                ttInputEst.copy-runship[8] = string(est-qty.whsed[8])
                ttInputEst.copy-runship[9] = string(est-qty.whsed[9])
                ttInputEst.copy-runship[10] = string(est-qty.whsed[10])
                ttInputEst.copy-runship[11] = string(est-qty.whsed[11])
                ttInputEst.copy-runship[12] = string(est-qty.whsed[12])
                ttInputEst.copy-runship[13] = string(est-qty.whsed[13])
                ttInputEst.copy-runship[14] = string(est-qty.whsed[14])
                ttInputEst.copy-runship[15] = string(est-qty.whsed[15])
                ttInputEst.copy-runship[16] = string(est-qty.whsed[16])
                ttInputEst.copy-runship[17] = string(est-qty.whsed[17])
                ttInputEst.copy-runship[18] = string(est-qty.whsed[18])
                ttInputEst.copy-runship[19] = string(est-qty.whsed[19])
                ttInputEst.copy-runship[20] = string(est-qty.whsed[20]).                
           END.
         END.  
       END.
          
    ttInputEst.riParentEst = lv-crt-est-rowid .

    FIND FIRST item WHERE item.company = cocode
        AND item.i-no EQ board NO-LOCK NO-ERROR .

    IF AVAILABLE ITEM THEN
        ASSIGN 
            ttInputEst.cFlute = item.flute 
            ttInputEst.cTest  = ITEM.reg-no .
   
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
  DISPLAY quantity cCustNo ship-to cCustPart fg-no item-name item-dscr len wid 
          dep style-cod style-dscr board fg-cat sub-unit sub-Unit-dscr 
          iUnitCount iPerPallet iPartial pallet pallet-dscr tot-iUnitCount 
          dWeightPerM iStackHeight cust-name ship-name board-dscr cat-dscr cSEst 
      WITH FRAME D-Dialog.
  ENABLE quantity cCustNo ship-to cCustPart fg-no item-name item-dscr len wid 
         dep style-cod board fg-cat sub-unit iUnitCount iPerPallet iPartial 
         pallet dWeightPerM iStackHeight Btn_OK Btn_Cancel cSEst 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckRelQty D-Dialog 
PROCEDURE pCheckRelQty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplDelete AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lFoundOldRelease AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cRelQtyFound AS CHARACTER NO-UNDO .
    
    DEFINE BUFFER bf-estRelease FOR estRelease.
    
    DO WITH FRAME {&FRAME-NAME}:

       FIND FIRST eb NO-LOCK 
           WHERE eb.company EQ cocode
           AND ROWID(eb) EQ ipriRowid NO-ERROR .
       IF AVAIL eb  THEN do:
           FIND est-qty 
               WHERE est-qty.company EQ eb.company
               AND est-qty.est-no EQ eb.est-no
               AND est-qty.eqty EQ eb.eqty 
               NO-ERROR.

           IF AVAIL est-qty AND lv-copy-qty[1] NE 0 THEN
               DO i = 1 TO 20: 
               IF est-qty.qty[i] NE 0 AND lv-copy-qty[i] NE est-qty.qty[i] THEN do:
                   FIND FIRST bf-estRelease NO-LOCK
                       WHERE bf-estRelease.company =  eb.company 
                       AND bf-estRelease.estimateNo = eb.est-no  
                       AND bf-estRelease.FormNo     = eb.form-no 
                       AND bf-estRelease.BlankNo    = eb.blank-No 
                       AND bf-estRelease.quantity   = est-qty.qty[i]  NO-ERROR .
                   IF AVAIL bf-estRelease THEN do:
                       ASSIGN lFoundOldRelease = TRUE  .
                       IF cRelQtyFound EQ "" THEN
                           cRelQtyFound = string(est-qty.qty[i]) .
                       ELSE cRelQtyFound = cRelQtyFound + "," + string(est-qty.qty[i]) .
                       IF iplDelete THEN DO:
                           FIND CURRENT bf-estRelease EXCLUSIVE-LOCK.
                           DELETE bf-estRelease.
                       END.                           
                   END.
               END.
           END.
           ELSE IF AVAIL eb AND eb.eqty NE integer(quantity:SCREEN-VALUE) AND integer(quantity:SCREEN-VALUE) NE 0  THEN DO:
                FIND FIRST bf-estRelease NO-LOCK
                       WHERE bf-estRelease.company  = eb.company 
                       AND bf-estRelease.estimateNo = eb.est-no  
                       AND bf-estRelease.FormNo     = eb.form-no 
                       AND bf-estRelease.BlankNo    = eb.blank-No 
                       AND bf-estRelease.quantity   = eb.eqty  NO-ERROR .
                   IF AVAIL bf-estRelease THEN do:
                       ASSIGN lFoundOldRelease = TRUE  .
                       cRelQtyFound = string(eb.eqty) .
                       IF iplDelete THEN DO:
                           FIND CURRENT bf-estRelease EXCLUSIVE-LOCK.
                           DELETE bf-estRelease.
                       END.
                   END.
           END.

           IF NOT iplDelete AND lFoundOldRelease AND NOT lShowMessage THEN do:
               MESSAGE "There is at least one release tied to the quantity that is no longer valid.  These releases will be deleted when you save -" SKIP "Rel Qty: " STRING(cRelQtyFound)
               VIEW-AS ALERT-BOX INFO .
               lShowMessage = TRUE .
           END.
           

       END.  /* avail eb  */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDefaultPackInfo D-Dialog 
PROCEDURE pDefaultPackInfo :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
         FIND FIRST ITEM NO-LOCK
            WHERE item.company = cocode 
              AND item.i-no = sub-unit:SCREEN-VALUE
            NO-ERROR.

         IF AVAIL item THEN ASSIGN sub-unit-dscr:SCREEN-VALUE = ITEM.i-name
                                  iUnitCount:SCREEN-VALUE = STRING(item.box-case)
                                  iPerPallet:SCREEN-VALUE = STRING(item.case-pall)
                                  tot-iUnitCount:SCREEN-VALUE = string(item.box-case * item.case-pall )  .      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDefaultValue D-Dialog 
PROCEDURE pDefaultValue :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST ce-ctrl WHERE ce-ctrl.company = cocode AND
                                 ce-ctrl.loc = locode
                                 NO-LOCK NO-ERROR.
        ASSIGN
        /*sub-unit:SCREEN-VALUE = ce-ctrl.def-case*/
        pallet:SCREEN-VALUE = ce-ctrl.def-pal.  
       
        iStackHeight:SCREEN-VALUE = string("1") .
        IF cCEMiscDefaultStyle NE "" THEN
            ASSIGN style-cod:SCREEN-VALUE  = cCEMiscDefaultStyle .
        IF cCEMiscDefaultBoard NE "" THEN
            ASSIGN board:SCREEN-VALUE  = cCEMiscDefaultBoard .

        IF cCEMiscDefaultStackCode NE "" THEN
            ASSIGN cStackCode = cCEMiscDefaultStackCode .

        FIND FIRST ITEM NO-LOCK WHERE ITEM.company = cocode
            AND ITEM.i-no EQ pallet:SCREEN-VALUE NO-ERROR .
        
        IF AVAILABLE ITEM THEN
            ASSIGN pallet-dscr:SCREEN-VALUE = ITEM.i-name .

        FIND FIRST ITEM NO-LOCK WHERE item.company = cocode
            AND item.i-no EQ board:SCREEN-VALUE NO-ERROR .
        IF AVAILABLE ITEM THEN
            ASSIGN board-dscr:SCREEN-VALUE = item.i-name .

        FIND FIRST style NO-LOCK WHERE style.company = cocode
            AND style.style EQ style-cod:SCREEN-VALUE NO-ERROR .
        
        IF AVAILABLE style THEN
            ASSIGN style-dscr:SCREEN-VALUE = style.dscr
                   sub-unit:SCREEN-VALUE   = style.spare-char-5 .

        RUN pDefaultPackInfo .
      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValue D-Dialog 
PROCEDURE pDisplayValue :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-eb FOR eb .
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST eb NO-LOCK 
            WHERE eb.company EQ cocode
              AND ROWID(eb) EQ ipriRowid NO-ERROR .
        
        IF AVAIL eb THEN DO:
            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ eb.company
                AND ef.est-no EQ eb.est-no
                AND ef.form-no EQ eb.form-no  
                NO-ERROR.

            ASSIGN
             quantity:SCREEN-VALUE   = string(eb.eqty) 
             cCustNo:SCREEN-VALUE    = eb.cust-no
             ship-to:SCREEN-VALUE    = eb.ship-id
             cCustPart:SCREEN-VALUE  = eb.part-no
             fg-no:SCREEN-VALUE      = eb.stock-no
             item-name:SCREEN-VALUE  = eb.part-dscr1
             item-dscr:SCREEN-VALUE  = eb.part-dscr2
             len:SCREEN-VALUE        = string(eb.len)
             wid:SCREEN-VALUE        = string(eb.wid)
             dep:SCREEN-VALUE        = string(eb.dep)
             style-cod:SCREEN-VALUE  = eb.style
             board:SCREEN-VALUE      = IF AVAIL ef THEN ef.board ELSE ""
             fg-cat:SCREEN-VALUE     = eb.procat
             sub-unit:SCREEN-VALUE   = eb.cas-no
             iUnitCount:SCREEN-VALUE = string(eb.cas-cnt)
             iPerPallet:SCREEN-VALUE = string(eb.cas-pal)
             iPartial:SCREEN-VALUE = string(eb.quantityPartial)
             pallet:SCREEN-VALUE     = eb.tr-no
             dWeightPerM:SCREEN-VALUE = string(eb.weight)
             tot-iUnitCount:SCREEN-VALUE = string(eb.cas-cnt * eb.cas-pal + eb.quantityPartial )
             cStackCode = eb.stack-code .
             
             IF ipType EQ "Edit" THEN
                cSEst:SCREEN-VALUE = STRING(eb.sourceEstimate) .
             ELSE IF ipType EQ "" THEN DO:   
                item-dscr:SCREEN-VALUE = IF eb.pur-man THEN "Purchased Item" ELSE "Inhouse Manufacture" .
                FIND FIRST bf-eb NO-LOCK
                     WHERE bf-eb.company EQ eb.company 
                     AND bf-eb.est-no EQ eb.est-no 
                     AND bf-eb.form-no EQ 0 NO-ERROR .
                IF avail bf-eb THEN 
                cCustPart:SCREEN-VALUE  = bf-eb.part-no .
             END.
             
        IF eb.stackHeight GT 0 THEN 
            ASSIGN iStackHeight:SCREEN-VALUE = string(eb.stackHeight) .
        ELSE iStackHeight:SCREEN-VALUE = string("1") .

        FIND FIRST ITEM NO-LOCK WHERE ITEM.company = cocode
            AND ITEM.i-no EQ pallet:SCREEN-VALUE NO-ERROR .
        
        IF AVAILABLE ITEM THEN
            ASSIGN pallet-dscr:SCREEN-VALUE = ITEM.i-name .

        FIND FIRST ITEM NO-LOCK WHERE ITEM.company = cocode
            AND ITEM.i-no EQ sub-unit:SCREEN-VALUE NO-ERROR .
        IF AVAILABLE ITEM THEN
            ASSIGN sub-unit-dscr:SCREEN-VALUE = ITEM.i-name .

        FIND FIRST ITEM NO-LOCK WHERE item.company = cocode
            AND item.i-no EQ board:SCREEN-VALUE NO-ERROR .
        IF AVAILABLE ITEM THEN
            ASSIGN board-dscr:SCREEN-VALUE = item.i-name .

        FIND FIRST style NO-LOCK WHERE style.company = cocode
            AND style.style EQ style-cod:SCREEN-VALUE NO-ERROR .
        
        IF AVAILABLE style THEN
            ASSIGN style-dscr:SCREEN-VALUE = style.dscr .

        FIND FIRST cust NO-LOCK WHERE cust.company = cocode
            AND cust.cust-no EQ cCustNo:SCREEN-VALUE NO-ERROR .
        IF AVAILABLE cust THEN
            ASSIGN cust-name:SCREEN-VALUE = cust.NAME .

        FIND FIRST shipto NO-LOCK WHERE shipto.company = cocode
            AND shipto.cust-no EQ cCustNo:SCREEN-VALUE
            AND shipto.ship-id EQ ship-to:SCREEN-VALUE NO-ERROR .
        IF AVAILABLE shipto THEN
            ASSIGN ship-name:SCREEN-VALUE = shipto.ship-name .

        FIND FIRST fgcat NO-LOCK WHERE fgcat.company = cocode
            AND fgcat.procat EQ fg-cat:SCREEN-VALUE NO-ERROR .
        IF AVAILABLE fgcat THEN
            ASSIGN cat-dscr:SCREEN-VALUE = fgcat.dscr .
        
        Btn_OK:LABEL = "Save" .

        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetBoardFromStyle D-Dialog 
PROCEDURE pGetBoardFromStyle :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETE ipcStyle AS CHARACTER NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
    
    FIND FIRST flute NO-LOCK
       WHERE flute.company EQ cocode NO-ERROR .
    IF AVAIL flute THEN
      FIND FIRST reftable WHERE reftable.reftable = "STYFLU" AND reftable.company = ipcStyle 
             AND reftable.loc = flute.code
             AND reftable.code = "BOARD"
             NO-LOCK NO-ERROR. 
      board:screen-value = IF AVAIL reftable AND AVAIL flute AND reftable.dscr NE "" THEN reftable.dscr ELSE board:screen-value.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetEstDetail D-Dialog 
PROCEDURE pGetEstDetail :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-eb NO-LOCK
             WHERE bf-eb.company EQ cocode
               AND bf-eb.est-no EQ cSEst:SCREEN-VALUE
               AND bf-eb.form-no NE 0 NO-ERROR.
                  
        IF AVAILABLE bf-eb THEN do:           
          ipriRowid = ROWID(bf-eb) .        
          RUN pDisplayValue.
          RUN pDisplayQty.
           Btn_OK:LABEL = "&Next" .                                         
        END.         
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayQty D-Dialog 
PROCEDURE pDisplayQty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-eb FOR eb .
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST bf-eb NO-LOCK 
            WHERE bf-eb.company EQ cocode
              AND ROWID(bf-eb) EQ ipriRowid NO-ERROR .
        
        IF AVAIL bf-eb THEN DO:
            FIND FIRST est-qty NO-LOCK
                 WHERE est-qty.company EQ bf-eb.company
                 AND est-qty.est-no EQ bf-eb.est-no
                 AND est-qty.eqty EQ bf-eb.eqty NO-ERROR .
                 
           IF avail est-qty then do:  
             ASSIGN
              lv-copy-qty[2] = est-qty.qty[2]
              lv-copy-qty[3] = est-qty.qty[3]
              lv-copy-qty[4] = est-qty.qty[4]
              lv-copy-qty[5] = est-qty.qty[5]
              lv-copy-qty[6] = est-qty.qty[6]
              lv-copy-qty[7] = est-qty.qty[7]
              lv-copy-qty[8] = est-qty.qty[8]
              lv-copy-qty[9] = est-qty.qty[9]
              lv-copy-qty[10] = est-qty.qty[10]
              lv-copy-qty[11] = est-qty.qty[11]
              lv-copy-qty[12] = est-qty.qty[12]
              lv-copy-qty[13] = est-qty.qty[13]
              lv-copy-qty[14] = est-qty.qty[14]
              lv-copy-qty[15] = est-qty.qty[15]
              lv-copy-qty[16] = est-qty.qty[16]
              lv-copy-qty[17] = est-qty.qty[17]
              lv-copy-qty[18] = est-qty.qty[18]
              lv-copy-qty[19] = est-qty.qty[19]
              lv-copy-qty[20] = est-qty.qty[20].
                     
             ASSIGN 
              lv-copy-rel[1]  = est-qty.qty[21]
              lv-copy-rel[2]  = est-qty.qty[22]
              lv-copy-rel[3]  = est-qty.qty[23]
              lv-copy-rel[4]  = est-qty.qty[24]
              lv-copy-rel[5]  = est-qty.qty[25]
              lv-copy-rel[6]  = est-qty.qty[26]
              lv-copy-rel[7]  = est-qty.qty[27]
              lv-copy-rel[8]  = est-qty.qty[28]
              lv-copy-rel[9]  = est-qty.qty[29]
              lv-copy-rel[10] = est-qty.qty[30]
              lv-copy-rel[11] = est-qty.qty[31]
              lv-copy-rel[12] = est-qty.qty[32]
              lv-copy-rel[13] = est-qty.qty[33]
              lv-copy-rel[14] = est-qty.qty[34]
              lv-copy-rel[15] = est-qty.qty[35]
              lv-copy-rel[16] = est-qty.qty[36]
              lv-copy-rel[17] = est-qty.qty[37]
              lv-copy-rel[18] = est-qty.qty[38]
              lv-copy-rel[19] = est-qty.qty[39]
              lv-copy-rel[20] = est-qty.qty[40].
             assign 
              cLogicalRunShip[1] = STRING(est-qty.whsed[1])
              cLogicalRunShip[2] = STRING(est-qty.whsed[2]) 
              cLogicalRunShip[3] = STRING(est-qty.whsed[3])
              cLogicalRunShip[4] = STRING(est-qty.whsed[4])
              cLogicalRunShip[5] = STRING(est-qty.whsed[5])
              cLogicalRunShip[6] = STRING(est-qty.whsed[6])
              cLogicalRunShip[7] = STRING(est-qty.whsed[7])
              cLogicalRunShip[8] = STRING(est-qty.whsed[8])
              cLogicalRunShip[9] = STRING(est-qty.whsed[9])
              cLogicalRunShip[10] = STRING(est-qty.whsed[10])
              cLogicalRunShip[11] = STRING(est-qty.whsed[11])
              cLogicalRunShip[12] = STRING(est-qty.whsed[12])
              cLogicalRunShip[13] = STRING(est-qty.whsed[13])
              cLogicalRunShip[14] = STRING(est-qty.whsed[14])
              cLogicalRunShip[15] = STRING(est-qty.whsed[15])
              cLogicalRunShip[16] = STRING(est-qty.whsed[16])
              cLogicalRunShip[17] = STRING(est-qty.whsed[17])
              cLogicalRunShip[18] = STRING(est-qty.whsed[18])
              cLogicalRunShip[19] = STRING(est-qty.whsed[19])
              cLogicalRunShip[20] = STRING(est-qty.whsed[20])  .

           END.
        END.
    END.

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
        IF NOT CAN-FIND(item WHERE item.company = gcompany
            AND item.i-no = board:screen-value
            AND LOOKUP(ITEM.mat-type,"P,R,B,F" ) GT 0)
            THEN 
        DO:
            MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO board.
            oplOutError = YES .
        END.
    END.

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
            WHERE cust.company  EQ gcompany
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fgitem D-Dialog 
PROCEDURE valid-fgitem :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
     
        IF fg-no:SCREEN-VALUE  NE "" AND NOT lCreateNewFG THEN 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company  EQ gcompany
                AND itemfg.i-no    EQ fg-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE itemfg  THEN 
            DO:
                MESSAGE "This item does not exist, would you like to add it?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll-ans AS LOG.
                IF ll-ans THEN
                    ASSIGN lCreateNewFG = TRUE .
                IF NOT ll-ans THEN do:
                    APPLY "entry" TO fg-no .
                    oplOutError = YES .
                END.
            END.
        END.
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
        IF item-dscr:SCREEN-VALUE EQ "" THEN 
        DO:
            MESSAGE "Description must not be blank. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO item-dscr.
            oplOutError = YES .
        END.
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
        IF item-name:SCREEN-VALUE EQ "" THEN 
        DO:
            MESSAGE "Item Name must not be blank. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO item-name.
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pack D-Dialog 
PROCEDURE valid-pack :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(item WHERE item.company = gcompany
            AND item.i-no = sub-unit:screen-value
            AND item.mat-type = "C")
            THEN 
        DO:
            MESSAGE "Invalid Sub Unit. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO sub-unit.
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pallet D-Dialog 
PROCEDURE valid-pallet :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(item WHERE item.company = gcompany
            AND item.i-no = pallet:screen-value
            AND item.mat-type EQ "D" )
            THEN 
        DO:
            MESSAGE "Invalid Pallet. Try Help. " VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO pallet.
            oplOutError = YES .
        END.
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
        IF cCustPart:SCREEN-VALUE  EQ "" THEN 
        DO:
            MESSAGE "Invalid Cust Part# must be enter..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO cCustPart .
            oplOutError = YES .
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-PerPallet D-Dialog 
PROCEDURE valid-PerPallet :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(iPerPallet:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Per Pallet must not be 0. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO iPerPallet.
            oplOutError = YES .
        END.
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
        fg-cat:SCREEN-VALUE  = CAPS(fg-cat:SCREEN-VALUE).

        IF NOT CAN-FIND(FIRST fgcat
            WHERE fgcat.company EQ cocode
            AND fgcat.procat  EQ fg-cat:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Invalid FG Category, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-cat .
            oplOutError = YES .
        END.
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
            WHERE shipto.company EQ gcompany
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style D-Dialog 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST style
            WHERE style.company  EQ gcompany
            AND style.style    EQ style-cod:SCREEN-VALUE
            AND style.industry EQ "2")  THEN 
        DO:
            MESSAGE "Invalid Style Code, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO style-cod .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sub-UnitCount D-Dialog 
PROCEDURE valid-sub-UnitCount :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(iUnitCount:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Sub Unit Count must not be 0. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO iUnitCount.
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no D-Dialog 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE BUFFER b-eb FOR eb.
    DEFINE BUFFER b-est FOR est.
    DO WITH FRAME {&FRAME-NAME}:
        IF trim(cSEst:SCREEN-VALUE) NE "0" AND NOT CAN-FIND(FIRST b-eb
            WHERE b-eb.company  EQ gcompany
            AND b-eb.est-no    EQ cSEst:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Estimate, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cSEst .
            oplOutError = YES .
        END.
        IF lMiscEstimateSource AND iMiscEstimateSource EQ 1 AND CAN-FIND(FIRST b-est
            WHERE b-est.company  EQ gcompany
            AND b-est.est-no    EQ cSEst:SCREEN-VALUE
            AND b-est.estimateTypeID EQ "MISC")  THEN 
        DO:
            MESSAGE "Estimate is already Misc estimate , try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cSEst .
            oplOutError = YES .
        END.
        IF trim(cSEst:SCREEN-VALUE) NE "0" AND  cMiscEstimateSource EQ "Quote" THEN
        DO:
          IF NOT CAN-FIND(FIRST quotehd
            WHERE quotehd.company  EQ gcompany
            AND quotehd.est-no    EQ cSEst:SCREEN-VALUE)  THEN 
          DO:
            MESSAGE "N-K-1 MiscEstimateSource is set to get prices from the quote, so without a quote you may not proceed." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cSEst .
            oplOutError = YES .
          END.        
        END.
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



