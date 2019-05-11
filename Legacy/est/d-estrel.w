&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-estrel.w
  
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


DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oprEstRowid AS ROWID NO-UNDO .
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

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{est/frmotvar.i "shared"}
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
DEFINE VARIABLE lv-crt-est-rowid AS ROWID   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS cust-no ship-to style-cod len wid dep ~
cst-part fg-no fg-cat quantity board Btn_OK Btn_Cancel RECT-1 RECT-2 RECT-3 ~
RECT-4 item-name item-dscr sub-unit unit-count per-pallet pallet 
&Scoped-Define DISPLAYED-OBJECTS cust-no ship-to style-cod style-dscr len ~
wid dep cst-part fg-no fg-cat quantity board est-no cust-name ship-name ~
item-name item-dscr board-dscr cat-dscr sub-unit sun-Unit-dscr unit-count ~
per-pallet pallet pallet-dscr tot-unit-count 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 15 BY 1.29
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "&Next" 
    SIZE 15 BY 1.29
    BGCOLOR 8 .

DEFINE VARIABLE board          AS CHARACTER FORMAT "X(12)":U 
    LABEL "Board" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE board-dscr     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cat-dscr       AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cst-part       AS CHARACTER FORMAT "X(15)":U 
    LABEL "Cust Part#" 
    VIEW-AS FILL-IN 
    SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cust-name      AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-no        AS CHARACTER FORMAT "X(8)":U 
    LABEL "Cust#" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dep            AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    LABEL "Depth" 
    VIEW-AS FILL-IN 
    SIZE 9.6 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no         AS CHARACTER FORMAT "X(8)":U 
    LABEL "Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fg-cat         AS CHARACTER FORMAT "X(5)":U 
    LABEL "FG Category" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fg-no          AS CHARACTER FORMAT "X(15)":U 
    LABEL "FG Item Code" 
    VIEW-AS FILL-IN 
    SIZE 26 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-dscr      AS CHARACTER FORMAT "X(30)":U 
    LABEL "Description" 
    VIEW-AS FILL-IN 
    SIZE 26 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-name      AS CHARACTER FORMAT "X(30)":U 
    LABEL "Item Name" 
    VIEW-AS FILL-IN 
    SIZE 26 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE len            AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    LABEL "Length" 
    VIEW-AS FILL-IN 
    SIZE 9.6 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pallet         AS CHARACTER FORMAT "X(10)":U 
    LABEL "Pallet" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE pallet-dscr    AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE per-pallet     AS INTEGER   FORMAT "->>,>>9":U INITIAL 0 
    LABEL "Per Pallet" 
    VIEW-AS FILL-IN 
    SIZE 8.6 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE quantity       AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Quantity" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-name      AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-to        AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To" 
    VIEW-AS FILL-IN 
    SIZE 17.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-cod      AS CHARACTER FORMAT "X(8)":U 
    LABEL "Style Code" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-dscr     AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sub-unit       AS CHARACTER FORMAT "X(10)":U 
    LABEL "Sub Unit" 
    VIEW-AS FILL-IN 
    SIZE 14.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE sun-Unit-dscr  AS CHARACTER FORMAT "X(25)":U 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE tot-unit-count AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Unit/Pallet Count" 
    VIEW-AS FILL-IN 
    SIZE 13.4 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE unit-count     AS INTEGER   FORMAT "->>,>>9":U INITIAL 0 
    LABEL "Sub Unit Count" 
    VIEW-AS FILL-IN 
    SIZE 8.6 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE wid            AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    LABEL "Width" 
    VIEW-AS FILL-IN 
    SIZE 9.6 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 122.2 BY 3
    BGCOLOR 15.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 63.2 BY 10
    BGCOLOR 15.

DEFINE RECTANGLE RECT-3
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 58.6 BY 7.38
    BGCOLOR 15.

DEFINE RECTANGLE RECT-4
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
    SIZE 122 BY 16.67
    BGCOLOR 15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
    quantity AT ROW 2.14 COL 46.2 COLON-ALIGNED WIDGET-ID 198
    cust-no AT ROW 3.33 COL 13.4 COLON-ALIGNED WIDGET-ID 176
    ship-to AT ROW 3.33 COL 71 COLON-ALIGNED WIDGET-ID 178
    cst-part AT ROW 5.86 COL 17 COLON-ALIGNED WIDGET-ID 88
    fg-no AT ROW 6.95 COL 17 COLON-ALIGNED WIDGET-ID 42
    item-name AT ROW 8.05 COL 17 COLON-ALIGNED WIDGET-ID 208
    item-dscr AT ROW 9.19 COL 17 COLON-ALIGNED WIDGET-ID 210
    len AT ROW 10.38 COL 9.8 COLON-ALIGNED WIDGET-ID 190
    wid AT ROW 10.38 COL 29 COLON-ALIGNED WIDGET-ID 194
    dep AT ROW 10.38 COL 48.8 COLON-ALIGNED WIDGET-ID 192
    style-cod AT ROW 11.71 COL 17 COLON-ALIGNED WIDGET-ID 180
    style-dscr AT ROW 11.71 COL 32 COLON-ALIGNED NO-LABELS WIDGET-ID 182
    board AT ROW 12.81 COL 17 COLON-ALIGNED WIDGET-ID 174
    fg-cat AT ROW 13.91 COL 17 COLON-ALIGNED WIDGET-ID 196
    sub-unit AT ROW 6.43 COL 76.6 COLON-ALIGNED WIDGET-ID 216
    sun-Unit-dscr AT ROW 6.43 COL 91.5 COLON-ALIGNED NO-LABELS WIDGET-ID 218
    unit-count AT ROW 7.91 COL 82.2 COLON-ALIGNED WIDGET-ID 220
    per-pallet AT ROW 7.91 COL 111.2 COLON-ALIGNED WIDGET-ID 222
    pallet AT ROW 9.29 COL 76.8 COLON-ALIGNED WIDGET-ID 224
    pallet-dscr AT ROW 9.29 COL 91.6 COLON-ALIGNED NO-LABELS WIDGET-ID 226
    tot-unit-count AT ROW 10.71 COL 85.2 COLON-ALIGNED WIDGET-ID 228
    Btn_OK AT ROW 15.67 COL 45.4
    Btn_Cancel AT ROW 15.67 COL 66.8
    est-no AT ROW 2.14 COL 13.4 COLON-ALIGNED WIDGET-ID 200
    cust-name AT ROW 3.33 COL 31.2 COLON-ALIGNED NO-LABELS WIDGET-ID 202
    ship-name AT ROW 3.33 COL 89.4 COLON-ALIGNED NO-LABELS WIDGET-ID 204
    board-dscr AT ROW 12.86 COL 32 COLON-ALIGNED NO-LABELS WIDGET-ID 212
    cat-dscr AT ROW 13.95 COL 32 COLON-ALIGNED NO-LABELS WIDGET-ID 214
     
     
    "Product Input" VIEW-AS TEXT
    SIZE 16 BY .86 AT ROW 4.81 COL 6 WIDGET-ID 166
    "Main Input" VIEW-AS TEXT
    SIZE 13 BY 1 AT ROW 1.14 COL 6 WIDGET-ID 206
    "Packing" VIEW-AS TEXT
    SIZE 11 BY .86 AT ROW 4.86 COL 71.4 WIDGET-ID 234
    RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
    RECT-2 AT ROW 5.24 COL 1.8 WIDGET-ID 230
    RECT-3 AT ROW 5.24 COL 65.4 WIDGET-ID 232
    RECT-4 AT ROW 1.1 COL 1.8 WIDGET-ID 236
    SPACE(0.59) SKIP(0.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Create Estimate for Misc Release"
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
    FRAME D-Dialog:SCROLLABLE = FALSE
    FRAME D-Dialog:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN board-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cat-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN est-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pallet-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sun-Unit-dscr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tot-unit-count IN FRAME D-Dialog
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Estimate for Misc Release */
    DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid  NO-ERROR.
        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST ef OF eb NO-LOCK  NO-ERROR.
            FIND FIRST est OF ef NO-LOCK NO-ERROR.

            DELETE est.
        END.

        EMPTY TEMP-TABLE tt-frmout .
        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Next */
    DO:
        FIND eb WHERE ROWID(eb) EQ lv-crt-est-rowid  NO-ERROR.
        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST ef OF eb NO-LOCK  NO-ERROR.
            FIND FIRST est OF ef NO-LOCK NO-ERROR.
            DELETE est.
        END.
        EMPTY TEMP-TABLE tt-frmout .
        APPLY "go" TO FRAME {&FRAME-NAME}.
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
                AND item.i-no EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE ITEM THEN
                ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
        END.
            .                                    
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
            MESSAGE "Quantity must be enter..." VIEW-AS ALERT-BOX INFORMATION .
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

        RUN valid-style(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .   

        RUN valid-board(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .   

        RUN valid-pack(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY . 

        RUN valid-pallet(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY . 

        RUN valid-sub-unit-count(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .

        RUN valid-per-pallet(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .
         
        SESSION:SET-WAIT-STATE("general").
  
        RUN create-ttfrmout.
        oprEstRowid = lv-crt-est-rowid .

        SESSION:SET-WAIT-STATE("").
  
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cst-part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cst-part D-Dialog
ON HELP OF cst-part IN FRAME D-Dialog /* Cust Part# */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cst-part D-Dialog
ON LEAVE OF cst-part IN FRAME D-Dialog /* Cust Part# */
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


&Scoped-define SELF-NAME cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-no D-Dialog
ON HELP OF cust-no IN FRAME D-Dialog /* Cust# */
    DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-custact.w (gcompany,"", OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)
                cust-name:screen-value = ENTRY(2,char-val)
                .                                    
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


&Scoped-define SELF-NAME cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-no D-Dialog
ON LEAVE OF cust-no IN FRAME D-Dialog /* customer */
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
                    AND fgcat.procat EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .
             
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
                cst-part:SCREEN-VALUE  = itemfg.part-no
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
            WHERE itemfg.company = ipCompany
            AND itemfg.i-no EQ fg-no:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                cst-part:SCREEN-VALUE  = itemfg.part-no
                item-name:SCREEN-VALUE = itemfg.i-name
                item-dscr:SCREEN-VALUE = itemfg.part-dscr1 .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-dscr D-Dialog
ON HELP OF item-dscr IN FRAME D-Dialog /* Description */
    DO:
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-dscr D-Dialog
ON LEAVE OF item-dscr IN FRAME D-Dialog /* Description */
    DO: 
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-name D-Dialog
ON HELP OF item-name IN FRAME D-Dialog /* Item Name */
    DO:
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-name D-Dialog
ON LEAVE OF item-name IN FRAME D-Dialog /* Item Name */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
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
                AND ITEM.i-no EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE ITEM THEN
                ASSIGN pallet-dscr:SCREEN-VALUE = ITEM.i-name .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME per-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL per-pallet D-Dialog
ON VALUE-CHANGED OF per-pallet IN FRAME D-Dialog /* Per Pallet */
    DO:
        tot-unit-count:SCREEN-VALUE = STRING(INTEGER(unit-count:SCREEN-VALUE) * INTEGER(per-pallet:SCREEN-VALUE)) .  
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

    /*
        run est/estqtyfr.w (len:screen-value,wid:SCREEN-VALUE, quantity:SCREEN-VALUE, output char-val, output char-val2, output date-val, output date-val2) .
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
            .*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ship-to D-Dialog
ON HELP OF ship-to IN FRAME D-Dialog /* Ship To */
    DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-shipto.w (gcompany,"",cust-no:SCREEN-VALUE,"", OUTPUT char-val).
   
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
                    AND shipto.cust-no EQ cust-no:SCREEN-VALUE
                    AND shipto.ship-id EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .
             
                IF AVAILABLE shipto THEN
                    ASSIGN ship-name:SCREEN-VALUE = shipto.ship-name .
            END.
        END.                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quantity D-Dialog
ON LEAVE OF quantity IN FRAME D-Dialog /* quantity */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF INTEGER(quantity:SCREEN-VALUE) LE 0 THEN 
            DO:
                MESSAGE "Quantity must be enter..." VIEW-AS ALERT-BOX INFORMATION .
                APPLY "entry" TO quantity .
                RETURN NO-APPLY.
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

            IF AVAILABLE style THEN
                ASSIGN style-dscr:SCREEN-VALUE = style.dscr
                    sub-unit:SCREEN-VALUE   = style.spare-char-5 
                    .
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
                ASSIGN style-dscr:SCREEN-VALUE = style.dscr
                    sub-unit:SCREEN-VALUE   = style.spare-char-5  .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sub-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sub-unit D-Dialog
ON HELP OF sub-unit IN FRAME D-Dialog /* Sub Unit */
    DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-item.w (cocode, "", "C", FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value          = ENTRY(1,char-val)
                sun-Unit-dscr:screen-value = ENTRY(2,char-val)
                .    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sub-unit D-Dialog
ON LEAVE OF sub-unit IN FRAME D-Dialog /* Sub Unit */
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
                    ASSIGN sun-Unit-dscr:SCREEN-VALUE = ITEM.i-name .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tot-unit-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tot-unit-count D-Dialog
ON LEAVE OF tot-unit-count IN FRAME D-Dialog /* Unit/Pallet Count */
    DO:
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME unit-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL unit-count D-Dialog
ON VALUE-CHANGED OF unit-count IN FRAME D-Dialog /* Sub Unit Count */
    DO:
        tot-unit-count:SCREEN-VALUE = STRING(INTEGER(unit-count:SCREEN-VALUE) * INTEGER(per-pallet:SCREEN-VALUE)) .  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME unit-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL unit-count D-Dialog
ON LEAVE OF unit-count IN FRAME D-Dialog /* Sub Unit Count */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:

            RUN valid-sub-unit-count(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY .
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME per-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL per-pallet D-Dialog
ON LEAVE OF per-pallet IN FRAME D-Dialog /* Sub Unit Count */
    DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:

            RUN valid-per-pallet(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY .
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
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /*{src/adm/template/dialogmn.i}*/

    RUN enable_UI.

    {methods/nowait.i}
     
    DO WITH FRAME {&frame-name}:
      
        APPLY "entry" TO quantity IN FRAME {&FRAME-NAME}.

        RUN est/NewEstimate.p ('C', 5 ,OUTPUT lv-crt-est-rowid).
        FIND FIRST eb WHERE ROWID(eb) EQ lv-crt-est-rowid  NO-LOCK NO-ERROR.
        IF AVAILABLE eb THEN
            est-no:SCREEN-VALUE = eb.est-no .
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
  
    CREATE tt-frmout.
    ASSIGN 
      
        tt-frmout.part-no    = cst-part
        tt-frmout.stack-no   = fg-no
        tt-frmout.part-dscr1 = item-dscr
        tt-frmout.item-name  = item-name
        tt-frmout.bndl-cod   = sub-unit
        tt-frmout.unit-count = unit-count
        tt-frmout.per-pallet = per-pallet
        tt-frmout.pallet     = pallet
        tt-frmout.form-no    = iFormNumber
        tt-frmout.blank-no   = iBlankNumber 

        tt-frmout.cust-no    = cust-no 
        tt-frmout.ship-id    = ship-to 
        tt-frmout.style      = style-cod 
        tt-frmout.len        = len 
        tt-frmout.wid        = wid 
        tt-frmout.dep        = dep 
        tt-frmout.bord       = board  
        tt-frmout.quantity   = quantity 
        tt-frmout.cat        = fg-cat 
        .

    FIND FIRST item WHERE item.company = cocode
        AND item.i-no EQ board NO-LOCK NO-ERROR .

    IF AVAILABLE ITEM THEN
        ASSIGN 
            tt-frmout.flute = item.flute 
            tt-frmout.test  = ITEM.reg-no .
   
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
    DISPLAY cust-no ship-to style-cod style-dscr len wid dep cst-part fg-no fg-cat 
        quantity board est-no cust-name ship-name item-name item-dscr 
        board-dscr cat-dscr sub-unit sun-Unit-dscr unit-count per-pallet 
        pallet pallet-dscr tot-unit-count 
        WITH FRAME D-Dialog.
    ENABLE cust-no ship-to style-cod len wid dep cst-part fg-no fg-cat quantity 
        board Btn_OK Btn_Cancel RECT-1 RECT-2 RECT-3 RECT-4 item-name 
        item-dscr sub-unit unit-count per-pallet pallet 
        WITH FRAME D-Dialog.
    VIEW FRAME D-Dialog.
    {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fgitem D-Dialog 
PROCEDURE valid-fgitem :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
     
        IF fg-no:SCREEN-VALUE  NE "" THEN 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company  EQ gcompany
                AND itemfg.i-no    EQ fg-no:SCREEN-VALUE  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE itemfg  THEN 
            DO:
                MESSAGE "Invalid Fg Item, try help..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO fg-no .
                oplOutError = YES .
            END.
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
            AND cust.cust-no   EQ cust-no:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Customer, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cust-no .
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
        IF cst-part:SCREEN-VALUE  EQ "" THEN 
        DO:
            MESSAGE "Invalid Cust Part# must be enter..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO cst-part .
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
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
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
        IF NOT CAN-FIND(FIRST shipto
            WHERE shipto.company EQ gcompany
            AND shipto.cust-no EQ cust-no:SCREEN-VALUE
            AND shipto.ship-id EQ ship-to:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "            Invalid entry, try help...             "  VIEW-AS ALERT-BOX ERROR.
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
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO style-cod .
            oplOutError = YES .
        END.
    END.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sub-unit-count D-Dialog 
PROCEDURE valid-sub-unit-count :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(unit-count:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Sub Unit Count must be enter. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO unit-count.
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-per-pallet D-Dialog 
PROCEDURE valid-per-pallet :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF INTEGER(per-pallet:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Sub Unit Count must be enter. " VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO per-pallet.
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

