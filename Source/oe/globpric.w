&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe\globpric.w

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-process       AS LOG       NO-UNDO.

DEFINE VARIABLE start-cust-no   LIKE oe-prmtx.cust-no NO-UNDO.
DEFINE VARIABLE end-cust-no     LIKE oe-prmtx.cust-no INIT "zzzzzzzz" NO-UNDO.

DEFINE VARIABLE start-ship-to   LIKE oe-prmtx.custShipID NO-UNDO.
DEFINE VARIABLE end-ship-to     LIKE oe-prmtx.custShipID NO-UNDO.

DEFINE VARIABLE start-cust-type LIKE oe-prmtx.custype NO-UNDO.
DEFINE VARIABLE end-cust-type   LIKE oe-prmtx.custype INIT "zzzzzzzz" NO-UNDO.

DEFINE VARIABLE start-item-no   LIKE oe-prmtx.i-no NO-UNDO.
DEFINE VARIABLE end-item-no     LIKE oe-prmtx.i-no INIT "zzzzzzzzzzzzzzz" NO-UNDO.

DEFINE VARIABLE start-prod-cat  LIKE oe-prmtx.procat NO-UNDO.
DEFINE VARIABLE end-prod-cat    LIKE oe-prmtx.procat INIT "zzzzz" NO-UNDO.

DEFINE VARIABLE start-level     AS INTEGER   INIT 1 FORMAT ">9" NO-UNDO.
DEFINE VARIABLE end-level       AS INTEGER   INIT 10 FORMAT ">9" NO-UNDO.

DEFINE VARIABLE price-basis     AS CHARACTER FORMAT "X" INIT "P" NO-UNDO.
DEFINE VARIABLE pct-divide      AS CHARACTER FORMAT "X" INIT "D" NO-UNDO.

DEFINE VARIABLE percent-change  AS DECIMAL   FORMAT ">>9.99-" NO-UNDO.

DEFINE VARIABLE li-factor       AS INTEGER   NO-UNDO.

DEFINE VARIABLE ctr             AS INTEGER   NO-UNDO.

DEFINE VARIABLE iPurgeCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO .
DEFINE VARIABLE lProcess        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cExcelHeader    AS CHARACTER NO-UNDO.     
DEFINE VARIABLE hdOutputProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdQuoteProcs    AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
RUN est/QuoteProcs.p PERSISTENT SET hdQuoteProcs.

DEFINE TEMP-TABLE ttRowidsToPurge
    FIELD ttRowid AS ROWID.
DEFINE STREAM excel.

DEFINE TEMP-TABLE ttPriceMatrix NO-UNDO
    LIKE oe-prmtx
    FIELD oldPrice AS DECIMAL EXTENT 10 FORMAT "->>,>>>,>>9.99<<<<"
    .
cExcelHeader = "Eff. Date,Customer,Type,Category,Item Code,Price Basis,Minimum Order Qty.,Qty1,Old Price1,New Price 1,Dsc1,UOM1,Qty2,Old Price2,New Price 2,Dsc2,UOM2,"+
    "Qty3,Old Price3,New Price3,Dsc3,UOM3,Qty4,Old Price4,New Price4,Dsc4,UOM4,Qty5,Old Price5,New Price5,Dsc5,UOM5,Qty6,Old Price6,New Price6,Dsc6,UOM6," + 
    "Qty7,Old Price7,New Price7,Dsc7,UOM7,Qty8,Old Price8,New Price8,Dsc8,UOM8,Qty9,Old Price9,New Price9,Dsc9,UOM9,Qty10,Old Price10,New Price10,Dsc10,UOM10," +
    "Exp Date,ShipTo,Online,Customer Part #,Item Name,Item Description 1".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 tbPurge begin_cust begin_ship-to ~
end_cust end_ship-to begin_cust-type end_cust-type begin_i-no end_i-no ~
begin_cat end_cat begin_level end_level beg_eff_date end_eff_date ~
tg_new_eff_date td_imported cbPriceBasis cbUse cbMatrixPrecision ~
cbMatrixRounding percent_chg tbAutoClose btSimulate btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tbPurge begin_cust begin_ship-to end_cust ~
end_ship-to begin_cust-type end_cust-type begin_i-no end_i-no begin_cat ~
end_cat begin_level end_level beg_eff_date end_eff_date new_eff_date ~
tg_new_eff_date tg_newmatrix td_imported cbPriceBasis cbUse ~
cbMatrixPrecision cbMatrixRounding percent_chg tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine C-Win 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isLatestEffDate C-Win 
FUNCTION isLatestEffDate RETURNS LOGICAL
    ( BUFFER ipbf-oe-prmtx FOR oe-prmtx)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btSimulate 
    LABEL "Simulate" 
    SIZE 16.2 BY 1.29.

DEFINE VARIABLE cbMatrixPrecision AS CHARACTER FORMAT "X(256)":U INITIAL "2" 
    LABEL "Matrix Precision" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "0","1","2","3","4","5","6","Customer" 
    DROP-DOWN-LIST
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbMatrixRounding  AS CHARACTER FORMAT "X(256)":U INITIAL "U" 
    LABEL "Matrix Rounding" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEM-PAIRS "Up","U",
    "Down","D",
    "Normal","N",
    "Customer","C"
    DROP-DOWN-LIST
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbPriceBasis      AS CHARACTER FORMAT "X(256)":U INITIAL "Price" 
    LABEL "Price Basis" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Price","Discount" 
    DROP-DOWN-LIST
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbUse             AS CHARACTER FORMAT "X(256)":U INITIAL "M" 
    LABEL "Use" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEM-PAIRS "% Profit","D",
    "% Increase","M"
    DROP-DOWN-LIST
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cat         AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Category" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust        AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-type   AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Type" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no        AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_level       AS INTEGER   FORMAT ">>":U INITIAL 1 
    LABEL "Beginning Level" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE beg_eff_date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beg. Effective Date" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat           AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Category" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust          AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-type     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Type" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_eff_date      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/2999 
    LABEL "End. Effective Date" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no          AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_level         AS INTEGER   FORMAT ">>":U INITIAL 10 
    LABEL "Ending Level" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to       AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE new_eff_date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "New Effective Date" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg       AS DECIMAL   FORMAT "->>>>9.99":U INITIAL 0 
    LABEL "Percent Change" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 128 BY 14.48.

DEFINE VARIABLE tbAutoClose     AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tbPurge         AS LOGICAL INITIAL NO 
    LABEL "Purge?" 
    VIEW-AS TOGGLE-BOX
    SIZE 13.2 BY 1 TOOLTIP "This option allows PURGING Price Matrix records" NO-UNDO.

DEFINE VARIABLE td_imported     AS LOGICAL INITIAL NO 
    LABEL "Include Contract Pricing Customers?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40.8 BY .81 NO-UNDO.

DEFINE VARIABLE tg_newmatrix    AS LOGICAL INITIAL NO 
    LABEL "Create New Matrix?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42.8 BY .81 NO-UNDO.

DEFINE VARIABLE tg_new_eff_date AS LOGICAL INITIAL NO 
    LABEL "Update Effective Date?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tbPurge AT ROW 1.57 COL 88.6
    begin_cust AT ROW 2.76 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    begin_ship-to AT ROW 2.76 COL 53 COLON-ALIGNED HELP
    "Enter Beginning Ship To"
    end_cust AT ROW 2.76 COL 86.4 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    end_ship-to AT ROW 2.76 COL 115 COLON-ALIGNED HELP
    "Enter Ending Ship To"
    begin_cust-type AT ROW 3.95 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Customer Type"
    end_cust-type AT ROW 3.95 COL 86.4 COLON-ALIGNED HELP
    "Enter Ending Customer Type"
    begin_i-no AT ROW 5.14 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_i-no AT ROW 5.14 COL 86.4 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_cat AT ROW 6.33 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Category"
    end_cat AT ROW 6.33 COL 86.4 COLON-ALIGNED HELP
    "Enter Ending Category"
    begin_level AT ROW 7.52 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Level"
    end_level AT ROW 7.52 COL 86.4 COLON-ALIGNED HELP
    "Enter Ending Level"
    beg_eff_date AT ROW 8.71 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Effective Date"
    end_eff_date AT ROW 8.71 COL 86.4 COLON-ALIGNED HELP
    "Enter Ending Effective Date"
    new_eff_date AT ROW 10.05 COL 86.4 COLON-ALIGNED HELP
    "Enter Beginning Effective Date"
    tg_new_eff_date AT ROW 10.19 COL 19.2
    tg_newmatrix AT ROW 11.1 COL 19.2 WIDGET-ID 8
    td_imported AT ROW 11.1 COL 66.4
    cbPriceBasis AT ROW 12.05 COL 24.8 COLON-ALIGNED WIDGET-ID 10
    cbUse AT ROW 12.05 COL 86.6 COLON-ALIGNED WIDGET-ID 12
    cbMatrixPrecision AT ROW 13.24 COL 25 COLON-ALIGNED WIDGET-ID 16
    cbMatrixRounding AT ROW 13.24 COL 86.8 COLON-ALIGNED WIDGET-ID 14
    percent_chg AT ROW 14.71 COL 55.8 COLON-ALIGNED HELP
    "Enter a Negative or Positive Percentage"
    tbAutoClose AT ROW 16.24 COL 57.8 WIDGET-ID 64
    btSimulate AT ROW 17.33 COL 35.4 WIDGET-ID 18
    btn-process AT ROW 17.33 COL 56
    btn-cancel AT ROW 17.33 COL 77.6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY 1 AT ROW 1 COL 4
    RECT-17 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 131.8 BY 17.91
    BGCOLOR 15 .


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
        TITLE              = "Global Price Matrix Change"
        HEIGHT             = 17.91
        WIDTH              = 131.8
        MAX-HEIGHT         = 20.71
        MAX-WIDTH          = 163.4
        VIRTUAL-HEIGHT     = 20.71
        VIRTUAL-WIDTH      = 163.4
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN new_eff_date IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg_newmatrix IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Price Matrix Change */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Global Price Matrix Change */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to C-Win
ON HELP OF begin_ship-to IN FRAME FRAME-A /* Ship To */
    DO:
        DEFINE VARIABLE char-val   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID     NO-UNDO.
    
        RUN windows/l-shipt2.w (g_company,g_loc, begin_cust:SCREEN-VALUE IN FRAME {&frame-name}, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
        FIND shipto WHERE RECID(shipto) EQ look-recid NO-LOCK NO-ERROR.
        IF AVAILABLE shipto AND shipto.ship-id NE FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN 
        DO:
            FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = shipto.ship-id.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        EMPTY TEMP-TABLE ttPriceMatrix.
    
        DEFINE VARIABLE cConfirmPurge AS CHARACTER NO-UNDO.
    
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                {&displayed-objects}
                lProcess = YES
                .
        END.

        IF NOT tbPurge:CHECKED IN FRAME {&frame-name} THEN 
        DO:  /* 'Normal' processing */
            MESSAGE 
                "Are you sure you want to change the Price Matrix(es) within the selection parameters?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
            IF v-process THEN RUN run-process.
        END.
        ELSE 
        DO:  /* Purge confirmation and execution */
            MESSAGE 
                "You have chosen to PURGE the Price Matrix records for the selected criteria.  Are you sure you want to PURGE?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lPurge AS LOG.
            IF NOT lPurge THEN 
            DO:
                MESSAGE 
                    "You did not choose to proceed with the PURGE operation."
                    VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO:
                RUN pPurgePhase1.

                DISPLAY
                    STRING(iPurgeCount) + " records will be purged." FORMAT "x(30)" SKIP 
                    "An export file is stored in location " + cFileName + ".csv" FORMAT "x(75)" SKIP 
                    WITH FRAME dPurge VIEW-AS DIALOG-BOX THREE-D SIDE-LABELS 
                    TITLE "Confirm Purge".
                UPDATE 
                    cConfirmPurge FORMAT "x(5)" LABEL " Enter 'PURGE' to continue" 
                    WITH FRAME dPurge.
                IF cConfirmPurge EQ "PURGE" THEN 
                    RUN pPurgePhase2.
                ELSE 
                DO:
                    MESSAGE 
                        "You chose to cancel the PURGE process."
                        VIEW-AS ALERT-BOX.
                    RETURN NO-APPLY.
                END.
            END.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulate C-Win
ON CHOOSE OF btSimulate IN FRAME FRAME-A /* Simulate */
    DO:
        ASSIGN 
            {&displayed-objects}
            .
        MESSAGE 
            "Are you sure you want to simulate the process within the selection parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
            UPDATE lResponse AS LOGICAL .
        IF lResponse THEN 
        DO:
            EMPTY TEMP-TABLE ttPriceMatrix.
            lProcess = NO.
            IF tbPurge:CHECKED THEN 
                RUN pPurgePhase1. 
            ELSE     
                RUN run-process.     
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMatrixPrecision
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMatrixPrecision C-Win
ON VALUE-CHANGED OF cbMatrixPrecision IN FRAME FRAME-A /* Matrix Precision */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMatrixRounding
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMatrixRounding C-Win
ON VALUE-CHANGED OF cbMatrixRounding IN FRAME FRAME-A /* Matrix Rounding */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbPriceBasis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbPriceBasis C-Win
ON VALUE-CHANGED OF cbPriceBasis IN FRAME FRAME-A /* Price Basis */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbUse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUse C-Win
ON VALUE-CHANGED OF cbUse IN FRAME FRAME-A /* Use */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to C-Win
ON HELP OF end_ship-to IN FRAME FRAME-A /* Ship To */
    DO:
        DEFINE VARIABLE char-val   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID     NO-UNDO.
    
        RUN windows/l-shipt2.w (g_company,g_loc, end_cust:SCREEN-VALUE IN FRAME {&frame-name}, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
        FIND shipto WHERE RECID(shipto) EQ look-recid NO-LOCK NO-ERROR.
        IF AVAILABLE shipto AND shipto.ship-id NE FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN 
        DO:
            FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = shipto.ship-id.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_newmatrix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_newmatrix C-Win
ON VALUE-CHANGED OF tg_newmatrix IN FRAME FRAME-A /* Create New Matrix? */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN tg_newmatrix.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_new_eff_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_new_eff_date C-Win
ON VALUE-CHANGED OF tg_new_eff_date IN FRAME FRAME-A /* Update Effective Date? */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN tg_new_eff_date
                new_eff_date:SENSITIVE    = tg_new_eff_date
                tg_newmatrix:SENSITIVE    = tg_new_eff_date
                tg_newmatrix:SCREEN-VALUE = tg_new_eff_date:SCREEN-VALUE.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN.
    END.

    new_eff_date = TODAY.
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    btSimulate:LOAD-IMAGE("Graphics/32x32/Simulate.png").
    RUN enable_UI.
    {methods/nowait.i}

    APPLY "ENTRY":U TO begin_cust IN FRAME {&FRAME-NAME}.

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
    DISPLAY tbPurge begin_cust begin_ship-to end_cust end_ship-to begin_cust-type 
        end_cust-type begin_i-no end_i-no begin_cat end_cat begin_level 
        end_level beg_eff_date end_eff_date new_eff_date tg_new_eff_date 
        tg_newmatrix td_imported cbPriceBasis cbUse cbMatrixPrecision 
        cbMatrixRounding percent_chg tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-17 tbPurge begin_cust begin_ship-to end_cust end_ship-to 
        begin_cust-type end_cust-type begin_i-no end_i-no begin_cat end_cat 
        begin_level end_level beg_eff_date end_eff_date tg_new_eff_date 
        td_imported cbPriceBasis cbUse cbMatrixPrecision cbMatrixRounding 
        percent_chg tbAutoClose btSimulate btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markdown C-Win 
PROCEDURE markdown :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipb-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT  PARAMETER ipcRoundingType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRoundingLevel AS INTEGER   NO-UNDO.

    DO ctr = start-level TO min(10,end-level):
        IF price-basis EQ "D" THEN 
            ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] +
                (ipb-oe-prmtx.discount[ctr] * percent-change).
        ELSE
            IF price-basis EQ "P" THEN 
                ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] +
                    (ipb-oe-prmtx.price[ctr] * percent-change).
                                                     
        IF ipcRoundingType EQ "U" THEN  
            RUN rounding (INPUT-OUTPUT ipb-oe-prmtx.price[ctr]).
        ELSE IF ipcRoundingType EQ "D" THEN
                ipb-oe-prmtx.price[ctr] = TRUNCATE(ipb-oe-prmtx.price[ctr],ipiRoundingLevel).
            ELSE
                ipb-oe-prmtx.price[ctr] = ROUND(ipb-oe-prmtx.price[ctr],ipiRoundingLevel).      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markup C-Win 
PROCEDURE markup :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipb-oe-prmtx FOR oe-prmtx.
    DEFINE INPUT  PARAMETER ipcRoundingType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRoundingLevel AS INTEGER   NO-UNDO.

    DO ctr = start-level TO min(10,end-level):
        IF percent-change EQ 1 THEN 
        DO:
            IF price-basis EQ "D" THEN 
                ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] * 2.
            ELSE
                IF price-basis EQ "P" THEN 
                    ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] * 2.
        END.

        ELSE 
        DO:
            IF price-basis EQ "D" THEN 
            DO:
                IF pct-divide = "D" THEN
                    ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] / (1 - percent-change).
                ELSE
                    ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] * (1 + percent-change).
            END.
            ELSE 
                IF price-basis EQ "P" THEN 
                DO:
                    IF pct-divide = "D" THEN
                        ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] / (1 - percent-change).
                    ELSE
                        ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] * (1 + percent-change).
                END.
        END.
        IF ipcRoundingType EQ "U" THEN  
            RUN rounding (INPUT-OUTPUT ipb-oe-prmtx.price[ctr]).
        ELSE IF ipcRoundingType EQ "D" THEN
                ipb-oe-prmtx.price[ctr] = TRUNCATE(ipb-oe-prmtx.price[ctr],ipiRoundingLevel).
            ELSE
                ipb-oe-prmtx.price[ctr] = ROUND(ipb-oe-prmtx.price[ctr],ipiRoundingLevel).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateCSVRecords C-Win 
PROCEDURE pCreateCSVRecords PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbfttPriceMatrix FOR ttPriceMatrix.
    
    DEFINE VARIABLE cConcatRecords AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPartNo        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDescr     AS CHARACTER NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipbfttPriceMatrix.company
        AND itemfg.i-no    EQ SUBSTRING(ipbfttPriceMatrix.i-no,1,15)
        NO-ERROR.
    IF AVAILABLE itemfg THEN
        ASSIGN
            cItemName  = itemfg.i-name
            cPartNo    = itemfg.part-no
            cItemDescr = itemfg.part-dscr1
            .
    
    ASSIGN
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.eff-date,"99/99/9999"))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.cust-no))
        cConcatRecords = cConcatRecords + appendXLLine(ipbfttPriceMatrix.custype)
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.procat))
        cConcatRecords = cConcatRecords + appendXLLine(SUBSTRING(ipbfttPriceMatrix.i-no,1,15))                      
        cConcatRecords = cConcatRecords + appendXLLine(STRING(IF ipbfttPriceMatrix.meth THEN "Price" ELSE "Discount"))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.minOrderQty ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[1]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[1] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[1]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[1] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[1]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[2]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[2] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[2]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[2] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[2]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[3]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[3] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[3]    )) 
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[3] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[3]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[4]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[4] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[4]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[4] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[4]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[5]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[5] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[5]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[5] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[5]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[6]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[6] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[6]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[6] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[6]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[7]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[7] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[7]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[7] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[7]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[8]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[8] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[8]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[8] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[8]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[9]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[9] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[9]    ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[9] ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[9]      ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.qty[10]     ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.oldPrice[10]))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.price[10]   ))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.discount[10]))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.uom[10]     )) 
        cConcatRecords = cConcatRecords + appendXLLine(IF ipbfttPriceMatrix.exp-date NE ? THEN  STRING(ipbfttPriceMatrix.exp-date,"99/99/9999") ELSE "")
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.custShipID))
        cConcatRecords = cConcatRecords + appendXLLine(STRING(ipbfttPriceMatrix.online))
        cConcatRecords = cConcatRecords + appendXLLine(cPartNo) 
        cConcatRecords = cConcatRecords + appendXLLine(cItemName) 
        cConcatRecords = cConcatRecords + appendXLLine(cItemDescr)
        .
    PUT STREAM excel UNFORMATTED cConcatRecords SKIP.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPurgePhase1 C-Win 
PROCEDURE pPurgePhase1 :
    /*------------------------------------------------------------------------------
     Purpose: create temp-table for every record to be purged; write export file
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-prmtx FOR oe-prmtx.
    
    DEFINE VARIABLE ino       AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE pricbas   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustPart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIDesc1   AS CHARACTER NO-UNDO.

    RUN sys/ref/ExcelNameExt.p (INPUT "c:\tmp\PriceMtxPurge.csv", OUTPUT cFileName) .

    ASSIGN 
        start-cust-no   = begin_cust
        end-cust-no     = end_cust
        start-ship-to   = begin_ship-to
        end-ship-to     = end_ship-to
        start-cust-type = begin_cust-type
        end-cust-type   = end_cust-type
        start-item-no   = begin_i-no
        end-item-no     = end_i-no
        start-prod-cat  = begin_cat
        end-prod-cat    = end_cat
        start-level     = begin_level
        end-level       = end_level
        price-basis     = SUBSTR(cbPriceBasis,1,1)
        pct-divide      = SUBSTR(cbUse,1,1)
        percent-change  = percent_chg / 100
        li-factor       = 1
        iPurgeCount     = 0.
        
    EMPTY TEMP-TABLE ttRowidsToPurge.

    SESSION:SET-WAIT-STATE ("general").

    OUTPUT STREAM excel TO VALUE(cFileName).
    PUT STREAM excel UNFORMATTED cExcelHeader SKIP.
    
    FOR EACH b-oe-prmtx WHERE b-oe-prmtx.company = cocode 
        AND b-oe-prmtx.cust-no GE begin_cust
        AND b-oe-prmtx.cust-no LE end_cust
        AND b-oe-prmtx.custShipID GE begin_ship-to
        AND b-oe-prmtx.custShipID LE end_ship-to
        AND b-oe-prmtx.procat GE begin_cat
        AND b-oe-prmtx.procat LE end_cat 
        AND b-oe-prmtx.i-no GE begin_i-no 
        AND b-oe-prmtx.i-no LE end_i-no
        AND b-oe-prmtx.custype GE begin_cust-type
        AND b-oe-prmtx.custype LE end_cust-type 
        AND b-oe-prmtx.eff-date GE beg_eff_date
        AND b-oe-prmtx.eff-date LE end_eff_date 
        NO-LOCK:

        CREATE ttRowidsToPurge.
        ASSIGN 
            ttRowidsToPurge.ttRowid = ROWID(b-oe-prmtx)
            iPurgeCount             = iPurgeCount + 1.
        CREATE ttPriceMatrix.
        BUFFER-COPY b-oe-prmtx TO ttPriceMatrix.    

        RUN pCreateCSVRecords(
            BUFFER ttPriceMatrix  
            ).
    END.

    OUTPUT STREAM excel CLOSE.

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPurgePhase2 C-Win 
PROCEDURE pPurgePhase2 :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-prmtx FOR oe-prmtx.
    FOR EACH ttRowidsToPurge:
        FIND b-oe-prmtx WHERE 
            ROWID(b-oe-prmtx) EQ ttRowidsToPurge.ttRowid
        EXCLUSIVE NO-ERROR.
        IF AVAILABLE b-oe-prmtx THEN 
        DO:
            DELETE b-oe-prmtx.
        END.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSimulateRounding C-Win 
PROCEDURE pSimulateRounding :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbfttPriceMatrix FOR ttPriceMatrix.
    DEFINE INPUT  PARAMETER iplMarkup         AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRoundingType   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRoundingLevel  AS INTEGER   NO-UNDO.
 

    DO ctr = start-level TO min(10,end-level):
        IF iplMarkup THEN 
        DO:            
            IF percent-change EQ 1 THEN 
            DO:
                IF price-basis EQ "D" THEN 
                    ipbfttPriceMatrix.discount[ctr] = ipbfttPriceMatrix.discount[ctr] * 2.
                ELSE
                    IF price-basis EQ "P" THEN 
                        ipbfttPriceMatrix.price[ctr]    = ipbfttPriceMatrix.price[ctr] * 2.
            END.
        
            ELSE 
            DO:
                IF price-basis EQ "D" THEN 
                DO:
                    IF pct-divide = "D" THEN
                        ipbfttPriceMatrix.discount[ctr] = ipbfttPriceMatrix.discount[ctr] / (1 - percent-change).
                    ELSE
                        ipbfttPriceMatrix.discount[ctr] = ipbfttPriceMatrix.discount[ctr] * (1 + percent-change).
                END.
                ELSE IF price-basis EQ "P" THEN 
                    DO:
                        IF pct-divide = "D" THEN
                            ipbfttPriceMatrix.price[ctr] = ipbfttPriceMatrix.price[ctr] / (1 - percent-change).
                        ELSE                             
                            ipbfttPriceMatrix.price[ctr] = ipbfttPriceMatrix.price[ctr] * (1 + percent-change).
                    END.
            END.
        END.
        ELSE 
        DO:
            IF price-basis EQ "D" THEN 
                ipbfttPriceMatrix.discount[ctr] = ipbfttPriceMatrix.discount[ctr] + (ipbfttPriceMatrix.discount[ctr] * percent-change).
            ELSE IF price-basis EQ "P" THEN 
                    ipbfttPriceMatrix.price[ctr]    = ipbfttPriceMatrix.price[ctr] + (ipbfttPriceMatrix.price[ctr] * percent-change).           
        END. 
        IF ipcRoundingType EQ "U" THEN  
            RUN rounding (INPUT-OUTPUT ipbfttPriceMatrix.price[ctr]).
        
        ELSE IF ipcRoundingType EQ "D" THEN
                ipbfttPriceMatrix.price[ctr] = TRUNCATE(ipbfttPriceMatrix.price[ctr],ipiRoundingLevel).
            ELSE
                ipbfttPriceMatrix.price[ctr] = ROUND(ipbfttPriceMatrix.price[ctr],ipiRoundingLevel).        
    END.                        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rounding C-Win 
PROCEDURE rounding :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-price AS DECIMAL DECIMALS 10 NO-UNDO.


    io-price = io-price * li-factor.

    {sys/inc/roundup.i io-price}

    io-price = io-price / li-factor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    /* ---------------------------------------------------oe/oe-pmgpc.p 05/00 DJK */
    /* Global Price Matrix Change                                                 */
    /* -------------------------------------------------------------------------- */

    SESSION:SET-WAIT-STATE("General").

    DEFINE VARIABLE v-date                  AS DATE      NO-UNDO.
    DEFINE VARIABLE v-date-str              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-start-i-no            AS CHARACTER FORMAT "X(108)" NO-UNDO.
    DEFINE VARIABLE cRoundingType           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRoundingLevel          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOutputDir              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIsTempTableRecordAvail AS LOGICAL   NO-UNDO.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.

    ASSIGN
        start-cust-no   = begin_cust
        end-cust-no     = end_cust
        start-ship-to   = begin_ship-to
        end-ship-to     = end_ship-to
        start-cust-type = begin_cust-type
        end-cust-type   = end_cust-type
        start-item-no   = begin_i-no
        end-item-no     = end_i-no
        start-prod-cat  = begin_cat
        end-prod-cat    = end_cat
        start-level     = begin_level
        end-level       = end_level
        price-basis     = SUBSTR(cbPriceBasis,1,1)
        pct-divide      = SUBSTR(cbUse,1,1)
        percent-change  = percent_chg / 100
        li-factor       = 1
        cRoundingType   = cbMatrixRounding
        .

    IF cbMatrixPrecision NE "Customer" THEN 
    DO:
        DO ctr = 1 TO INTEGER(cbMatrixPrecision):
            li-factor = li-factor * 10.    
        END.
        iRoundingLevel = INTEGER(cbMatrixPrecision).
    END.
    RUN FileSys_GetTempDirectory(
        OUTPUT cOutputDir
        ).
    IF tbPurge:CHECKED IN FRAME {&FRAME-NAME} THEN 
        cOutputDir = cOutputDir + "\PriceMtxPurge.csv".
    ELSE    
        cOutputDir = cOutputDir + "\PriceMatrix.csv".
             
    RUN sys/ref/ExcelNameExt.p (
        INPUT cOutputDir, 
        OUTPUT cFileName
        ).   
    OUTPUT STREAM excel TO VALUE(cFileName).
    PUT STREAM excel UNFORMATTED cExcelHeader SKIP.
                
    MAIN:
    REPEAT PRESELECT EACH oe-prmtx EXCLUSIVE-LOCK
        WHERE oe-prmtx.company    EQ cocode 
        AND oe-prmtx.cust-no    GE start-cust-no
        AND oe-prmtx.cust-no    LE end-cust-no
        AND oe-prmtx.custShipID GE start-ship-to
        AND oe-prmtx.custShipID LE end-ship-to
        AND oe-prmtx.custype    GE start-cust-type
        AND oe-prmtx.custype    LE end-cust-type
        AND oe-prmtx.procat     GE start-prod-cat
        AND oe-prmtx.procat     LE end-prod-cat
        AND oe-prmtx.i-no       GE start-item-no
        AND oe-prmtx.i-no       LE end-item-no
        AND oe-prmtx.eff-date   GE beg_eff_date
        AND oe-prmtx.eff-date   LE end_eff_date :
  
        FIND NEXT oe-prmtx.
        RELEASE bf-oe-prmtx.

    
        FIND FIRST cust  NO-LOCK 
            WHERE cust.company EQ cocode 
            AND cust.cust-no EQ  oe-prmtx.cust-no 
            NO-ERROR .

        IF AVAILABLE cust AND cust.imported EQ YES AND NOT td_imported THEN NEXT MAIN.
     
        /*     FIND FIRST reftable WHERE                            */
        /*           reftable.rec_key  EQ oe-prmtx.rec_key AND      */
        /*           reftable.company  EQ "oe-prmtx"                */
        /*           USE-INDEX rec_key                              */
        /*           NO-LOCK NO-ERROR.                              */
        /*     IF AVAIL reftable THEN v-date = DATE(reftable.CODE). */
        /*                                                          */
        /*     v-start-i-no = SUBSTR(oe-prmtx.i-no,1,100).          */
        /*     IF v-start-i-no GE start-item-no AND                 */
        /*        v-start-i-no LE end-item-no AND                   */
        /*        v-date GE beg_eff_date AND                        */
        /*        v-date LE end_eff_date THEN                       */
        /*        DO:                                               */
        
        lIsTempTableRecordAvail = NO.
        /* Set Rounding level to 2 if cust.matrixRounding is blank */
        IF cbMatrixPrecision EQ "Customer" THEN 
        DO:
            li-factor = 1.  /* Reset to 1 */ 
            IF cust.matrixRounding EQ "" THEN 
                iRoundingLevel = 2.  
            ELSE 
                iRoundingLevel = cust.matrixPrecision.
              
            DO ctr = 1 TO INTEGER(iRoundingLevel):
                li-factor = li-factor * 10.    
            END.        
        END.              
              
        IF cRoundingType EQ "C" THEN 
        DO:
            IF cust.matrixRounding EQ "" THEN
                cRoundingType = "U".
            ELSE
                cRoundingType = cust.matrixRounding.               
        END.              
        IF tg_newmatrix THEN 
        DO:
            IF isLatestEffDate(BUFFER oe-prmtx) THEN 
            DO:  
                IF lProcess THEN 
                DO:     
                    CREATE bf-oe-prmtx.
                    BUFFER-COPY oe-prmtx EXCEPT oe-prmtx.rec_key TO bf-oe-prmtx.
                END.
                CREATE ttPriceMatrix.
                BUFFER-COPY oe-prmtx TO ttPriceMatrix.
                ttPriceMatrix.oldPrice = oe-prmtx.price. /* Store old price into temp table */
                lIsTempTableRecordAvail = YES.
            END.
        END.
        ELSE 
        DO:
            FIND FIRST bf-oe-prmtx WHERE ROWID(bf-oe-prmtx) EQ ROWID(oe-prmtx).
            IF AVAILABLE bf-oe-prmtx THEN 
            DO:
                CREATE ttPriceMatrix.
                BUFFER-COPY bf-oe-prmtx TO ttPriceMatrix. 
                ttPriceMatrix.oldPrice = bf-oe-prmtx.price.  /* Store old price into temp table */
            END.    
        END.    
        IF AVAILABLE bf-oe-prmtx OR lIsTempTableRecordAvail THEN 
        DO:
            IF tg_new_eff_date THEN 
            DO:
                IF lProcess THEN 
                    bf-oe-prmtx.eff-date = new_eff_date.
                ttPriceMatrix.eff-date = new_eff_date. 
            END.    
            /*                  ASSIGN                                                    */
            /*                     v-date-str = STRING(new_eff_date,"99/99/9999")         */
            /*                     lb-oe-prmtx.i-no = STRING(lb-oe-prmtx.i-no,"x(100)") + */
            /*                                     SUBSTR(v-date-str,7,4) +               */
            /*                                     SUBSTR(v-date-str,1,2) +               */
            /*                                     SUBSTR(v-date-str,4,2).                */
            RUN pSimulateRounding(
                BUFFER ttPriceMatrix,
                INPUT  IF percent-change LE 0 THEN NO ELSE YES,
                INPUT  cRoundingType,
                INPUT  iRoundingLevel
                ).
            RUN pCreateCSVRecords(
                BUFFER ttPriceMatrix
                ) .        
            IF lProcess THEN 
            DO:
                IF percent-change LE 0 THEN
                    RUN markdown (
                        BUFFER bf-oe-prmtx,
                        INPUT  cRoundingType,
                        INPUT  iRoundingLevel
                        ).
                ELSE 
                    RUN markup (
                        BUFFER bf-oe-prmtx,
                        INPUT  cRoundingType,
                        INPUT  iRoundingLevel
                        ).
                FIND CURRENT bf-oe-prmtx NO-LOCK NO-ERROR.  
                  
                RUN Price_ExpireOldPrice(
                    INPUT bf-oe-prmtx.company,
                    INPUT bf-oe-prmtx.i-no,
                    INPUT bf-oe-prmtx.custshipid,
                    INPUT bf-oe-prmtx.cust-no,
                    INPUT bf-oe-prmtx.custype,
                    INPUT bf-oe-prmtx.procat
                    ).
                        
                RUN UpdateQuotePriceFromMatrix IN hdQuoteProcs(ROWID(bf-oe-prmtx)).       
            END.                       
        END.
    /*        END. */
    END.

    SESSION:SET-WAIT-STATE("").
    OUTPUT STREAM excel CLOSE.  

    MESSAGE (IF lProcess THEN TRIM(c-win:TITLE) ELSE "Simulation")+ " Process Is Completed."
        + "~n" + "An Export File is Stored in Location " + cFileName VIEW-AS ALERT-BOX.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine C-Win 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.
    
    ipc-append = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, ipc-append).
    
    lc-line = quoter(ipc-append) + ",".
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isLatestEffDate C-Win 
FUNCTION isLatestEffDate RETURNS LOGICAL
    ( BUFFER ipbf-oe-prmtx FOR oe-prmtx) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    /*     DEF BUFFER bf-reftable FOR reftable. */

    FOR EACH bf-oe-prmtx NO-LOCK
        WHERE bf-oe-prmtx.company    EQ cocode 
        AND bf-oe-prmtx.cust-no    EQ ipbf-oe-prmtx.cust-no
        AND bf-oe-prmtx.custype    EQ ipbf-oe-prmtx.custype
        AND bf-oe-prmtx.procat     EQ ipbf-oe-prmtx.procat
        AND SUBSTR(bf-oe-prmtx.i-no,1,100) 
        EQ SUBSTR(ipbf-oe-prmtx.i-no,1,100)
        AND bf-oe-prmtx.rec_key NE ipbf-oe-prmtx.rec_key       
        :
        /*         ,                                                 */
        /*         FIRST bf-reftable WHERE                           */
        /*           lb-reftable.rec_key  EQ lb-oe-prmtx.rec_key AND */
        /*           lb-reftable.company  EQ "oe-prmtx"              */
        /*           USE-INDEX rec_key                               */
        /*           NO-LOCK */

        IF bf-oe-prmtx.eff-date GT ipbf-oe-prmtx.eff-date THEN 
            RETURN NO.
    END. /*each price matrix for same item*/

    RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

