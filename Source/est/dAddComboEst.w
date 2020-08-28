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

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
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

DEFINE VARIABLE lv-copy-qty      AS INTEGER   EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-copy-rel      AS INTEGER   EXTENT 20 NO-UNDO.
DEFINE VARIABLE cLogicalRunShip  AS CHARACTER EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-crt-est-rowid AS ROWID     NO-UNDO.
DEFINE VARIABLE cStackCode       AS CHARACTER NO-UNDO .
DEFINE VARIABLE iOldQty          AS INTEGER   NO-UNDO .
DEFINE VARIABLE lShowMessage     AS LOGICAL   NO-UNDO .  

DEFINE SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.
DEFINE BUFFER bf-eb FOR eb.

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
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 //ttInputEst.iFormNo ttInputEst.iBlankNo ttInputEst.cStockNo ttInputEst.cPartID ttInputEst.iNumOnWidth //ttInputEst.cBoard //ttInputEst.cStyle ttInputEst.dLength ttInputEst.dWidth ttInputEst.dDepth ttInputEst.cPartName ttInputEst.cPartDescription //ttInputEst.lPurchased   
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
&Scoped-Define ENABLED-OBJECTS cMachineID quantity cBoardID Btn_OK ~
Btn_Cancel BROWSE-1 btn-add btn-copy btn-update btn-delete 
&Scoped-Define DISPLAYED-OBJECTS cMachineID quantity cBoardID len wid ~
cMachineName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
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
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE cBoardID AS CHARACTER FORMAT "X(8)":U 
     LABEL "Furnish" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMachineID AS CHARACTER FORMAT "X(8)":U 
     LABEL "Line" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMachineName AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE len AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "L" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE quantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE wid AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "W" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 5
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 17.14
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttInputEst SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
  QUERY BROWSE-1 DISPLAY
      //ttInputEst.iFormNo LABEL "F" WIDTH 3 LABEL-BGCOLOR 14 FORMAT ">9"
    ttInputEst.iBlankNo LABEL "B" WIDTH 3 LABEL-BGCOLOR 14 FORMAT ">9"
//    ttInputEst.cStockNo LABEL "Item ID" FORMAT "x(15)" WIDTH 21 LABEL-BGCOLOR 14 
    ttInputEst.cPartID LABEL "Part #" FORMAT "x(15)" WIDTH 21 LABEL-BGCOLOR 14
    ttInputEst.iNumOnWidth LABEL "Up" FORMAT ">9" WIDTH 10 LABEL-BGCOLOR 14
    //ttInputEst.cBoard LABEL "Board" FORMAT "x(10)" WIDTH 13 LABEL-BGCOLOR 14
    //ttInputEst.cStyle LABEL "Style" FORMAT "x(8)" WIDTH 10 LABEL-BGCOLOR 14
    ttInputEst.dLength LABEL "L" WIDTH 8 LABEL-BGCOLOR 14
    ttInputEst.dWidth LABEL "W" WIDTH 8 LABEL-BGCOLOR 14
    ttInputEst.dDepth LABEL "D" WIDTH 8 LABEL-BGCOLOR 14
    ttInputEst.cPartName LABEL "Part Name" FORMAT "x(30)" WIDTH 35 LABEL-BGCOLOR 14
    ttInputEst.cPartDescription LABEL "Part Description" FORMAT "x(30)" WIDTH 35 LABEL-BGCOLOR 14
    //ttInputEst.lPurchased LABEL "P/M" WIDTH 8 FORMAT "P/M" LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 124.2 BY 14.52
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cMachineID AT ROW 2.24 COL 19.6 COLON-ALIGNED WIDGET-ID 176
     quantity AT ROW 3.38 COL 19.8 COLON-ALIGNED WIDGET-ID 198
     cBoardID AT ROW 4.62 COL 19.8 COLON-ALIGNED WIDGET-ID 268
     len AT ROW 2.19 COL 73 COLON-ALIGNED WIDGET-ID 190
     wid AT ROW 2.19 COL 90 COLON-ALIGNED WIDGET-ID 194
     Btn_OK AT ROW 24.33 COL 51
     Btn_Cancel AT ROW 24.33 COL 67
     cMachineName AT ROW 2.24 COL 37.4 COLON-ALIGNED NO-LABEL WIDGET-ID 202
     BROWSE-1 AT ROW 9.1 COL 2.8
     btn-add AT ROW 7.67 COL 4 WIDGET-ID 16
     btn-copy AT ROW 7.67 COL 19.6 WIDGET-ID 252
     btn-update AT ROW 7.67 COL 35.2 WIDGET-ID 256
     btn-delete AT ROW 7.67 COL 51.2 WIDGET-ID 254
     "Head" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 6 WIDGET-ID 206
     "Molds" VIEW-AS TEXT
          SIZE 9 BY .71 AT ROW 6.62 COL 6.2 WIDGET-ID 264
     RECT-4 AT ROW 1.48 COL 2 WIDGET-ID 236
     RECT-5 AT ROW 6.95 COL 2 WIDGET-ID 262
     SPACE(1.19) SKIP(1.76)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add Head"
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
/* BROWSE-TAB BROWSE-1 cMachineName D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cMachineName IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN len IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wid IN FRAME D-Dialog
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Add Head */
DO:             
        DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
        IF ipType EQ "" THEN 
        DO:
            MESSAGE "Cancelling will not create the estimate and you will lose the molds that you entered."
                SKIP 
                "Are you sure you want to continue?" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lCheck .
        END.
        ELSE lCheck = TRUE.
               
        IF lCheck THEN 
        DO:   
            EMPTY TEMP-TABLE ttInputEst .
            EMPTY TEMP-TABLE tt-eb-set.
        
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
        
        RUN validMachineID(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .
    
        RUN est/dAddEditSingleEst.w (?,ROWID(eb),"Add",
            OUTPUT lv-rowid) . 
        
        FIND FIRST bff-ttInputEst NO-LOCK
            WHERE bff-ttInputEst.cCompany EQ cocode
            AND ROWID(bff-ttInputEst) EQ lv-rowid NO-ERROR .
        
        IF AVAILABLE bff-ttInputEst THEN
            RUN repo-query (lv-rowid).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy D-Dialog
ON CHOOSE OF btn-copy IN FRAME D-Dialog /* Copy  */
DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
        DEFINE BUFFER bff-ttInputEst FOR ttInputEst.         
    
        IF AVAILABLE ttInputEst THEN
        DO:   
            BUFFER-COPY ttInputEst  TO bff-ttInputEst .
            lv-rowid = IF AVAILABLE eb THEN ROWID(eb) ELSE ?.
            RUN est/dAddEditSingleEst.w (RECID(ttInputEst),lv-rowid,"Copy", OUTPUT lv-rowid) . 
            
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


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update D-Dialog
ON CHOOSE OF btn-update IN FRAME D-Dialog /* Update  */
DO:
        DEFINE VARIABLE lv-rowid  AS ROWID NO-UNDO. 
        DEFINE VARIABLE rwRowidEb AS ROWID NO-UNDO. 
        IF AVAILABLE ttInputEst THEN 
        DO:
            rwRowidEb = IF AVAILABLE eb THEN ROWID(eb) ELSE ?.
            RUN est/dAddEditSingleEst.w (RECID(ttInputEst),rwRowidEb,"Update",OUTPUT lv-rowid) . 
   
            RUN repo-query (ROWID(ttInputEst)).
        END.

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
            EMPTY TEMP-TABLE tt-eb-set.
            APPLY "END-ERROR":U TO SELF.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Save */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        iCount = 0.
        FOR EACH ttInputEst NO-LOCK :
            iCount = iCount + 1.
        END.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        IF INTEGER(quantity:SCREEN-VALUE) LE 0 THEN 
        DO:
            MESSAGE "Quantity must not be 0..." VIEW-AS ALERT-BOX INFORMATION .
            APPLY "entry" TO quantity .
            RETURN NO-APPLY.
        END.         

        RUN validMachineID(OUTPUT lError) NO-ERROR.
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
  
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cBoardID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cBoardID D-Dialog
ON HELP OF cBoardID IN FRAME D-Dialog /* Furnish */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-board.w (cocode, locode, cBoardID:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:SCREEN-VALUE = ENTRY(1,char-val)
               
                .     
        APPLY "value-changed" TO cBoardID IN FRAME {&FRAME-NAME}.
        APPLY "entry" TO cBoardID IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cBoardID D-Dialog
ON LEAVE OF cBoardID IN FRAME D-Dialog /* Furnish */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
   
        IF LASTKEY NE -1 THEN 
        DO:
            RUN validMachineID(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST mach NO-LOCK 
                WHERE mach.company EQ cocode
                AND mach.m-code EQ SELF:SCREEN-VALUE 
                NO-ERROR .
             
            IF AVAILABLE mach THEN
                ASSIGN 
                    cMachineName:SCREEN-VALUE = mach.m-dscr
                    len:SCREEN-VALUE = STRING(mach.max-len)
                    wid:SCREEN-VALUE = STRING(mach.max-wid)
                    .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMachineID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMachineID D-Dialog
ON HELP OF cMachineID IN FRAME D-Dialog /* Line */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
   
        RUN windows/l-mach.w (gcompany,locode, cMachineID:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:SCREEN-VALUE = ENTRY(1,char-val)
                cMachineName:SCREEN-VALUE = ENTRY(2,char-val)
                .     
        APPLY "value-changed" TO cMachineID IN FRAME {&FRAME-NAME}.
        APPLY "entry" TO cMachineID IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMachineID D-Dialog
ON LEAVE OF cMachineID IN FRAME D-Dialog /* Line */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
   
        IF LASTKEY NE -1 THEN 
        DO:
            RUN validMachineID(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST mach NO-LOCK 
                WHERE mach.company EQ cocode
                AND mach.m-code EQ SELF:SCREEN-VALUE 
                NO-ERROR .
             
            IF AVAILABLE mach THEN
                ASSIGN 
                    cMachineName:SCREEN-VALUE = mach.m-dscr
                    len:SCREEN-VALUE = STRING(mach.max-len)
                    wid:SCREEN-VALUE = STRING(mach.max-wid)
                    .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL len D-Dialog
ON LEAVE OF len IN FRAME D-Dialog /* L */
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


&Scoped-define SELF-NAME quantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quantity D-Dialog
ON HELP OF quantity IN FRAME D-Dialog /* Quantity */
DO:
/*        DEFINE VARIABLE char-val    AS cha       NO-UNDO.                                                                                                                                  */
/*        DEFINE VARIABLE look-recid  AS RECID     NO-UNDO.                                                                                                                                  */
/*        DEFINE VARIABLE char-val2   AS cha       NO-UNDO.                                                                                                                                  */
/*        DEFINE VARIABLE date-val    AS cha       NO-UNDO.                                                                                                                                  */
/*        DEFINE VARIABLE date-val2   AS cha       NO-UNDO.                                                                                                                                  */
/*        DEFINE VARIABLE cLogicalVal AS CHARACTER NO-UNDO.                                                                                                                                  */
/*                                                                                                                                                                                           */
/*                                                                                                                                                                                           */
/*        RUN est/estqtyfr.w (len:screen-value,wid:SCREEN-VALUE, quantity:SCREEN-VALUE,ipriRowid, OUTPUT char-val, OUTPUT char-val2, OUTPUT date-val, OUTPUT date-val2, OUTPUT cLogicalVal) .*/
/*        IF char-val <> "?"                                                                                                                                                                 */
/*            THEN ASSIGN quantity:screen-value = ENTRY(1,char-val)                                                                                                                          */
/*                lv-copy-qty[1]        = INTEGER(ENTRY(1,char-val))                                                                                                                         */
/*                lv-copy-qty[2]        = INTEGER(ENTRY(2,char-val))                                                                                                                         */
/*                lv-copy-qty[3]        = INTEGER(ENTRY(3,char-val))                                                                                                                         */
/*                lv-copy-qty[4]        = INTEGER(ENTRY(4,char-val))                                                                                                                         */
/*                lv-copy-qty[5]        = INTEGER(ENTRY(5,char-val))                                                                                                                         */
/*                lv-copy-qty[6]        = INTEGER(ENTRY(6,char-val))                                                                                                                         */
/*                lv-copy-qty[7]        = INTEGER(ENTRY(7,char-val))                                                                                                                         */
/*                lv-copy-qty[8]        = INTEGER(ENTRY(8,char-val))                                                                                                                         */
/*                lv-copy-qty[9]        = INTEGER(ENTRY(9,char-val))                                                                                                                         */
/*                lv-copy-qty[10]       = INTEGER(ENTRY(10,char-val))                                                                                                                        */
/*                lv-copy-rel[1]        = INTEGER(ENTRY(11,char-val))                                                                                                                        */
/*                lv-copy-rel[2]        = INTEGER(ENTRY(12,char-val))                                                                                                                        */
/*                lv-copy-rel[3]        = INTEGER(ENTRY(13,char-val))                                                                                                                        */
/*                lv-copy-rel[4]        = INTEGER(ENTRY(14,char-val))                                                                                                                        */
/*                lv-copy-rel[5]        = INTEGER(ENTRY(15,char-val))                                                                                                                        */
/*                lv-copy-rel[6]        = INTEGER(ENTRY(16,char-val))                                                                                                                        */
/*                lv-copy-rel[7]        = INTEGER(ENTRY(17,char-val))                                                                                                                        */
/*                lv-copy-rel[8]        = INTEGER(ENTRY(18,char-val))                                                                                                                        */
/*                lv-copy-rel[9]        = INTEGER(ENTRY(19,char-val))                                                                                                                        */
/*                lv-copy-rel[10]       = INTEGER(ENTRY(20,char-val)).                                                                                                                       */
/*        IF char-val2 <> "?"                                                                                                                                                                */
/*            THEN ASSIGN lv-copy-qty[11] = INTEGER(ENTRY(1,char-val2))                                                                                                                      */
/*                lv-copy-qty[12] = INTEGER(ENTRY(2,char-val2))                                                                                                                              */
/*                lv-copy-qty[13] = INTEGER(ENTRY(3,char-val2))                                                                                                                              */
/*                lv-copy-qty[14] = INTEGER(ENTRY(4,char-val2))                                                                                                                              */
/*                lv-copy-qty[15] = INTEGER(ENTRY(5,char-val2))                                                                                                                              */
/*                lv-copy-qty[16] = INTEGER(ENTRY(6,char-val2))                                                                                                                              */
/*                lv-copy-qty[17] = INTEGER(ENTRY(7,char-val2))                                                                                                                              */
/*                lv-copy-qty[18] = INTEGER(ENTRY(8,char-val2))                                                                                                                              */
/*                lv-copy-qty[19] = INTEGER(ENTRY(9,char-val2))                                                                                                                              */
/*                lv-copy-qty[20] = INTEGER(ENTRY(10,char-val2))                                                                                                                             */
/*                lv-copy-rel[11] = INTEGER(ENTRY(11,char-val2))                                                                                                                             */
/*                lv-copy-rel[12] = INTEGER(ENTRY(12,char-val2))                                                                                                                             */
/*                lv-copy-rel[13] = INTEGER(ENTRY(13,char-val2))                                                                                                                             */
/*                lv-copy-rel[14] = INTEGER(ENTRY(14,char-val2))                                                                                                                             */
/*                lv-copy-rel[15] = INTEGER(ENTRY(15,char-val2))                                                                                                                             */
/*                lv-copy-rel[16] = INTEGER(ENTRY(16,char-val2))                                                                                                                             */
/*                lv-copy-rel[17] = INTEGER(ENTRY(17,char-val2))                                                                                                                             */
/*                lv-copy-rel[18] = INTEGER(ENTRY(18,char-val2))                                                                                                                             */
/*                lv-copy-rel[19] = INTEGER(ENTRY(19,char-val2))                                                                                                                             */
/*                lv-copy-rel[20] = INTEGER(ENTRY(20,char-val2))                                                                                                                             */
/*                .                                                                                                                                                                          */
/*        IF cLogicalVal NE "?"   THEN                                                                                                                                                       */
/*            ASSIGN                                                                                                                                                                         */
/*                cLogicalRunShip[1]  = STRING(ENTRY(1,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[2]  = STRING(ENTRY(2,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[3]  = STRING(ENTRY(3,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[4]  = STRING(ENTRY(4,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[5]  = STRING(ENTRY(5,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[6]  = STRING(ENTRY(6,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[7]  = STRING(ENTRY(7,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[8]  = STRING(ENTRY(8,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[9]  = STRING(ENTRY(9,cLogicalVal))                                                                                                                         */
/*                cLogicalRunShip[10] = STRING(ENTRY(10,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[11] = STRING(ENTRY(11,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[12] = STRING(ENTRY(12,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[13] = STRING(ENTRY(13,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[14] = STRING(ENTRY(14,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[15] = STRING(ENTRY(15,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[16] = STRING(ENTRY(16,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[17] = STRING(ENTRY(17,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[18] = STRING(ENTRY(18,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[19] = STRING(ENTRY(19,cLogicalVal))                                                                                                                        */
/*                cLogicalRunShip[20] = STRING(ENTRY(20,cLogicalVal)) .                                                                                                                      */
/*        ELSE cLogicalRunShip[1] = "?" .                                                                                                                                                    */
/*                                                                                                                                                                                           */
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


&Scoped-define SELF-NAME wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wid D-Dialog
ON LEAVE OF wid IN FRAME D-Dialog /* W */
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
        IF ipType EQ "Edit" THEN 
        DO:
            RUN pDisplayValue.
            RUN pDisplayQty.                                              
            
            APPLY "entry" TO quantity IN FRAME {&FRAME-NAME}.
            
        END.    
        ELSE 
        DO:
            //RUN pDefaultValue. 
            
            APPLY "entry" TO cMachineID IN FRAME {&FRAME-NAME}.              
        END.
    /*RUN repo-query.*/          
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
/*    CREATE tt-eb-set .                                                             */
/*    ASSIGN                                                                         */
/*        tt-eb-set.est-type         = 6                                             */
/*        tt-eb-set.Company          = cocode                                        */
/*        tt-eb-set.loc              = locode                                        */
/*        tt-eb-set.form-no          = 0                                             */
/*        tt-eb-set.blank-no         = 0                                             */
/*        tt-eb-set.part-no          = cCustPart                                     */
/*        tt-eb-set.stock-no         = fg-no                                         */
/*        tt-eb-set.part-dscr1       = item-name                                     */
/*        tt-eb-set.part-dscr2       = item-dscr                                     */
/*        tt-eb-set.cust-no          = cCustNo                                       */
/*        tt-eb-set.ship-id          = ship-to                                       */
/*        tt-eb-set.len              = len                                           */
/*        tt-eb-set.wid              = wid                                           */
/*        tt-eb-set.dep              = dep                                           */
/*        tt-eb-set.procat           = fg-cat .                                      */
/*        IF cType EQ "No" THEN                                                      */
/*        ASSIGN                                                                     */
/*          tt-eb-set.set-is-assembled = FALSE                                       */
/*          tt-eb-set.pur-man          = TRUE .                                      */
/*        ELSE IF cType EQ "Auto" THEN                                               */
/*        ASSIGN                                                                     */
/*          tt-eb-set.set-is-assembled = TRUE                                        */
/*          tt-eb-set.pur-man          = TRUE .                                      */
/*        ELSE IF cType EQ "Yes" THEN                                                */
/*        ASSIGN                                                                     */
/*          tt-eb-set.set-is-assembled = FALSE                                       */
/*          tt-eb-set.pur-man          = FALSE .                                     */
/*        ELSE IF cType EQ "Q" THEN                                                  */
/*        ASSIGN                                                                     */
/*          tt-eb-set.set-is-assembled = TRUE                                        */
/*          tt-eb-set.pur-man          = FALSE .                                     */
/*                                                                                   */
/*                                                                                   */
/*    FOR EACH  bf-ttInputEst EXCLUSIVE-LOCK:                                        */
/*        ASSIGN                                                                     */
/*            bf-ttInputEst.iQuantity = quantity                                     */
/*            bf-ttInputEst.cCustomer = cCustNo                                      */
/*            bf-ttInputEst.cShipTo   = ship-to                                      */
/*            .                                                                      */
/*                                                                                   */
/*        ASSIGN                                                                     */
/*            bf-ttInputEst.copy-qty[2]  = lv-copy-qty[2]                            */
/*            bf-ttInputEst.copy-qty[3]  = lv-copy-qty[3]                            */
/*            bf-ttInputEst.copy-qty[4]  = lv-copy-qty[4]                            */
/*            bf-ttInputEst.copy-qty[5]  = lv-copy-qty[5]                            */
/*            bf-ttInputEst.copy-qty[6]  = lv-copy-qty[6]                            */
/*            bf-ttInputEst.copy-qty[7]  = lv-copy-qty[7]                            */
/*            bf-ttInputEst.copy-qty[8]  = lv-copy-qty[8]                            */
/*            bf-ttInputEst.copy-qty[9]  = lv-copy-qty[9]                            */
/*            bf-ttInputEst.copy-qty[10] = lv-copy-qty[10]                           */
/*                                                                                   */
/*            bf-ttInputEst.copy-qty[11] = lv-copy-qty[11]                           */
/*            bf-ttInputEst.copy-qty[12] = lv-copy-qty[12]                           */
/*            bf-ttInputEst.copy-qty[13] = lv-copy-qty[13]                           */
/*            bf-ttInputEst.copy-qty[14] = lv-copy-qty[14]                           */
/*            bf-ttInputEst.copy-qty[15] = lv-copy-qty[15]                           */
/*            bf-ttInputEst.copy-qty[16] = lv-copy-qty[16]                           */
/*            bf-ttInputEst.copy-qty[17] = lv-copy-qty[17]                           */
/*            bf-ttInputEst.copy-qty[18] = lv-copy-qty[18]                           */
/*            bf-ttInputEst.copy-qty[19] = lv-copy-qty[19]                           */
/*            bf-ttInputEst.copy-qty[20] = lv-copy-qty[20]                           */
/*                                                                                   */
/*            bf-ttInputEst.copy-rel[1]  = lv-copy-rel[1]                            */
/*            bf-ttInputEst.copy-rel[2]  = lv-copy-rel[2]                            */
/*            bf-ttInputEst.copy-rel[3]  = lv-copy-rel[3]                            */
/*            bf-ttInputEst.copy-rel[4]  = lv-copy-rel[4]                            */
/*            bf-ttInputEst.copy-rel[5]  = lv-copy-rel[5]                            */
/*            bf-ttInputEst.copy-rel[6]  = lv-copy-rel[6]                            */
/*            bf-ttInputEst.copy-rel[7]  = lv-copy-rel[7]                            */
/*            bf-ttInputEst.copy-rel[8]  = lv-copy-rel[8]                            */
/*            bf-ttInputEst.copy-rel[9]  = lv-copy-rel[9]                            */
/*            bf-ttInputEst.copy-rel[10] = lv-copy-rel[10]                           */
/*                                                                                   */
/*            bf-ttInputEst.copy-rel[11] = lv-copy-rel[11]                           */
/*            bf-ttInputEst.copy-rel[12] = lv-copy-rel[12]                           */
/*            bf-ttInputEst.copy-rel[13] = lv-copy-rel[13]                           */
/*            bf-ttInputEst.copy-rel[14] = lv-copy-rel[14]                           */
/*            bf-ttInputEst.copy-rel[15] = lv-copy-rel[15]                           */
/*            bf-ttInputEst.copy-rel[16] = lv-copy-rel[16]                           */
/*            bf-ttInputEst.copy-rel[17] = lv-copy-rel[17]                           */
/*            bf-ttInputEst.copy-rel[18] = lv-copy-rel[18]                           */
/*            bf-ttInputEst.copy-rel[19] = lv-copy-rel[19]                           */
/*            bf-ttInputEst.copy-rel[20] = lv-copy-rel[20] .                         */
/*                                                                                   */
/*        IF cLogicalRunShip[1] NE "" THEN                                           */
/*            ASSIGN                                                                 */
/*                bf-ttInputEst.copy-runship[1]  = cLogicalRunShip[1]                */
/*                bf-ttInputEst.copy-runship[2]  = cLogicalRunShip[2]                */
/*                bf-ttInputEst.copy-runship[3]  = cLogicalRunShip[3]                */
/*                bf-ttInputEst.copy-runship[4]  = cLogicalRunShip[4]                */
/*                bf-ttInputEst.copy-runship[5]  = cLogicalRunShip[5]                */
/*                bf-ttInputEst.copy-runship[6]  = cLogicalRunShip[6]                */
/*                bf-ttInputEst.copy-runship[7]  = cLogicalRunShip[7]                */
/*                bf-ttInputEst.copy-runship[8]  = cLogicalRunShip[8]                */
/*                bf-ttInputEst.copy-runship[9]  = cLogicalRunShip[9]                */
/*                bf-ttInputEst.copy-runship[10] = cLogicalRunShip[10]               */
/*                bf-ttInputEst.copy-runship[11] = cLogicalRunShip[11]               */
/*                bf-ttInputEst.copy-runship[12] = cLogicalRunShip[12]               */
/*                bf-ttInputEst.copy-runship[13] = cLogicalRunShip[13]               */
/*                bf-ttInputEst.copy-runship[14] = cLogicalRunShip[14]               */
/*                bf-ttInputEst.copy-runship[15] = cLogicalRunShip[15]               */
/*                bf-ttInputEst.copy-runship[16] = cLogicalRunShip[16]               */
/*                bf-ttInputEst.copy-runship[17] = cLogicalRunShip[17]               */
/*                bf-ttInputEst.copy-runship[18] = cLogicalRunShip[18]               */
/*                bf-ttInputEst.copy-runship[19] = cLogicalRunShip[19]               */
/*                bf-ttInputEst.copy-runship[20] = cLogicalRunShip[20].              */
/*        ELSE                                                                       */
/*        DO:                                                                        */
/*            FIND FIRST eb NO-LOCK                                                  */
/*                WHERE eb.company EQ cocode                                         */
/*                AND ROWID(eb) EQ ipriRowid NO-ERROR .                              */
/*            IF AVAILABLE eb  THEN                                                  */
/*            DO:                                                                    */
/*                FIND est-qty NO-LOCK                                               */
/*                    WHERE est-qty.company EQ eb.company                            */
/*                    AND est-qty.est-no EQ eb.est-no                                */
/*                    AND est-qty.eqty EQ eb.eqty                                    */
/*                    NO-ERROR.                                                      */
/*                IF AVAILABLE est-qty THEN                                          */
/*                DO:                                                                */
/*                    ASSIGN                                                         */
/*                        bf-ttInputEst.copy-runship[1]  = STRING(est-qty.whsed[1])  */
/*                        bf-ttInputEst.copy-runship[2]  = STRING(est-qty.whsed[2])  */
/*                        bf-ttInputEst.copy-runship[3]  = STRING(est-qty.whsed[3])  */
/*                        bf-ttInputEst.copy-runship[4]  = STRING(est-qty.whsed[4])  */
/*                        bf-ttInputEst.copy-runship[5]  = STRING(est-qty.whsed[5])  */
/*                        bf-ttInputEst.copy-runship[6]  = STRING(est-qty.whsed[6])  */
/*                        bf-ttInputEst.copy-runship[7]  = STRING(est-qty.whsed[7])  */
/*                        bf-ttInputEst.copy-runship[8]  = STRING(est-qty.whsed[8])  */
/*                        bf-ttInputEst.copy-runship[9]  = STRING(est-qty.whsed[9])  */
/*                        bf-ttInputEst.copy-runship[10] = STRING(est-qty.whsed[10]) */
/*                        bf-ttInputEst.copy-runship[11] = STRING(est-qty.whsed[11]) */
/*                        bf-ttInputEst.copy-runship[12] = STRING(est-qty.whsed[12]) */
/*                        bf-ttInputEst.copy-runship[13] = STRING(est-qty.whsed[13]) */
/*                        bf-ttInputEst.copy-runship[14] = STRING(est-qty.whsed[14]) */
/*                        bf-ttInputEst.copy-runship[15] = STRING(est-qty.whsed[15]) */
/*                        bf-ttInputEst.copy-runship[16] = STRING(est-qty.whsed[16]) */
/*                        bf-ttInputEst.copy-runship[17] = STRING(est-qty.whsed[17]) */
/*                        bf-ttInputEst.copy-runship[18] = STRING(est-qty.whsed[18]) */
/*                        bf-ttInputEst.copy-runship[19] = STRING(est-qty.whsed[19]) */
/*                        bf-ttInputEst.copy-runship[20] = STRING(est-qty.whsed[20]).*/
/*                END.                                                               */
/*            END.                                                                   */
/*        END.                                                                       */
/*                                                                                   */
/*                                                                                   */
/*    END.                                                                           */
    RELEASE tt-eb-set.
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
  DISPLAY cMachineID quantity cBoardID len wid cMachineName 
      WITH FRAME D-Dialog.
  ENABLE cMachineID quantity cBoardID Btn_OK Btn_Cancel BROWSE-1 btn-add 
         btn-copy btn-update btn-delete 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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
        
        IF AVAILABLE bf-eb THEN 
        DO:
            FIND FIRST est-qty NO-LOCK
                WHERE est-qty.company EQ bf-eb.company
                AND est-qty.est-no EQ bf-eb.est-no
                AND est-qty.eqty EQ bf-eb.eqty NO-ERROR .
                 
            IF AVAILABLE est-qty THEN 
            DO:  
                ASSIGN
                    lv-copy-qty[2]  = est-qty.qty[2]
                    lv-copy-qty[3]  = est-qty.qty[3]
                    lv-copy-qty[4]  = est-qty.qty[4]
                    lv-copy-qty[5]  = est-qty.qty[5]
                    lv-copy-qty[6]  = est-qty.qty[6]
                    lv-copy-qty[7]  = est-qty.qty[7]
                    lv-copy-qty[8]  = est-qty.qty[8]
                    lv-copy-qty[9]  = est-qty.qty[9]
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
                ASSIGN 
                    cLogicalRunShip[1]  = STRING(est-qty.whsed[1])
                    cLogicalRunShip[2]  = STRING(est-qty.whsed[2]) 
                    cLogicalRunShip[3]  = STRING(est-qty.whsed[3])
                    cLogicalRunShip[4]  = STRING(est-qty.whsed[4])
                    cLogicalRunShip[5]  = STRING(est-qty.whsed[5])
                    cLogicalRunShip[6]  = STRING(est-qty.whsed[6])
                    cLogicalRunShip[7]  = STRING(est-qty.whsed[7])
                    cLogicalRunShip[8]  = STRING(est-qty.whsed[8])
                    cLogicalRunShip[9]  = STRING(est-qty.whsed[9])
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValue D-Dialog 
PROCEDURE pDisplayValue :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
    DEFINE BUFFER bf-eb FOR eb .
    DEFINE BUFFER bf-ef FOR ef .
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST eb NO-LOCK 
            WHERE eb.company EQ cocode
            AND ROWID(eb) EQ ipriRowid NO-ERROR .
        
        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ eb.company
                AND ef.est-no EQ eb.est-no
                AND ef.form-no EQ eb.form-no  
                NO-ERROR.
            FIND FIRST bf-eb NO-LOCK
                WHERE bf-eb.company EQ eb.company
                AND bf-eb.est-no EQ eb.est-no
                AND bf-eb.form-no EQ 0 NO-ERROR.
                 
/*            IF AVAILABLE bf-eb THEN do:                                                                                                              */
/*                ASSIGN                                                                                                                               */
/*                    quantity:SCREEN-VALUE  = STRING(eb.eqty)                                                                                         */
/*                    cCustNo:SCREEN-VALUE   = eb.cust-no                                                                                              */
/*                    ship-to:SCREEN-VALUE   = eb.ship-id                                                                                              */
/*                    cCustPart:SCREEN-VALUE = bf-eb.part-no                                                                                           */
/*                    fg-no:SCREEN-VALUE     = bf-eb.stock-no                                                                                          */
/*                    item-name:SCREEN-VALUE = bf-eb.part-dscr1                                                                                        */
/*                    item-dscr:SCREEN-VALUE = bf-eb.part-dscr2                                                                                        */
/*                    len:SCREEN-VALUE       = STRING(bf-eb.len)                                                                                       */
/*                    wid:SCREEN-VALUE       = STRING(bf-eb.wid)                                                                                       */
/*                    dep:SCREEN-VALUE       = STRING(bf-eb.dep)                                                                                       */
/*                    fg-cat:SCREEN-VALUE    = bf-eb.procat                                                                                            */
/*                    cType:SCREEN-VALUE     = IF bf-eb.set-is-assembled EQ TRUE THEN "Yes" ELSE IF bf-eb.set-is-assembled EQ FALSE THEN "No" ELSE "Q".*/
/*                                                                                                                                                     */
/*                IF bf-eb.set-is-assembled EQ FALSE AND bf-eb.pur-man THEN                                                                            */
/*                ASSIGN                                                                                                                               */
/*                    cType:SCREEN-VALUE = "No".                                                                                                       */
/*                ELSE IF bf-eb.set-is-assembled EQ TRUE AND bf-eb.pur-man THEN                                                                        */
/*                ASSIGN                                                                                                                               */
/*                    cType:SCREEN-VALUE = "Auto".                                                                                                     */
/*                ELSE IF bf-eb.set-is-assembled EQ FALSE AND NOT bf-eb.pur-man THEN                                                                   */
/*                ASSIGN                                                                                                                               */
/*                    cType:SCREEN-VALUE = "Yes".                                                                                                      */
/*                ELSE IF  bf-eb.set-is-assembled EQ TRUE AND NOT bf-eb.pur-man THEN                                                                   */
/*                ASSIGN                                                                                                                               */
/*                    cType:SCREEN-VALUE = "Q".                                                                                                        */
/*                     .                                                                                                                               */
/*            END.                                                                                                                                     */
/*                                                                                                                                                     */
/*            FIND FIRST cust NO-LOCK WHERE cust.company = cocode                                                                                      */
/*                AND cust.cust-no EQ cCustNo:SCREEN-VALUE NO-ERROR .                                                                                  */
/*            IF AVAILABLE cust THEN                                                                                                                   */
/*                ASSIGN cust-name:SCREEN-VALUE = cust.NAME .                                                                                          */
/*                                                                                                                                                     */
/*            FIND FIRST shipto NO-LOCK WHERE shipto.company = cocode                                                                                  */
/*                AND shipto.cust-no EQ cCustNo:SCREEN-VALUE                                                                                           */
/*                AND shipto.ship-id EQ ship-to:SCREEN-VALUE NO-ERROR .                                                                                */
/*            IF AVAILABLE shipto THEN                                                                                                                 */
/*                ASSIGN ship-name:SCREEN-VALUE = shipto.ship-name .                                                                                   */
/*                                                                                                                                                     */
/*            FOR EACH bf-eb NO-LOCK                                                                                                                   */
/*                WHERE bf-eb.company EQ cocode                                                                                                        */
/*                AND bf-eb.est-no EQ eb.est-no                                                                                                        */
/*                AND bf-eb.form-no NE 0:                                                                                                              */
/*                                                                                                                                                     */
/*                FIND FIRST bf-ef NO-LOCK                                                                                                             */
/*                    WHERE bf-ef.company EQ bf-eb.company                                                                                             */
/*                    AND bf-ef.est-no EQ bf-eb.est-no                                                                                                 */
/*                    AND bf-ef.form-no EQ bf-eb.form-no                                                                                               */
/*                    NO-ERROR.                                                                                                                        */
/*                                                                                                                                                     */
/*                CREATE ttInputEst.                                                                                                                   */
/*                ASSIGN                                                                                                                               */
/*                    ttInputEst.cEstType         = "NewSetEstimate"                                                                                   */
/*                    ttInputEst.cSetType         = "Set"                                                                                              */
/*                    ttInputEst.cCompany         = cocode                                                                                             */
/*                    ttInputEst.iFormNo          = bf-eb.form-no                                                                                      */
/*                    ttInputEst.iBlankNo         = bf-eb.blank-no                                                                                     */
/*                    ttInputEst.cPartID          = bf-eb.part-no                                                                                      */
/*                    ttInputEst.cPartName        = bf-eb.part-dscr1                                                                                   */
/*                    ttInputEst.cPartDescription = eb.part-dscr2                                                                                      */
/*                    ttInputEst.dLength          = bf-eb.len                                                                                          */
/*                    ttInputEst.dWidth           = bf-eb.wid                                                                                          */
/*                    ttInputEst.dDepth           = bf-eb.dep                                                                                          */
/*                    ttInputEst.cCategory        = bf-eb.procat                                                                                       */
/*                    ttInputEst.cBoard           = IF AVAILABLE bf-ef THEN bf-ef.board ELSE ""                                                        */
/*                    ttInputEst.cStyle           = bf-eb.style                                                                                        */
/*                    ttInputEst.dQtyPerSet       = bf-eb.quantityPerSet                                                                               */
/*                    ttInputEst.lPurchased       = bf-eb.pur-man                                                                                      */
/*                    ttInputEst.riParentEst      = ROWID(bf-eb)                                                                                       */
/*                    ttInputEst.iEstNo           = INTEGER(bf-eb.est-no) .                                                                            */
/*            END.                                                                                                                                     */
/*            RUN repo-query (lv-rowid).                                                                                                               */
        
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
    
    

    CLOSE QUERY BROWSE-1.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY BROWSE-1 FOR EACH ttInputEst
            NO-LOCK BY ttInputEst.iFormNo.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validMachineID D-Dialog 
PROCEDURE validMachineID :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST mach
            WHERE mach.company  EQ gcompany
            AND mach.m-code   EQ cMachineID:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Machine, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cMachineID .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

