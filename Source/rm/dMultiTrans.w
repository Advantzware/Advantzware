&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/dMultiTrans.w
  
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

{rm/ttRmRctd.i shared}
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

/*DEFINE SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.   */
DEFINE BUFFER bf-eb FOR eb.

DEFINE NEW SHARED TEMP-TABLE tt-selected 
    FIELD tt-rowid AS ROWID.

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
&Scoped-define INTERNAL-TABLES tt-rm-rctd

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-rm-rctd.i-no tt-rm-rctd.loc tt-rm-rctd.loc-bin tt-rm-rctd.tag tt-rm-rctd.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-rm-rctd WHERE tt-rm-rctd.Company = cocode ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-rm-rctd WHERE tt-rm-rctd.Company = cocode ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-rm-rctd


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dtTrandDate btnCalendar-1 cItemNo cToWhs ~
cToBin Btn_select-tag Btn_OK Btn_Cancel BROWSE-1 cFromWhs cFromBin ~
Btn_delete 
&Scoped-Define DISPLAYED-OBJECTS dtTrandDate cItemNo cUom item-name cToWhs ~
cToBin cFromWhs cFromBin 

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

DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_delete AUTO-GO 
     LABEL "&Delete" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Save" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_select-tag AUTO-GO 
     LABEL "&Select Bins" 
     SIZE 22 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE cFromBin AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Bin" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cFromWhs AS CHARACTER FORMAT "X(5)":U 
     LABEL "From Whse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cItemNo AS CHARACTER FORMAT "X(15)":U 
     LABEL "Item No" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cToBin AS CHARACTER FORMAT "X(8)":U 
     LABEL "To Bin" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cToWhs AS CHARACTER FORMAT "X(5)":U 
     LABEL "To Whse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cUom AS CHARACTER FORMAT "X(3)":U 
     LABEL "Uom" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dtTrandDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Transfer Date" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 43.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 97 BY 6.67
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 98 BY 14.52
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-rm-rctd.i-no LABEL "Item" WIDTH 18 LABEL-BGCOLOR 14 FORMAT "x(15)"
    tt-rm-rctd.loc  LABEL "Whse" WIDTH 8 LABEL-BGCOLOR 14 FORMAT "x(5)"
    tt-rm-rctd.loc-bin  LABEL "Bin" WIDTH 10 LABEL-BGCOLOR 14 FORMAT "x(8)"
    tt-rm-rctd.tag LABEL "Tag" WIDTH 28 LABEL-BGCOLOR 14 FORMAT "x(20)"
    tt-rm-rctd.qty LABEL "Qty" WIDTH 15 LABEL-BGCOLOR 14 FORMAT "->>,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 96.2 BY 13.05
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     dtTrandDate AT ROW 2.05 COL 24.2 COLON-ALIGNED WIDGET-ID 178
     btnCalendar-1 AT ROW 2.05 COL 43.8
     cItemNo AT ROW 3.19 COL 24.2 COLON-ALIGNED WIDGET-ID 42
     cUom AT ROW 3.19 COL 59.8 COLON-ALIGNED WIDGET-ID 274
     item-name AT ROW 4.38 COL 24.2 COLON-ALIGNED WIDGET-ID 208
     cFromWhs AT ROW 5.52 COL 24.2 COLON-ALIGNED WIDGET-ID 284
     cFromBin AT ROW 6.71 COL 24.2 COLON-ALIGNED WIDGET-ID 282
     cToWhs AT ROW 5.52 COL 53 COLON-ALIGNED WIDGET-ID 266
     cToBin AT ROW 6.71 COL 53 COLON-ALIGNED WIDGET-ID 270
     Btn_select-tag AT ROW 6.43 COL 73.8 WIDGET-ID 278
     Btn_OK AT ROW 23.67 COL 41.6
     Btn_Cancel AT ROW 23.67 COL 57.4
     BROWSE-1 AT ROW 9.19 COL 2.8        
     Btn_delete AT ROW 23.67 COL 80 WIDGET-ID 286
     "Parameters" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1.19 COL 5 WIDGET-ID 206
     "Bin List" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 8.29 COL 5 WIDGET-ID 264
     RECT-4 AT ROW 1.57 COL 2 WIDGET-ID 236
     RECT-5 AT ROW 8.62 COL 2 WIDGET-ID 262
     SPACE(1.39) SKIP(2.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Warehouse Multi Transaction Transfers"
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

/* SETTINGS FOR FILL-IN cUom IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN item-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rm-rctd WHERE tt-rm-rctd.Company = cocode ~
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Warehouse Multi Transaction Transfers */
DO:             
        DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
        lCheck = TRUE.
               
        IF lCheck THEN 
        DO:   
            EMPTY TEMP-TABLE tt-rm-rctd .              
        
            APPLY "END-ERROR":U TO SELF.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 D-Dialog
ON CHOOSE OF btnCalendar-1 IN FRAME D-Dialog
DO:
        {methods/btnCalendar.i dtTrandDate }
        APPLY "entry" TO dtTrandDate .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
        DEFINE VARIABLE lCheck AS LOGICAL NO-UNDO.
        lCheck = TRUE.
        IF lCheck THEN 
        DO:
            EMPTY TEMP-TABLE tt-rm-rctd .            
            APPLY "END-ERROR":U TO SELF.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_delete D-Dialog
ON CHOOSE OF Btn_delete IN FRAME D-Dialog /* Delete */
DO:
        DEFINE VARIABLE rwRowid AS ROWID NO-UNDO .
                
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        
        IF avail tt-rm-rctd THEN
           DELETE tt-rm-rctd.
           
        RUN repo-query(rwRowid).          
   
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
        FOR EACH tt-rm-rctd NO-LOCK :
            iCount = iCount + 1.
        END.
        IF iCount EQ 0 THEN
        DO:
             MESSAGE "Please select a bin..." VIEW-AS ALERT-BOX INFO.
             APPLY "ENTRY" TO cItemNo .
             RETURN NO-APPLY .
        END.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        RUN valid-loc2(INPUT cToWhs,INPUT 2,OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
        
        RUN valid-loc-bin2(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
                
        RUN valid-rmitem(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
               
        FOR EACH tt-rm-rctd:
            IF tt-rm-rctd.loc EQ cToWhs AND tt-rm-rctd.loc-bin EQ cToBin THEN
            DO:                 
                MESSAGE "To Whse/Bin may not be the same as From Whse/Bin" VIEW-AS ALERT-BOX INFO.
                DELETE tt-rm-rctd.                                    
            END.
        END.
        
        FOR EACH tt-rm-rctd:
        ASSIGN
           tt-rm-rctd.rct-date = dtTrandDate
           tt-rm-rctd.i-name = item-name              
           tt-rm-rctd.pur-uom   = cUom
           tt-rm-rctd.loc2      = cToWhs
           tt-rm-rctd.loc-bin2  = cToBin .          
        END.
                  
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_select-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_select-tag D-Dialog
ON CHOOSE OF Btn_select-tag IN FRAME D-Dialog /* Select Bins */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        DEFINE VARIABLE lv-rowid   AS ROWID     NO-UNDO.
        DEFINE VARIABLE save-rowid AS ROWID     NO-UNDO.
        DEFINE VARIABLE save-focus AS CHARACTER NO-UNDO.
        DEFINE VARIABLE char-hdl   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-error   AS LOG       NO-UNDO.
        DEFINE VARIABLE lMessageCheck AS LOGICAL NO-UNDO.
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.        

        RUN valid-rmitem(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
        
        RUN valid-loc2(INPUT cToWhs,INPUT 2,OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
        
        RUN valid-loc-bin2(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.


        DO WITH FRAME {&FRAME-NAME}:
        RUN windows/l-rmibn2.w (cocode, cItemNo , cFromWhs , cFromBin , "",0, OUTPUT lv-rowid).
        EMPTY TEMP-TABLE tt-rm-rctd.
            MAIN-LOOP:
            FOR EACH tt-selected ,
                FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid 
                             AND rm-bin.qty GT 0:
                             
                 IF rm-bin.loc EQ cToWhs AND rm-bin.loc-bin EQ cToBin THEN
                 DO:
                      IF NOT lMessageCheck THEN
                      MESSAGE "To Whse/Bin may not be the same as From Whse/Bin" VIEW-AS ALERT-BOX INFO.
                      DELETE tt-selected.
                      lMessageCheck = TRUE .
                      NEXT MAIN-LOOP.                     
                 END.
                
                CREATE tt-rm-rctd.
                ASSIGN 
                 tt-rm-rctd.i-no = cItemNo 
                 tt-rm-rctd.loc = rm-bin.loc
                 tt-rm-rctd.loc-bin = rm-bin.loc-bin
                 tt-rm-rctd.tag = rm-bin.tag 
                 tt-rm-rctd.tag2 = rm-bin.tag
                 tt-rm-rctd.qty = rm-bin.qty                 
                 tt-rm-rctd.rct-date = dtTrandDate
                 tt-rm-rctd.i-name = item-name               
                 tt-rm-rctd.pur-uom   = cUom
                 tt-rm-rctd.loc2      = cToWhs
                 tt-rm-rctd.loc-bin2  = cToBin
                 tt-rm-rctd.USER-ID   = USERID(LDBNAME(1))         
                 .           
                 DELETE tt-selected.
            END.
        END.
          
        RUN repo-query(ROWID(tt-rm-rctd)).
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFromBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFromBin D-Dialog
ON HELP OF cFromBin IN FRAME D-Dialog /* From Bin */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        
        RUN windows/l-locbin.w (gcompany,cFromWhs:screen-value, "", OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                cFromBin:screen-value      = ENTRY(1,char-val)   . 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFromBin D-Dialog
ON LEAVE OF cFromBin IN FRAME D-Dialog /* From Bin */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.               
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFromWhs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFromWhs D-Dialog
ON HELP OF cFromWhs IN FRAME D-Dialog /* From Whse */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-loc.w (gcompany, cFromWhs:screen-value, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                cFromWhs:screen-value      = ENTRY(1,char-val) . 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFromWhs D-Dialog
ON LEAVE OF cFromWhs IN FRAME D-Dialog /* From Whse */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-loc2(INPUT cFromWhs, INPUT 1, OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemNo D-Dialog
ON HELP OF cItemNo IN FRAME D-Dialog /* Item No */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
                
        RUN windows/l-itmre.w (gcompany, "", "", "R", FOCUS:SCREEN-VALUE , OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value = ENTRY(1,char-val) .
        FIND FIRST ITEM WHERE RECID(ITEM) = look-recid NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN                 
                item-name:SCREEN-VALUE = ITEM.i-name
                cUom:SCREEN-VALUE = ITEM.cons-uom.
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemNo D-Dialog
ON LEAVE OF cItemNo IN FRAME D-Dialog /* Item No */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-rmitem (OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemNo D-Dialog
ON VALUE-CHANGED OF cItemNo IN FRAME D-Dialog /* Item No */
DO:
        FIND FIRST ITEM NO-LOCK
            WHERE ITEM.company = cocode
            AND ITEM.i-no EQ cItemNo:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN
                
                item-name:SCREEN-VALUE = ITEM.i-name
                cUom:SCREEN-VALUE = ITEM.cons-uom
                 .
        ASSIGN 
            lCreateNewFG = FALSE .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cToBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cToBin D-Dialog
ON HELP OF cToBin IN FRAME D-Dialog /* To Bin */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        
        RUN windows/l-locbin.w (gcompany,cToWhs:screen-value, "", OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                cToBin:screen-value      = ENTRY(1,char-val)   . 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cToBin D-Dialog
ON LEAVE OF cToBin IN FRAME D-Dialog /* To Bin */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-loc-bin2(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cToWhs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cToWhs D-Dialog
ON HELP OF cToWhs IN FRAME D-Dialog /* To Whse */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-loc.w (gcompany, cToWhs:screen-value, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                cToWhs:screen-value      = ENTRY(1,char-val) . 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cToWhs D-Dialog
ON LEAVE OF cToWhs IN FRAME D-Dialog /* To Whse */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-loc2(INPUT cToWhs, INPUT 2,OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cUom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUom D-Dialog
ON HELP OF cUom IN FRAME D-Dialog /* Uom */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        /*RUN windows/l-cstprt.w (gcompany, "", FOCUS:SCREEN-VALUE, "", OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)
                item-name:screen-value = ENTRY(2,char-val)                  
                cItemNo:screen-value     = ENTRY(4,char-val) . */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUom D-Dialog
ON LEAVE OF cUom IN FRAME D-Dialog /* Uom */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           /* RUN valid-part-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY. */
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dtTrandDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dtTrandDate D-Dialog
ON HELP OF dtTrandDate IN FRAME D-Dialog /* Transfer Date */
DO:
        {methods/calpopup.i}
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
      dtTrandDate = TODAY .
      dtTrandDate:SCREEN-VALUE = string(TODAY).
      
      APPLY "entry" TO dtTrandDate IN FRAME {&FRAME-NAME}.              
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
  DISPLAY dtTrandDate cItemNo cUom item-name cToWhs cToBin cFromWhs cFromBin 
      WITH FRAME D-Dialog.
  ENABLE dtTrandDate btnCalendar-1 cItemNo cToWhs cToBin Btn_select-tag Btn_OK 
         Btn_Cancel BROWSE-1 cFromWhs cFromBin Btn_delete 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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
         
        OPEN QUERY BROWSE-1 FOR EACH tt-rm-rctd
            NO-LOCK BY tt-rm-rctd.tag.              

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
  {src/adm/template/snd-list.i "tt-rm-rctd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin2 D-Dialog 
PROCEDURE valid-loc-bin2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF lv-msg EQ "" THEN
            IF cToBin:SCREEN-VALUE  EQ "" THEN
                lv-msg = "To Bin may not be spaces".

        /*IF lv-msg EQ "" THEN
            IF rm-rctd.loc:SCREEN-VALUE       EQ
                rm-rctd.loc2:SCREEN-VALUE      AND
                rm-rctd.loc-bin:SCREEN-VALUE   EQ
                rm-rctd.loc-bin2:SCREEN-VALUE  THEN
                lv-msg = "To Whse/Bin may not be the same as From Whse/Bin". */

        IF lv-msg EQ "" THEN 
        DO:
            FIND FIRST rm-bin
                WHERE rm-bin.company EQ cocode
                AND rm-bin.i-no    EQ ""
                AND rm-bin.loc     EQ cToWhs:SCREEN-VALUE 
                AND rm-bin.loc-bin EQ cToBin:SCREEN-VALUE 
                USE-INDEX loc-bin NO-LOCK NO-ERROR.

            IF NOT AVAILABLE rm-bin THEN lv-msg = "Invalid entry, try help...".
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
            FIND FIRST loc
                WHERE loc.company EQ cocode
                AND loc.loc     EQ cToWhs:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
             IF avail loc THEN 
             APPLY "entry" TO cToBin .
             ELSE APPLY "entry" TO cToWhs . 
            oplOutError = YES.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc2 D-Dialog 
PROCEDURE valid-loc2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipcWhse AS CHARACTER NO-UNDO . 
   DEFINE INPUT PARAMETER ipiType AS INTEGER NO-UNDO .
   DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF ipiType EQ 1 AND ipcWhse  EQ "" THEN
        RETURN .
        
        IF lv-msg EQ "" THEN
            IF ipcWhse  EQ "" THEN
                lv-msg = "To Warehouse may not be spaces".

        IF lv-msg EQ "" THEN 
        DO:
            FIND FIRST loc
                WHERE loc.company EQ cocode
                AND loc.loc     EQ ipcWhse 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE loc THEN lv-msg = "Invalid entry, try help".
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
            IF ipiType EQ 1 THEN
            APPLY "entry" TO cFromWhs .
            ELSE APPLY "entry" TO cToWhs .
            oplOutError = YES.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rmitem D-Dialog 
PROCEDURE valid-rmitem :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
   DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ cItemNo:SCREEN-VALUE 
            AND ITEM.i-no    NE ""
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE item THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO cItemNo .
           oplOutError = YES.
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

