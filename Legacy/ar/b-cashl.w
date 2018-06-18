&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF BUFFER bf-cash FOR ar-cash.
DEF BUFFER bf-cashl FOR ar-cashl.
DEF BUFFER b-ar-inv FOR ar-inv.
DEF BUFFER ar-c-memo FOR reftable.

DEF VAR lv-account-recid AS RECID NO-UNDO.
DEF VAR account_dscr AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-unapp-amt AS DEC NO-UNDO.
DEF VAR lv-pay-amt AS DEC FORM "->>>,>>9.99" NO-UNDO.
DEF VAR v-overpay AS LOG NO-UNDO.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR lv-tot-pay AS DEC NO-UNDO.  /* line cash total pay amt */
DEF VAR lv-disc-calced AS LOG NO-UNDO.
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR ll-from-memo AS LOG NO-UNDO.

&SCOPED-DEFINE ARCASH ARCASH
&SCOPED-DEFINE where-ar-c-memo                                      ~
        WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"             ~
          AND ar-c-memo.company  EQ ar-cash.company                 ~
          AND ar-c-memo.loc      EQ ""

/*task 09090816 prevent stack trace box from coming up when hitting F1
  on invoice number*/
SESSION:DEBUG-ALERT = NO.

/* gdm - */
DEF VAR v-inv-bal AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ar-cash
&Scoped-define FIRST-EXTERNAL-TABLE ar-cash


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-cash.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-cashl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ar-cashl.inv-no ~
ar-cashl.inv-date ar-cashl.amt-due ar-cashl.amt-disc ~
calc-paid-amt(YES) @ lv-pay-amt calc-tot-app(YES) @ ar-cashl.amt-paid ~
ar-cashl.amt-paid calc-inv-bal(YES) @ v-inv-bal ar-cashl.actnum ~
display-account() @ account_dscr ar-cashl.rec_key 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-cashl.inv-no ~
ar-cashl.amt-disc ar-cashl.amt-paid ar-cashl.actnum 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ar-cashl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ar-cashl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ar-cashl OF ar-cash WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ar-cashl OF ar-cash WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ar-cashl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ar-cashl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-inv-bal B-table-Win 
FUNCTION calc-inv-bal RETURNS DECIMAL
    (ip-browser AS LOG) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-paid-amt B-table-Win 
FUNCTION calc-paid-amt RETURNS DECIMAL
  (ip-browser AS LOG) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-tot-app B-table-Win 
FUNCTION calc-tot-app RETURNS DECIMAL
  (ip-browser AS LOG) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-account B-table-Win 
FUNCTION display-account RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 87 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ar-cashl
    FIELDS(ar-cashl.inv-no
      ar-cashl.inv-date
      ar-cashl.amt-due
      ar-cashl.amt-disc
      ar-cashl.amt-paid
      ar-cashl.amt-paid
      ar-cashl.actnum
      ar-cashl.rec_key) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-cashl.inv-no FORMAT ">>>>>9":U WIDTH 15.2
      ar-cashl.inv-date COLUMN-LABEL "Invoice Date" FORMAT "99/99/9999":U
            WIDTH 17.2
      ar-cashl.amt-due COLUMN-LABEL "Balance Due" FORMAT "->>,>>>,>>9.99":U
            WIDTH 22.2
      ar-cashl.amt-disc FORMAT "->>,>>9.99":U WIDTH 19.2
      calc-paid-amt(YES) @ lv-pay-amt COLUMN-LABEL "Cash Payment"
      calc-tot-app(YES) @ ar-cashl.amt-paid
      ar-cashl.amt-paid COLUMN-LABEL "Total Applied" FORMAT "->>,>>>,>>9.99":U
            WIDTH 19.2
      calc-inv-bal(YES) @ v-inv-bal COLUMN-LABEL "Bal After Pymt" FORMAT "->>,>>>,>>9.99":U
      ar-cashl.actnum COLUMN-LABEL "Account Number" FORMAT "x(25)":U
            WIDTH 43
      display-account() @ account_dscr COLUMN-LABEL "Account Description"
      ar-cashl.rec_key FORMAT "X(20)":U
  ENABLE
      ar-cashl.inv-no
      ar-cashl.amt-disc
      ar-cashl.amt-paid
      ar-cashl.actnum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.76 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 10.76 COL 105 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 10.76 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.76 COL 2
     RECT-4 AT ROW 10.52 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.ar-cash
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10.95
         WIDTH              = 146.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       ar-cashl.rec_key:VISIBLE IN BROWSE Browser-Table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ar-cashl OF ASI.ar-cash"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > ASI.ar-cashl.inv-no
"ar-cashl.inv-no" ? ? "integer" ? ? ? ? ? ? yes ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-cashl.inv-date
"ar-cashl.inv-date" "Invoice Date" ? "date" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ar-cashl.amt-due
"ar-cashl.amt-due" "Balance Due" ? "decimal" ? ? ? ? ? ? no ? no no "22.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-cashl.amt-disc
"ar-cashl.amt-disc" ? ? "decimal" ? ? ? ? ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"calc-paid-amt(YES) @ lv-pay-amt" "Cash Payment" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"calc-tot-app(YES) @ ar-cashl.amt-paid" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.ar-cashl.amt-paid
"ar-cashl.amt-paid" "Total Applied" ? "decimal" ? ? ? ? ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"calc-inv-bal(YES) @ v-inv-bal" "Bal After Pymt" "->>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ar-cashl.actnum
"ar-cashl.actnum" "Account Number" ? "character" ? ? ? ? ? ? yes ? no no "43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"display-account() @ account_dscr" "Account Description" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.ar-cashl.rec_key
"ar-cashl.rec_key" ? ? "character" ? ? ? ? ? ? no "" no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  def var phandle as widget-handle no-undo.
  def var char-hdl as cha no-undo.   


  RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).

  IF NOT ll-inquiry THEN RUN new-state in phandle ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.
    DEF VAR lvSelection AS cha NO-UNDO.

    CASE FOCUS:NAME:
        WHEN "inv-no" THEN DO:
             RUN ar/d-arinvq.w (OUTPUT lvSelection).
             IF lvSelection = "Summary" THEN
                RUN ar/l-arinv.w (ar-cash.company,ar-cash.cust-no,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
             ELSE 
                RUN ar/l-arinvD.w (ar-cash.company,ar-cash.cust-no,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
             IF lk-recid <> ? THEN DO:
                RUN display-arinv (lk-recid).
             END.
             RETURN NO-APPLY.
        END.
        WHEN "actnum" THEN DO:
            RUN windows/l-acct3.w (ar-cash.company,"T",FOCUS:SCREEN-VALUE, OUTPUT char-val).
            IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                          account_dscr:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val).
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
  {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}

  ll-from-memo = AVAIL ar-cashl AND
                 CAN-FIND(FIRST ar-c-memo
                          {&where-ar-c-memo}
                            AND ar-c-memo.code EQ ar-cashl.rec_key).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF ar-cashl.inv-no:MODIFIED IN BROWSE {&browse-name} THEN DO:
       RELEASE bf-cashl.

       IF NOT ll-from-memo THEN
       FIND FIRST bf-cashl WHERE bf-cashl.c-no = ar-cashl.c-no
                             AND bf-cashl.inv-no = INPUT ar-cashl.inv-no
                             AND RECID(bf-cashl) <> RECID(ar-cashl)
                             AND NOT CAN-FIND(FIRST ar-c-memo
                                              {&where-ar-c-memo}
                                                AND ar-c-memo.code EQ bf-cashl.rec_key)
                             NO-LOCK NO-ERROR.
       IF AVAIL bf-cashl THEN DO:
           MESSAGE "Invoice already on Cash Receipt." SKIP
                   "Please Re-Enter Invoice Number." VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
       END.
       FIND FIRST ar-inv WHERE ar-inv.company = g_company 
                           AND ar-inv.posted
                           AND ar-inv.cust-no = ar-cash.cust-no
                           AND ar-inv.inv-no = INPUT ar-cashl.inv-no
                           AND (ar-inv.due <> 0 OR v-overpay)
                           NO-LOCK NO-ERROR.
       IF NOT AVAIL ar-inv THEN DO:
         /* MESSAGE "Invalid Invoice Number." VIEW-AS ALERT-BOX ERROR.          
        */
          ASSIGN ar-cashl.inv-no:SCREEN-VALUE = "0"
                 ar-cashl.amt-due:SCREEN-VALUE = "0"
                 ar-cashl.inv-date:SCREEN-VALUE = STRING(ar-cash.check-date)
                 ar-cashl.amt-paid:SCREEN-VALUE = STRING(INPUT ar-cashl.amt-paid -
                                                         INPUT ar-cashl.amt-disc)
                 ar-cashl.amt-disc:SCREEN-VALUE = "0".
          lv-pay-amt:SCREEN-VALUE = STRING(calc-paid-amt(NO)).   
          RETURN NO-APPLY.
       END.
       RUN display-arinv (recid(ar-inv)).                      
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.amt-disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-disc Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.amt-disc IN BROWSE Browser-Table /* Discount */
DO:
  IF ll-from-memo THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-disc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.amt-disc IN BROWSE Browser-Table /* Discount */
DO:
   IF LASTKEY = -1 THEN RETURN.
   IF ar-cashl.amt-disc:MODIFIED IN BROWSE {&browse-name} THEN DO:
      IF INPUT ar-cashl.inv-no = 0 AND INPUT ar-cashl.amt-disc <> 0 THEN DO:
         MESSAGE "Discount cannot be entered on account." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
          
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-disc Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ar-cashl.amt-disc IN BROWSE Browser-Table /* Discount */
DO:
  IF INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
    IF DEC(lv-pay-amt:SCREEN-VALUE IN BROWSE {&browse-name}) +
       DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) GT
       DEC(ar-cashl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name})  THEN DO:
      ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name} =
                       ar-cashl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name}.
      lv-pay-amt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(calc-paid-amt(NO)).
    END.

    ELSE
      ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name} =
            STRING(DEC(lv-pay-amt:SCREEN-VALUE IN BROWSE {&browse-name}) +
                   DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name})).
                   
       v-inv-bal:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(calc-inv-bal(NO)).
          
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.amt-paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-paid Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.amt-paid IN BROWSE Browser-Table /* Total Applied */
DO:
  IF ll-from-memo THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-paid Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.amt-paid IN BROWSE Browser-Table /* Total Applied */
DO:
   IF LASTKEY = -1 THEN RETURN.
   IF ar-cashl.amt-paid:MODIFIED IN BROWSE {&browse-name} THEN DO:      

      IF INPUT ar-cashl.inv-no = 0 AND INPUT ar-cashl.amt-paid LT 0 THEN DO:
         MESSAGE "Total Applied cannot be negative." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.

      ASSIGN 
          v-inv-bal:SCREEN-VALUE = STRING(calc-inv-bal(NO), "->>,>>>,>>9.99").
        
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-paid Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ar-cashl.amt-paid IN BROWSE Browser-Table /* Total Applied */
DO:
   lv-pay-amt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(calc-paid-amt(NO)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
  IF ll-from-memo THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF ar-cashl.actnum:MODIFIED IN BROWSE {&browse-name} THEN DO:
       FIND FIRST account WHERE account.company = g_company AND
                                account.TYPE <> "T" AND
                                account.actnum = ar-cashl.actnum:SCREEN-VALUE
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          MESSAGE "Invalid GL Account Number." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       account_dscr:SCREEN-VALUE = display-account() .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ar-cash"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-cash"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add B-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN auto-add IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-copy B-table-Win 
PROCEDURE auto-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN auto-copy IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-onaccount B-table-Win 
PROCEDURE create-onaccount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-all-applied AS LOG.
  DEF VAR z AS INT NO-UNDO.
  DEF VAR ld-unapplied AS DEC NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  
  FOR EACH bf-cashl NO-LOCK
      WHERE bf-cashl.c-no EQ ar-cash.c-no
        AND NOT CAN-FIND(FIRST ar-c-memo
                         {&where-ar-c-memo}
                           AND ar-c-memo.code EQ bf-cashl.rec_key):
    ld-unapplied = ld-unapplied + (bf-cashl.amt-paid - bf-cashl.amt-disc).
  END.
  ld-unapplied = ar-cash.check-amt - ld-unapplied.

  IF ld-unapplied GT 0 THEN
     MESSAGE "Put UNAPPLIED monies ON ACCOUNT?"
        VIEW-AS ALERT-BOX BUTTON YES-NO
        UPDATE choice.

  if choice then do:
     find first bf-cashl WHERE bf-cashl.c-no = ar-cash.c-no and
                bf-cashl.on-account = yes no-error.
     if not available bf-cashl then
     do:
        for each bf-cashl where bf-cashl.c-no = ar-cash.c-no
                        use-index c-no by bf-cashl.line descending:
            z = bf-cashl.line.
           leave.
        end.
        create bf-cashl.
        bf-cashl.line    = z + 1.
     end.

     find first ar-ctrl where ar-ctrl.company = cocode no-lock no-error.

     find first bank where bank.company = cocode and
                           bank.bank-code = ar-cash.bank-code no-lock no-error.
     if avail bank then
     do:
       find first account where account.company = cocode and
                                account.actnum  = bank.actnum
                          no-lock no-error.
         assign bf-cashl.actnum = bank.actnum.
     end.
     else
     do:
       if ar-cash.check-no > 90000000 then
             find first account where account.company = cocode and
                                   account.actnum  = ar-ctrl.sales 
                            no-lock no-error.
         else
         find first account where account.company = cocode and
                                   account.actnum  = ar-ctrl.cash-act 
                            no-lock no-error.
       if avail account then
         assign bf-cashl.actnum = account.actnum.
     end.

     ASSIGN
     bf-cashl.check-no = string(ar-cash.check-no,"9999999999")
     bf-cashl.on-account = yes
     bf-cashl.company = cocode
     bf-cashl.cust-no = ar-cash.cust-no
     bf-cashl.c-no    = ar-cash.c-no
     bf-cashl.inv-no  = 0
     bf-cashl.inv-date = today.

     if bf-cashl.amt-paid <> ld-unapplied then
        assign bf-cashl.amt-paid = bf-cashl.amt-paid + ld-unapplied.
     else
       assign bf-cashl.amt-paid = bf-cashl.amt-paid.
     
       op-all-applied = YES.
       RUN reopen-query.
       RETURN.
  END.
  op-all-applied = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-arinv B-table-Win 
PROCEDURE display-arinv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DEF VAR ld-net1 AS DEC NO-UNDO.
  DEF VAR ld-net2 AS DEC NO-UNDO.
  DEF VAR ld-due AS DEC NO-UNDO.


  FIND ar-inv WHERE RECID(ar-inv) = ip-recid NO-LOCK NO-ERROR.

  IF AVAIL ar-inv THEN DO:
    RUN get-unapp-amt.
    DISPLAY ar-inv.inv-no @ ar-cashl.inv-no 
            ar-inv.inv-date @ ar-cashl.inv-date
            ar-inv.due @ ar-cashl.amt-due
         WITH BROWSE {&browse-name}.
    IF NOT ll-from-memo THEN DO:
     IF ar-inv.disc-days <> 0 AND
        ar-inv.inv-date + ar-inv.disc-days >= ar-cash.check-date
     THEN DO:
        ld-net1 = 0.
        FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ ar-inv.x-no:
          ld-net1 = ld-net1 + ar-invl.amt.
        END.
        RUN custom/inv-dnet.p (ROWID(ar-inv), OUTPUT ld-net2).

        ld-due = (ar-inv.due -
                  (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0) - 
                  ar-inv.tax-amt) * ld-net2 / ld-net1.

        IF ld-due GT 0 THEN DO:
          DISPLAY ROUND(ld-due * (ar-inv.disc-% / 100),2) @ ar-cashl.amt-disc
              WITH BROWSE {&browse-name}.
          IF lv-unapp-amt < ar-inv.due - int(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
            ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
        END.
     END.
     IF lv-unapp-amt >= ar-inv.due - int(ar-cashl.amt-disc:SCREEN-VALUE) THEN
        DISP ar-inv.due @ ar-cashl.amt-paid WITH BROWSE {&browse-name}.
     ELSE DISP lv-unapp-amt @ ar-cashl.amt-paid
               0 @ ar-cashl.amt-disc WITH BROWSE {&browse-name}.
     lv-pay-amt = calc-paid-amt(NO).
     DISPLAY lv-pay-amt WITH BROWSE {&browse-name}.
    END.
  END.
  lv-inv-displayed = YES.
  account_dscr:SCREEN-VALUE = display-account().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-unapp-amt B-table-Win 
PROCEDURE get-unapp-amt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  lv-unapp-amt = ar-cash.check-amt.
  FOR EACH bf-cashl OF ar-cash NO-LOCK
      WHERE ROWID(bf-cashl) NE ROWID(ar-cashl)
        AND NOT CAN-FIND(FIRST ar-c-memo
                         {&where-ar-c-memo}
                           AND ar-c-memo.code EQ bf-cashl.rec_key):
    lv-unapp-amt = lv-unapp-amt - (bf-cashl.amt-paid - bf-cashl.amt-disc).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inquiry-mode B-table-Win 
PROCEDURE inquiry-mode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-inquiry = NO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"inquiry-source", OUTPUT char-hdl).

  DO WITH FRAME {&FRAME-NAME}:
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN ll-inquiry = YES.

    ELSE ENABLE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-canceled AS LOG NO-UNDO.
  DEF VAR lvSelection AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ar-cash.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No addings are allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.

  END.

  FIND FIRST bf-cashl OF ar-cash NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-cashl THEN DO:
     RUN ar/d-arinvq.w (OUTPUT lvSelection).
     IF lvSelection = "Summary" THEN
        RUN ar/d-selinv.w (RECID(ar-cash), NO, OUTPUT lv-canceled).
     ELSE
         RUN ar/d-selinvD.w (RECID(ar-cash), NO, OUTPUT lv-canceled).

     FIND FIRST bf-cashl OF ar-cash NO-LOCK NO-ERROR.
     IF AVAIL bf-cashl AND NOT lv-canceled THEN do:
         RUN reopen-query .
         RETURN.
     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-inv-date AS DATE NO-UNDO.
  DEF VAR lv-amt-due AS DEC NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN lv-inv-date = DATE(ar-cashl.inv-date:SCREEN-VALUE IN BROWSE {&browse-name})
         lv-amt-due = DEC(ar-cashl.amt-due:SCREEN-VALUE).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */  
  ASSIGN ar-cashl.inv-date = lv-inv-date
         ar-cashl.amt-due = lv-amt-due.
   IF ar-cashl.inv-no = 0 THEN ar-cashl.on-account = yes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-next-line AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH bf-cashl OF ar-cash NO-LOCK BY LINE DESCENDING:
      li-next-line = bf-cashl.LINE.
      LEAVE.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ar-cashl.company = ar-cash.company
         ar-cashl.c-no = ar-cash.c-no
         ar-cashl.LINE = li-next-line + 1
         ar-cashl.cust-no = ar-cash.cust-no
         ar-cashl.check-no = STRING(ar-cash.check-no,"9999999999")
         ar-cashl.inv-date = TODAY.

  find first ar-ctrl where ar-ctrl.company = ar-cash.company no-lock no-error.

  find first bank where bank.company = ar-cash.company and
                        bank.bank-code = ar-cash.bank-code no-lock no-error.
  if avail bank THEN do:
     find first account where account.company = ar-cash.company and
                              account.actnum  = bank.actnum no-lock no-error.
     assign ar-cashl.actnum = bank.actnum.
  end.
  ELSE do:
    if ar-cash.check-no ge 90000000 AND ar-cash.check-no le 99999999 
    THEN find first account where account.company = ar-cash.company and
                            account.actnum  = ar-ctrl.sales no-lock no-error.
    ELSE find first account where account.company = ar-cash.company and
                             account.actnum  = ar-ctrl.cash-act no-lock no-error.
    if avail account THEN assign ar-cashl.actnum = account.actnum.
  end.
  
  if available account then lv-account-recid = RECID(account). /* for displaying*/
  /*  display ar-cashl.actnum + " " + 
            account.dscr format "x(35)" @ ar-cashl.actnum.
  else
    display ar-cashl.actnum + " " @ ar-cashl.actnum.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL ar-cash AND ar-cash.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No deletion allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN redisplay-header.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ar-cash.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No updates are allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.

  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}.
  APPLY "cursor-left" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  adm-new-record = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"inquiryaq-source",OUTPUT char-hdl).

  IF AVAIL ar-cashl AND VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
        ar-cashl.amt-due:LABEL IN  BROWSE {&browse-name} = "Original Balance" .
  END.

  IF AVAIL ar-cashl THEN
  DO:
     /*set the rec_key so notes work properly*/
     {methods\template\local\setvalue.i} 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.

  
  /* Code placed here will execute PRIOR to standard behavior. */
  /*=-== validateion ==== */
  lv-rowid = ROWID(ar-cashl).

  RUN validate-line /*NO-ERROR*/.
  IF /*ERROR-STATUS:ERROR*/ RETURN-VALUE = "ValidationError" THEN DO:
    /*RUN dispatch ("display-fields").      */
    RETURN /*ERROR*/ .
  END.
  ELSE ll-new-record = adm-new-record.

 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
  RUN redisplay-header.

  IF ll-new-record THEN RUN auto-add.

  lv-disc-calced = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printInv B-table-Win 
PROCEDURE printInv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST b-ar-inv WHERE
        b-ar-inv.company EQ cocode AND
        b-ar-inv.cust-no EQ ar-cashl.cust-no AND
        b-ar-inv.inv-no  EQ ar-cashl.inv-no
        NO-LOCK NO-ERROR.
  
   IF AVAILABLE b-ar-inv THEN DO:
     RUN custom/setUserPrint.p (b-ar-inv.company,'ar-inv_.',
                                'begin_inv,end_inv,begin_cust,end_cust,tb_reprint,tb_posted',
                                STRING(b-ar-inv.inv-no) + ',' + STRING(b-ar-inv.inv-no) + ',' +
                                b-ar-inv.cust-no + ',' + b-ar-inv.cust-no + ',' +
                                STRING(b-ar-inv.printed) + ',' + STRING(b-ar-inv.posted)).

     RUN listobjs/ar-inv_.w.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redisplay-header B-table-Win 
PROCEDURE redisplay-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-detail B-table-Win 
PROCEDURE refresh-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch ("open-query").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch ("open-query").
  RUN redisplay-header.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ar-cash"}
  {src/adm/template/snd-list.i "ar-cashl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-added B-table-Win 
PROCEDURE undo-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF adm-new-record THEN RUN dispatch ("cancel-record").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-line B-table-Win 
PROCEDURE validate-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
     IF ar-cashl.inv-no:MODIFIED IN BROWSE {&browse-name} THEN DO:
        RELEASE bf-cashl.
       
        IF NOT ll-from-memo THEN
           FIND FIRST bf-cashl WHERE bf-cashl.c-no = ar-cashl.c-no
                                 AND bf-cashl.inv-no = INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name})
                                 AND RECID(bf-cashl) <> RECID(ar-cashl)
                                 AND NOT CAN-FIND(FIRST ar-c-memo
                                                  {&where-ar-c-memo}
                                                    AND ar-c-memo.code EQ bf-cashl.rec_key)
                NO-LOCK NO-ERROR.
        IF AVAIL bf-cashl THEN DO:
           MESSAGE "Invoice already on Cash Receipt." SKIP
                   "Please Re-Enter Invoice Number." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ar-cashl.inv-no.
           RETURN "ValidationERROR".
        END.
       
        FIND FIRST ar-inv WHERE ar-inv.company = g_company 
                            AND ar-inv.posted
                            AND ar-inv.cust-no = ar-cash.cust-no
                            AND ar-inv.inv-no = INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name})
                            AND (ar-inv.due <> 0 OR v-overpay)
                          NO-LOCK NO-ERROR.
        IF NOT AVAIL ar-inv THEN DO:
           ASSIGN ar-cashl.inv-no:SCREEN-VALUE = "0"
                  ar-cashl.amt-due:SCREEN-VALUE = "0"
                  ar-cashl.inv-date:SCREEN-VALUE = STRING(ar-cash.check-date)
                  ar-cashl.amt-paid:SCREEN-VALUE = STRING(INPUT ar-cashl.amt-paid -
                                                          INPUT ar-cashl.amt-disc)
                  ar-cashl.amt-disc:SCREEN-VALUE = "0"
                  lv-pay-amt:SCREEN-VALUE = STRING(calc-paid-amt(NO)).
           APPLY "entry" TO ar-cashl.inv-no.
           RETURN "ValidationERROR".
        END.
       
        IF NOT lv-inv-displayed THEN
           RUN display-arinv (recid(ar-inv)).

        IF ar-inv.due < INPUT ar-cashl.amt-paid THEN DO:
           DEF VAR choice AS LOG NO-UNDO.
           choice = v-overpay.
           IF choice THEN
              MESSAGE "Allow overpayment on invoice?" VIEW-AS ALERT-BOX QUESTION
                      BUTTON YES-NO UPDATE choice.
           
           IF NOT choice THEN DO:
              MESSAGE "Amount being applied may not exceed the invoice amount." VIEW-AS ALERT-BOX ERROR.
              ASSIGN ar-cashl.amt-paid:SCREEN-VALUE = STRING(ar-inv.due)
                     lv-pay-amt:SCREEN-VALUE = STRING(ar-inv.due).
           END.
        END.
        END.
       
        FIND FIRST account WHERE account.company = g_company AND
                                 account.TYPE <> "T" AND
                                 account.actnum = ar-cashl.actnum:SCREEN-VALUE
                             NO-LOCK NO-ERROR.
        IF NOT AVAIL account THEN DO:
           MESSAGE "Invalid GL Account Number." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ar-cashl.actnum IN BROWSE {&browse-name}.
           RETURN "ValidationERROR".
        END.
        account_dscr:SCREEN-VALUE = display-account() .                      
       
        IF INPUT ar-cashl.inv-no = 0 AND INPUT ar-cashl.amt-disc <> 0 THEN DO:
           MESSAGE "Discount can not be entered on account." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ar-cashl.amt-disc IN BROWSE {&browse-name}.
           RETURN "ValidationERROR".
        END.
       
        IF INPUT ar-cashl.amt-disc = 0 AND INPUT ar-cashl.amt-paid = 0 THEN DO:
           MESSAGE "Line Item Total must be greater than 0."
                VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
           RETURN "ValidationERROR".           
        END.
       
        IF NOT ll-from-memo THEN DO:
           lv-tot-pay = INPUT ar-cashl.amt-paid - INPUT ar-cashl.amt-disc .
           FOR EACH bf-cashl OF ar-cash NO-LOCK
               WHERE RECID(bf-cashl) <> RECID(ar-cashl)
                 AND NOT CAN-FIND(FIRST ar-c-memo
                                  {&where-ar-c-memo}
                                    AND ar-c-memo.code EQ bf-cashl.rec_key):
             lv-tot-pay = lv-tot-pay + bf-cashl.amt-paid - bf-cashl.amt-disc.
           END. 
          
           IF lv-tot-pay > ar-cash.check-amt THEN DO:
              MESSAGE "Amount being applied may not exceed the check amount..."
                 VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
              RETURN "ValidationERROR".
           END.
        END.
       
        IF INPUT ar-cashl.inv-no = 0 AND INPUT ar-cashl.amt-paid LT 0 THEN DO:
           MESSAGE "Total Applied cannot be negative." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
           RETURN "ValidationERROR".
        END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-inv-bal B-table-Win 
FUNCTION calc-inv-bal RETURNS DECIMAL
    (ip-browser AS LOG):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v-amt-due  LIKE ar-cashl.amt-due  NO-UNDO.
  DEF VAR v-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
  

  IF ip-browser AND AVAIL ar-cashl THEN
    ASSIGN
     v-amt-due  = ar-cashl.amt-due
     v-amt-paid = ar-cashl.amt-paid.
  ELSE
    ASSIGN
     v-amt-due  =  DEC(ar-cashl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name})
     v-amt-paid =  DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}).  

  /* Function return value. */
  RETURN IF AVAIL ar-cashl AND ar-cashl.inv-no NE 0 
           THEN v-amt-due - (IF ar-cash.posted THEN 0 ELSE v-amt-paid)
           ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-paid-amt B-table-Win 
FUNCTION calc-paid-amt RETURNS DECIMAL
  (ip-browser AS LOG):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
  DEF VAR ld-amt-disc LIKE ar-cashl.amt-disc NO-UNDO.


  IF ip-browser AND AVAIL ar-cashl THEN
    ASSIGN
     ld-amt-paid = ar-cashl.amt-paid
     ld-amt-disc = ar-cashl.amt-disc.
  ELSE
    ASSIGN
     ld-amt-paid = DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-amt-disc = DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}).

  /* Function return value. */
  RETURN ld-amt-paid - (IF ar-cash.posted THEN 0 ELSE ld-amt-disc).
         
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-tot-app B-table-Win 
FUNCTION calc-tot-app RETURNS DECIMAL
  (ip-browser AS LOG):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
  DEF VAR ld-amt-disc LIKE ar-cashl.amt-disc NO-UNDO.


  IF ip-browser AND AVAIL ar-cashl THEN
    ASSIGN
     ld-amt-paid = ar-cashl.amt-paid
     ld-amt-disc = ar-cashl.amt-disc.
  ELSE
    ASSIGN
     ld-amt-paid = DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-amt-disc = DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}).

  /* Function return value. */
  RETURN ld-amt-paid + (IF ar-cash.posted THEN ld-amt-disc ELSE 0).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-account B-table-Win 
FUNCTION display-account RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  IF lv-account-recid <> ? THEN DO:
       FIND account WHERE RECID(account) = lv-account-recid NO-LOCK NO-ERROR.
       IF AVAIL account THEN RETURN account.dscr.
       ELSE RETURN "".
  END.
  ELSE IF AVAIL ar-cashl THEN DO:
       FIND FIRST account WHERE account.company = g_company
                            AND account.actnum = ar-cashl.actnum NO-LOCK NO-ERROR.         
       IF AVAIL account THEN RETURN account.dscr.
       ELSE RETURN "". 
  END.
  RETURN "".
    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

