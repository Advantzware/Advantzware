&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ar\b-dbcr2.w

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

DEF BUFFER bf-cashl FOR ar-cashl.
DEF BUFFER b-ar-inv FOR ar-inv.
DEF BUFFER ar-cashl-inv-line FOR reftable.

DEF VAR lv-account-recid AS RECID NO-UNDO.
DEF VAR account_dscr AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR ll-new-record AS LOG NO-UNDO.
DEF VAR ll-is-a-return AS LOG NO-UNDO.
DEF VAR v-armemo-log AS LOG NO-UNDO.

find first sys-ctrl where
     sys-ctrl.company eq cocode and
     sys-ctrl.name    eq "ARMEMO"
     no-lock no-error.

if not avail sys-ctrl then
do transaction:
   create sys-ctrl.
   assign
       sys-ctrl.company = cocode
       sys-ctrl.name    = "ARMEMO"
       sys-ctrl.descrip = "Credit/Debit Memo Form".
   message "System control record NOT found.  Which Memo format should print?"
     update sys-ctrl.char-fld.
end.

v-armemo-log = sys-ctrl.log-fld.

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
ar-cashl.inv-date ar-cashl.amt-due ar-cashl.amt-disc ar-cashl.amt-paid ~
ar-cashl.actnum display-account() @ account_dscr ar-cashl.dscr ~
ar-cashl.rec_key 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-cashl.inv-no ~
ar-cashl.inv-date ar-cashl.amt-due ar-cashl.amt-disc ar-cashl.amt-paid ~
ar-cashl.actnum ar-cashl.dscr 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ar-cashl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ar-cashl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ar-cashl WHERE ar-cashl.c-no eq ar-cash.c-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ar-cashl WHERE ar-cashl.c-no eq ar-cash.c-no NO-LOCK ~
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
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

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
      ar-cashl.actnum
      ar-cashl.dscr
      ar-cashl.rec_key) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-cashl.inv-no FORMAT ">>>>>9":U WIDTH 14.2
      ar-cashl.inv-date COLUMN-LABEL "Invoice Date" FORMAT "99/99/9999":U
            WIDTH 18.2
      ar-cashl.amt-due COLUMN-LABEL "Balance Due" FORMAT "->>,>>>,>>9.99":U
            WIDTH 18.2
      ar-cashl.amt-disc FORMAT "->>,>>9.99":U WIDTH 19.2
      ar-cashl.amt-paid COLUMN-LABEL "Total Applied" FORMAT "->>,>>>,>>9.99":U
            WIDTH 19.2
      ar-cashl.actnum COLUMN-LABEL "Account Number" FORMAT "x(25)":U
      display-account() @ account_dscr COLUMN-LABEL "Account Description"
      ar-cashl.dscr COLUMN-LABEL "Memo Description" FORMAT "x(45)":U
      ar-cashl.rec_key COLUMN-LABEL "" FORMAT "X(20)":U
  ENABLE
      ar-cashl.inv-no
      ar-cashl.inv-date
      ar-cashl.amt-due
      ar-cashl.amt-disc
      ar-cashl.amt-paid
      ar-cashl.actnum
      ar-cashl.dscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 8.57
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 9.81 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 9.81 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 9.81 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 9.81 COL 2
     RECT-4 AT ROW 9.57 COL 1
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
         HEIGHT             = 10
         WIDTH              = 145.
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
     _TblList          = "ASI.ar-cashl WHERE ASI.ar-cash ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "ar-cashl.c-no eq ar-cash.c-no"
     _FldNameList[1]   > ASI.ar-cashl.inv-no
"ar-cashl.inv-no" ? ? "integer" ? ? ? ? ? ? yes ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-cashl.inv-date
"ar-cashl.inv-date" "Invoice Date" ? "date" ? ? ? ? ? ? yes ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ar-cashl.amt-due
"ar-cashl.amt-due" "Balance Due" ? "decimal" ? ? ? ? ? ? yes ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-cashl.amt-disc
"ar-cashl.amt-disc" ? ? "decimal" ? ? ? ? ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ar-cashl.amt-paid
"ar-cashl.amt-paid" "Total Applied" ? "decimal" ? ? ? ? ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ar-cashl.actnum
"ar-cashl.actnum" "Account Number" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"display-account() @ account_dscr" "Account Description" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ar-cashl.dscr
"ar-cashl.dscr" "Memo Description" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.ar-cashl.rec_key
"ar-cashl.rec_key" "" ? "character" ? ? ? ? ? ? no "" no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
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

    CASE FOCUS:NAME:
        WHEN "inv-no" THEN DO:
             RUN ar/l-arinv.w (ar-cash.company,ar-cash.cust-no,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
             IF lk-recid <> ? THEN DO:
                RUN display-arinv (lk-recid).
             END.
             RETURN NO-APPLY.
        END.
        WHEN "actnum" THEN DO:
            RUN windows/l-acct3.w (ar-cash.company,"T",FOCUS:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE THEN DO:
              FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
              RUN new-actnum.
            END.
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
  IF ar-cashl.dscr:SCREEN-VALUE IN BROWSE {&browse-name}
      BEGINS "CREDIT MEMO CREATED FROM OE RETURN" THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
  DEF VAR v-old-inv-no AS INT NO-UNDO.
  DEF VAR v-old-bal-due AS CHAR NO-UNDO.
  DEF VAR v-old-inv-date AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
  
     IF LASTKEY NE -1 THEN DO:
        RUN valid-inv-no (FOCUS) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
       
        IF v-armemo-log AND INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
           NOT ll-is-a-return THEN DO:

           ASSIGN
              v-old-inv-no = INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name})
              v-old-inv-date = ar-cashl.inv-date:SCREEN-VALUE IN BROWSE {&browse-name}
              v-old-bal-due = ar-cashl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name}.

           RUN ar/d-selinl.w (v-old-inv-no,
                             ROWID(ar-cashl)).
         
           RUN dispatch ("display-fields").

           /*when hitting cancel from d-selinl.w, was blanking out invoice and bal due*/
           IF INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
              ASSIGN ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-old-inv-no)
                     ar-cashl.inv-date:SCREEN-VALUE IN BROWSE {&browse-name} = v-old-inv-date
                     ar-cashl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name} = v-old-bal-due.
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ar-cashl.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
  lv-inv-displayed = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.inv-date Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.inv-date IN BROWSE Browser-Table /* Invoice Date */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.amt-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-due Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.amt-due IN BROWSE Browser-Table /* Balance Due */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.amt-disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-disc Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.amt-disc IN BROWSE Browser-Table /* Discount */
DO:
  IF NOT ll-is-a-return THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-disc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.amt-disc IN BROWSE Browser-Table /* Discount */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-amt-disc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.amt-paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-paid Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ar-cashl.amt-paid IN BROWSE Browser-Table /* Total Applied */
DO:
  IF ll-is-a-return                                                    OR
     DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-paid Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.amt-paid IN BROWSE Browser-Table /* Total Applied */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-amt-paid NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-inv-act NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ar-cashl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
  RUN new-actnum.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

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


  FIND ar-inv WHERE RECID(ar-inv) EQ ip-recid NO-LOCK NO-ERROR.

  IF AVAIL ar-inv THEN
    DISPLAY ar-inv.inv-no   @ ar-cashl.inv-no 
            ar-inv.inv-date @ ar-cashl.inv-date
            ar-inv.due      @ ar-cashl.amt-due
        WITH BROWSE {&browse-name}.

  ELSE
    DISPLAY 0                  @ ar-cashl.inv-no 
            ar-cash.check-date @ ar-cashl.inv-date
            0                  @ ar-cashl.amt-due
        WITH BROWSE {&browse-name}.

  ASSIGN
   lv-inv-displayed          = YES
   account_dscr:SCREEN-VALUE = display-account().

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

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"inquiry-source", OUTPUT char-hdl).

  ll-inquiry = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

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

  /* Code placed here will execute PRIOR to standard behavior. */

  IF ar-cash.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No addings are allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.

  END.

  FIND FIRST bf-cashl OF ar-cash NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-cashl THEN DO:
    RUN ar/d-selinv.w (RECID(ar-cash), YES, OUTPUT lv-canceled).  /* db/cr memo = yes , cash = no */
    IF NOT lv-canceled THEN DO:
      RUN reopen-query.
      RETURN.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-new-record = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-amt AS DEC NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ld-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-is-a-return THEN RUN recalc-check-amt.

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
  RUN reset-adm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-new-record = YES.

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
  DEF VAR lv-dscr LIKE ar-cashl.dscr NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH bf-cashl OF ar-cash NO-LOCK BY bf-cashl.LINE DESCENDING:
    ASSIGN
     li-next-line = bf-cashl.LINE
     lv-dscr      = bf-cashl.dscr.
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
         ar-cashl.dscr = lv-dscr
         ar-cashl.memo = YES.

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
  IF ar-cash.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No deletion allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF NOT ll-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-is-a-return THEN RUN recalc-check-amt.

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
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-is-a-return = NO.

  IF AVAIL ar-cashl THEN DO:

     /*set the rec_key so notes work properly*/
     {methods\template\local\setvalue.i} 

     RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).
     ll-is-a-return = AVAIL reftable OR ar-cashl.dscr MATCHES "*oe return*".
  END.
 IF AVAIL ar-cash AND ar-cash.memo AND AVAIL ar-cashl and ar-cashl.amt-disc gt 0 THEN do: 
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"disableadd-target",OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
         RUN add-return-proc IN WIDGET-HANDLE(char-hdl)(YES).
 END.
 ELSE DO:
 RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"disableadd-target",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
          RUN add-return-proc IN WIDGET-HANDLE(char-hdl)(NO).
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
  DEF VAR lv-tot-pay AS DEC NO-UNDO.
  DEF VAR choice AS LOG INIT YES NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-inv-no (ar-cashl.inv-no:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN valid-amt-paid NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-amt-disc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-inv-act NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "You have entered a"
            STRING(DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0,"DEBIT/CREDIT")
            "MEMO. Is this correct?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.             

    IF NOT choice THEN DO:
      APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}. 
      RETURN NO-APPLY.
    END.

    /*IF ar-cashl.inv-no:MODIFIED IN BROWSE {&browse-name}   OR
       ar-cashl.amt-paid:MODIFIED IN BROWSE {&browse-name} THEN DO:

      IF INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
         MESSAGE "On Account MEMO not allowed, please re-enter." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}.
         RETURN NO-APPLY.
      END.

      IF INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0   AND
         DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN DO:
        MESSAGE "On Account DEBIT MEMO not allowed, please re-enter..."
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
      END.

      ELSE
      IF INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0   AND
         DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) LT 0 THEN DO:
        MESSAGE "On Account CREDIT MEMO not allowed, please re-enter..."
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
      END. 
    END.*/
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-rowid = ROWID(ar-cashl).

  RUN redisplay-header.

  RUN repo-query (lv-rowid).

  IF ll-new-record THEN RUN auto-add.
  ELSE RUN reset-adm.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-actnum B-table-Win 
PROCEDURE new-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        lv-account-recid = ?.
    
     FIND FIRST account WHERE
          account.company EQ g_company AND
          account.actnum  EQ ar-cashl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
    
     account_dscr:SCREEN-VALUE IN BROWSE {&browse-name} = IF AVAIL account THEN account.dscr ELSE "".
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-check-amt B-table-Win 
PROCEDURE recalc-check-amt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND CURRENT ar-cash NO-ERROR.

  IF AVAIL ar-cash THEN DO:
    ar-cash.check-amt = 0.
        
    FOR EACH bf-cashl WHERE bf-cashl.c-no EQ ar-cash.c-no NO-LOCK:
      ar-cash.check-amt = ar-cash.check-amt +
                          (bf-cashl.amt-paid + bf-cashl.amt-disc).
    END.
  END.

  FIND CURRENT ar-cash NO-LOCK NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN redisplay-header.
  RUN dispatch ("open-query").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-adm B-table-Win 
PROCEDURE reset-adm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
   ll-new-record = NO
   adm-adding-record = NO
   adm-updating-record = NO
   adm-new-record = NO.

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
  DEF BUFFER b-ar-cashl FOR ar-cashl.

  DEF VAR char-hdl AS cha NO-UNDO.


  IF adm-new-record THEN RUN dispatch ("cancel-record").

  IF AVAIL ar-cash                                                    AND
     NOT CAN-FIND(FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no) THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"invhead-target", OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dispatch IN WIDGET-HANDLE(char-hdl) ("delete-record").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum B-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST account
                    WHERE account.company EQ g_company
                      AND account.actnum  EQ ar-cashl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND account.TYPE    NE "T") THEN DO:
      MESSAGE "Invalid GL Account Number" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ar-cashl.actnum.
      RETURN ERROR.
    END.                      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-amt-disc B-table-Win 
PROCEDURE valid-amt-disc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF ll-is-a-return THEN
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) LT 0 THEN DO:
      MESSAGE TRIM(ar-cashl.amt-disc:LABEL IN BROWSE {&browse-name}) +
              " may not be negative..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ar-cashl.amt-disc IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-amt-paid B-table-Win 
PROCEDURE valid-amt-paid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 AND
       DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "The CREDIT or DEBIT AMOUNT must be greater than 0..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-act B-table-Win 
PROCEDURE valid-inv-act :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG INIT YES NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-cashl
        WHERE bf-cashl.c-no   EQ ar-cash.c-no
          AND bf-cashl.inv-no EQ INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name})
          AND bf-cashl.actnum EQ ar-cashl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
          AND ROWID(bf-cashl) NE ROWID(ar-cashl)
        NO-LOCK NO-ERROR.

    IF AVAIL bf-cashl THEN DO:
      ll = FOCUS:NAME IN BROWSE {&browse-name} EQ "inv-no".
      MESSAGE "Invoice/Acct# already on Credit/Debit Memo."
             /* "re-enter Invoice?  (NO to re-enter Acct#)" */
              "Is this right line item to enter?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll. 
      
      IF ll THEN RETURN.        
      ELSE APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}. 
      RETURN ERROR.
    END.                       
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-no B-table-Win 
PROCEDURE valid-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RELEASE ar-inv.

    IF INT(ip-focus:SCREEN-VALUE) NE 0 THEN DO:
      FIND FIRST ar-inv NO-LOCK
          WHERE ar-inv.company EQ g_company 
            AND ar-inv.posted  EQ YES
            AND ar-inv.cust-no EQ ar-cash.cust-no
            AND ar-inv.inv-no  EQ INT(ip-focus:SCREEN-VALUE)
          NO-ERROR.
      IF NOT AVAIL ar-inv THEN DO:
        MESSAGE "Invalid Invoice Number, try help..." VIEW-AS ALERT-BOX ERROR.           
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END.
    END.

    IF NOT lv-inv-displayed THEN
      RUN display-arinv (IF AVAIL ar-inv THEN RECID(ar-inv) ELSE ?).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-account B-table-Win 
FUNCTION display-account RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RELEASE account.

  IF lv-account-recid NE ? THEN
  FIND account WHERE RECID(account) EQ lv-account-recid NO-LOCK NO-ERROR.

  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ (IF AVAIL ar-cashl THEN ar-cashl.actnum
                                  ELSE ar-cashl.actnum:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.         
  END.

  RETURN IF AVAIL account THEN account.dscr ELSE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

