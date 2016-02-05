&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ar\b-onact1.w

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
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-app-amt AS DEC NO-UNDO.
DEF VAR lv-unapp-amt AS DEC NO-UNDO.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR lv-tmp-amt AS DEC NO-UNDO.
DEF VAR lv-new-recid AS RECID NO-UNDO.
DEF VAR ll-reapply AS LOG NO-UNDO.
DEF VAR hold-inv-no LIKE ar-cashl.inv-no NO-UNDO.

DEF BUFFER bf-cashl FOR ar-cashl.

DEF TEMP-TABLE tt-cashl LIKE ar-cashl.

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
ar-cashl.inv-date ar-cashl.amt-due ar-cashl.amt-paid ar-cashl.amt-disc ~
ar-cashl.on-account ar-cashl.memo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-cashl.inv-no ~
ar-cashl.inv-date ar-cashl.amt-due ar-cashl.amt-paid 
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



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 139 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ar-cashl
    FIELDS(ar-cashl.inv-no
      ar-cashl.inv-date
      ar-cashl.amt-due
      ar-cashl.amt-paid
      ar-cashl.amt-disc
      ar-cashl.on-account
      ar-cashl.memo) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-cashl.inv-no FORMAT ">>>>>9":U WIDTH 20.2
      ar-cashl.inv-date COLUMN-LABEL "Invoice Date" FORMAT "99/99/9999":U
            WIDTH 19.2
      ar-cashl.amt-due FORMAT "->>,>>>,>>9.99":U WIDTH 21
      ar-cashl.amt-paid COLUMN-LABEL "Applied" FORMAT "->>,>>>,>>9.99":U
            WIDTH 20.2
      ar-cashl.amt-disc FORMAT "->>,>>9.99":U WIDTH 18.2
      ar-cashl.on-account FORMAT "Yes/No":U
      ar-cashl.memo COLUMN-LABEL "" FORMAT "yes/no":U
  ENABLE
      ar-cashl.inv-no
      ar-cashl.inv-date
      ar-cashl.amt-due
      ar-cashl.amt-paid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 139 BY 8.33
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 9.57 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 9.57 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 9.57 COL 126 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 9.57 COL 2
     RECT-4 AT ROW 9.33 COL 1
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
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 9.76
         WIDTH              = 139.
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
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       ar-cashl.on-account:VISIBLE IN BROWSE Browser-Table = FALSE
       ar-cashl.memo:VISIBLE IN BROWSE Browser-Table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ar-cashl OF ASI.ar-cash"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   > ASI.ar-cashl.inv-no
"ar-cashl.inv-no" ? ? "integer" ? ? ? ? ? ? yes ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-cashl.inv-date
"ar-cashl.inv-date" "Invoice Date" ? "date" ? ? ? ? ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ar-cashl.amt-due
"ar-cashl.amt-due" ? ? "decimal" ? ? ? ? ? ? yes ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-cashl.amt-paid
"ar-cashl.amt-paid" "Applied" ? "decimal" ? ? ? ? ? ? yes ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ar-cashl.amt-disc
"ar-cashl.amt-disc" ? ? "decimal" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ar-cashl.on-account
"ar-cashl.on-account" ? ? "logical" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.ar-cashl.memo
"ar-cashl.memo" "" ? "logical" ? ? ? ? ? ? no "" no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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

    RUN new-state in phandle ('update-begin':U).
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

  lv-inv-displayed = NO.
  IF INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:  
    /*ar-cashl.amt-paid:READ-ONLY = YES.*/
    lv-inv-displayed = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /* {src/adm/template/brsleave.i} */
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
  IF NOT ar-cashl.on-account THEN DO:
    IF NOT ll-reapply THEN
      MESSAGE "Do you wish to reapply this check/memo?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-reapply.

    IF NOT ll-reapply THEN RUN dispatch ("cancel-record").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-inv-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ar-cashl.memo THEN RUN dispatch ("update-record").
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
ON ENTRY OF ar-cashl.amt-due IN BROWSE Browser-Table /* Balance */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cashl.amt-paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cashl.amt-paid Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ar-cashl.amt-paid IN BROWSE Browser-Table /* Applied */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-amt-paid NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-onaccount B-table-Win 
PROCEDURE check-onaccount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  lv-tmp-amt = 0.

  FOR EACH bf-cashl OF ar-cash NO-LOCK:
    lv-tmp-amt = lv-tmp-amt + bf-cashl.amt-paid.
  END.

  IF ar-cash.check-amt NE lv-tmp-amt THEN DO:
    RUN create-onaccount.
    
    RUN display-header.

    RUN dispatch ("open-query").
  END.

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
  DEF VAR li-next-line AS INT NO-UNDO.


  lv-tmp-amt = 0.
  FOR EACH bf-cashl OF ar-cash WHERE NOT ar-cashl.memo NO-LOCK:
    lv-tmp-amt = lv-tmp-amt + bf-cashl.amt-paid.
  END.

  IF ar-cash.check-amt NE lv-tmp-amt THEN DO:
    FIND FIRST bf-cashl OF ar-cash WHERE bf-cashl.inv-no EQ 0 EXCLUSIVE NO-ERROR.  
    IF NOT AVAIL bf-cashl THEN do:
      FOR EACH bf-cashl OF ar-cash NO-LOCK BY bf-cashl.LINE DESCENDING:
        li-next-line = bf-cashl.LINE.
        LEAVE.
      END.

      CREATE bf-cashl.
      BUFFER-COPY ar-cashl EXCEPT rec_key TO bf-cashl
      ASSIGN
       bf-cashl.LINE     = li-next-line + 1
       bf-cashl.inv-no   = 0
       bf-cashl.amt-paid = 0
       bf-cashl.amt-disc = 0
       bf-cashl.amt-due  = 0.

      find first ar-ctrl where ar-ctrl.company = ar-cash.company no-lock no-error.

      find first bank where bank.company = ar-cash.company and
                             bank.bank-code = ar-cash.bank-code no-lock no-error.
      if avail bank THEN do:
        find first account where account.company = ar-cash.company and
                                   account.actnum  = bank.actnum no-lock no-error.
        assign bf-cashl.actnum = bank.actnum.
      end.
      ELSE do:
        if ar-cash.check-no ge 90000000 AND ar-cash.check-no le 99999999 
        THEN find first account where account.company = ar-cash.company and
                                 account.actnum  = ar-ctrl.sales no-lock no-error.
        ELSE find first account where account.company = ar-cash.company and
                                  account.actnum  = ar-ctrl.cash-act no-lock no-error.
        if avail account THEN assign bf-cashl.actnum = account.actnum.
      end.
    END.
  
    bf-cashl.amt-paid = bf-cashl.amt-paid + (ar-cash.check-amt - lv-tmp-amt).
  END.

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
  DEF VAR ld-tmp-amt AS DEC NO-UNDO.
  DEF VAR ld-tmp-unapp AS DEC NO-UNDO.

  ld-tmp-amt = 0.
  FOR EACH bf-cashl OF ar-cash NO-LOCK:
     IF AVAIL ar-cashl AND RECID(bf-cashl) = RECID(ar-cashl) THEN NEXT.
     ld-tmp-amt = ld-tmp-amt + bf-cashl.amt-paid.
  END.
  ld-tmp-unapp = ar-cash.check-amt - ld-tmp-amt.

  FIND ar-inv WHERE RECID(ar-inv) = ip-recid NO-LOCK NO-ERROR.
  IF AVAIL ar-inv THEN DO:
     DISPLAY ar-inv.inv-no @ ar-cashl.inv-no 
             ar-inv.inv-date @ ar-cashl.inv-date
             ar-inv.due @ ar-cashl.amt-due
              WITH BROWSE {&browse-name}.
/*     IF ar-inv.disc-days <> 0 AND
        ar-inv.inv-date + ar-inv.disc-days >= ar-cash.check-date
     THEN DO:
        IF ar-inv.f-bill THEN  
           DISP ROUND ((ar-inv.due - ar-inv.freight - ar-inv.tax-amt) * 
                      (ar-inv.disc-% / 100) ,2) @ ar-cashl.amt-disc
                    WITH BROWSE {&browse-name}.
        ELSE DISP ROUND((ar-inv.due - ar-inv.tax-amt) * 
                       (ar-inv.disc-% / 100) ,2) @ ar-cashl.amt-disc WITH BROWSE {&browse-name}.
        IF ld-tmp-unapp < ar-inv.due - int(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
           ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name} = "0".
     END.*/
     IF (ar-cash.check-amt GT 0 AND
         ld-tmp-unapp GE ar-inv.due - DEC(ar-cashl.amt-disc:SCREEN-VALUE)) OR
        (ar-cash.check-amt LT 0 AND
         ld-tmp-unapp LE ar-inv.due - DEC(ar-cashl.amt-disc:SCREEN-VALUE)) THEN
        DISP ar-inv.due - dec(ar-cashl.amt-disc:SCREEN-VALUE)
                 @ ar-cashl.amt-paid WITH BROWSE {&browse-name}.
     ELSE
     IF (ar-cash.check-amt GT 0 AND
         ld-tmp-unapp LT DEC(ar-cashl.amt-paid:SCREEN-VALUE)) OR
        (ar-cash.check-amt LT 0 AND
         ld-tmp-unapp GT DEC(ar-cashl.amt-paid:SCREEN-VALUE)) THEN
        DISP ld-tmp-unapp @ ar-cashl.amt-paid
             0 @ ar-cashl.amt-disc WITH BROWSE {&browse-name}.
  END.
  lv-inv-displayed = YES.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-header B-table-Win 
PROCEDURE display-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("display-fields") .
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

  /* Do not delete this procedure */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-amt-due AS DEC NO-UNDO.
  DEF VAR ld-amt-disc AS DEC NO-UNDO.
  DEF VAR ld-prev-paid AS DEC NO-UNDO.
  DEF VAR ld-prev-disc AS DEC NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   ld-amt-due   = DEC(ar-cashl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name})
   ld-amt-disc  = DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name})
   ld-prev-paid = IF ar-cashl.inv-no NE 0 THEN ar-cashl.amt-paid ELSE 0
   ld-prev-disc = IF ar-cashl.inv-no NE 0 THEN ar-cashl.amt-disc ELSE 0.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF tt-cashl.inv-no NE ar-cashl.inv-no AND
     NOT ar-cashl.on-account            THEN DO:
    {ar/ar-oreg.i tt-cashl -1}
    ar-cashl.on-account = YES.
  END.

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
      li-next-line = ar-cashl.LINE.
      LEAVE.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ar-cashl.company = ar-cash.company
         ar-cashl.c-no = ar-cash.c-no
         ar-cashl.LINE = li-next-line + 1
         ar-cashl.cust-no = ar-cash.cust-no
         ar-cashl.check-no = STRING(ar-cash.check-no,"99999999")
         ar-cashl.inv-date = TODAY
         ar-cashl.amt-paid = ar-cash.check-amt - lv-tmp-amt
         ar-cashl.inv-no = 0.

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
  
  /*if available account then lv-account-recid = RECID(account). /* for displaying*/
    display ar-cashl.actnum + " " + 
            account.dscr format "x(35)" @ ar-cashl.actnum.
  else
    display ar-cashl.actnum + " " @ ar-cashl.actnum.
  */
  lv-new-recid = RECID(ar-cashl).

  DISPLAY ar-cashl.inv-no ar-cashl.inv-date
          ar-cashl.amt-paid WITH BROWSE {&browse-name}.


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
  IF ar-cashl.inv-no EQ 0 OR ar-cashl.memo THEN RETURN.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN check-onaccount.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  IF NOT AVAIL ar-cashl AND lv-new-recid <> ? THEN
         FIND ar-cashl WHERE RECID(ar-cashl) = lv-new-recid NO-LOCK NO-ERROR.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  ar-cashl.amt-paid:READ-ONLY IN BROWSE {&browse-name} = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}.
  
  ll-reapply = NO.

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
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "value-changed" TO browse-order.
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
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li-next-line AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-inv-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-amt-paid NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FOR EACH tt-cashl:
    DELETE tt-cashl.
  END.
  CREATE tt-cashl.
  BUFFER-COPY ar-cashl TO tt-cashl.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  SESSION:SET-WAIT-STATE("general").

  lv-rowid = ROWID(ar-cashl).

  RUN check-onaccount.

  RUN display-header.

  RUN repo-query (lv-rowid).

  SESSION:SET-WAIT-STATE("").

  ASSIGN adm-brs-in-update = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-header B-table-Win 
PROCEDURE reopen-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
  
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
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-amt-paid B-table-Win 
PROCEDURE valid-amt-paid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-onacct AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    /*IF ar-cash.check-amt GT 0 THEN
      IF DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) LE 0 THEN DO:
        APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
        MESSAGE "Paid amount must be positive..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.
      ELSE.
    ELSE
      IF DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) GE 0 THEN DO:
        APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
        MESSAGE "Paid amount must be negative..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.

    IF ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "0" THEN DO:
      find first ar-inv where ar-inv.company = ar-cashl.company and
                              ar-inv.posted  = yes and
                              ar-inv.cust-no = ar-cashl.cust-no and
                              ar-inv.inv-no  = input ar-cashl.inv-no
                  use-index posted no-error.
      IF AVAIL ar-inv THEN DO:
        IF DEC(ar-cashl.amt-paid:SCREEN-VALUE) > ar-inv.due  - dec(ar-cashl.amt-disc:SCREEN-VALUE)
         THEN DO:
            APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
            MESSAGE "Paid amount can not be greater than Due amount..."
                 VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
         END.         
      END.
    END.*/

    FIND FIRST bf-cashl OF ar-cash WHERE bf-cashl.inv-no = 0 NO-LOCK NO-ERROR.
    ld-onacct = IF AVAIL bf-cashl THEN bf-cashl.amt-paid ELSE 0.

    lv-app-amt = 0.
    FOR EACH bf-cashl OF ar-cash
        WHERE bf-cashl.inv-no NE 0
          AND ROWID(bf-cashl) NE ROWID(ar-cashl)
        NO-LOCK:
      lv-app-amt = lv-app-amt + bf-cashl.amt-paid - bf-cashl.amt-disc.
    END.
    lv-unapp-amt = ar-cash.check-amt - lv-app-amt.

    IF (lv-unapp-amt GE 0 AND ar-cash.check-amt GT 0 AND
        DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) - DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name})
                                                GT lv-unapp-amt) OR
       (ar-cash.check-amt LT 0 AND
        DEC(ar-cashl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) - DEC(ar-cashl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name})
                                                LT lv-unapp-amt) THEN DO:
      APPLY "entry" TO ar-cashl.amt-paid IN BROWSE {&browse-name}.
      MESSAGE "Total applied amount cannot be " +
              TRIM(STRING(ar-cash.check-amt GT 0,"greater/less")) +
              " than the check amount..." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-buttons B-table-Win 
PROCEDURE valid-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*add and delete not valid buttons*/
   DEF OUTPUT PARAMETER op-add-valid AS LOG NO-UNDO.
   DEF OUTPUT PARAMETER op-delete-valid AS LOG NO-UNDO.
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
  DEF VAR lv-inv-no LIKE ar-inv.inv-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-inv-no = INT(ar-cashl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND bf-cashl WHERE ROWID(bf-cashl) EQ ROWID(ar-cashl) NO-LOCK NO-ERROR.

    FIND FIRST ar-inv NO-LOCK
        WHERE ar-inv.company EQ bf-cashl.company
          AND ar-inv.posted  EQ YES
          AND ar-inv.cust-no EQ bf-cashl.cust-no
          AND ar-inv.inv-no  EQ lv-inv-no
        USE-INDEX posted NO-ERROR.
    IF NOT AVAIL ar-inv OR lv-inv-no EQ 0 THEN DO:
      MESSAGE "Invalid " +
              TRIM(ar-cashl.inv-no:LABEL IN BROWSE {&browse-name}) +
              ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO ar-cashl.inv-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    IF NOT lv-inv-displayed THEN RUN display-arinv (RECID(ar-inv)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

