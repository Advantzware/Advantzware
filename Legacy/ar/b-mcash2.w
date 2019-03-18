&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ar\b-mcash2.w

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
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEF BUFFER bf-mcash FOR ar-mcash.
DEF BUFFER bf-reftable FOR reftable.

DEF TEMP-TABLE ar-mcashl NO-UNDO LIKE ar-mcash
    FIELD acct-dscr LIKE account.dscr.

DEF VAR ll-new-record AS LOG NO-UNDO.
    
&SCOPED-DEFINE BRWSDEFS ar-mcashl

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
&Scoped-define EXTERNAL-TABLES ar-mcash
&Scoped-define FIRST-EXTERNAL-TABLE ar-mcash


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-mcash.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ar-mcashl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ar-mcashl.actnum ar-mcashl.acct-dscr ar-mcashl.check-amt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ar-mcashl.actnum ~
ar-mcashl.acct-dscr ~
ar-mcashl.check-amt   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ar-mcashl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ar-mcashl
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ar-mcashl WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH ar-mcashl WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ar-mcashl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ar-mcashl


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

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
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 54 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ar-mcashl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      ar-mcashl.actnum COLUMN-LABEL "Account#" FORMAT "x(25)":U
            WIDTH 32
      ar-mcashl.acct-dscr COLUMN-LABEL "Account Description" FORMAT "x(45)":U
            WIDTH 55
      ar-mcashl.check-amt COLUMN-LABEL "Account# Amt" FORMAT "->>,>>>,>>9.99":U
            WIDTH 20
  ENABLE
      ar-mcashl.actnum
      ar-mcashl.acct-dscr
      ar-mcashl.check-amt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 8.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 9.33 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 9.33 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 9.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 9.33 COL 2
     RECT-4 AT ROW 9.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.ar-mcash
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
         HEIGHT             = 9.67
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
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ar-mcashl WHERE ~{&KEY-PHRASE} ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Query            is OPENED
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
   /*{src/adm/template/brsleave.i}*/
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
DEF VAR lw-focus AS HANDLE NO-UNDO.
DEF VAR lv-char AS CHAR NO-UNDO.


ON HELP OF ar-mcashl.actnum IN BROWSE Browser-Table /* Reconciled */
DO:
  lw-focus = FOCUS.

  RUN windows/l-acct3.w (cocode, "T", lw-focus:SCREEN-VALUE, OUTPUT lv-char) NO-ERROR.
  IF lv-char NE "" AND ENTRY(1,lv-char) NE lw-focus:SCREEN-VALUE THEN DO:
    lw-focus:SCREEN-VALUE = ENTRY(1,lv-char).
    RUN new-actnum (lw-focus).
  END.
END.

ON LEAVE OF ar-mcashl.actnum IN BROWSE Browser-Table /* Reconciled */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-actnum (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.    
    
ON VALUE-CHANGED OF ar-mcashl.actnum IN BROWSE Browser-Table /* Reconciled */
DO:
  RUN new-actnum (FOCUS).
END.  
    
ON ENTRY OF ar-mcashl.acct-dscr IN BROWSE Browser-Table /* Reconciled */
DO:
  APPLY "tab" TO SELF.
  RETURN NO-APPLY.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-first-line B-table-Win 
PROCEDURE add-first-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  FIND ar-mcash WHERE ROWID(ar-mcash) EQ ip-rowid NO-LOCK NO-ERROR.

  RUN dispatch ('open-query').

  ll-new-record = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "ar-mcash"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-mcash"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-acct-dscr B-table-Win 
PROCEDURE get-acct-dscr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-actnum LIKE account.actnum NO-UNDO.
  DEF OUTPUT PARAM op-dscr LIKE account.dscr NO-UNDO.

  FIND FIRST account NO-LOCK
      WHERE account.company EQ cocode
        AND account.actnum  EQ ip-actnum
      NO-ERROR.
  op-dscr = IF AVAIL account THEN account.dscr ELSE "Not on file".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-check-no AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST bf-mcash WHERE bf-mcash.m-no EQ ar-mcashl.m-no NO-ERROR.
  IF NOT AVAIL bf-mcash THEN CREATE bf-mcash.
  BUFFER-COPY ar-mcashl TO bf-mcash.

  FIND FIRST bf-reftable WHERE
       bf-reftable.reftable = "AR-MCASH" AND
       bf-reftable.company  = bf-mcash.company and
       bf-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
       bf-reftable.code     = bf-mcash.rec_key
       NO-ERROR.

  IF NOT AVAIL bf-reftable THEN
  DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN get-check-no IN WIDGET-HANDLE(char-hdl) (OUTPUT v-check-no).

     CREATE bf-reftable.
     ASSIGN
        bf-reftable.reftable = "AR-MCASH"
        bf-reftable.company  = bf-mcash.company
        bf-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9")
        bf-reftable.code     = bf-mcash.rec_key
        bf-reftable.code2    = v-check-no.
  END.

  FIND CURRENT bf-reftable NO-LOCK.
  FIND CURRENT bf-mcash NO-LOCK.
  
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
  ll-new-record = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-next-mno AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-mcash USE-INDEX m-no NO-LOCK NO-ERROR.
  li-next-mno = (IF AVAIL bf-mcash THEN bf-mcash.m-no ELSE 0) + 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  BUFFER-COPY ar-mcash EXCEPT rec_key actnum check-amt TO ar-mcashl
  ASSIGN
   ar-mcashl.m-no = li-next-mno
   ar-mcashl.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey")
   .

  CREATE rec_key.
  ASSIGN
     rec_key.rec_key    = ar-mcashl.rec_key
     rec_key.table_name = "ar-mcash".
  RELEASE rec_key.

  CREATE bf-mcash.
  BUFFER-COPY ar-mcashl TO bf-mcash.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.

  DEF VAR lv-company AS CHAR NO-UNDO.
  DEF VAR lv-posted AS LOG NO-UNDO.
  DEF VAR lv-payer AS CHAR NO-UNDO.
  DEF VAR lv-check-date AS DATE NO-UNDO.
  DEF VAR lv-bank-code AS CHAR NO-UNDO.
  DEF VAR lv-curr-code AS CHAR NO-UNDO.
  DEF VAR lv-check-no AS CHAR NO-UNDO.
  DEF VAR lv-count AS INT NO-UNDO.

  DEF BUFFER buf-ar-mcash FOR ar-mcash.
  DEF BUFFER b-reftable FOR reftable.

  IF AVAIL ar-mcash THEN
  DO:
     ASSIGN
        lv-company = ar-mcash.company
        lv-posted  = ar-mcash.posted
        lv-payer   = ar-mcash.payer
        lv-check-date = ar-mcash.check-date
        lv-bank-code = ar-mcash.bank-code
        lv-curr-code = ar-mcash.curr-code[1].

     FIND FIRST b-reftable WHERE
          b-reftable.reftable = "AR-MCASH" AND
          b-reftable.company  = ar-mcash.company and
          b-reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
          b-reftable.code     = ar-mcash.rec_key
          NO-LOCK NO-ERROR.

     IF AVAIL b-reftable THEN
        lv-check-no = b-reftable.code2.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */

  FOR EACH bf-mcash WHERE
      bf-mcash.company      EQ ar-mcash.company AND
      bf-mcash.posted       EQ ar-mcash.posted AND
      bf-mcash.payer        EQ ar-mcash.payer AND
      bf-mcash.check-date   EQ ar-mcash.check-date AND
      bf-mcash.bank-code    EQ ar-mcash.bank-code AND
      bf-mcash.curr-code[1] EQ ar-mcash.curr-code[1]
      NO-LOCK,
      FIRST b-reftable WHERE
            b-reftable.reftable = "AR-MCASH" AND
            b-reftable.company  = bf-mcash.company and
            b-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
            b-reftable.code     = bf-mcash.rec_key AND
            b-reftable.code2    = lv-check-no
            NO-LOCK:

      lv-count = lv-count + 1.

      IF lv-count EQ 2 THEN
         LEAVE.
  END.

  IF lv-count EQ 1 THEN DO:

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
       RUN dispatch IN WIDGET-HANDLE(char-hdl) ('delete-record').
  END.
  
  ELSE DO:
    IF NOT adm-new-record THEN DO:
      {custom/askdel.i}
    END.

    FIND FIRST bf-mcash WHERE bf-mcash.m-no EQ ar-mcashl.m-no NO-ERROR.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL bf-mcash THEN DELETE bf-mcash.
  END.

  IF lv-company NE "" THEN
     FOR EACH buf-ar-mcash WHERE
         buf-ar-mcash.company      EQ lv-company AND
         buf-ar-mcash.posted       EQ lv-posted AND
         buf-ar-mcash.payer        EQ lv-payer AND
         buf-ar-mcash.check-date   EQ lv-check-date AND
         buf-ar-mcash.bank-code    EQ lv-bank-code AND
         buf-ar-mcash.curr-code[1] EQ lv-curr-code
         NO-LOCK,
         FIRST b-reftable WHERE
                b-reftable.reftable = "AR-MCASH" AND
                b-reftable.company  = buf-ar-mcash.company and
                b-reftable.loc      = STRING(buf-ar-mcash.m-no,">>>>>>9") AND
                b-reftable.code     = buf-ar-mcash.rec_key AND
                b-reftable.code2    = lv-check-no
                NO-LOCK:
         
            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
            IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
               RUN update-tt-line IN WIDGET-HANDLE(char-hdl)(lv-posted,
                                                             lv-payer,
                                                             lv-check-date,
                                                             lv-bank-code,
                                                             lv-curr-code,
                                                             lv-check-no).
            LEAVE.
     END.
  ELSE
  DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN update-tt IN WIDGET-HANDLE(char-hdl).
  END.
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

  DEF VAR v-check-no AS CHAR NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF BUFFER b-reftable FOR reftable.

  EMPTY TEMP-TABLE ar-mcashl.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN get-check-no IN WIDGET-HANDLE(char-hdl) (OUTPUT v-check-no).

  FOR EACH bf-mcash NO-LOCK
      WHERE bf-mcash.company      EQ ar-mcash.company
        AND bf-mcash.posted       EQ ar-mcash.posted
        AND bf-mcash.payer        EQ ar-mcash.payer
        AND bf-mcash.check-date   EQ ar-mcash.check-date
        AND bf-mcash.bank-code    EQ ar-mcash.bank-code
        AND bf-mcash.curr-code[1] EQ ar-mcash.curr-code[1],
    FIRST b-reftable WHERE
          b-reftable.reftable = "AR-MCASH" AND
          b-reftable.company  = bf-mcash.company AND
          b-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
          b-reftable.code     = bf-mcash.rec_key AND
          b-reftable.code2    = v-check-no
          NO-LOCK:

          CREATE ar-mcashl.
          BUFFER-COPY bf-mcash TO ar-mcashl.
          
          RUN get-acct-dscr (ar-mcashl.actnum, OUTPUT ar-mcashl.acct-dscr).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new AS LOG NO-UNDO.
  DEF VAR v-check-no AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-actnum (ar-mcashl.actnum:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  ASSIGN
   ll-new        = adm-new-record OR ll-new-record
   ll-new-record = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*IF ll-new THEN RUN auto-add.*/

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
  DO:
     FIND FIRST reftable WHERE
          reftable.reftable = "AR-MCASH" AND
          reftable.company  = ar-mcashl.company and
          reftable.loc      = STRING(ar-mcashl.m-no,">>>>>>9") AND
          reftable.code     = ar-mcashl.rec_key
          NO-LOCK NO-ERROR.

     IF AVAIL reftable THEN
        v-check-no = reftable.code2.

     RUN update-tt-line IN WIDGET-HANDLE(char-hdl)(ar-mcashl.posted,
                                                   ar-mcashl.payer,
                                                   ar-mcashl.check-date,
                                                   ar-mcashl.bank-code,
                                                   ar-mcashl.curr-code[1],
                                                   v-check-no).
  END.

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
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-dscr LIKE ar-mcashl.acct-dscr NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN get-acct-dscr (ip-focus:SCREEN-VALUE, OUTPUT lv-dscr).

    IF lv-dscr NE "Not on file" THEN 
      ar-mcashl.acct-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = lv-dscr.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum B-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST account
                    WHERE account.company EQ cocode 
                      AND account.actnum  EQ ip-focus:SCREEN-VALUE
                      AND account.type    NE "T") THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

