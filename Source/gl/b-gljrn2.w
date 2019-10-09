&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: gl\b-gljrn2.w

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
{sys/inc/var.i new shared }

assign
 cocode = g_company
 locode = g_loc.

DEF BUFFER bf-jrnl FOR gl-jrnl.
DEF VAR lv-account-recid AS RECID NO-UNDO.
DEF VAR v-debit AS DEC NO-UNDO.
DEF VAR v-credit AS DEC NO-UNDO.
DEF VAR lv-acct-dscr AS cha FORM "x(30)" LABEL "Account Name " NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES gl-jrn
&Scoped-define FIRST-EXTERNAL-TABLE gl-jrn


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gl-jrn.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES gl-jrnl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table gl-jrnl.line gl-jrnl.actnum ~
display-account() @ lv-acct-dscr gl-jrnl.dscr gl-jrnl.tr-amt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table gl-jrnl.line ~
gl-jrnl.actnum gl-jrnl.dscr gl-jrnl.tr-amt 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table gl-jrnl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table gl-jrnl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH gl-jrnl OF gl-jrn WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH gl-jrnl OF gl-jrn WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table gl-jrnl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table gl-jrnl


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-credit B-table-Win 
FUNCTION display-credit RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-debit B-table-Win 
FUNCTION display-debit RETURNS DECIMAL
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
     SIZE 60 BY 1 DROP-TARGET NO-UNDO.

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
      gl-jrnl
    FIELDS(gl-jrnl.line
      gl-jrnl.actnum
      gl-jrnl.dscr
      gl-jrnl.tr-amt) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      gl-jrnl.line COLUMN-LABEL "Line" FORMAT "999":U WIDTH 7.2
      gl-jrnl.actnum COLUMN-LABEL "Account Number" FORMAT "x(25)":U
            WIDTH 36.2
      display-account() @ lv-acct-dscr
      gl-jrnl.dscr FORMAT "x(30)":U WIDTH 51.2
      gl-jrnl.tr-amt FORMAT "->>>,>>>,>>9.99":U
  ENABLE
      gl-jrnl.line
      gl-jrnl.actnum
      gl-jrnl.dscr
      gl-jrnl.tr-amt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.29
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.52 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 10.52 COL 70 COLON-ALIGNED
     Btn_Clear_Find AT ROW 10.52 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.52 COL 2
     RECT-4 AT ROW 10.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.gl-jrn
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
         HEIGHT             = 16.43
         WIDTH              = 154.2.
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
     _TblList          = "ASI.gl-jrnl OF ASI.gl-jrn"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   > ASI.gl-jrnl.line
"gl-jrnl.line" "Line" ? "integer" ? ? ? ? ? ? yes ? no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.gl-jrnl.actnum
"gl-jrnl.actnum" "Account Number" ? "character" ? ? ? ? ? ? yes ? no no "36.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"display-account() @ lv-acct-dscr" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.gl-jrnl.dscr
"gl-jrnl.dscr" ? ? "character" ? ? ? ? ? ? yes ? no no "51.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.gl-jrnl.tr-amt
"gl-jrnl.tr-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    WHEN "actnum" THEN DO:
      RUN windows/l-acct3.w (gl-jrn.company,"T",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        RUN new-actnum (0).
      END.
    END.
  END CASE.

  RETURN NO-APPLY.
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
  DEF VAR char-hdl AS CHAR NO-UNDO.

  /* This code displays initial values for newly added or copied rows. */
  RUN get-link-handle IN adm-broker-hdl
                    (THIS-PROCEDURE, "dont-enable-source", OUTPUT char-hdl).
                      
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN RETURN NO-APPLY.
  
  {src/adm/template/brsentry.i}

  IF NOT adm-adding-record AND NOT adm-brs-in-update THEN RETURN NO-APPLY.
  
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


&Scoped-define SELF-NAME gl-jrnl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-jrnl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF gl-jrnl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
    IF LASTKEY NE -1 THEN DO:
       /* gdm - 09200703*/
      FIND FIRST account
        WHERE account.company EQ g_company
          AND account.type    NE "T"
          AND account.actnum  EQ gl-jrnl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
      IF AVAIL account THEN lv-account-recid = RECID(account).
      ELSE DO:
          MESSAGE TRIM(gl-jrnl.actnum:LABEL IN BROWSE {&browse-name}) +
                  " is invalid, try help..."
              VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO gl-jrnl.actnum IN BROWSE {&browse-name}.
        RETURN NO-APPLY.    
      END.

/*      RUN valid-actnum NO-ERROR.                   */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.   */
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-jrnl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF gl-jrnl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
  RUN new-actnum (0).
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
  {src/adm/template/row-list.i "gl-jrn"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gl-jrn"}

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
IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
  THEN RUN auto-add IN WIDGET-HANDLE(char-hdl).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF gl-jrn.posted THEN DO:
     MESSAGE "GL Entry already Posted. No Adding allowed." VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .  

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
  DEF VAR X AS INT NO-UNDO.
  DEF VAR v-dscr AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */  
  for each bf-jrnl WHERE bf-jrnl.j-no = gl-jrn.j-no NO-LOCK by line descending:
      assign
        x      = bf-jrnl.line
        v-dscr = bf-jrnl.dscr.
      leave.
  end.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN gl-jrnl.j-no = gl-jrn.j-no
         gl-jrnl.line = x + 1
         gl-jrnl.dscr = v-dscr.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  IF gl-jrn.posted THEN do:
     MESSAGE "This GL Journal has been posted. No deletion allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  
  FIND CURRENT gl-jrn .
  IF gl-jrnl.tr-amt > 0 THEN ASSIGN gl-jrn.tdeb = gl-jrn.tdeb - gl-jrnl.tr-amt.
  ELSE gl-jrn.tcred = gl-jrn.tcred - gl-jrnl.tr-amt.
  gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.

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
  DEF VAR ll AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF gl-jrn.posted THEN do:
    MESSAGE "This GL Journal has been posted. No updates are allowed!"  VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  /* ticket 15382 */
  /*IF USERID("nosweat") NE gl-jrn.user-id AND
     gl-jrn.user-id NE ""                THEN DO:
    MESSAGE "This Journal was created by User: " +
            TRIM(gl-jrn.user-id) + ", do you wish update anyway?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE ll.
    IF NOT ll THEN RETURN.
  END.*/ 

  /* Dispatch standard ADM method.                             */  

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO gl-jrnl.actnum IN BROWSE {&browse-name}.
  
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
  APPLY "value-changed" TO browse-order IN FRAME {&FRAME-NAME}.

  IF AVAIL gl-jrnl THEN
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
  /* gdm - 09200703 */
  DEF VAR ll-new-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* gdm - 09200703 */
  ASSIGN ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN redisplay-header.

  /* gdm - 09200703 */
  IF ll-new-record THEN RUN auto-add.

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
  DEF INPUT PARAM ip-type AS INT NO-UNDO.

  IF gl-jrnl.actnum:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      ip-type = 0.

  DO WITH FRAME {&FRAME-NAME}:

     lv-acct-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = "". 
   
    FIND account
        WHERE account.company EQ g_company
          AND account.type    NE "T"
          AND account.actnum  BEGINS gl-jrnl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN DO:
      IF ip-type EQ 1
          THEN
        gl-jrnl.actnum:SCREEN-VALUE IN BROWSE {&browse-name} = account.actnum.
      lv-acct-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = account.dscr.
    END.
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
  DEF BUFFER b-gl-jrnl FOR gl-jrnl.


  DO TRANSACTION:
    FIND CURRENT gl-jrn.

    ASSIGN
     gl-jrn.tdeb  = 0
     gl-jrn.tcred = 0.

    FOR EACH b-gl-jrnl OF gl-jrn NO-LOCK:
      IF b-gl-jrnl.tr-amt GT 0 THEN
        gl-jrn.tdeb  = gl-jrn.tdeb  + b-gl-jrnl.tr-amt.
      ELSE
        gl-jrn.tcred = gl-jrn.tcred + b-gl-jrnl.tr-amt.
    END.

    gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.

    FIND CURRENT gl-jrn NO-LOCK.
  END.

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
  {src/adm/template/snd-list.i "gl-jrn"}
  {src/adm/template/snd-list.i "gl-jrnl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum B-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN new-actnum (1).

    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.type    NE "T"
          AND account.actnum  EQ gl-jrnl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL account THEN lv-account-recid = RECID(account).

    ELSE DO:
      MESSAGE TRIM(gl-jrnl.actnum:LABEL IN BROWSE {&browse-name}) +
              " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO gl-jrnl.actnum IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-delete B-table-Win 
PROCEDURE is-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Task - 11121308 
------------------------------------------------------------------------------*/
 DEF BUFFER b-gl-jrnl FOR gl-jrnl .

    FOR EACH  b-gl-jrnl OF gl-jrn EXCLUSIVE-LOCK:
        IF b-gl-jrnl.tr-amt = 0  THEN DO:
            DELETE b-gl-jrnl .
        END.
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
  DO WITH FRAME {&frame-name}:
    RELEASE account.
    IF AVAIL gl-jrnl THEN
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ gl-jrnl.actnum /*:SCREEN-VALUE IN BROWSE {&browse-name} */
        NO-LOCK NO-ERROR.         
    RETURN IF AVAIL account THEN account.dscr ELSE "". 
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-credit B-table-Win 
FUNCTION display-credit RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL gl-jrnl AND gl-jrnl.tr-amt > 0 THEN RETURN 0.00. 
  ELSE RETURN  gl-jrnl.tr-amt.
    /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-debit B-table-Win 
FUNCTION display-debit RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL gl-jrnl AND gl-jrnl.tr-amt > 0 THEN RETURN gl-jrnl.tr-amt.
  ELSE RETURN 0.00.   /* Function return value. */
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

