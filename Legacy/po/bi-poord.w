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
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEF BUFFER b-ref1 FOR reftable.
DEF BUFFER b-ref2 FOR reftable.
    
DEF NEW SHARED VAR factor# AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR v-default-gl-log AS LOG NO-UNDO.
DEF NEW SHARED VAR v-default-gl-cha AS cha NO-UNDO.
DEF NEW SHARED VAR v-po-qty AS LOG INITIAL TRUE NO-UNDO.
DEF NEW SHARED VAR v-po-msf LIKE sys-ctrl.int-fld NO-UNDO.
DEF TEMP-TABLE tt-po-ordl-renum 
    FIELD po-line-num AS INT
    FIELD save-rowid AS ROWID.
DEF TEMP-TABLE tt-po-ordl LIKE po-ordl.

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
&Scoped-define EXTERNAL-TABLES po-ord
&Scoped-define FIRST-EXTERNAL-TABLE po-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR po-ord.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES po-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table po-ordl.i-no po-ordl.i-name ~
po-ordl.job-no po-ordl.job-no2 po-ordl.s-num po-ordl.ord-qty po-ordl.cost ~
po-ordl.cust-no po-ordl.due-date po-ordl.item-type po-ordl.LINE ~
getOrdQty() @ po-ordl.ord-qty getCost() @ po-ordl.cost po-ordl.spare-int-1 ~
po-ordl.spare-int-2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH po-ordl WHERE po-ordl.company eq po-ord.company and ~
po-ordl.po-no eq po-ord.po-no ~
      AND po-ordl.line GT 0 AND ~
ASI.po-ordl.line LT 99999999 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH po-ordl WHERE po-ordl.company eq po-ord.company and ~
po-ordl.po-no eq po-ord.po-no ~
      AND po-ordl.line GT 0 AND ~
ASI.po-ordl.line LT 99999999 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table po-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table po-ordl


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCost B-table-Win 
FUNCTION getCost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrdQty B-table-Win 
FUNCTION getOrdQty RETURNS DECIMAL
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
      po-ordl
    FIELDS(po-ordl.i-no
      po-ordl.i-name
      po-ordl.job-no
      po-ordl.job-no2
      po-ordl.s-num
      po-ordl.ord-qty
      po-ordl.cost
      po-ordl.cust-no
      po-ordl.due-date
      po-ordl.item-type
      po-ordl.ord-qty
      po-ordl.cost
      po-ordl.spare-int-1
      po-ordl.spare-int-2
      po-ordl.LINE) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      po-ordl.i-no COLUMN-LABEL "RM/FG Item#" FORMAT "x(15)":U
            WIDTH 25
      po-ordl.i-name COLUMN-LABEL "RM/FG Item Name" FORMAT "x(30)":U
      po-ordl.job-no FORMAT "x(6)":U
      po-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U
      po-ordl.s-num COLUMN-LABEL "Form#" FORMAT ">9":U
      po-ordl.ord-qty FORMAT "->>>,>>>,>>9.9<<<<<":U
      po-ordl.cost FORMAT "->,>>>,>>9.99<<<<":U
      po-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
      po-ordl.due-date FORMAT "99/99/9999":U
      po-ordl.item-type FORMAT "R/F":U
      getOrdQty() @ po-ordl.ord-qty COLUMN-LABEL "Quantity" FORMAT "->>>,>>>,>>9.9<<<<<":U
      getCost() @ po-ordl.cost COLUMN-LABEL "Unit Cost" FORMAT "->,>>>,>>9.99<<<<":U
      po-ordl.spare-int-1 FORMAT "->,>>>,>>9":U
      po-ordl.spare-int-2 FORMAT "->,>>>,>>9":U
      po-ordl.LINE  COLUMN-LABEL "Line #" FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 13.33
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 14.57 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 14.57 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 14.57 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 14.57 COL 2
     RECT-4 AT ROW 14.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.po-ord
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
         HEIGHT             = 19.52
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

ASSIGN 
       po-ordl.spare-int-1:VISIBLE IN BROWSE Browser-Table = FALSE
       po-ordl.spare-int-2:VISIBLE IN BROWSE Browser-Table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.po-ordl WHERE ASI.po-ord <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "po-ordl.company eq po-ord.company and
po-ordl.po-no eq po-ord.po-no"
     _Where[1]         = "ASI.po-ordl.line GT 0 AND
ASI.po-ordl.line LT 99999999"
     _FldNameList[1]   > ASI.po-ordl.i-no
"po-ordl.i-no" "RM/FG Item#" ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.po-ordl.i-name
"po-ordl.i-name" "RM/FG Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = ASI.po-ordl.job-no
     _FldNameList[4]   > ASI.po-ordl.job-no2
"po-ordl.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.po-ordl.s-num
"po-ordl.s-num" "Form#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = ASI.po-ordl.ord-qty
     _FldNameList[7]   = ASI.po-ordl.cost
     _FldNameList[8]   > ASI.po-ordl.cust-no
"po-ordl.cust-no" "Customer#" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = ASI.po-ordl.due-date
     _FldNameList[10]   = ASI.po-ordl.item-type
     _FldNameList[11]   > "_<CALC>"
"getOrdQty() @ po-ordl.ord-qty" "Quantity" "->>>,>>>,>>9.9<<<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"getCost() @ po-ordl.cost" "Unit Cost" "->,>>>,>>9.99<<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.po-ordl.spare-int-1
"po-ordl.spare-int-1" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.po-ordl.spare-int-2
"po-ordl.spare-int-2" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.po-ordl.line
"po-ordl.line" "Line #" ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"buttons-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
       THEN RUN browser-dbclicked IN WIDGET-HANDLE(char-hdl).
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
   {src/adm/template/brsleave.i}
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
      
  RUN spec-book-image-proc.
  RUN dept-pan-image-proc .
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
  {src/adm/template/row-list.i "po-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "po-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-reopen B-table-Win 
PROCEDURE close-reopen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


  IF po-ord.opened AND AVAIL po-ordl THEN DO:
    MESSAGE "Do you wish to " +
            TRIM(STRING(po-ordl.stat EQ "C","Reopen/Close")) +
            " this Purchase Order Line Item?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
    IF ll THEN DO TRANSACTION:
      RUN po/closepol.p (ROWID(po-ordl), IF po-ordl.stat EQ "C" THEN 2 ELSE -1).
      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      RUN dispatch ("row-changed").
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete_item B-table-Win 
PROCEDURE delete_item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-dumb AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-loc LIKE rm-rctd.loc NO-UNDO.
  DEF VAR ll-renumber AS LOG NO-UNDO.
  DEF BUFFER b-po-ordl FOR po-ordl.


  /*RELEASE rm-rdtlh.
  RELEASE rm-rctd.

  lv-loc = "".

  IF po-ordl.item-type THEN
  DO WHILE TRUE:
    FIND FIRST rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.company EQ po-ordl.company
          AND rm-rdtlh.loc     GT lv-loc
        USE-INDEX tag NO-ERROR.
    IF NOT AVAIL rm-rdtlh THEN LEAVE.

    lv-loc = rm-rdtlh.loc.

    FIND FIRST rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.company EQ po-ordl.company
          AND rm-rdtlh.loc     EQ lv-loc
          AND rm-rdtlh.tag     BEGINS STRING(po-ordl.po-no,'9999999') +
                                      STRING(po-ordl.line,'999')
        USE-INDEX tag NO-ERROR.
    IF AVAIL rm-rdtlh THEN LEAVE.
  END.

  lv-loc = "".

  IF NOT AVAIL rm-rdtlh THEN
  DO WHILE TRUE:
    FIND FIRST rm-rctd NO-LOCK
        WHERE rm-rctd.company EQ po-ordl.company
          AND rm-rctd.loc     GT lv-loc
        USE-INDEX tag NO-ERROR.
    IF NOT AVAIL rm-rctd THEN LEAVE.

    lv-loc = rm-rctd.loc.

    FIND FIRST rm-rctd NO-LOCK
        WHERE rm-rctd.company EQ po-ordl.company
          AND rm-rctd.loc     EQ lv-loc
          AND rm-rctd.tag     BEGINS STRING(po-ordl.po-no,'9999999') +
                                     STRING(po-ordl.line,'999')
        USE-INDEX tag NO-ERROR.
    IF AVAIL rm-rctd THEN LEAVE.
  END.

  IF AVAIL rm-rdtlh OR AVAIL rm-rctd THEN DO:
    MESSAGE "This PO Line has tags created, no deletion allowed..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.*/

  {custom/askdel.i}
  IF po-ord.printed = NO THEN DO:
       MESSAGE "Renumber lines after delete?" VIEW-AS ALERT-BOX QUESTION
          BUTTON YES-NO UPDATE ll-renumber.
  END.
  SESSION:SET-WAIT-STATE("general").

  FIND CURRENT po-ordl EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    IF CAN-FIND(FIRST b-po-ordl
                WHERE b-po-ordl.company EQ po-ord.company
                  AND b-po-ordl.po-no   EQ po-ordl.po-no
                  AND ROWID(b-po-ordl)  NE ROWID(po-ordl)) THEN DELETE po-ordl.

    ELSE DO:
      RUN po/del-po-ordl.p (BUFFER po-ordl).

      FOR EACH tt-po-ordl:
        DELETE tt-po-ordl.
      END.

      CREATE tt-po-ordl.
      ASSIGN
       tt-po-ordl.company  = po-ord.company
       tt-po-ordl.po-no    = po-ord.po-no
       tt-po-ordl.line     = 0
       tt-po-ordl.due-date = po-ord.due-date.

      BUFFER-COPY tt-po-ordl EXCEPT rec_key TO po-ordl.
      DELETE tt-po-ordl.

      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl),"record-source", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN RUN dispatch IN WIDGET-HANDLE(char-hdl) ('row-changed').
      END.
    END.

    IF NOT AVAIL po-ordl THEN DO WITH FRAME {&FRAME-NAME}:
      IF ll-renumber THEN
          RUN renumber-lines.
      ll-dumb = BROWSE {&browse-name}:DELETE-CURRENT-ROW().
      RUN dispatch ('row-changed').
    END.
  END.
  FIND CURRENT po-ordl NO-LOCK NO-ERROR.
  
  SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF AVAIL(po-ordl) AND  po-ordl.item-type = NO  THEN
    FIND itemfg NO-LOCK WHERE itemfg.company EQ po-ordl.company
      AND itemfg.i-no EQ po-ordl.i-no
    NO-ERROR.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN po/po-sysct.p.  /* for vars factor#.... need for d-poordl.w  */
                           
  RUN spec-book-image-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renumber-lines B-table-Win 
PROCEDURE renumber-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-po-ordl FOR po-ordl.
DEF BUFFER bf-tt-po-ordl FOR tt-po-ordl.

DEF VAR v-new-num AS INT NO-UNDO.

/* Create temp table to store new line number per rowid */
FOR EACH bf-po-ordl WHERE bf-po-ordl.company EQ po-ord.company
               AND bf-po-ordl.po-no   EQ po-ord.po-no NO-LOCK
               BY bf-po-ordl.LINE.

    v-new-num = v-new-num + 1.
    CREATE tt-po-ordl-renum .
    ASSIGN tt-po-ordl-renum.po-line-num = v-new-num
           tt-po-ordl-renum.save-rowid  = ROWID(bf-po-ordl).


END.

FOR EACH tt-po-ordl-renum:

    /* find by rowid and update the line number to new one */
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ tt-po-ordl-renum.save-rowid
                    EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-po-ordl THEN DO:
        FOR EACH tt-po-ordl WHERE tt-po-ordl.LINE = bf-po-ordl.LINE.
            FIND bf-tt-po-ordl WHERE ROWID(bf-tt-po-ordl) EQ ROWID(tt-po-ordl).
            bf-tt-po-ordl.LINE = tt-po-ordl-renum.po-line-num.
        END.
        bf-po-ordl.LINE = tt-po-ordl-renum.po-line-num.
    END.
    DELETE tt-po-ordl-renum.
    FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.

END.

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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF VAR CHAR-hdl AS cha NO-UNDO.

  DEF BUFFER bf-ordl FOR po-ordl.

  RUN dispatch ('open-query').
  IF ip-rowid = ? THEN DO:
     FIND LAST bf-ordl NO-LOCK WHERE bf-ordl.company = po-ordl.company
                         AND bf-ordl.po-no = po-ordl.po-no
                         NO-ERROR.
     IF AVAIL bf-ordl THEN DO:
        REPOSITION {&browse-name} TO ROWID ROWID(bf-ordl) NO-ERROR.
        RUN dispatch ('row-changed').
     END.   
  END.
  ELSE DO:
     REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
     RUN dispatch ('row-changed').
  END.
  /* === reopen browser  later
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl). /* viewer */
  RUN get-link-handle IN adm-broker-hdl (widget-handle(char-hdl),"record-source", OUTPUT char-hdl). /* browser */
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
  */

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
  {src/adm/template/snd-list.i "po-ord"}
  {src/adm/template/snd-list.i "po-ordl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-rec-key B-table-Win 
PROCEDURE set-rec-key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "value-changed" TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-book-image-proc B-table-Win 
PROCEDURE spec-book-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   DEF BUFFER bf2-itemfg FOR itemfg.
   DEF BUFFER bf2-item FOR ITEM.
 IF AVAIL po-ordl THEN
   FIND FIRST bf2-itemfg WHERE
        bf2-itemfg.company = po-ordl.company AND
        bf2-itemfg.i-no EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL bf2-itemfg THEN
      v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = bf2-itemfg.rec_key AND
               notes.note_type = "S").
   IF NOT AVAIL bf2-itemfg THEN DO:
       FIND FIRST bf2-item WHERE
        bf2-item.company = po-ordl.company AND
        bf2-item.i-no EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL bf2-item THEN
      v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = bf2-item.rec_key AND
               notes.note_type = "S").

   END.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'specpo-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-pan-image-proc B-table-Win 
PROCEDURE dept-pan-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   v-spec = NO . 
   IF AVAIL po-ordl THEN
        v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = po-ordl.rec_key ).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'specpo-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCost B-table-Win 
FUNCTION getCost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 IF AVAIL po-ordl AND po-ordl.spare-int-2 EQ 1 AND AVAIL(itemfg) THEN
    RETURN po-ordl.cost * itemfg.case-count.
  ELSE 
    RETURN (IF AVAIL(po-ordl) THEN po-ordl.cost ELSE 0).   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrdQty B-table-Win 
FUNCTION getOrdQty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF AVAIL po-ordl AND po-ordl.spare-int-1 EQ 1 AND AVAIL(itemfg) THEN
    RETURN po-ordl.ord-qty / itemfg.case-count.
  ELSE 
    RETURN (IF AVAIL(po-ordl) THEN po-ordl.ord-qty ELSE 0).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

