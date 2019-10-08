&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  oe\b-invitm.w

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
&SCOPED-DEFINE yellowColumnsName b-invitm
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-bolno AS cha LABEL "BOL#" NO-UNDO.
DEF VAR lv-lot-no AS CHAR NO-UNDO.

/* gdm - 11180901*/
DEF VAR v-sort-name  AS LOG NO-UNDO.
DEF VAR invcopys-cha AS CHAR NO-UNDO.

{sys/inc/invcopys.i}
IF AVAIL sys-ctrl THEN
   invcopys-cha = sys-ctrl.char-fld.
&Scoped-define SORTBY-PHRASE BY inv-line.line

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
&Scoped-define EXTERNAL-TABLES inv-head
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES inv-line

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table inv-line.line inv-line.i-no ~
inv-line.i-name inv-line.inv-qty inv-line.ord-no display-bolno() @ lv-bolno ~
inv-line.price inv-line.pr-uom inv-line.t-price inv-line.est-no ~
get-lot-no() @ lv-lot-no inv-line.sman[1] inv-line.sname[1] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH inv-line OF inv-head WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH inv-line OF inv-head WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table inv-line
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table inv-line


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find fi_By fi_AutoFindLabel 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find fi_By ~
fi_AutoFindLabel fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-bolno B-table-Win 
FUNCTION display-bolno RETURNS character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-lot-no B-table-Win 
FUNCTION get-lot-no RETURNS CHARACTER
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
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_AutoFindLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Find:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fi_By AS CHARACTER FORMAT "X(256)":U INITIAL "By:" 
      VIEW-AS TEXT 
     SIZE 3.6 BY .62 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 23 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      inv-line SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      inv-line.line FORMAT ">>99":U WIDTH 6.2 LABEL-BGCOLOR 14
      inv-line.i-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      inv-line.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      inv-line.inv-qty COLUMN-LABEL "Quantity" FORMAT "->>,>>>,>>9":U
            WIDTH 15.6 LABEL-BGCOLOR 14
      inv-line.ord-no FORMAT ">>>>>9":U WIDTH 9.2 LABEL-BGCOLOR 14
      display-bolno() @ lv-bolno COLUMN-LABEL "Bol#" FORMAT "x(8)":U LABEL-BGCOLOR 14
      inv-line.price FORMAT "->>,>>>,>>9.99<<<<":U WIDTH 14.2 LABEL-BGCOLOR 14
      inv-line.pr-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U LABEL-BGCOLOR 14
      inv-line.t-price COLUMN-LABEL "Total$" FORMAT "->>,>>>,>>9.99":U LABEL-BGCOLOR 14
      inv-line.est-no COLUMN-LABEL "    Est#" FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      get-lot-no() @ lv-lot-no COLUMN-LABEL "Customer Lot #" FORMAT "X(15)":U
            WIDTH 21.8 LABEL-BGCOLOR 14
      inv-line.sman[1] COLUMN-LABEL "Sales Rep" FORMAT "x(3)":U LABEL-BGCOLOR 14
      inv-line.sname[1] COLUMN-LABEL "Sales Rep Name" FORMAT "x(20)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.67 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 17.67 COL 33 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 17.67 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 17.67 COL 132 HELP
          "CLEAR AUTO FIND Value"
     fi_By AT ROW 17.81 COL 2 NO-LABEL
     fi_AutoFindLabel AT ROW 17.95 COL 61.4 NO-LABEL
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.67 COL 31
     RECT-4 AT ROW 17.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.inv-head
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
         HEIGHT             = 24.86
         WIDTH              = 171.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

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

 Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_AutoFindLabel IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_By IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.inv-line OF ASI.inv-head"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > ASI.inv-line.line
"inv-line.line" ? ">>99" "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ASI.inv-line.i-no
     _FldNameList[3]   = ASI.inv-line.i-name
     _FldNameList[4]   > ASI.inv-line.inv-qty
"inv-line.inv-qty" "Quantity" "->>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.inv-line.ord-no
"inv-line.ord-no" ? ? "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"display-bolno() @ lv-bolno" "Bol#" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.inv-line.price
"inv-line.price" ? "->>,>>>,>>9.99<<<<" "decimal" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.inv-line.pr-uom
"inv-line.pr-uom" "UOM" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.inv-line.t-price
"inv-line.t-price" "Total$" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.inv-line.est-no
"inv-line.est-no" "    Est#" "x(8)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"get-lot-no() @ lv-lot-no" "Customer Lot #" "X(15)" ? ? ? ? ? ? ? no ? no no "21.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.inv-line.sman[1]
"inv-line.sman[1]" "Sales Rep" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.inv-line.sname[1]
"inv-line.sname[1]" "Sales Rep Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    run oe/d-invitm.w (recid(inv-line), inv-line.r-no,"Update").
    run get-link-handle in adm-broker-hdl(this-procedure,"record-source", output char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
  /* gdm - 11180901 */
  IF invcopys-cha NE "" THEN
     RUN set-row-bgcolor. 
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
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  RUN startsearch.
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
  {src/adm/template/row-list.i "inv-head"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-item B-table-Win 
PROCEDURE delete-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-dumb AS LOG NO-UNDO.

IF inv-head.bol-no <> 0 THEN DO:
    MESSAGE "Can't delete item for Invoices created via BOL. " VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.


if avail inv-line then do:
   {custom/askdel.i}
   
  /*  find inv-line where recid(inv-line) = fil_id2 no-error no-wait.
    if not avail inv-line and locked inv-line then do:
      bell.
      pause 3 message " This Invoice Line Is Inuse!  Unable to Delete Item. ".
      return.
    end.
  */
  FIND CURRENT inv-line EXCLUSIVE-LOCK.
  session:set-wait-state("GENERAL").
  if inv-line.ord-no eq 0 then
     for each oe-bolh where oe-bolh.b-no    eq inv-line.b-no
                            and oe-bolh.printed eq yes
                            and oe-bolh.posted  eq yes
                            and oe-bolh.deleted eq yes:

         for each oe-boll where oe-boll.company eq oe-bolh.company
                                and oe-boll.b-no    eq oe-bolh.b-no
                                and oe-boll.i-no    eq inv-line.i-no
                                use-index b-no:
                 delete oe-boll.
         end.

         FOR EACH oe-relh
             WHERE oe-relh.company  EQ oe-bolh.company
               AND oe-relh.release# EQ oe-bolh.release#:
           DELETE oe-relh.
         END.

         delete oe-bolh.
     end.
     delete inv-line.

     RUN refresh-value.
  END.
  lv-dumb = browse {&browse-name}:delete-current-row().

  run dispatch ('row-changed').
  
  session:set-wait-state("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-line-est B-table-Win 
PROCEDURE get-line-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-est-no as cha no-undo.
  
  op-est-no = if available inv-line then inv-line.est-no else "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-value B-table-Win 
PROCEDURE refresh-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  RUN refresh-value IN WIDGET-HANDLE(char-hdl).

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
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF VAR CHAR-hdl AS cha NO-UNDO.

  def buffer bf-line for inv-line.
  
  run dispatch ('open-query').
  if ip-recid = ? then do:
     find last bf-line where bf-line.company = inv-head.company
                         and bf-line.r-no = inv-head.r-no
                         no-lock no-error.
     if avail bf-line then do:
        reposition {&browse-name} to rowid rowid(bf-line).
        run dispatch ('row-changed').
     end.   
  end.
  else do:
     reposition {&browse-name} to RECID ip-recid.
     run dispatch ('row-changed').
  end.
  /* === reopen browser 
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl). /* viewer */
  RUN get-link-handle IN adm-broker-hdl (widget-handle(char-hdl),"record-source", OUTPUT char-hdl). /* browser */
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
  */

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  RUN refresh-value IN WIDGET-HANDLE(char-hdl). 


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
  {src/adm/template/snd-list.i "inv-head"}
  {src/adm/template/snd-list.i "inv-line"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-row-bgcolor B-table-Win 
PROCEDURE set-row-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-inv-head FOR inv-head.
DEF BUFFER bf-inv-line FOR inv-line.
DEF BUFFER bf-oe-ord   FOR oe-ord.
DEF BUFFER bf-oe-ordl  FOR oe-ordl.

DEF VAR v-difqty LIKE inv-line.qty NO-UNDO.
DEF VAR v-totqty LIKE inv-line.qty NO-UNDO.

IF AVAIL inv-line THEN DO:
    
   IF inv-line.ord-no NE 0 THEN DO:
  
      FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company  EQ inv-line.company 
          AND bf-oe-ordl.ord-no   EQ inv-line.ord-no  
          AND bf-oe-ordl.i-no     EQ inv-line.i-no NO-ERROR.
      
      FIND FIRST bf-oe-ord NO-LOCK 
        WHERE bf-oe-ord.company EQ bf-oe-ordl.company
          AND bf-oe-ord.ord-no  EQ bf-oe-ordl.ord-no  NO-ERROR.
      
      FOR EACH bf-inv-line FIELDS(ship-qty) NO-LOCK
        WHERE bf-inv-line.company EQ inv-line.company 
          AND bf-inv-line.inv-no  EQ inv-line.inv-no  
          AND bf-inv-line.line    EQ inv-line.line 
          AND bf-inv-line.ord-no  EQ inv-line.ord-no
          AND bf-inv-line.i-no    EQ inv-line.i-no:
     
        ASSIGN v-totqty = v-totqty + bf-inv-line.ship-qty.
      END.
     
      IF v-totqty NE inv-line.qty THEN DO:
     
        /* UNDERRUN */
        IF inv-line.qty GT v-totqty THEN
           ASSIGN v-difqty = inv-line.qty * (bf-oe-ord.under-pct / 100).
     
        /* OVERRUN */
        IF inv-line.qty LT v-totqty THEN
           ASSIGN v-difqty = inv-line.qty * (bf-oe-ord.over-pct / 100).
     
        IF (inv-line.qty LT v-totqty AND
            ((inv-line.qty - v-totqty) * -1) NE v-difqty) OR 
           (inv-line.qty GT v-totqty AND
            inv-line.qty - v-totqty NE v-difqty) THEN
           ASSIGN inv-line.line:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
                  inv-line.i-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
                  inv-line.i-name:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12
                  inv-line.inv-qty:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
                  inv-line.ord-no:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12
                  lv-bolno:BGCOLOR IN BROWSE {&BROWSE-NAME}         = 12
                  inv-line.price:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 12
                  inv-line.pr-uom:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12
                  inv-line.t-price:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
                  inv-line.est-no:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12
                  lv-lot-no:BGCOLOR IN BROWSE {&BROWSE-NAME}        = 12.
      END.
   END.
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-bolno B-table-Win 
FUNCTION display-bolno RETURNS character
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 

 IF AVAIL inv-line THEN DO:
     
     FIND FIRST oe-bolh WHERE oe-bolh.company = inv-line.company AND
                              oe-bolh.b-no = inv-line.b-no NO-LOCK NO-ERROR.
     
     IF AVAIL oe-bolh THEN RETURN string(oe-bolh.bol-no,">>>>>>>9").
     ELSE RETURN "".   /* Function return value. */

     

  END.
  ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-lot-no B-table-Win 
FUNCTION get-lot-no RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v-lot-no AS CHAR NO-UNDO.

    IF AVAIL inv-line THEN 
     v-lot-no = inv-line.lot-no.

  RETURN v-lot-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

