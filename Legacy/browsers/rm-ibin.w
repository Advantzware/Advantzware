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

&SCOPED-DEFINE yellowColumnsName rm-ibin
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR v-acs-code AS cha NO-UNDO.
DEF VAR v-ext-cost AS DEC NO-UNDO.
DEF VAR v-msf-cost AS DEC NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-bin

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-bin.po-no rm-bin.loc ~
rm-bin.loc-bin rm-bin.tag rm-bin.qty rm-bin.cost ext-cost (1) @ v-ext-cost ~
msf-cost (1) @ v-msf-cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-bin WHERE ~{&KEY-PHRASE} ~
  AND rm-bin.company = item.company ~
  AND rm-bin.i-no = item.i-no ~
  AND rm-bin.i-no <> " " ~
  AND (((rm-bin.qty > 0 OR rm-bin.qty < 0) AND NOT tb_show-qty) OR tb_show-qty ) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-bin WHERE ~{&KEY-PHRASE} ~
  AND rm-bin.company = item.company ~
  AND rm-bin.i-no = item.i-no ~
  AND rm-bin.i-no <> " "  ~
  AND (((rm-bin.qty > 0 OR rm-bin.qty < 0) AND NOT tb_show-qty) OR tb_show-qty ) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-bin
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-bin


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find tb_show-qty
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find tb_show-qty

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ext-cost B-table-Win 
FUNCTION ext-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD msf-cost B-table-Win 
FUNCTION msf-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-rmcost AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-qty AS LOGICAL INITIAL NO 
     LABEL "Show Bins with Qty = 0 ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-bin
    FIELDS(rm-bin.po-no
      rm-bin.loc
      rm-bin.loc-bin
      rm-bin.tag
      rm-bin.qty
      rm-bin.cost) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-bin.po-no COLUMN-LABEL "Vendor PO#" FORMAT ">>>>>>>>":U
            WIDTH 14 LABEL-BGCOLOR 14
      rm-bin.loc FORMAT "x(5)":U LABEL-BGCOLOR 14
      rm-bin.loc-bin COLUMN-LABEL "Bin Loc" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-bin.tag COLUMN-LABEL "Tag #" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-bin.qty COLUMN-LABEL "Quantity" FORMAT "->>>,>>9.9<<":U
            LABEL-BGCOLOR 14
      rm-bin.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
      ext-cost (1) @ v-ext-cost COLUMN-LABEL "Ext. Cost" FORMAT "->>,>>>,>>9.99<<":U
            WIDTH 20
      msf-cost (1) @ v-msf-cost COLUMN-LABEL "Cost/MSF" FORMAT "->>>,>>9.99<<<":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.48 COL 5 HELP
          "Select Browser Sort Order" NO-LABEL
     tb_show-qty AT ROW 18.50 COL 104 
     fi_sortby AT ROW 16.48 COL 64 COLON-ALIGNED
     auto_find AT ROW 16.48 COL 108 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.48 COL 131 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.48 COL 1
     RECT-4 AT ROW 16.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.item
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
{custom/yellowColumns.i}

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
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.rm-bin WHERE ASI.item ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "ASI.rm-bin.company = ASI.item.company
  AND ASI.rm-bin.i-no = ASI.item.i-no
  AND ASI.rm-bin.i-no <> "" """
     _FldNameList[1]   > ASI.rm-bin.po-no
"rm-bin.po-no" "Vendor PO#" ">>>>>>>>" "integer" ? ? ? 14 ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.rm-bin.loc
"rm-bin.loc" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.rm-bin.loc-bin
"rm-bin.loc-bin" "Bin Loc" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.rm-bin.tag
"rm-bin.tag" "Tag #" "x(20)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.rm-bin.qty
"rm-bin.qty" "Quantity" "->>>,>>9.9<<" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.rm-bin.cost
"rm-bin.cost" "Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"ext-cost (1) @ v-ext-cost" "Ext. Cost" "->>,>>>,>>9.99<<" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"msf-cost (1) @ v-msf-cost" "Cost/MSF" "->>>,>>9.99<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
 /* {methods/template/local/setvalue.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browse-order B-table-Win
ON VALUE-CHANGED OF browse-order IN FRAME F-Main
DO:
  APPLY "entry" TO auto_find .
  auto_find:SCREEN-VALUE = "" .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-qty B-table-Win
ON VALUE-CHANGED OF tb_show-qty IN FRAME F-Main
DO:
    ASSIGN tb_show-qty .
    APPLY "entry" TO auto_find .
    auto_find:SCREEN-VALUE = "" .
    RUN dispatch ("open-query") .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects B-table-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-rmcost.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-rmcost ).
       RUN set-position IN h_p-rmcost ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 36.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rmcost ,
             Browser-Table:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 9 */

  END CASE.

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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-cost B-table-Win 
PROCEDURE calc-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-override AS LOG LABEL " Enter Password " NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR lv-qty LIKE rm-bin.qty NO-UNDO.
  DEF VAR lv-cost LIKE rm-bin.cost NO-UNDO.

  DEF BUFFER bf-rm-bin FOR rm-bin.
  DEF BUFFER bf-item FOR item.
    

  v-override = NO.

  FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID(LDBNAME(1)) NO-ERROR.

IF AVAIL users AND users.securityLevel GE 900 THEN
    ASSIGN v-override = YES.
IF not(v-override) THEN DO:
  /*IF v-acs-code NE "YORKIE" THEN RUN windows/chkcode.w (OUTPUT v-acs-code).*/
   IF NOT v-override THEN do:  
     RUN sys/ref/d-passwd.w (10, OUTPUT v-override). 
     IF NOT v-override THEN RETURN NO-APPLY.
   END.  
END.
     
  IF v-override THEN DO:
    /*find bf-rm-bin where recid(bf-rm-bin) = recid(rm-bin).
    lv-cost = rm-bin.cost.
    DO WHILE TRUE:
      message "Enter New Cost: " update lv-cost.
      IF lv-cost NE ? THEN LEAVE.
    END.
    assign bf-rm-bin.cost = lv-cost
           rm-bin.cost:screen-value in browse {&browse-name} = string(lv-cost).
    find bf-item where recid(bf-item) = recid(item). 
    choice = no.
    MESSAGE "Is this also the last cost?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.

    IF choice THEN bf-item.last-cost = bf-rm-bin.cost.*/
   IF AVAIL rm-bin THEN
    ASSIGN
     lv-rowid = ROWID(rm-bin)
     lv-qty   = rm-bin.qty
     lv-cost  = rm-bin.cost.

    RUN rm/d-rm-bin.w (lv-rowid).
    IF AVAIL rm-bin THEN
    IF lv-qty  NE rm-bin.qty  OR
       lv-cost NE rm-bin.cost THEN DO:

      RUN rm/cre-tran.p (ROWID(rm-bin), "A", rm-bin.qty - lv-qty).

      ASSIGN
       lv-cost = 0
       lv-qty  = 0.

      FOR EACH bf-rm-bin NO-LOCK
          WHERE bf-rm-bin.company EQ item.company
            AND bf-rm-bin.i-no    EQ item.i-no
            AND bf-rm-bin.cost    NE ?:
        ASSIGN
         lv-cost = lv-cost + (bf-rm-bin.cost *
                   (bf-rm-bin.qty * IF bf-rm-bin.qty LT 0 THEN -1 ELSE 1))
         lv-qty  = lv-qty +
                   (bf-rm-bin.qty * IF bf-rm-bin.qty LT 0 THEN -1 ELSE 1).
      END.

      FIND CURRENT item.
      item.avg-cost = IF lv-qty EQ 0 THEN 0 ELSE (lv-cost / lv-qty).
      FIND CURRENT item NO-LOCK.

      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).

      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(item)).

      RUN repo-query (lv-rowid).
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  RUN dispatch ("open-query").

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.

  RUN dispatch ("row-changed").

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
  APPLY "value-changed" TO BROWSE {&browse-name}.
  APPLY "entry" TO BROWSE {&browse-name}.
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
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "rm-bin"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ext-cost B-table-Win 
FUNCTION ext-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF ip-type EQ 1 THEN rm-bin.qty * rm-bin.cost
         ELSE DEC(rm-bin.qty:SCREEN-VALUE IN BROWSE {&browse-name}) *
              DEC(rm-bin.qty:SCREEN-VALUE IN BROWSE {&browse-name}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION msf-cost B-table-Win 
FUNCTION msf-cost RETURNS DECIMAL
  ( INPUT ip-type AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld-cost AS DEC NO-UNDO.


  ld-cost = IF ip-type EQ 1 THEN rm-bin.cost
            ELSE DEC(rm-bin.cost:SCREEN-VALUE IN BROWSE {&browse-name}).

  IF item.cons-uom NE "MSF" THEN
    RUN custom/convcuom.p (cocode,
                           item.cons-uom,
                           "MSF",
                           item.basis-w,
                           item.s-len, 
                           (IF item.r-wid GT 0 THEN item.r-wid
                                               ELSE item.s-wid),
                           item.s-dep,
                           ld-cost, OUTPUT ld-cost).

  RETURN ld-cost.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

