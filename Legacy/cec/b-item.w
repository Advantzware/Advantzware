&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/item.w

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

&SCOPED-DEFINE yellowColumnsName b-item
&SCOPED-DEFINE cellColumnDat browsers-b-item
&SCOPED-DEFINE winReSize
/*&SCOPED-DEFINE sizeOption HEIGHT*/  /* task 01071407*/
&SCOPED-DEFINE useMatches
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEFINE VARIABLE lvFirstRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID AS ROWID NO-UNDO.
DEF VAR v-called-setCellColumns AS LOG NO-UNDO.
DEF VAR v-col-move AS LOG NO-UNDO INIT TRUE.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table item.i-no item.i-name ~
item.i-code item.mat-type item.procat item.i-dscr item.cost-type item.q-onh ~
item.q-ono item.q-comm item.cons-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH item WHERE ~{&KEY-PHRASE} ~
      AND item.company = gcompany ~
 AND item.industry = "2" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH item WHERE ~{&KEY-PHRASE} ~
      AND item.company = gcompany ~
 AND item.industry = "2" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table item
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table item


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear" 
     SIZE 8 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 56 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      item
    FIELDS(item.i-no
      item.i-name
      item.i-code
      item.mat-type
      item.procat
      item.i-dscr
      item.cost-type
      item.q-onh
      item.q-ono
      item.q-comm
      item.cons-uom) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      item.i-no FORMAT "x(10)":U WIDTH 19.2 LABEL-BGCOLOR 14
      item.i-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      item.i-code COLUMN-LABEL "Real?" FORMAT "X":U
      item.mat-type COLUMN-LABEL "Mat'l!Type" FORMAT "X":U LABEL-BGCOLOR 14
      item.procat COLUMN-LABEL "Catgy" FORMAT "x(5)":U LABEL-BGCOLOR 14
      item.i-dscr COLUMN-LABEL "Description" FORMAT "x(30)":U LABEL-BGCOLOR 14
      item.cost-type COLUMN-LABEL "CType" FORMAT "XXX":U LABEL-BGCOLOR 14
      item.q-onh COLUMN-LABEL "Qty On Hand" FORMAT "->>>,>>>,>>9.99":U
            LABEL-BGCOLOR 14
      item.q-ono COLUMN-LABEL "Qty On Order" FORMAT "->>>,>>>,>>9.99":U
            LABEL-BGCOLOR 14
      item.q-comm FORMAT ">>>,>>>,>>9.99":U LABEL-BGCOLOR 14
      item.cons-uom COLUMN-LABEL "C-UOM" FORMAT "x(3)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 16.67
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.91 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 17.91 COL 62 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 17.91 COL 103 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 17.91 COL 130 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.91 COL 2
     RECT-4 AT ROW 17.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
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
         HEIGHT             = 21.29
         WIDTH              = 131.2.
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

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
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
     _TblList          = "ASI.item"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "item.company = gcompany
 AND ASI.item.industry = ""2"""
     _FldNameList[1]   > ASI.item.i-no
"item.i-no" ? ? "character" ? ? ? 14 ? ? no ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.item.i-name
"item.i-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.item.i-code
"item.i-code" "Real?" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.item.mat-type
"item.mat-type" "Mat'l!Type" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.item.procat
"item.procat" "Catgy" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.item.i-dscr
"item.i-dscr" "Description" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.item.cost-type
"item.cost-type" "CType" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.item.q-onh
"item.q-onh" "Qty On Hand" "->>>,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.item.q-ono
"item.q-ono" "Qty On Order" "->>>,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.item.q-comm
"item.q-comm" ? ">>>,>>>,>>9.99" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.item.cons-uom
"item.cons-uom" "C-UOM" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  {methods/template/local/setvalue.i}
  
  /* not for corrugated item 
  if avail item and item.i-code = "E" then do:
     run get-link-handle in adm-broker-hdl(this-procedure, "container-source", output char-hdl).
     run get-link-handle in adm-broker-hdl(widget-handle(char-hdl), "page-source", output char-hdl).     
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 5).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 7).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 8).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 9).
  end.  
  else if avail item and item.i-code = "R" then do:
     run get-link-handle in adm-broker-hdl(this-procedure, "container-source", output char-hdl).
     run get-link-handle in adm-broker-hdl(widget-handle(char-hdl), "page-source", output char-hdl).     
     RUN enable-folder-page IN widget-handle(char-hdl) (INPUT 5).
     RUN enable-folder-page IN widget-handle(char-hdl) (INPUT 7).
     RUN enable-folder-page IN widget-handle(char-hdl) (INPUT 8).
     RUN enable-folder-page IN widget-handle(char-hdl) (INPUT 9).
  end.  
  */

  /* need to assign rec_key value for notes.   noreckey.i has item */
  FIND CURRENT ITEM NO-LOCK.
   {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(item.rec_key,{methods/headers/item.i})"}
   {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
     "(CAN-FIND(FIRST notes WHERE notes.rec_key = item.rec_key and notes.note_type = 'S'))"}
   {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
     "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = item.rec_key))"} 

   RUN spec-image-proc .

  DEF VAR char-hdl AS cha NO-UNDO.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-target", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN new-item IN WIDGET-HANDLE(char-hdl).
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

&SCOPED-DEFINE cellColumnDat browsers-item

{methods/browsers/setCellColumns.i}

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN rm/dRMExport.w (INPUT auto_find,
                     INPUT browse-order,
                     INPUT "2").
/* RUN cec/citm-exp.w (auto_find,browse-order). */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-i-no B-table-Win 
PROCEDURE get-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-i-no LIKE item.i-no NO-UNDO.


  op-i-no = IF AVAIL item THEN item.i-no ELSE "".

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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v-called-setCellColumns = NO THEN DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch ('get-last':U).
  IF AVAILABLE item THEN lvLastRowID = ROWID(item).
    
  RUN dispatch ('get-first':U).
  IF AVAILABLE item THEN lvFirstRowID = ROWID(item).
  
  APPLY "value-changed" TO browse-order IN FRAME {&FRAME-NAME}.
  APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-target", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN new-item IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
      {&BROWSE-NAME}:COLUMN-MOVABLE = v-col-move
         {&BROWSE-NAME}:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move .
       /* FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
     DISPLAY FI_moveCol.*/
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipNavType AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opNavType AS CHARACTER NO-UNDO.

  IF ipNavType NE '' THEN
  CASE ipNavType:
    WHEN 'F' THEN do:
        RUN dispatch ('get-first':U).
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-target", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN new-item IN WIDGET-HANDLE(char-hdl).
    END.
    WHEN 'L' THEN do:
        RUN dispatch ('get-last':U).
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-target", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN new-item IN WIDGET-HANDLE(char-hdl).
    END.
    WHEN 'N' THEN 
    do:
       RUN dispatch ('get-next':U).
      
       RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-target", OUTPUT char-hdl).
       IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
          RUN new-item IN WIDGET-HANDLE(char-hdl).
    END.
    WHEN 'P' THEN do:
        RUN dispatch ('get-prev':U).
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"history-target", OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN new-item IN WIDGET-HANDLE(char-hdl).
    END.
  END CASE.
    
  IF ROWID(item) EQ lvLastRowID THEN
  opNavType = 'L'.
      
  IF ROWID(item) EQ lvFirstRowID THEN
  opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-query B-table-Win 
PROCEDURE refresh-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-rowid as rowid no-undo. /* new record's rowid */
  def var i as int no-undo.
  def var lv-rowid as rowid no-undo.  /* current record's rowid */
  def var lv-stat as log no-undo.
  
  lv-rowid = if avail item then rowid(item)  else ?.
  i = browse {&browse-name}:focused-row.
  
 /*  run dispatch ('open-query') .   - update yes/no message pop-up */
/* {&open-query-{&browse-name}} */

  
  {&open-query-{&browse-name}}
  lv-stat = browse {&browse-name}:set-repositioned-row(i,"always").
  
  reposition {&browse-name} to rowid ip-rowid /*lv-rowid*/ .
  run dispatch ('row-available'). 


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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-image-proc B-table-Win 
PROCEDURE spec-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lspec AS LOGICAL NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

 
 IF AVAIL ITEM THEN
   lspec = CAN-FIND(FIRST notes WHERE
            notes.rec_key = ITEM.rec_key AND
            notes.note_type = "S").

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'spechk-target':U, OUTPUT char-hdl).
  
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT lspec).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

