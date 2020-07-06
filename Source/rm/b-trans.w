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
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED} 

ASSIGN
 cocode = g_company
 locode = g_loc.

def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.

def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

DO TRANSACTION:
  {sys/inc/rmrecpt.i}
END.

{rm/ttRmRctd.i new shared}

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
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 rm-rctd.loc-bin2 rm-rctd.tag2 ~
rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 rm-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "T" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "T" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
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
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U
      rm-rctd.rct-date COLUMN-LABEL "Transfer!Date" FORMAT "99/99/9999":U
            WIDTH 15
      rm-rctd.i-no FORMAT "x(10)":U WIDTH 15
      rm-rctd.i-name FORMAT "x(30)":U
      rm-rctd.tag COLUMN-LABEL "From!Tag" FORMAT "x(20)":U WIDTH 28
      rm-rctd.loc COLUMN-LABEL "From!Whs" FORMAT "x(5)":U WIDTH 8
      rm-rctd.loc-bin COLUMN-LABEL "From!Bin" FORMAT "x(8)":U WIDTH 12
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
      rm-rctd.loc2 COLUMN-LABEL "To !Whs" FORMAT "x(5)":U WIDTH 8
      rm-rctd.loc-bin2 COLUMN-LABEL "To!Bin" FORMAT "x(8)":U WIDTH 12
      rm-rctd.tag2 COLUMN-LABEL "To !Tag" FORMAT "x(20)":U WIDTH 28
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U WIDTH 15
  ENABLE
      rm-rctd.rct-date
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.tag
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.qty
      rm-rctd.pur-uom HELP "Unit of Measure"
      rm-rctd.loc2
      rm-rctd.loc-bin2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 96 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


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
         HEIGHT             = 17.14
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
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       rm-rctd.pur-uom:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""T"""
     _FldNameList[1]   > asi.rm-rctd.r-no
"r-no" "Seq#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.rct-date
"rct-date" "Transfer!Date" ? "date" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.i-no
"i-no" ? ? "character" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.i-name
"i-name" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.tag
"tag" "From!Tag" "x(20)" "character" ? ? ? ? ? ? yes ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.loc
"loc" "From!Whs" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.loc-bin
"loc-bin" "From!Bin" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.pur-uom
"pur-uom" "UOM" ? "character" ? ? ? ? ? ? yes "Unit of Measure" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.loc2
"loc2" "To !Whs" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.loc-bin2
"loc-bin2" "To!Bin" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.tag2
"tag2" "To !Tag" "x(20)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.user-id
"user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO.

    IF AVAILABLE rm-rctd THEN DO:
        RUN rm/d-trans.w (RECID(rm-rctd), "update", OUTPUT lv-rowid) . 
        RUN repo-query (lv-rowid).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /* {src/adm/template/brsleave.i}*/
   {est/brsleave.i}  /* same as src but update will be same as add record*/

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
{custom/getcmpny.i}
{custom/getloc.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item B-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(hd-post-child) then  hd-post-child:sensitive = yes.
            /* value assigned from local-enable-fields*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
    assign
     hd-post-child = hd-post:current-window
     hd-post-child = hd-post-child:first-child /* frame */
     hd-post-child = hd-post-child:first-child /* field-group */
     hd-post-child = hd-post-child:first-child /* field */
     hd-post-child:sensitive = no.
  end.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply "entry" to rm-rctd.tag2 in browse {&browse-name}.
  return no-apply.
 
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


  RUN dispatch ('open-query').

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch ('row-changed').

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
  {src/adm/template/snd-list.i "rm-rctd"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord B-table-Win 
PROCEDURE pAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
  DEFINE BUFFER bff-rm-rctd FOR rm-rctd .

   RUN rm/d-trans.w (?,"add", OUTPUT lv-rowid) . 
   FIND FIRST bff-rm-rctd NO-LOCK
       WHERE bff-rm-rctd.company EQ cocode
       AND ROWID(bff-rm-rctd) EQ lv-rowid NO-ERROR .
   IF AVAIL bff-rm-rctd THEN
       RUN repo-query (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord B-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE rm-rctd THEN
    DO:
       RUN rm/d-trans.w (RECID(rm-rctd), "update", OUTPUT lv-rowid) . 
       RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord B-table-Win 
PROCEDURE pViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    
    IF AVAILABLE rm-rctd THEN
    DO:
       RUN rm/d-trans.w (RECID(rm-rctd),"view", OUTPUT lv-rowid) . 
       RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyRecord B-table-Win 
PROCEDURE pCopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID   NO-UNDO. 
    DEF VAR lv-rno LIKE rm-rctd.r-no NO-UNDO.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd .
    
    IF AVAILABLE rm-rctd THEN
    DO:
        RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

       CREATE bf-rm-rctd.
       BUFFER-COPY rm-rctd EXCEPT r-no rec_key TO bf-rm-rctd.
       ASSIGN bf-rm-rctd.r-no = lv-rno.

       RUN rm/d-trans.w (RECID(bf-rm-rctd),"copy", OUTPUT lv-rowid) . 
       IF lv-rowid NE ? THEN
           RUN repo-query (lv-rowid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMultiTrans B-table-Win 
PROCEDURE pMultiTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lv-rno LIKE rm-rctd.r-no NO-UNDO.
   DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
   
   RUN rm/dMultiTrans.w .
   
    FOR EACH tt-rm-rctd NO-LOCK:
     RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     
     CREATE bf-rm-rctd.
        ASSIGN 
            bf-rm-rctd.company   = cocode
            bf-rm-rctd.r-no      = lv-rno
            bf-rm-rctd.rita-code = "T"
            bf-rm-rctd.s-num     = 0
            bf-rm-rctd.rct-date  = TODAY
            bf-rm-rctd.i-no      = tt-rm-rctd.i-no
            bf-rm-rctd.loc       = tt-rm-rctd.loc
            bf-rm-rctd.loc-bin   = tt-rm-rctd.loc-bin
            bf-rm-rctd.tag       = tt-rm-rctd.tag 
            bf-rm-rctd.tag2      = tt-rm-rctd.tag2
            bf-rm-rctd.qty       = tt-rm-rctd.qty             
            bf-rm-rctd.rct-date  = tt-rm-rctd.rct-date
            bf-rm-rctd.i-name    = tt-rm-rctd.i-name               
            bf-rm-rctd.pur-uom   = tt-rm-rctd.pur-uom 
            bf-rm-rctd.loc2      = tt-rm-rctd.loc2
            bf-rm-rctd.loc-bin2  = tt-rm-rctd.loc-bin2
            bf-rm-rctd.USER-ID   = USERID(LDBNAME(1))  
            .      
    END.
     RUN repo-query (ROWID(bf-rm-rctd)).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  run get-matrix (true).
  return ext-cost.
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

