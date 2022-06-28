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

&SCOPED-DEFINE yellowColumnsName item-pos
&SCOPED-DEFINE autoFind
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

{custom/gcompany.i}

def var ls-vend-name like vend.name no-undo.
def var li-ord-qty like po-ordl.cons-qty no-undo.
def var li-due-qty like po-ordl.cons-qty no-undo.

DEFINE TEMP-TABLE tt-item-po 
       FIELD cItem AS CHARACTER 
       FIELD iPoNo AS INTEGER COLUMN-LABEL "Purchase Order#"
       FIELD iPoLine AS INTEGER  COLUMN-LABEL "Line#"
       FIELD cVendName AS CHARACTER FORMAT "x(30)"
       FIELD dtDueDate AS DATE  COLUMN-LABEL "Due Date"
       FIELD iQtyOrdered AS INTEGER
       FIELD iQtyDue AS INTEGER
       FIELD cShipId AS CHARACTER COLUMN-LABEL "Ship To"
       FIELD cJobNo  AS CHARACTER
       FIELD rec_key AS CHARACTER.

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
&Scoped-define INTERNAL-TABLES tt-item-po

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-item-po.iPoNo tt-item-po.cVendName tt-item-po.dtDueDate tt-item-po.iQtyOrdered tt-item-po.iQtyDue tt-item-po.cShipId   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table   
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-item-po NO-LOCK WHERE (tt-item-po.iPoNo EQ INTEGER(auto_find) OR INTEGER(auto_find) EQ 0) AND ((tt-item-po.iQtyDue GE 0 AND rd_select EQ "On Order") OR rd_select EQ "All")     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-item-po NO-LOCK WHERE (tt-item-po.iPoNo EQ INTEGER(auto_find) OR INTEGER(auto_find) EQ 0) AND ((tt-item-po.iQtyDue GE 0 AND rd_select EQ "On Order") OR rd_select EQ "All")     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-item-po
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-item-po


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 rd_select auto_find ~
Btn_ok Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS lbl_select rd_select auto_find fi_sortby 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convert-qty B-table-Win 
FUNCTION convert-qty RETURNS DECIMAL
  ( input ip-type as INT, input  ip-qty AS dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD vend-name B-table-Win 
FUNCTION vend-name RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON Btn_ok 
     LABEL "&Go" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "PO #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE lbl_select AS CHARACTER FORMAT "X(256)":U INITIAL "Select By ?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE rd_select AS CHARACTER INITIAL "On Order" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On Order", "On Order",
"All", "All"
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-item-po SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table DISPLAY
      tt-item-po.iPoNo FORMAT ">>>>>>>>9":U LABEL-BGCOLOR 14
      tt-item-po.cVendName COLUMN-LABEL "Vendor"
      tt-item-po.dtDueDate FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      tt-item-po.iQtyOrdered COLUMN-LABEL "Qty Ordered"
      tt-item-po.iQtyDue COLUMN-LABEL "Qty Due"
      tt-item-po.cShipId FORMAT "x(8)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     lbl_select AT ROW 19.29 COL 40.2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     rd_select AT ROW 19.29 COL 55.2 NO-LABEL WIDGET-ID 4
     auto_find AT ROW 19.33 COL 11 COLON-ALIGNED HELP
          "Enter PO # "
     fi_sortby AT ROW 19.33 COL 85 COLON-ALIGNED
     Btn_ok AT ROW 19.33 COL 118 HELP
          "CLEAR AUTO FIND Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 19.1 COL 1
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
                "6"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN lbl_select IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       lbl_select:PRIVATE-DATA IN FRAME F-Main     = 
                "rd_select".

ASSIGN 
       rd_select:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-item-po NO-LOCK
WHERE (tt-item-po.iPoNo EQ INTEGER(auto_find) OR INTEGER(auto_find) EQ 0)
AND ((tt-item-po.iQtyDue GE 0 AND rd_select EQ "On Order") OR rd_select EQ "All")
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
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
  //{methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear_Find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear_Find B-table-Win
ON CHOOSE OF Btn_Clear_Find IN FRAME F-Main /* Clear Find */
DO:
   ASSIGN
     auto_find:SCREEN-VALUE = "0"
     rd_select:SCREEN-VALUE = "All"
     auto_find = 0
     rd_select = "All".
    
    RUN local-open-query.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ok B-table-Win
ON CHOOSE OF Btn_ok IN FRAME F-Main /* Go */
DO:
   ASSIGN
     auto_find
     rd_select
     .        
    RUN local-open-query.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_select B-table-Win
ON VALUE-CHANGED OF rd_select IN FRAME F-Main
DO:
        ASSIGN {&self-name}.
        ASSIGN    
           auto_find
           rd_select
           .        
        RUN local-open-query.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:                 /** BUILD JOB WORK FILE **/
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH po-ordl NO-LOCK
        WHERE po-ordl.company   EQ item.company  
          AND po-ordl.i-no      EQ item.i-no 
          AND po-ordl.item-type EQ YES 
          AND LOOKUP(po-ordl.stat, "A,C,N,O,P,U") GT 0,  
        FIRST po-ord NO-LOCK
        WHERE po-ord.company EQ po-ordl.company
          AND po-ord.po-no   EQ po-ordl.po-no 
          AND LOOKUP(po-ord.stat, "H,N,O,R,U") GT 0
        :                   
        CREATE tt-item-po.
        ASSIGN 
            tt-item-po.iPoNo        = po-ordl.po-no
            tt-item-po.iPoLine      = po-ordl.LINE                     
            tt-item-po.cItem        = po-ordl.i-no                     
            tt-item-po.cVendName    = vend-name()                      
            tt-item-po.dtDueDate    = po-ordl.due-date                 
            tt-item-po.iQtyOrdered  = convert-qty(2, po-ordl.cons-qty) 
            tt-item-po.iQtyDue      = convert-qty(1, po-ordl.cons-qty) 
            tt-item-po.cShipId      = po-ord.ship-id
            tt-item-po.cJobNo       = po-ordl.job-no
            .            
    END. /* each itemfg-loc */
    
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
  /*
  if not avail po-ordl then return.
  
  find first vend where vend.company = item.company and
                        vend.vend-no = po-ord.vend-no no-lock no-error.
  ls-vend-name = if avail vend then vend.name else "N/A" .
  */
  
/* moved into function convert-qty
  if po-ordl.cons-uom eq item.cons-uom then
     assign   li-ord-qty = po-ordl.cons-qty
              li-due-qty = po-ordl.cons-qty - po-ordl.t-rec-qty. 
  else do:
     cocode = item.company.
     run sys/ref/convquom.p(po-ordl.cons-uom, item.cons-uom,
                         item.basis-w, po-ordl.s-len,
                         po-ordl.s-wid, item.s-dep,
                         po-ordl.cons-qty, output li-ord-qty).
                         
     run sys/ref/convquom.p(po-ordl.cons-uom, item.cons-uom,
                         item.basis-w, po-ordl.s-len,
                         po-ordl.s-wid, item.s-dep,
                         po-ordl.cons-qty - po-ordl.t-rec-qty,
                         output li-due-qty).
  end.
*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE ll-zero  AS LOG       INIT YES NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */
    EMPTY TEMP-TABLE tt-item-po.
    RUN build-table.
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) . 

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
  {src/adm/template/snd-list.i "tt-item-po"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convert-qty B-table-Win 
FUNCTION convert-qty RETURNS DECIMAL
  ( input ip-type as INT, input  ip-qty AS dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li-qty AS DEC NO-UNDO.


  IF NOT AVAIL po-ordl THEN RETURN li-qty.

  ASSIGN
   li-ord-qty = po-ordl.cons-qty
   li-due-qty = 0.

  IF po-ordl.cons-uom NE item.cons-uom THEN
    RUN sys/ref/convquom.p (po-ordl.cons-uom, item.cons-uom,
                            item.basis-w,
                            (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12
                             ELSE po-ordl.s-len),
                            po-ordl.s-wid, item.s-dep,
                            li-ord-qty,
                            OUTPUT li-ord-qty).

  IF ip-type EQ 1 THEN DO:
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ po-ordl.company
          AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
          AND rm-rcpth.po-line   EQ po-ordl.LINE
          AND rm-rcpth.i-no      EQ po-ordl.i-no
          AND rm-rcpth.rita-code EQ "R" NO-LOCK,
        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
          AND rm-rdtlh.job-no  EQ po-ordl.job-no
          AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
          AND rm-rdtlh.s-num   EQ po-ordl.s-num
        NO-LOCK:

      li-qty = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE item.cons-uom THEN DO:
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        RUN sys/ref/convquom.p (rm-rcpth.pur-uom, item.cons-uom,
                                item.basis-w,
                                (IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12
                                 ELSE po-ordl.s-len), po-ordl.s-wid,
                                (IF AVAIL item THEN item.s-dep ELSE 0),
                                li-qty,
                                OUTPUT li-qty).
      END.

      li-due-qty = li-due-qty + li-qty.
    END.

    li-due-qty = li-ord-qty - li-due-qty.
  END.

  IF item.cons-uom EQ "EA" THEN DO:
    {sys/inc/roundup.i li-ord-qty}
    {sys/inc/roundup.i li-due-qty}
  END.

  li-qty = IF ip-type EQ 1 THEN li-due-qty ELSE li-ord-qty.
  RETURN li-qty.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION vend-name B-table-Win 
FUNCTION vend-name RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  if not avail po-ordl then RETURN "".
  
  find first vend where vend.company = item.company and
                        vend.vend-no = po-ord.vend-no no-lock no-error.
  ls-vend-name = if avail vend then vend.name else "N/A" .
  RETURN ls-vend-name .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

