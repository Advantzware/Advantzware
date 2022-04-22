&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: poinq\f-porec.w
 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */


/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{methods/template/brwcustomdef.i}

{sys/inc/VAR.i NEW SHARED}

cocode = g_company.

DEF TEMP-TABLE temp-rec NO-UNDO
    FIELD item-no AS CHAR
    FIELD job-no AS CHAR
    FIELD job-no2 AS INT
    FIELD trans-date AS DATE
    FIELD whs AS CHAR
    FIELD bin AS CHAR
    FIELD qty AS DEC
    FIELD tag AS CHARACTER
    FIELD po-line AS INTEGER
    FIELD qty-uom AS CHARACTER
    FIELD po-price AS DECIMAL 
    FIELD po-price-uom AS CHARACTER
    FIELD vendor AS CHARACTER
    FIELD invoice AS CHARACTER    
    FIELD priceDesc AS CHARACTER 
    INDEX temp-rec-idx trans-date ASC.

DEF BUFFER b-po-ord FOR po-ord.
DEF BUFFER b-po-ordl FOR po-ordl.
DEF VAR lv-sort-by AS cha NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-sort-by-lab AS cha NO-UNDO.
DEF VAR v-col-move AS LOG INIT YES NO-UNDO.

&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES temp-rec

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 temp-rec.po-line temp-rec.item-no temp-rec.trans-date temp-rec.job-no temp-rec.job-no2 temp-rec.whs temp-rec.bin temp-rec.qty temp-rec.tag  temp-rec.qty-uom temp-rec.po-price temp-rec.po-price-uom temp-rec.vendor temp-rec.invoice temp-rec.priceDesc
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH temp-rec  
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH temp-rec.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 temp-rec
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 temp-rec


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      temp-rec SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 F-Frame-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
    temp-rec.po-line COLUMN-LABEL "PO Line" LABEL-FONT 6 FORMAT ">>9" WIDTH 10
    temp-rec.item-no COLUMN-LABEL "Item No" LABEL-FONT 6 FORMAT "X(15)" WIDTH 20
    temp-rec.trans-date COLUMN-LABEL "Receipt Date" LABEL-FONT 6 FORMAT "99/99/9999" WIDTH 15
    temp-rec.job-no COLUMN-LABEL "Job #" FORMAT "X(9)" LABEL-FONT 6 WIDTH 13
    temp-rec.job-no2 COLUMN-LABEL "" FORMAT ">>>" LABEL-FONT 6 WIDTH 5.4
    temp-rec.whs COLUMN-LABEL "Warehouse" FORMAT "X(5)" LABEL-FONT 6 WIDTH 13
    temp-rec.bin COLUMN-LABEL "Bin" FORMAT "X(8)" LABEL-FONT 6
    temp-rec.qty COLUMN-LABEL "Qty." FORMAT "->,>>>,>>9.9<<" LABEL-FONT 6 WIDTH 20
    temp-rec.tag COLUMN-LABEL "Tag" FORMAT "X(20)" LABEL-FONT 6 
    temp-rec.qty-uom COLUMN-LABEL "Qty UOM" LABEL-FONT 6 FORMAT "x(3)" WIDTH 10
    temp-rec.po-price COLUMN-LABEL "PO Price" LABEL-FONT 6 FORMAT "->>,>>>,>>9.99<<" WIDTH 20
    temp-rec.po-price-uom COLUMN-LABEL "Price UOM" LABEL-FONT 6 FORMAT "x(3)" WIDTH 15
    temp-rec.vendor COLUMN-LABEL "Vendor" LABEL-FONT 6 FORMAT "x(10)" WIDTH 13
    temp-rec.invoice COLUMN-LABEL "Invoice" LABEL-FONT 6 FORMAT "x(20)" WIDTH 30
    temp-rec.priceDesc COLUMN-LABEL "Quantity Price/Uom" LABEL-FONT 6 FORMAT "x(40)" WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 145 BY 16.33
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-4 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1  
         SIZE 148.8 BY 17.00
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
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
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 17
         WIDTH              = 149.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-4 1 F-Main */

   BROWSE-4:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH temp-rec.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

/* ************************  Control Triggers  ************************ */
&Scoped-define BROWSE-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 F-Frame-Win
ON START-SEARCH OF BROWSE-4 IN FRAME F-Main
DO:
    {methods/template/sortindicator.i} 
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by = lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ASSIGN
     lv-sort-by     = lv-column-nam.
 /*    lv-sort-by-lab = lv-column-lab.
*/    
    
  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*APPLY "choose" TO btn-inq. */
  CASE lv-column-nam:
      WHEN "item-no" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.item-no.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.item-no {&sortby-des}.           
      END.
      WHEN "trans-date" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.trans-date.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.trans-date {&sortby-des}.           
      END.
      WHEN "job-no" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.job-no.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.job-no {&sortby-des}.
      END.
      WHEN "job-no2" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.job-no2.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.job-no2 {&sortby-des}.
      END.
      WHEN "whs" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.whs.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.whs {&sortby-des}.
      END.
      WHEN "bin" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.bin.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.bin {&sortby-des}.
      END.
      WHEN "qty" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.qty.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.qty {&sortby-des}.
      END.
      WHEN "tag" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.tag.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.tag {&sortby-des}.
      END.  
      WHEN "po-line" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.po-line.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.po-line {&sortby-des}.
      END. 
      WHEN "qty-uom" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.qty-uom.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.qty-uom {&sortby-des}.
      END.
      WHEN "po-price" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.po-price.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.po-price {&sortby-des}.
      END.
      WHEN "po-price-uom" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.po-price-uom.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.po-price-uom {&sortby-des}.
      END. 
      WHEN "vendor" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.vendor.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.vendor {&sortby-des}.
      END.
      WHEN "invoice" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.invoice.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.invoice {&sortby-des}.
      END.
      WHEN "priceDesc" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.priceDesc.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH temp-rec BY temp-rec.priceDesc {&sortby-des}.
      END.
       
      
  END CASE.
  {methods/template/sortindicatorend.i}         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */
{methods/template/brwcustom.i}

&SCOPED-DEFINE cellColumnDat poinq-po-rec

{methods/browsers/setCellColumns.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF
PROCEDURE browse-identifier:
/* Purpose: To make external programs identify this is a navigation brower using current procedure handle. */
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BROWSE-4 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize F-Frame-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-open AS LOG INIT ? NO-UNDO.
  DEFINE VARIABLE cScreenType AS CHARACTER NO-UNDO.
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN setCellColumns.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populate-tt F-Frame-Win 
PROCEDURE populate-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-po-no  AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.

   DEFINE VARIABLE v-po-no AS CHARACTER NO-UNDO.

   v-po-no = STRING(ip-po-no).

   EMPTY TEMP-TABLE temp-rec.

   FOR EACH rm-rcpth fields(r-no rita-code i-no job-no job-no2 trans-date po-line pur-uom vend-no)NO-LOCK
       WHERE rm-rcpth.company  EQ cocode 
       AND rm-rcpth.po-no      EQ v-po-no 
       AND rm-rcpth.rita-code  EQ "R",
       EACH rm-rdtlh FIELDS(loc loc-bin qty tag cost rec_key)NO-LOCK
       WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no 
         AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code:
   
       CREATE temp-rec.
       ASSIGN temp-rec.item-no = rm-rcpth.i-no
              temp-rec.job-no = rm-rcpth.job-no
              temp-rec.job-no2 = rm-rcpth.job-no2
              temp-rec.trans-date = rm-rcpth.trans-date
              temp-rec.whs = rm-rdtlh.loc
              temp-rec.bin = rm-rdtlh.loc-bin
              temp-rec.qty = rm-rdtlh.qty
              temp-rec.tag = rm-rdtlh.tag
              temp-rec.po-line = rm-rcpth.po-line
              temp-rec.qty-uom = rm-rcpth.pur-uom
              temp-rec.po-price = rm-rdtlh.cost
              temp-rec.po-price-uom = rm-rcpth.pur-uom
              temp-rec.vendor = rm-rcpth.vend-no 
              temp-rec.priceDesc = STRING(rm-rdtlh.qty) + "@ $" + STRING(rm-rdtlh.cost) + "/" + rm-rcpth.pur-uom
           .
           RUN pGetInvoiceNo(INPUT rm-rdtlh.rec_key, OUTPUT temp-rec.invoice).             

       RELEASE temp-rec.
   END.
   
   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ cocode AND
        b-po-ord.po-no EQ ip-po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN DO:
      FOR EACH b-po-ordl NO-LOCK 
         WHERE b-po-ordl.company EQ cocode
           AND b-po-ordl.po-no   EQ ip-po-no,
          EACH  fg-rcpth FIELDS(r-no rita-code i-no job-no job-no2 trans-date po-line pur-uom vend-no) NO-LOCK
          WHERE fg-rcpth.company                 EQ cocode 
            AND fg-rcpth.po-no                   EQ v-po-no 
            AND fg-rcpth.i-no                    EQ b-po-ordl.i-no 
            AND LOOKUP(fg-rcpth.rita-code,"R,E") GT 0
            AND fg-rcpth.po-line                 EQ b-po-ordl.LINE,
          EACH  fg-rdtlh FIELDS(loc loc-bin qty tag cost rec_key) NO-LOCK
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no 
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code :

          CREATE temp-rec.
          ASSIGN temp-rec.item-no = fg-rcpth.i-no
                 temp-rec.job-no = fg-rcpth.job-no
                 temp-rec.job-no2 = fg-rcpth.job-no2
                 temp-rec.trans-date = fg-rcpth.trans-date
                 temp-rec.whs = fg-rdtlh.loc
                 temp-rec.bin = fg-rdtlh.loc-bin
                 temp-rec.qty = fg-rdtlh.qty
                 temp-rec.tag = fg-rdtlh.tag
                 temp-rec.po-line = fg-rcpth.po-line
                 temp-rec.qty-uom = fg-rcpth.pur-uom
                 temp-rec.po-price = fg-rdtlh.cost
                 temp-rec.po-price-uom = fg-rcpth.pur-uom
                 temp-rec.vendor = fg-rcpth.vend-no 
                 temp-rec.priceDesc = STRING(fg-rdtlh.qty) + "@ $" + STRING(fg-rdtlh.cost) + "/" + fg-rcpth.pur-uom
                 .
                 RUN pGetInvoiceNo(INPUT fg-rdtlh.rec_key, OUTPUT temp-rec.invoice).

          RELEASE temp-rec.
      END.
   END. 
   {&open-query-browse-4}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "temp-rec"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns F-Frame-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        BROWSE-4:COLUMN-MOVABLE = v-col-move
        BROWSE-4:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move.
       
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetInvoiceNo F-Frame-Win 
PROCEDURE pGetInvoiceNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcRecKey   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcInvoice AS CHARACTER NO-UNDO.
  FIND FIRST POReceiptLink NO-LOCK
       WHERE POReceiptLink.inventoryStockRecKey EQ ipcRecKey NO-ERROR.
   IF AVAIL POReceiptLink THEN
   DO:
        FIND FIRST ap-invl NO-LOCK 
             WHERE ap-invl.rec_key EQ POReceiptLink.apInvoiceLineRecKey NO-ERROR.
        IF AVAIL ap-invl THEN do:
         FIND FIRST ap-inv NO-LOCK 
              WHERE ap-inv.company EQ cocode
              AND ap-inv.i-no EQ ap-invl.i-no NO-ERROR.
         IF avail ap-inv THEN
             opcInvoice = ap-inv.inv-no .
        END. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
