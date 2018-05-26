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
DEFINE VARIABLE hdPriceProcs AS HANDLE.
{oe/ttPriceHold.i "NEW SHARED"}
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.

def var li-rels as int form ">9"no-undo.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

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
&Scoped-define EXTERNAL-TABLES quoteitm
&Scoped-define FIRST-EXTERNAL-TABLE quoteitm


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR quoteitm.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES quoteqty

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table quoteqty.qty quoteqty.price ~
quoteqty.uom quoteqty.profit quoteqty.rels quoteqty.mat-cost ~
quoteqty.lab-cost quoteqty.fo-cost quoteqty.vo-cost ~
tot-msf() @ quoteqty.tot-lbs quoteqty.quote-date quoteqty.quote-user 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table quoteqty.qty ~
quoteqty.price quoteqty.uom quoteqty.profit quoteqty.rels quoteqty.mat-cost ~
quoteqty.lab-cost quoteqty.fo-cost quoteqty.vo-cost quoteqty.quote-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table quoteqty
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table quoteqty
&Scoped-define QUERY-STRING-Browser-Table FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company ~
  AND quoteqty.loc = quoteitm.loc ~
  AND quoteqty.q-no = quoteitm.q-no ~
  AND quoteqty.line = quoteitm.line NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company ~
  AND quoteqty.loc = quoteitm.loc ~
  AND quoteqty.q-no = quoteitm.q-no ~
  AND quoteqty.line = quoteitm.line NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table quoteqty
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table quoteqty


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tot-msf B-table-Win 
FUNCTION tot-msf RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      quoteqty
    FIELDS(quoteqty.qty
      quoteqty.price
      quoteqty.uom
      quoteqty.profit
      quoteqty.rels
      quoteqty.mat-cost
      quoteqty.lab-cost
      quoteqty.fo-cost
      quoteqty.vo-cost
      quoteqty.tot-lbs
      quoteqty.quote-date
      quoteqty.quote-user) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      quoteqty.qty FORMAT ">>>,>>>,>>9":U
      quoteqty.price FORMAT ">>,>>9.9999":U
      quoteqty.uom FORMAT "x(3)":U
      quoteqty.profit FORMAT "->>9.99%":U
      quoteqty.rels COLUMN-LABEL "Rel" FORMAT ">>9":U
      quoteqty.mat-cost COLUMN-LABEL "Mat'l Cost/M" FORMAT "->>>,>>9.99<<<":U
      quoteqty.lab-cost COLUMN-LABEL "DL Cost/M" FORMAT "->>>,>>9.99<<<":U
      quoteqty.fo-cost COLUMN-LABEL "FO Cost/M" FORMAT "->>>,>>9.99<<<":U
      quoteqty.vo-cost COLUMN-LABEL "VO Cost/M" FORMAT "->>>,>>9.99<<<":U
      tot-msf() @ quoteqty.tot-lbs COLUMN-LABEL "Total!MSF" FORMAT ">>>>9.99":U
      quoteqty.quote-date FORMAT "99/99/9999":U
      quoteqty.quote-user FORMAT "X(8)":U
  ENABLE
      quoteqty.qty
      quoteqty.price
      quoteqty.uom
      quoteqty.profit
      quoteqty.rels
      quoteqty.mat-cost
      quoteqty.lab-cost
      quoteqty.fo-cost
      quoteqty.vo-cost
      quoteqty.quote-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 11.43
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.quoteitm
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
         HEIGHT             = 11.48
         WIDTH              = 145.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.quoteqty WHERE ASI.quoteitm  ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "ASI.quoteqty.company = ASI.quoteitm.company
  AND ASI.quoteqty.loc = ASI.quoteitm.loc
  AND ASI.quoteqty.q-no = ASI.quoteitm.q-no
  AND ASI.quoteqty.line = ASI.quoteitm.line"
     _FldNameList[1]   > ASI.quoteqty.qty
"quoteqty.qty" ? ">>>,>>>,>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.quoteqty.price
"quoteqty.price" ? ">>,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.quoteqty.uom
"quoteqty.uom" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.quoteqty.profit
"quoteqty.profit" ? "->>9.99%" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.quoteqty.rels
"quoteqty.rels" "Rel" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.quoteqty.mat-cost
"quoteqty.mat-cost" "Mat'l Cost/M" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.quoteqty.lab-cost
"quoteqty.lab-cost" "DL Cost/M" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.quoteqty.fo-cost
"quoteqty.fo-cost" "FO Cost/M" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.quoteqty.vo-cost
"quoteqty.vo-cost" "VO Cost/M" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"tot-msf() @ quoteqty.tot-lbs" "Total!MSF" ">>>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.quoteqty.quote-date
"quoteqty.quote-date" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = ASI.quoteqty.quote-user
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quoteqty.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quoteqty.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quoteqty.qty IN BROWSE Browser-Table /* Qty */
DO:
  FIND FIRST quotehd OF quoteitm NO-LOCK NO-ERROR.

  IF AVAIL quotehd THEN DO:
    {est/quoprice.i "quoteqty"}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quoteqty.uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quoteqty.uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF quoteqty.uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN
 cocode = g_company
 locode = g_loc.

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
  {src/adm/template/row-list.i "quoteitm"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "quoteitm"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/   

  /* Code placed here will execute PRIOR to standard behavior. */
  DEFINE VARIABLE lvQty AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvPrice AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvUom AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvRels AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-tot-cost AS DEC NO-UNDO.
  DEFINE VARIABLE iPrvQty AS INTEGER NO-UNDO.
  DEFINE VARIABLE rowidqty AS ROWID NO-UNDO.
  IF AVAIL quoteqty THEN DO:
      ASSIGN
          lvPrice = quoteqty.price 
          lvUom = quoteqty.uom 
          iPrvQty = quoteqty.qty
          .
  END.
  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN quoteqty.quote-user = USERID('nosweat')
        /* quoteqty.quote-date = TODAY*/ .

  /* update rfqitem qty - start */
  FIND FIRST quotehd OF quoteitm NO-LOCK NO-ERROR.
 
  IF AVAIL quoteitm AND AVAIL(quotehd) THEN
      FIND FIRST itemfg
          WHERE itemfg.company  EQ quoteitm.company
            AND itemfg.part-no  EQ quoteitm.part-no
            AND itemfg.part-no  NE ""
            AND (itemfg.cust-no EQ quotehd.cust-no OR
                 itemfg.i-code  EQ "S")
          NO-LOCK NO-ERROR.
      
  IF AVAIL itemfg AND (quoteqty.price NE lvPrice OR quoteqty.uom NE lvUom) THEN DO:
    v-tot-cost = quoteqty.mat-cost + quoteqty.lab-cost 
             + quoteqty.fo-cost
             + quoteqty.vo-cost.

    IF quotehd.est-no = "" AND itemfg.sell-price GT 0 THEN DO:

         CASE quoteqty.uom:
          WHEN "EA" THEN
               quoteqty.profit = ((quoteqty.price * 1000) - v-tot-cost) / (quoteqty.price * 1000) * 100.
           WHEN "M" THEN
               quoteqty.profit = ((quoteqty.price) - v-tot-cost) / (quoteqty.price)  * 100.
           WHEN "CS" THEN
               quoteqty.profit = ((quoteqty.price / itemfg.case-count * 1000) - v-tot-cost) / (quoteqty.price / itemfg.case-count * 1000)  * 100.
           WHEN "LOT" THEN
               quoteqty.profit = ((quoteqty.price / quoteit.qty * 1000) - v-tot-cost) / (quoteqty.price / quoteit.qty * 1000)  * 100.
         END CASE.
         BROWSE {&BROWSE-NAME}:REFRESH().
    END. /* if est-no = "" */
  END.

  IF AVAILABLE quoteqty AND quoteqty.qty NE iPrvQty AND NOT adm-new-record  THEN DO:
  
      FOR EACH quotechg EXCLUSIVE-LOCK
         WHERE quotechg.company EQ quoteqty.company 
           AND quotechg.loc EQ quoteqty.loc 
           AND quotechg.q-no EQ quoteqty.q-no 
           AND ASI.quotechg.line EQ quoteqty.line
           AND quotechg.qty EQ iPrvQty  :
          ASSIGN quotechg.qty = quoteqty.qty  .
      END.
      ASSIGN rowidqty = ROWID(quoteqty) .
       BROWSE {&BROWSE-NAME}:REFRESH().
       RUN dispatch ('open-query').
       REPOSITION {&browse-name} TO ROWID rowidqty NO-ERROR.
       RUN dispatch IN THIS-PROCEDURE ("row-changed").
       APPLY "value-changed" TO BROWSE {&browse-name}.
  END.

   IF NOT AVAILABLE quotehd OR quotehd.rfq EQ '' THEN RETURN.
  {custom/rfq-qty.i}
  /* update rfqitem qty - end */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 assign quoteqty.company = quoteitm.company
        quoteqty.loc = quoteitm.loc
        quoteqty.q-no = quoteitm.q-no
        quoteqty.line = quoteitm.line
        quoteqty.quote-date = TODAY
        quoteqty.quote-user = USERID("nosweat")
        .

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
  {custom/askdel.i}

  /* Code Placed here after confirm delete*/
  FOR EACH quotechg EXCLUSIVE-LOCK
      WHERE quotechg.company EQ quoteqty.company 
        AND quotechg.loc EQ quoteqty.loc 
        AND quotechg.q-no EQ quoteqty.q-no 
        AND ASI.quotechg.line EQ quoteqty.line
        AND quotechg.qty EQ quoteqty.qty  :
      DELETE quotechg .
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprice-quote B-table-Win 
PROCEDURE reprice-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-factor AS DEC NO-UNDO.
  DEF VAR ls-reprice-to AS cha NO-UNDO.
  DEF VAR lv-cas-cnt LIKE eb.cas-cnt NO-UNDO.

  DEF BUFFER bf-qty FOR quoteqty.


  MESSAGE "Are you sure you wish to Reprice this quote?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTON YES-NO UPDATE choice AS LOG.

  IF choice THEN DO:
    
    RUN est/g-qtrprc.w (OUTPUT ls-reprice-to).
    IF ls-reprice-to NE "" THEN DO:
        FOR EACH quotehd OF quoteitm NO-LOCK,
            EACH bf-qty
            WHERE bf-qty.company EQ quoteitm.company 
              AND bf-qty.loc     EQ quoteitm.loc
              AND bf-qty.q-no    EQ quoteitm.q-no
              AND bf-qty.line    EQ quoteitm.line
              AND bf-qty.uom     NE ls-reprice-to:
    
          FIND FIRST eb
              WHERE eb.company EQ quotehd.company
                AND eb.est-no  EQ quotehd.est-no
                AND eb.part-no EQ quoteitm.part-no
              NO-LOCK NO-ERROR.
    
          ASSIGN 
           lv-cas-cnt   = IF AVAIL eb AND eb.cas-cnt NE 0 THEN eb.cas-cnt ELSE 1
           bf-qty.price = IF bf-qty.uom EQ "CS" THEN
                            (bf-qty.price * bf-qty.qty / lv-cas-cnt)
                          ELSE
                          IF bf-qty.uom EQ "EA" THEN
                            (bf-qty.price * bf-qty.qty)
                          ELSE
                          IF bf-qty.uom EQ "MSF" THEN
                            (bf-qty.price * bf-qty.tot-lbs / 1000)
                          ELSE
                          IF bf-qty.uom EQ "M" THEN
                            (bf-qty.price * bf-qty.qty / 1000)
                          ELSE /*L*/
                             0
    
           bf-qty.uom   = ls-reprice-to 
           bf-qty.price = IF bf-qty.uom EQ "CS" THEN
                            (bf-qty.price / (bf-qty.qty / lv-cas-cnt))
                          ELSE
                          IF bf-qty.uom EQ "EA" THEN
                            (bf-qty.price / bf-qty.qty)
                          ELSE
                          IF bf-qty.uom EQ "MSF" THEN
                            (bf-qty.price / (bf-qty.tot-lbs / 1000))
                          ELSE
                          IF bf-qty.uom EQ "M" THEN
                            (bf-qty.price / (bf-qty.qty / 1000))
                          ELSE /*L*/
                             0.
    
          IF bf-qty.qty EQ quoteitm.qty THEN DO:
            FIND CURRENT quoteitm EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL quoteitm THEN
              ASSIGN
               quoteitm.uom   = bf-qty.uom
               quoteitm.price = bf-qty.price.
            FIND CURRENT quoteitm NO-LOCK NO-ERROR.
          END.
          
        END. /* each quotehd */
        
        /* connect to rfq database - start */
        FIND FIRST asi.module NO-LOCK WHERE module.module EQ 'rfq' NO-ERROR.
        IF AVAILABLE module AND module.is-used THEN DO:
          IF module.expire-date EQ ? OR module.expire-date GE TODAY THEN DO:
            IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
            CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.
          END. /* expire-date */
        END. /* avail module */
        /* connect to rfq database - end */
        
        IF CONNECTED('rfq') THEN
        FOR EACH quotehd OF quoteitm NO-LOCK,
            EACH bf-qty NO-LOCK
            WHERE bf-qty.company EQ quoteitm.company 
              AND bf-qty.loc     EQ quoteitm.loc
              AND bf-qty.q-no    EQ quoteitm.q-no
              AND bf-qty.line    EQ quoteitm.line:
          IF quotehd.rfq NE '' THEN
          RUN custom/rfq-qty.p (quotehd.company,quotehd.loc,quotehd.est-no,
                                quotehd.rfq,quoteitm.part-no,bf-qty.qty,
                                bf-qty.price,bf-qty.uom,TODAY,bf-qty.rels).
        END. /* each quotehd */
        
        /* disconnect to rfq database */
        /* IF CONNECTED('rfq') THEN DISCONNECT rfq. */

    END. /* if a reprice method was chosen */

  END. /* if choice */

  RUN dispatch ('open-query').

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
  {src/adm/template/snd-list.i "quoteitm"}
  {src/adm/template/snd-list.i "quoteqty"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-uom LIKE uom.uom NO-UNDO.
  DEF VAR uom-list AS CHAR INIT "M,C,EA,CS" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN sys/ref/uom-fg.p (NO ,OUTPUT uom-list).

    ASSIGN
     quoteqty.uom:SCREEN-VALUE IN BROWSE {&browse-name} =
         CAPS(quoteqty.uom:SCREEN-VALUE IN BROWSE {&browse-name})
     lv-uom = quoteqty.uom:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF LOOKUP(lv-uom,uom-list) LE 0 THEN DO:
      MESSAGE "UOM must be " + TRIM(uom-list) VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO quoteqty.uom IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tot-msf B-table-Win 
FUNCTION tot-msf RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   
IF AVAIL quoteqty THEN RETURN round(quoteqty.tot-lbs / 1000,2).
ELSE RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

