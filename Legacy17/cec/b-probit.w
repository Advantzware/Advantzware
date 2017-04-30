&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

&SCOPED-DEFINE yellowColumnsName probeit

DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xeb FOR eb.
DEF VAR ld-prev-sell-price AS DEC NO-UNDO.

DEF BUFFER b-probeit FOR probeit.

ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES probe
&Scoped-define FIRST-EXTERNAL-TABLE probe


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR probe.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES probeit eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table probeit.cust-no probeit.part-no probeit.bl-qty probeit.yld-qty probeit.fact-cost probeit.full-cost probeit.sell-price probeit.YRprice   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table probeit.sell-price   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table probeit
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table probeit
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH probeit WHERE probeit.company = probe.company   AND probeit.est-no = probe.est-no   AND probeit.line = probe.line NO-LOCK, ~
             FIRST eb WHERE eb.company = probeit.company   AND eb.est-no = probeit.est-no   AND eb.part-no = probeit.part-no NO-LOCK     ~{&SORTBY-PHRASE}     BY eb.form-no     BY eb.blank-no
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH probeit WHERE probeit.company = probe.company   AND probeit.est-no = probe.est-no   AND probeit.line = probe.line NO-LOCK, ~
             FIRST eb WHERE eb.company = probeit.company   AND eb.est-no = probeit.est-no   AND eb.part-no = probeit.part-no NO-LOCK     ~{&SORTBY-PHRASE}     BY eb.form-no     BY eb.blank-no.
&Scoped-define TABLES-IN-QUERY-br_table probeit eb
&Scoped-define FIRST-TABLE-IN-QUERY-br_table probeit
&Scoped-define SECOND-TABLE-IN-QUERY-br_table eb


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table btn-mul 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.probeit.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-mul 
     LABEL "Update Multiple Sell Prices" 
     SIZE 29 BY 1.14.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      probeit, 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      probeit.cust-no FORMAT "x(8)":U
      probeit.part-no COLUMN-LABEL "Part Number" FORMAT "x(20)":U
            LABEL-BGCOLOR 14
      probeit.bl-qty COLUMN-LABEL "Requested Qty" FORMAT ">>>,>>>,>>>":U
      probeit.yld-qty FORMAT ">>>,>>>,>>>":U
      probeit.fact-cost COLUMN-LABEL "Fact!Cost/M" FORMAT ">>>,>>9.99":U
      probeit.full-cost COLUMN-LABEL "Full!Cost/M" FORMAT ">>>,>>9.99":U
      probeit.sell-price COLUMN-LABEL "Sell!Price/M" FORMAT ">>>,>>9.99":U
      probeit.YRprice COLUMN-LABEL "On" FORMAT "Y/R":U
  ENABLE
      probeit.sell-price
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 131 BY 11.19
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     fi_sortby AT ROW 6.48 COL 100 COLON-ALIGNED NO-LABEL
     btn-mul AT ROW 12.33 COL 55.2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.probe
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 13.52
         WIDTH              = 131.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH probeit WHERE probeit.company = probe.company
  AND probeit.est-no = probe.est-no
  AND probeit.line = probe.line NO-LOCK,
      FIRST eb WHERE eb.company = probeit.company
  AND eb.est-no = probeit.est-no
  AND eb.part-no = probeit.part-no NO-LOCK
    ~{&SORTBY-PHRASE}
    BY eb.form-no
    BY eb.blank-no.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _JoinCode[1]      = "ASI.probeit.company = ASI.probe.company
  AND ASI.probeit.est-no = ASI.probe.est-no
  AND ASI.probeit.line = ASI.probe.line"
     _JoinCode[2]      = "ASI.eb.company = ASI.probeit.company
  AND ASI.eb.est-no = ASI.probeit.est-no
  AND ASI.eb.part-no = ASI.probeit.part-no"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i}*/
   {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
   RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-mul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-mul B-table-Win
ON CHOOSE OF btn-mul IN FRAME F-Main /* Update Multiple Sell Prices */
DO:
   RUN cec\b-updmul.w(INPUT probe.company,
                      INPUT locode,
                      INPUT probe.est-no,
                      INPUT probe.LINE).

   RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/yellowColumns.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.

  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN DO:
       &Scope KEY-PHRASE probeit.company eq key-value
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* company */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
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
  {src/adm/template/row-list.i "probe"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "probe"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avg-price B-table-Win 
PROCEDURE avg-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-label AS CHAR NO-UNDO.


  IF AVAIL probeit THEN DO:
    RUN avg-price-label (OUTPUT lv-label).

    IF lv-label BEGINS "&Avg" THEN
    FOR EACH b-probeit
        WHERE b-probeit.company EQ probe.company
          AND b-probeit.est-no  EQ probe.est-no
          AND b-probeit.line    EQ probe.line:

      b-probeit.sell-price = probe.sell-price.
    END.

    ELSE
    FOR EACH b-probeit
        WHERE b-probeit.company EQ probe.company
          AND b-probeit.est-no  EQ probe.est-no
          AND b-probeit.line    EQ probe.line:

      b-probeit.sell-price = ROUND(b-probeit.full-cost / probe.full-cost *
                                   probe.sell-price,2).
    END.

    RUN repo-query (ROWID(probeit)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avg-price-label B-table-Win 
PROCEDURE avg-price-label :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-label AS CHAR NO-UNDO.


  IF CAN-FIND(FIRST est
              WHERE est.company   EQ probe.company
                AND est.est-no    EQ probe.est-no
                AND (est.est-type EQ 4 OR est.est-type EQ 8)) THEN
    op-label = IF NOT CAN-FIND(FIRST b-probeit NO-LOCK
                               WHERE b-probeit.company    EQ probe.company
                                 AND b-probeit.est-no     EQ probe.est-no
                                 AND b-probeit.line       EQ probe.line
                                 AND b-probeit.full-cost  NE probe.full-cost) THEN ""
               ELSE
               IF CAN-FIND(FIRST b-probeit NO-LOCK
                           WHERE b-probeit.company    EQ probe.company
                             AND b-probeit.est-no     EQ probe.est-no
                             AND b-probeit.line       EQ probe.line
                             AND b-probeit.sell-price NE probe.sell-price) THEN
                 "&Avg Price" ELSE "&Price per Item".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-fields B-table-Win 
PROCEDURE calc-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.
  DEF VAR lv-changed AS cha NO-UNDO.

  DEF BUFFER b-probemk FOR reftable.


  FIND xest WHERE xest.company = probe.company
              AND xest.est-no = probe.est-no NO-LOCK NO-ERROR.

  {cec/combasis.i}
      
      
  FIND FIRST b-probemk
      WHERE b-probemk.reftable EQ "ce/com/probemk.p"
        AND b-probemk.company  EQ probeit.company
        AND b-probemk.loc      EQ probeit.est-no
        AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
        AND b-probemk.code2    EQ probeit.part-no
      NO-ERROR.
  IF AVAIL b-probemk THEN
    v-com = b-probemk.val[2] + b-probemk.val[3] +
            b-probemk.val[4] + b-probemk.val[5].

  {sys/inc/ceround.i}
  lv-changed = "S".

  IF lv-changed NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ld-price = ld-prev-sell-price
     ld-factc = DEC(probeit.fact-cost:SCREEN-VALUE IN BROWSE {&browse-name})
     ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                (v-com / 100)   
     ld-fullc = DEC(probeit.full-cost:SCREEN-VALUE IN BROWSE {&browse-name}) - ld-commc.

    IF lv-changed EQ "S" THEN
      ASSIGN
       ld-price = DEC(probeit.sell-price:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).
/*
    ELSE DO:
      v-pct = IF lv-changed EQ "G" THEN
                DEC(probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name})
              ELSE
                DEC(probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name}).
        RUN custom/sellpric.p (lv-changed,
                             v-basis,
                             ld-factc,
                             ld-fullc - ld-factc,
                             v-com,
                             v-pct,
                             OUTPUT ld-price,
                             OUTPUT ld-commc).      
  
*/

    ld-fullc = ld-fullc + ld-commc.
    FIND CURRENT probeit.
    probeit.full-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING(ld-fullc,probeit.full-cost:FORMAT IN BROWSE {&browse-name}) NO-ERROR.
    ASSIGN probeit.full-cost = DEC(probeit.full-cost:SCREEN-VALUE IN BROWSE {&browse-name})
   
/*
    IF lv-changed NE "S" AND NOT ERROR-STATUS:ERROR THEN
      probe.sell-price:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(ld-price,probe.sell-price:FORMAT IN BROWSE {&browse-name}) NO-ERROR.
        
    IF lv-changed NE "N" AND NOT ERROR-STATUS:ERROR THEN
      probe.net-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING((1 - (ld-fullc / ld-price)) * 100) NO-ERROR.

    IF lv-changed NE "G" AND NOT ERROR-STATUS:ERROR THEN
      probe.gross-profit:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING((1 - (ld-factc / ld-price)) * 100) NO-ERROR.
*/      
    lv-changed = "".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-proc B-table-Win 
PROCEDURE close-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ld-prev-sell-price = probeit.sell-price.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN calc-fields.
 /* MESSAGE "update" probeit.sell-price VIEW-AS ALERT-BOX.
*/

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

                              
  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").

    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

    RUN dispatch ("row-changed").

    APPLY "entry" TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "probeit" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "probe"}
  {src/adm/template/snd-list.i "probeit"}
  {src/adm/template/snd-list.i "eb"}

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

