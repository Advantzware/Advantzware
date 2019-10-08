&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

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

&SCOPED-DEFINE dataGridInclude dataGrid\oe\b-oeretl2.i
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.
/* Parameters Definitions ---                                           */
DEF VAR iprOeReth AS ROWID NO-UNDO.
/* Local Variable Definitions ---                                       */

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-reth oe-retl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table oe-reth.ra-no oe-reth.cust-no ~
oe-retl.i-no oe-retl.i-name oe-retl.ord-no oe-retl.tag ~
oe-retl.tot-qty-return oe-retl.qty-return-inv 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH oe-reth WHERE ~{&KEY-PHRASE} ~
      AND ROWID(oe-reth) eq iprOeReth NO-LOCK, ~
      EACH oe-retl WHERE oe-retl.company = oe-reth.company ~
  AND oe-retl.ra-no = oe-reth.ra-no NO-LOCK ~
    BY oe-reth.applied
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH oe-reth WHERE ~{&KEY-PHRASE} ~
      AND ROWID(oe-reth) eq iprOeReth NO-LOCK, ~
      EACH oe-retl WHERE oe-retl.company = oe-reth.company ~
  AND oe-retl.ra-no = oe-reth.ra-no NO-LOCK ~
    BY oe-reth.applied.
&Scoped-define TABLES-IN-QUERY-br_table oe-reth oe-retl
&Scoped-define FIRST-TABLE-IN-QUERY-br_table oe-reth
&Scoped-define SECOND-TABLE-IN-QUERY-br_table oe-retl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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
company||y|ASI.oe-retl.company
r-no||y|ASI.oe-retl.r-no
uom||y|ASI.oe-retl.uom
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,r-no,uom"':U).

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
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      oe-reth, 
      oe-retl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      oe-reth.ra-no FORMAT ">>>>>9":U
      oe-reth.cust-no FORMAT "x(8)":U
      oe-retl.i-no FORMAT "x(15)":U
      oe-retl.i-name FORMAT "x(30)":U
      oe-retl.ord-no COLUMN-LABEL "Order#" FORMAT ">>>>>9":U
      oe-retl.tag FORMAT "x(20)":U
      oe-retl.tot-qty-return FORMAT "->,>>>,>>9.99":U
      oe-retl.qty-return-inv FORMAT "->,>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 114 BY 6.71
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 9.14
         WIDTH              = 114.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

{Advantzware/WinKit/dataGridProc.i}

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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "asi.oe-reth,asi.oe-retl WHERE asi.oe-reth ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "asi.oe-reth.applied|yes"
     _Where[1]         = "ROWID(oe-reth) eq iprOeReth"
     _JoinCode[2]      = "asi.oe-retl.company = asi.oe-reth.company
  AND asi.oe-retl.ra-no = asi.oe-reth.ra-no"
     _FldNameList[1]   = asi.oe-reth.ra-no
     _FldNameList[2]   = asi.oe-reth.cust-no
     _FldNameList[3]   = ASI.oe-retl.i-no
     _FldNameList[4]   = ASI.oe-retl.i-name
     _FldNameList[5]   > ASI.oe-retl.ord-no
"oe-retl.ord-no" "Order#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-retl.tag
"oe-retl.tag" ? "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = ASI.oe-retl.tot-qty-return
     _FldNameList[8]   = ASI.oe-retl.qty-return-inv
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
   {src/adm/template/brsleave.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-navigation B-table-Win 
PROCEDURE Disable-navigation :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR char-hdl AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
  RUN get-rowid IN widget-handle(char-hdl) (OUTPUT iprOeReth).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  RUN pDataGridInit. /* added by script _admBrowsers.p */

  /* Code placed here will execute AFTER standard behavior.    */
  RUN local-open-query.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

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
BROWSE {&browse-name}:REFRESH() NO-ERROR.
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
  DEF INPUT PARAM ip-recid AS RECID.

  RUN dispatch("open-query").
  IF ip-recid <> ? THEN DO:
     REPOSITION {&browse-name} TO RECID ip-recid.
     RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-bin B-table-Win 
PROCEDURE select-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cnt AS INT NO-UNDO.
 DEF VAR lrCurRow AS ROWID NO-UNDO.
 DEF VAR v-line AS INT NO-UNDO.
 DEF BUFFER bf-oe-boll FOR oe-boll.
 DEF BUFFER bf-oe-retl FOR oe-retl.
 DEF VAR char-hdl AS CHAR NO-UNDO.
 DEF VAR char-val AS CHAR NO-UNDO.
 DEF BUFFER bf-retl FOR oe-retl .

 RUN windows/l-fgtg3.w (g_company,"",
                         "", string(oe-reth.inv-no),
                         "Multiple",
                         OUTPUT char-val).

  IF char-val <> "" THEN do:
    RUN tags-from-list (INPUT char-val).
     run oe/oe-retup.p (recid(oe-reth)).
     RUN local-open-query.
     run get-link-handle in adm-broker-hdl (this-procedure,"record-source",output char-hdl).
     RUN reopen-query IN widget-handle(char-hdl) (INPUT RECID(oe-reth)).

  END. /* if char-val <> "" */

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
  {src/adm/template/sndkycas.i "company" "oe-retl" "company"}
  {src/adm/template/sndkycas.i "r-no" "oe-retl" "r-no"}
  {src/adm/template/sndkycas.i "uom" "oe-retl" "uom"}

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
  {src/adm/template/snd-list.i "oe-reth"}
  {src/adm/template/snd-list.i "oe-retl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tags-from-list B-table-Win 
PROCEDURE tags-from-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER char-val AS CHAR.
 DEF VAR cnt AS INT NO-UNDO.
 DEF VAR lrCurRow AS ROWID NO-UNDO.
 DEF VAR v-line AS INT NO-UNDO.
 DEF BUFFER bf-oe-boll FOR oe-boll.
 DEF BUFFER bf-oe-retl FOR oe-retl.
 DEF VAR char-hdl AS CHAR NO-UNDO.
 DEF BUFFER bf-retl FOR oe-retl .

DO cnt = 1 TO NUM-ENTRIES(char-val).
   lrCurRow = TO-ROWID(ENTRY(cnt, char-val)).
   FIND bf-oe-boll WHERE ROWID(bf-oe-boll) EQ lrCurRow
      NO-LOCK NO-ERROR.

     FIND itemfg WHERE itemfg.company EQ bf-oe-boll.company
        AND itemfg.i-no EQ bf-oe-boll.i-no
       NO-LOCK NO-ERROR.

     FIND FIRST bf-oe-retl WHERE bf-oe-retl.company EQ oe-reth.company
       AND bf-oe-retl.r-no EQ oe-reth.r-no
       AND bf-oe-retl.tag EQ bf-oe-boll.tag
       NO-LOCK NO-ERROR.
     IF AVAIL bf-oe-retl THEN DO:
       MESSAGE "Tag # " + bf-oe-boll.tag + " is already on this return and will not be added again."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
       NEXT.
     END.
     CREATE bf-oe-retl.
     find last bf-retl where bf-retl.company = cocode and
                                 bf-retl.r-no    = oe-reth.r-no
                                   use-index r-no no-lock no-error.
     if available bf-retl THEN assign v-line = bf-retl.line + 1.
     ELSE assign v-line = 1.

     FIND CURRENT oe-reth EXCLUSIVE-LOCK.
     assign bf-oe-retl.company = oe-reth.company
                bf-oe-retl.ra-no   = oe-reth.ra-no
                bf-oe-retl.r-no    = oe-reth.r-no
                bf-oe-retl.line    = v-line
                /*oel_id          = recid(bf-retl)
                nufile          = yes */
                oe-reth.applied = yes.
     ASSIGN     bf-oe-retl.tag    = bf-oe-boll.tag
                bf-oe-retl.i-no   = bf-oe-boll.i-no
                bf-oe-retl.loc = bf-oe-boll.loc
                bf-oe-retl.loc-bin = bf-oe-boll.loc-bin
                bf-oe-retl.tot-qty-return = bf-oe-boll.qty
                bf-oe-retl.qty-return-inv = bf-oe-boll.qty
                bf-oe-retl.ord-no = bf-oe-boll.ord-no
                bf-oe-retl.job-no = bf-oe-boll.job-no
                bf-oe-retl.job-no2 = bf-oe-boll.job-no2
                bf-oe-retl.po-no   = bf-oe-boll.po-no.
     IF AVAIL itemfg THEN
       ASSIGN
         bf-oe-retl.i-name = itemfg.i-name
         bf-oe-retl.part-no = itemfg.part-no.
     FIND CURRENT oe-reth no-LOCK.                      
     RUN values-from-ar-inv (ROWID(bf-oe-retl)).
END.
/*      run oe/oe-retup.p (recid(oe-reth)). */
     RUN local-open-query.
/*      run get-link-handle in adm-broker-hdl (this-procedure,"record-source",output char-hdl). */
/*      RUN reopen-query IN widget-handle(char-hdl) (INPUT RECID(oe-reth)).                     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE values-from-ar-inv B-table-Win 
PROCEDURE values-from-ar-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER iprOeRetl AS ROWID NO-UNDO.
 DEF BUFFER bf-oe-retl FOR oe-retl.
 FIND bf-oe-retl WHERE ROWID(bf-oe-retl) EQ iprOeRetl 
   EXCLUSIVE-LOCK NO-ERROR.
 IF NOT AVAIL bf-oe-retl THEN
   RETURN.



 FIND FIRST ar-inv NO-LOCK
      WHERE ar-inv.company EQ oe-reth.company
        AND ar-inv.cust-no EQ oe-reth.cust-no
        AND ar-inv.inv-no  EQ oe-reth.inv-no
      USE-INDEX ar-inv NO-ERROR.

  IF AVAIL ar-inv THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND ar-invl NO-LOCK
        WHERE ar-invl.x-no EQ ar-inv.x-no
          AND ar-invl.i-no EQ bf-oe-retl.i-no
        USE-INDEX x-no NO-ERROR.

    IF NOT AVAIL ar-invl THEN
    FIND FIRST ar-invl NO-LOCK
        WHERE ar-invl.x-no   EQ ar-inv.x-no
          AND ar-invl.i-no   EQ bf-oe-retl.i-no
          AND (ar-invl.ord-no EQ INT(bf-oe-retl.ord-no) OR
               INT(bf-oe-retl.ord-no) EQ 0)
          AND (ar-invl.po-no  EQ bf-oe-retl.po-no OR
               bf-oe-retl.po-no EQ "")
        USE-INDEX x-no NO-ERROR.

    IF AVAIL ar-invl THEN
    DO:
      ASSIGN
       bf-oe-retl.i-name         = ar-invl.i-name         
       bf-oe-retl.i-dscr         = ar-invl.i-dscr
       bf-oe-retl.job-no         = ar-invl.job-no
       bf-oe-retl.job-no2        = ar-invl.job-no2
       bf-oe-retl.cost           = ar-invl.cost
       bf-oe-retl.ord-no         = ar-invl.ord-no
       bf-oe-retl.po-no          = ar-invl.po-no
       bf-oe-retl.uom            = ar-invl.pr-uom
       bf-oe-retl.est-no         = ar-invl.est-no
       bf-oe-retl.part-no        = ar-invl.part-no
       bf-oe-retl.unit-pr        = ar-invl.unit-pr.
       IF bf-oe-retl.qty-return-inv EQ 0 OR bf-oe-retl.tot-qty-return EQ 0 THEN
         ASSIGN bf-oe-retl.qty-return-inv = ar-invl.ship-qty
                bf-oe-retl.tot-qty-return = ar-invl.ship-qty.
       IF bf-oe-retl.loc EQ "" THEN
          bf-oe-retl.loc            = ar-invl.loc.


    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

