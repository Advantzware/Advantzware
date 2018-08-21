&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: est\b-estprp.w

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


&SCOPED-DEFINE yellowColumnsName b-estprp           /*Task# 01211414*/
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR li AS INT NO-UNDO.
DEF TEMP-TABLE tt-est-prep LIKE est-prep FIELD row-id AS ROWID INDEX row-id row-id.

{est/d-selblk.i NEW}
{sys/inc/ceprepprice.i}

&SCOPED-DEFINE sortby-phrase BY est-prep.s-num BY est-prep.b-num BY est-prep.CODE

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
&Scoped-define EXTERNAL-TABLES est
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-prep

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table est-prep.s-num est-prep.b-num ~
est-prep.code est-prep.qty est-prep.dscr est-prep.simon est-prep.cost ~
est-prep.mkup est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table est-prep.s-num ~
est-prep.b-num est-prep.code est-prep.qty est-prep.dscr est-prep.simon ~
est-prep.cost est-prep.mkup est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table est-prep
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table est-prep
&Scoped-define QUERY-STRING-br_table FOR EACH est-prep WHERE est-prep.company = est.company ~
  AND est-prep.est-no = est.est-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH est-prep WHERE est-prep.company = est.company ~
  AND est-prep.est-no = est.est-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table est-prep
&Scoped-define FIRST-TABLE-IN-QUERY-br_table est-prep


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
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
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
      est-prep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      est-prep.s-num COLUMN-LABEL "Sht #" FORMAT ">>>":U
      est-prep.b-num COLUMN-LABEL "B #" FORMAT ">>>":U
      est-prep.code FORMAT "x(20)":U WIDTH 20
      est-prep.qty FORMAT "->>,>>9.9":U
      est-prep.dscr FORMAT "x(20)":U
      est-prep.simon FORMAT "X":U
      est-prep.cost FORMAT "->>,>>9.99":U
      est-prep.mkup FORMAT "->>9.9999":U WIDTH 13
      est-prep.spare-dec-1 COLUMN-LABEL "Price" FORMAT "->>,>>9.99":U
      est-prep.ml FORMAT "M/L":U
      est-prep.amtz COLUMN-LABEL "Amort" FORMAT ">>9.99":U
  ENABLE
      est-prep.s-num
      est-prep.b-num
      est-prep.code
      est-prep.qty
      est-prep.dscr
      est-prep.simon
      est-prep.cost
      est-prep.mkup
      est-prep.spare-dec-1
      est-prep.ml
      est-prep.amtz
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 123 BY 7.86
         FONT 0
         TITLE "Preparation" ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.est
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
         HEIGHT             = 8.1
         WIDTH              = 123.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

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
     _TblList          = "ASI.est-prep WHERE ASI.est <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.est-prep.company = ASI.est.company
  AND ASI.est-prep.est-no = ASI.est.est-no"
     _FldNameList[1]   > ASI.est-prep.s-num
"est-prep.s-num" "Sht #" ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.est-prep.b-num
"est-prep.b-num" "B #" ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.est-prep.code
"est-prep.code" ? "x(20)" "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.est-prep.qty
"est-prep.qty" ? "->>,>>9.9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.est-prep.dscr
"est-prep.dscr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.est-prep.simon
"est-prep.simon" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.est-prep.cost
"est-prep.cost" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.est-prep.mkup
"est-prep.mkup" ? "->>9.9999" "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.est-prep.spare-dec-1
"est-prep.spare-dec-1" "Price" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.est-prep.ml
"est-prep.ml" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.est-prep.amtz
"est-prep.amtz" "Amort" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON return OF br_table IN FRAME F-Main /* Preparation */
anywhere
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
    DO:
        DEFINE VARIABLE phandle  AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE char-hdl AS cha           NO-UNDO.   

        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"buttons-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) 
            THEN RUN browser-dbclicked IN WIDGET-HANDLE(char-hdl).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Preparation */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Preparation */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
     {est/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main /* Preparation */
DO:
    
    /* gdm - 09190806 */
    RUN StartSearch.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Preparation */
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

{custom/yellowColumns2.i}           /*Task# 01211414*/

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
  {src/adm/template/row-list.i "est"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reft4plate B-table-Win 
PROCEDURE create-reft4plate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-prep-cnt AS INT NO-UNDO.
  DEF VAR lv-returnc AS cha NO-UNDO.
  DEF VAR lv-form# AS INT NO-UNDO.
  DEF VAR lv-line# AS INT NO-UNDO.
  DEF VAR lv-eqty AS INT NO-UNDO.

  FIND FIRST oe-ordm WHERE oe-ordm.company = est.company
                 AND oe-ordm.ord-no = est.ord-no
                 AND oe-ordm.charge = est-prep.CODE NO-ERROR.
  IF AVAIL oe-ordm AND 
     (oe-ordm.miscType <> 1 OR
      oe-ordm.estPrepEqty <> est-prep.eqty OR
      oe-ordm.estPrepLine <> est-prep.line)      
     
  THEN DO:
      ASSIGN 
             oe-ordm.miscType = 1
             oe-ordm.estPrepEqty   = est-prep.eqty
             oe-ordm.estPrepLine   = est-prep.line
             oe-ordm.est-no  = est-prep.est-no.  
    RELEASE oe-ordm.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

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

  ASSIGN 
      est-prep.s-num:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.b-num:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.code:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.qty:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.dscr:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.simon:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.cost:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.mkup:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.spare-dec-1:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.ml:READ-ONLY IN BROWSE {&browse-name} = YES
      est-prep.amtz:READ-ONLY IN BROWSE {&browse-name} = YES
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-b-num LIKE est-prep.b-num NO-UNDO.
  DEF VAR li-line AS INT NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
 DEF VAR op-prep AS LOG NO-UNDO .
  DEF BUFFER b-est-prep FOR est-prep.


  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  IF AVAIL est-prep THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-rowid = ROWID(est-prep)
     lv-b-num = INT(est-prep.b-num:SCREEN-VALUE IN BROWSE {&browse-name}).

    RELEASE eb.

    IF NOT CAN-FIND(eb WHERE eb.company EQ est.company
                         AND eb.est-no  EQ est.est-no
                         AND eb.form-no NE 0) THEN
    FIND FIRST eb
        WHERE eb.company   EQ est.company
          AND eb.est-no    EQ est.est-no
          AND eb.form-no   EQ INT(est-prep.s-num:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (eb.blank-no EQ lv-b-num OR lv-b-num EQ 0)
        NO-LOCK NO-ERROR.

    IF AVAIL eb THEN DO:
      RUN est/d-selblkp.w (ROWID(eb), "Copy " + TRIM({&browse-name}:TITLE),OUTPUT op-prep).
      
      
      FOR EACH tt-select WHERE tt-selected,
          FIRST eb WHERE ROWID(eb) EQ tt-rowid
          BREAK BY eb.form-no
                BY eb.blank-no:

       /* IF FIRST-OF(eb.form-no) OR lv-b-num NE 0 THEN DO:*/ /* task 4231527 */
          FIND FIRST b-est-prep
                WHERE b-est-prep.company EQ est-prep.company
                AND b-est-prep.est-no  EQ est-prep.est-no
                AND b-est-prep.code    EQ est-prep.code
                AND b-est-prep.s-num   EQ eb.form-no
                AND b-est-prep.b-num   EQ eb.blank-no
                NO-ERROR.

            IF AVAIL b-est-prep AND op-prep THEN
                ASSIGN
                b-est-prep.qty   = est-prep.qty
                b-est-prep.dscr  = est-prep.dscr
                b-est-prep.cost  = est-prep.cost
                b-est-prep.ml    = est-prep.ml
                b-est-prep.simon = est-prep.simon
                b-est-prep.mkup  = est-prep.mkup
                b-est-prep.amtz  = est-prep.amtz 
                b-est-prep.spare-dec-1 = est-prep.spare-dec-1 .

            ELSE DO:
             FIND LAST b-est-prep NO-LOCK
                WHERE b-est-prep.company EQ est-prep.company
                  AND b-est-prep.est-no  EQ est-prep.est-no
                  AND b-est-prep.eqty    EQ est-prep.eqty
                USE-INDEX est-qty NO-ERROR.
            li-line = IF AVAIL b-est-prep THEN b-est-prep.LINE + 1 ELSE 1.

            CREATE b-est-prep.

            BUFFER-COPY est-prep EXCEPT rec_key TO b-est-prep
            ASSIGN
             b-est-prep.line  = li-line
             b-est-prep.s-num = eb.form-no
             b-est-prep.b-num = eb.blank-no.
          END.

          lv-rowid = ROWID(b-est-prep).
        /*END.*/
      END.

      RUN repo-query (lv-rowid).
    END.

    ELSE DO:
      /* Dispatch standard ADM method.                             */
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

      /* Code placed here will execute AFTER standard behavior.    */
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-custom AS LOG INIT NO NO-UNDO.
DEF BUFFER b-est-prep FOR est-prep.

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

 /* IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END. */

  /* Dispatch standard ADM method.                             */ 
IF NOT adm-new-record THEN do:
  run est/d-delall.w (OUTPUT v-custom).
  IF v-custom = NO THEN
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  ELSE IF v-custom = YES THEN DO:
     FOR EACH b-est-prep
        WHERE b-est-prep.company EQ est-prep.company
          AND b-est-prep.est-no  EQ est-prep.est-no
        EXCLUSIVE-LOCK:
      DELETE b-est-prep .
    END.
    RUN dispatch ("open-query").
  END.
END.
ELSE
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

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
/*   IF AVAIL est-prep THEN RUN UpdatePrice. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO est-prep.s-num IN BROWSE {&browse-name}.
  END.

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

  run dispatch ('open-query'). /* reopen */

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
    RUN dispatch ('open-query').
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-job-stds B-table-Win 
PROCEDURE run-job-stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF AVAIL est THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ est.company
        AND job-hdr.est-no  EQ est.est-no
      NO-LOCK,
      FIRST job
      where job.company EQ job-hdr.company
        and job.job     EQ job-hdr.job
        and job.job-no  EQ job-hdr.job-no
        and job.job-no2 EQ job-hdr.job-no2
        and job.est-no  EQ job-hdr.est-no
        AND job.opened  EQ YES
      NO-LOCK
      BREAK BY job.job:
      
    IF LAST(job.job) OR job-hdr.ord-no EQ est.ord-no THEN DO:
      RUN jc/jobstds.p (ROWID(job)).
      LEAVE.
    END.
  END.

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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "est-prep"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateMarkup B-table-Win 
PROCEDURE UpdateMarkup :
/*------------------------------------------------------------------------------
  Purpose:    Calculates Markup
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dCost AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMkup AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPrice AS DECIMAL     NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
        dCost =  DEC(est-prep.cost:SCREEN-VALUE IN BROWSE {&browse-name})
        dPrice =  DEC(est-prep.spare-dec-1:SCREEN-VALUE IN BROWSE {&browse-name}).
   
    IF ceprepprice-chr EQ "Profit" THEN
        dMkup = (1 - dCost / dPrice) * 100. 
    ELSE
        dMkup = (dPrice / dCost - 1) * 100.
    
    IF dMkup LT 1000 AND dMkup GT -1000  THEN
        est-prep.mkup:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dMkup).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdatePrice B-table-Win 
PROCEDURE UpdatePrice :
/*------------------------------------------------------------------------------
  Purpose:    Calculates Price and displays it
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dCost AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMkup AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPrice AS DECIMAL     NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
        dCost =  DEC(est-prep.cost:SCREEN-VALUE IN BROWSE {&browse-name})
        dMkup =  DEC(est-prep.mkup:SCREEN-VALUE IN BROWSE {&browse-name}).
    IF ceprepprice-chr EQ "Profit" THEN
        dPrice = dCost / (1 - (dMkup / 100)).
    ELSE
        dPrice = dCost * (1 + (dMkup / 100)).
    est-prep.spare-dec-1:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dPrice).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

