&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: oe\b-ordm.w

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
{custom/globdefs.i}
{sys/inc/var.i new shared }

assign cocode = g_company
       locode = g_loc.
      
{oe/oe-sysct1.i NEW}

{oe/d-selmis.i NEW}
{sys/inc/ceprepprice.i}

DO TRANSACTION:
{sys/inc/OEPrepTaxCode.i}
END.
 

DEFINE VARIABLE lv-new-recid AS RECID NO-UNDO.
DEFINE VARIABLE lv-valid-charge AS LOGICAL NO-UNDO.
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

DEFINE NEW SHARED VARIABLE v-misc AS LOGICAL INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.

DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEFINE NEW SHARED BUFFER xest    FOR est.
DEFINE NEW SHARED BUFFER xef     FOR ef.
DEFINE NEW SHARED BUFFER xeb     FOR eb.


&IF DEFINED(UIB_is_Running) NE 0 &THEN
&SCOPED-DEFINE NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE SmartBrowser
&SCOPED-DEFINE DB-AWARE NO

&SCOPED-DEFINE ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME F-Main
&SCOPED-DEFINE BROWSE-NAME br_table

/* External Tables                                                      */
&SCOPED-DEFINE EXTERNAL-TABLES oe-ord
&SCOPED-DEFINE FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&SCOPED-DEFINE INTERNAL-TABLES oe-ordm

/* Define KEY-PHRASE in case it is used by any query. */
&SCOPED-DEFINE KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&SCOPED-DEFINE FIELDS-IN-QUERY-br_table oe-ordm.charge oe-ordm.amt ~
oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no oe-ordm.cost oe-ordm.ord-i-no ~
oe-ordm.ord-line oe-ordm.po-no-po oe-ordm.s-man[1] oe-ordm.s-pct[1] ~
oe-ordm.s-comm[1] oe-ordm.s-man[2] oe-ordm.s-pct[2] oe-ordm.s-comm[2] ~
oe-ordm.s-man[3] oe-ordm.s-pct[3] oe-ordm.s-comm[3] oe-ordm.tax ~
oe-ordm.spare-char-1 oe-ordm.bill oe-ordm.spare-int-1 oe-ordm.spare-char-2 ~
oe-ordm.est-no oe-ordm.form-no oe-ordm.blank-no 
&SCOPED-DEFINE ENABLED-FIELDS-IN-QUERY-br_table oe-ordm.charge oe-ordm.amt ~
oe-ordm.actnum oe-ordm.dscr oe-ordm.po-no oe-ordm.cost oe-ordm.ord-i-no ~
oe-ordm.ord-line oe-ordm.po-no-po oe-ordm.s-man[1] oe-ordm.s-pct[1] ~
oe-ordm.s-comm[1] oe-ordm.s-man[2] oe-ordm.s-pct[2] oe-ordm.s-comm[2] ~
oe-ordm.s-man[3] oe-ordm.s-pct[3] oe-ordm.s-comm[3] oe-ordm.tax ~
oe-ordm.bill oe-ordm.spare-int-1 oe-ordm.spare-char-2 oe-ordm.est-no ~
oe-ordm.form-no oe-ordm.blank-no 
&SCOPED-DEFINE ENABLED-TABLES-IN-QUERY-br_table oe-ordm
&SCOPED-DEFINE FIRST-ENABLED-TABLE-IN-QUERY-br_table oe-ordm
&SCOPED-DEFINE QUERY-STRING-br_table FOR EACH oe-ordm OF oe-ord WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&SCOPED-DEFINE OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH oe-ordm OF oe-ord WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&SCOPED-DEFINE TABLES-IN-QUERY-br_table oe-ordm
&SCOPED-DEFINE FIRST-TABLE-IN-QUERY-br_table oe-ordm


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS br_table 

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
company||y|ASI.oe-ordm.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&SCOPED-DEFINE OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
      oe-ordm SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      oe-ordm.charge FORMAT "x(20)":U WIDTH 35 COLUMN-FONT 0
      oe-ordm.amt COLUMN-LABEL "Sell Price" FORMAT "->>,>>>,>>9.99":U
            COLUMN-FONT 0
      oe-ordm.actnum COLUMN-LABEL "Account#" FORMAT "x(25)":U COLUMN-FONT 0
      oe-ordm.dscr FORMAT "x(30)":U WIDTH 40 COLUMN-FONT 0
      oe-ordm.po-no COLUMN-LABEL "Customer PO#" FORMAT "x(15)":U
            WIDTH 20
      oe-ordm.cost FORMAT "->>,>>>,>>9.99":U
      oe-ordm.ord-i-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 9
      oe-ordm.ord-line COLUMN-LABEL "" FORMAT "99":U WIDTH 4
      oe-ordm.po-no-po COLUMN-LABEL "Vendor PO#" FORMAT ">>>>>>":U
      oe-ordm.s-man[1] COLUMN-LABEL "Sls Rep" FORMAT "X(5)":U
      oe-ordm.s-pct[1] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
            WIDTH 13
      oe-ordm.s-comm[1] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
            WIDTH 9
      oe-ordm.s-man[2] COLUMN-LABEL "Sls Rep" FORMAT "x(3)":U WIDTH 10.4
      oe-ordm.s-pct[2] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
            WIDTH 13
      oe-ordm.s-comm[2] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
            WIDTH 9
      oe-ordm.s-man[3] COLUMN-LABEL "Sls Rep" FORMAT "x(3)":U
      oe-ordm.s-pct[3] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
            WIDTH 13
      oe-ordm.s-comm[3] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
            WIDTH 9
      oe-ordm.tax FORMAT "Y/N":U COLUMN-FONT 0
      oe-ordm.spare-char-1 COLUMN-LABEL "Tax Prep Code" FORMAT "x(3)":U
            WIDTH 12
      oe-ordm.bill FORMAT "X":U COLUMN-FONT 0
      oe-ordm.spare-int-1 COLUMN-LABEL "Line" FORMAT "->,>>>,>>9":U
            WIDTH 6
      oe-ordm.spare-char-2 COLUMN-LABEL "FG Item Code" FORMAT "x(15)":U
      oe-ordm.est-no COLUMN-LABEL "Estimate" FORMAT "x(12)":U WIDTH 12
      oe-ordm.form-no COLUMN-LABEL "S" FORMAT ">9":U WIDTH 2
      oe-ordm.blank-no COLUMN-LABEL "B" FORMAT ">9":U WIDTH 2
  ENABLE
      oe-ordm.charge
      oe-ordm.amt
      oe-ordm.actnum
      oe-ordm.dscr
      oe-ordm.po-no
      oe-ordm.cost
      oe-ordm.ord-i-no
      oe-ordm.ord-line
      oe-ordm.po-no-po
      oe-ordm.s-man[1]
      oe-ordm.s-pct[1]
      oe-ordm.s-comm[1]
      oe-ordm.s-man[2]
      oe-ordm.s-pct[2]
      oe-ordm.s-comm[2]
      oe-ordm.s-man[3]
      oe-ordm.s-pct[3]
      oe-ordm.s-comm[3]
      oe-ordm.tax
      oe-ordm.spare-char-1
      oe-ordm.bill
      oe-ordm.spare-int-1
      oe-ordm.spare-char-2
      oe-ordm.est-no
      oe-ordm.form-no
      oe-ordm.blank-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 151 BY 10.71
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 0.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.oe-ord
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
         HEIGHT             = 11.48
         WIDTH              = 201.
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.oe-ordm OF ASI.oe-ord"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > ASI.oe-ordm.charge
"oe-ordm.charge" ? ? "character" ? ? 0 ? ? ? yes ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ordm.amt
"oe-ordm.amt" "Sell Price" "->>,>>>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-ordm.actnum
"oe-ordm.actnum" "Account#" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-ordm.dscr
"oe-ordm.dscr" ? "x(30)" "character" ? ? 0 ? ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-ordm.po-no
"oe-ordm.po-no" "Customer PO#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-ordm.cost
"oe-ordm.cost" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-ordm.ord-i-no
"oe-ordm.ord-i-no" "Job#" "x(6)" "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-ordm.ord-line
"oe-ordm.ord-line" "" "99" "integer" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-ordm.po-no-po
"oe-ordm.po-no-po" "Vendor PO#" ">>>>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-ordm.s-man[1]
"oe-ordm.s-man[1]" "Sls Rep" "X(5)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-ordm.s-pct[1]
"oe-ordm.s-pct[1]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-ordm.s-comm[1]
"oe-ordm.s-comm[1]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-ordm.s-man[2]
"oe-ordm.s-man[2]" "Sls Rep" ? "character" ? ? ? ? ? ? yes ? no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-ordm.s-pct[2]
"oe-ordm.s-pct[2]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-ordm.s-comm[2]
"oe-ordm.s-comm[2]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.oe-ordm.s-man[3]
"oe-ordm.s-man[3]" "Sls Rep" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-ordm.s-pct[3]
"oe-ordm.s-pct[3]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-ordm.s-comm[3]
"oe-ordm.s-comm[3]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.oe-ordm.tax
"oe-ordm.tax" ? ? "logical" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.oe-ordm.spare-char-1
"oe-ordm.spare-char-1" "Tax Prep Code" "x(3)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.oe-ordm.bill
"oe-ordm.bill" ? ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.oe-ordm.spare-int-1
"oe-ordm.spare-int-1" "Line" ? "integer" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.oe-ordm.spare-char-2
"oe-ordm.spare-char-2" "FG Item Code" "x(15)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.oe-ordm.est-no
"oe-ordm.est-no" "Estimate" "x(12)" "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.oe-ordm.form-no
"oe-ordm.form-no" "S" ? "integer" ? ? ? ? ? ? yes ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.oe-ordm.blank-no
"oe-ordm.blank-no" "B" ? "integer" ? ? ? ? ? ? yes ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&SCOPED-DEFINE BROWSE-NAME br_table
&SCOPED-DEFINE SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
   
   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
   IF AVAIL oe-ordm THEN do:
       RUN oe/d-ordm.w (ROWID(oe-ordm),ROWID(oe-ord), "update", OUTPUT lv-rowid) .
       RUN reopen-query (lv-rowid).
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON RETURN OF br_table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   /*{src/adm/template/brsleave.i}  */
    {brsleave.i}                  
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

RUN oe/oe-sysct.p.

 IF NOT v-oecomm-log THEN RUN show-comm (NO).

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.    

FIND FIRST cust
      WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
      NO-LOCK NO-ERROR.

{sys/inc/f3help.i}
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assgn-prep-info B-table-Win 
PROCEDURE assgn-prep-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST prep EXCLUSIVE-LOCK
  WHERE prep.company EQ oe-ordm.company 
    AND prep.code    EQ oe-ordm.charge NO-ERROR.
IF AVAIL prep THEN DO:

  ASSIGN prep.last-order  = oe-ordm.ord-no
         prep.last-est-no = oe-ordm.est-no.
  
  FIND CURRENT prep NO-LOCK.
  RELEASE prep.
END.

RELEASE prep.
RELEASE reftable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-invoice B-table-Win 
PROCEDURE create-invoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-r-no LIKE inv-head.r-no NO-UNDO.
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE ls AS CHARACTER NO-UNDO.
  DEFINE VARIABLE v-line-count AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-start-pos AS INTEGER INIT 1 NO-UNDO.
  DEFINE VARIABLE v-li AS INTEGER NO-UNDO.

  DEFINE BUFFER b-oe-ordm FOR oe-ordm.

  IF AVAIL oe-ord THEN DO:
    IF AVAIL oe-ordm THEN lv-rowid = ROWID(oe-ordm).

    RUN oe/d-selmis.w (ROWID(oe-ord)).

    FOR EACH tt-mis WHERE selekt EQ YES,
        FIRST b-oe-ordm WHERE ROWID(b-oe-ordm) EQ tt-mis.row-id
        BREAK BY b-oe-ordm.ord-no:

      IF FIRST(b-oe-ordm.ord-no) THEN DO:
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ oe-ord.company
              AND cust.cust-no EQ oe-ord.cust-no.

        lv-r-no = NEXT-VALUE(inv_r_no_seq).
        
        FOR EACH shipto NO-LOCK
            WHERE shipto.company EQ oe-ord.company
              AND shipto.cust-no EQ oe-ord.cust-no
            BREAK BY shipto.ship-no DESC:
          IF shipto.ship-id EQ shipto.cust-no OR
             LAST(shipto.ship-no)             THEN LEAVE.
        END.

        CREATE inv-head.
        ASSIGN
         inv-head.sold-no      = shipto.ship-id
         inv-head.sold-name    = shipto.ship-name
         inv-head.sold-addr[1] = shipto.ship-addr[1]
         inv-head.sold-addr[2] = shipto.ship-addr[2]
         inv-head.sold-state   = shipto.ship-state
         inv-head.sold-city    = shipto.ship-city
         inv-head.sold-zip     = shipto.ship-zip
         inv-head.r-no         = lv-r-no
         inv-head.company      = oe-ord.company
         inv-head.bill-to      = oe-ord.cust-no
         inv-head.cust-no      = oe-ord.cust-no
         inv-head.frt-pay      = oe-ord.frt-pay
         inv-head.carrier      = oe-ord.carrier
         inv-head.ship-i[1]    = oe-ord.ship-i[1]
         inv-head.ship-i[2]    = oe-ord.ship-i[2]
         inv-head.ship-i[3]    = oe-ord.ship-i[3]
         inv-head.ship-i[4]    = oe-ord.ship-i[4]
         inv-head.fob-code     = oe-ord.fob-code
         inv-head.contact      = oe-ord.contact
         inv-head.terms        = oe-ord.terms
         inv-head.terms-d      = oe-ord.terms-d
         inv-head.f-bill       = NO
         inv-head.tax-gr       = IF AVAIL shipto AND shipto.tax-code NE ""
                                 THEN shipto.tax-code ELSE oe-ord.tax-gr
         inv-head.tot-ord      = 0
         inv-head.inv-no       = 0
         inv-head.stat         = ""
         inv-head.deleted      = NO
         inv-head.posted       = NO
         inv-head.inv-date     = TODAY
         inv-head.cust-name = cust.name
         inv-head.addr[1]   = cust.addr[1]
         inv-head.addr[2]   = cust.addr[2]
         inv-head.city      = cust.city
         inv-head.state     = cust.state
         inv-head.zip       = cust.zip.

        FIND FIRST usergrps NO-LOCK WHERE
             usergrps.usergrps = "IN"
              NO-ERROR.

        IF AVAIL usergrps THEN
        DO:
           DO v-li = 1 TO LENGTH(usergrps.users):
              ls = SUBSTRING(usergrps.users,v-li,1).
             
              IF v-line-count LT 5 AND ls EQ CHR(10) OR ls EQ CHR(13) THEN
                 ASSIGN
                    v-line-count = v-line-count + 1
                    inv-head.bill-i[v-line-count] = SUBSTRING(usergrps.users,v-start-pos,v-li - v-start-pos)
                    v-start-pos = v-li + 1.
           
              IF v-line-count LT 5 AND v-li EQ LENGTH(usergrps.users) AND
                 NOT(ls EQ CHR(10) OR ls EQ CHR(13)) THEN
                 ASSIGN
                    v-line-count = v-line-count + 1
                    inv-head.bill-i[v-line-count] = SUBSTRING(usergrps.users,v-start-pos,v-li - v-start-pos + 1).
           END.
           
           RELEASE usergrps.
        END.

        DO v-li = 1 TO 4:
           IF inv-head.bill-i[v-li] = "" THEN
              inv-head.bill-i[v-li] = oe-ord.bill-i[v-li].
        END.
      END.

      CREATE inv-misc.
      BUFFER-COPY b-oe-ordm EXCEPT rec_key TO inv-misc
      ASSIGN
       inv-misc.r-no           = lv-r-no
       inv-misc.posted         = NO
       inv-misc.deleted        = NO
       inv-misc.inv-i-no       = b-oe-ordm.ord-i-no
       inv-misc.inv-line       = b-oe-ordm.ord-line
       inv-misc.s-commbasis[1] = b-oe-ordm.commbasis[1].

      b-oe-ordm.bill = "I".   /** Set billing flag to (I)nvoiced **/

      IF LAST(b-oe-ordm.ord-no) THEN DO:
        RUN oe/oe-invup.p (ROWID(inv-head), INPUT YES).
        RUN reopen-query (lv-rowid).
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reft4prep B-table-Win 
PROCEDURE create-reft4prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-prep-cnt AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-returnc AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-form# AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-line# AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-eqty AS INTEGER NO-UNDO.

  IF CAN-FIND(FIRST prep WHERE prep.company EQ oe-ord.company 
                   AND prep.code EQ oe-ordm.charge /*AND prep.mat-type = "P"*/) AND
                   AVAIL oe-ordm AND 
                  (oe-ordm.miscType <> 1 )
  THEN DO:
      lv-prep-cnt = 0.
      FOR EACH est-prep NO-LOCK WHERE est-prep.company EQ oe-ordm.company
                          AND est-prep.est-no EQ oe-ordm.est-no 
                          AND est-prep.CODE EQ oe-ordm.charge
                          AND est-prep.simon   EQ "S" :
          lv-prep-cnt = lv-prep-cnt + 1.
      END.
      IF lv-prep-cnt > 1 THEN do:
         RUN oe/d-formno.w (INPUT oe-ordm.est-no, INPUT oe-ordm.charge, INPUT recid(oe-ordm), OUTPUT lv-returnc).
         IF lv-returnc = "" THEN RETURN ERROR.

         ASSIGN lv-form# = INTEGER(ENTRY(2,lv-returnc))
                lv-line# = INTEGER(ENTRY(4,lv-returnc))
                lv-eqty = INTEGER(ENTRY(6,lv-returnc)).
         FIND FIRST est-prep NO-LOCK WHERE est-prep.company EQ oe-ordm.company
                      AND est-prep.est-no  EQ oe-ordm.est-no
                      AND est-prep.eqty    EQ lv-eqty
                      AND est-prep.line    EQ lv-line#
                      AND est-prep.code    EQ oe-ordm.charge
                      AND est-prep.simon   EQ "S"
                      AND est-prep.amtz    EQ 100  NO-ERROR.
      END.
      ELSE FIND FIRST est-prep NO-LOCK WHERE est-prep.company EQ oe-ordm.company
                      AND est-prep.est-no  EQ oe-ordm.est-no
                    /*  AND est-prep.eqty    EQ lv-eqty
                      AND est-prep.line    EQ lv-line#*/
                      AND est-prep.code    EQ oe-ordm.charge
                      AND est-prep.simon   EQ "S"
                      AND est-prep.amtz    EQ 100  NO-ERROR.

      
        IF AVAIL est-prep THEN
         ASSIGN 
             oe-ordm.miscType = 1
             oe-ordm.estPrepEqty   = est-prep.eqty
             oe-ordm.estPrepLine   = est-prep.line
             oe-ordm.est-no  = est-prep.est-no.  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE custom-panel-state B-table-Win 
PROCEDURE custom-panel-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER io-panel-state AS CHARACTER NO-UNDO.


  IF NOT AVAIL oe-ord AND AVAIL oe-ordl THEN
  FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

  IF AVAIL oe-ord AND INDEX("CZ",oe-ord.stat) GT 0 THEN
    io-panel-state = "disable-all".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE job-help B-table-Win 
PROCEDURE job-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE look-recid AS RECID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-jobno.w (cocode, oe-ordm.ord-i-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val, OUTPUT look-recid).
    IF look-recid NE ? THEN DO:
      FIND job-hdr WHERE RECID(job-hdr) EQ look-recid NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN 
        ASSIGN
         oe-ordm.ord-i-no:SCREEN-VALUE = job-hdr.job-no
         oe-ordm.ord-line:SCREEN-VALUE = STRING(job-hdr.job-no2).                      
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ld-prev-amt LIKE oe-ordm.amt NO-UNDO.

IF NOT AVAIL oe-ordm THEN DO:
    /* task 12161102 - for some reason, record not avail when validation
        fails */
    FIND LAST oe-ordm WHERE oe-ordm.company = oe-ord.company 
                        AND oe-ordm.ord-no = oe-ord.ord-no 
                      NO-ERROR.
END.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
      ld-prev-amt = DECIMAL(oe-ordm.amt:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.
  
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT oe-ordm EXCLUSIVE.
  

  FIND CURRENT oe-ord EXCLUSIVE.

  IF oe-ordm.amt NE 0 AND
     (oe-ord.stat NE "N" AND oe-ord.stat NE "H" AND oe-ord.stat NE "A") THEN
    oe-ord.stat = "U".  /* order updated */
  FIND CURRENT oe-ord NO-LOCK.

  FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) EXCLUSIVE.

  RUN oe/oe-comm.p.

  RELEASE xoe-ord.
  
  RUN oe/calcordt.p (ROWID(oe-ord)).
  FIND FIRST cust NO-LOCK
      WHERE cust.company EQ cocode
      AND cust.cust-no EQ oe-ord.cust-no
      USE-INDEX cust  NO-ERROR.
  IF (oe-ordm.bill NE "N" AND ld-prev-amt NE oe-ordm.amt)
       AND AVAIL cust AND cust.active NE "X" AND AVAIL oe-ord AND oe-ord.TYPE NE "T" THEN
    RUN oe/creditck.p (ROWID(oe-ord), YES).
  
  /* create reftable for prep */
  IF oe-ordm.est-no NE "" THEN DO:
       RUN create-reft4prep NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END. 

  /* gdm - 06170905 */ 
  RUN assgn-prep-info.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */    
  IF NOT v-oecomm-log THEN RUN show-comm (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-ordm FOR oe-ordm.
  DEFINE VARIABLE li-line AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-fgitem AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-error AS CHARACTER NO-UNDO.
  DEFINE BUFFER bf-ordl  FOR oe-ordl.
  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-ordm NO-LOCK
      WHERE bf-ordm.company EQ oe-ord.company
        AND bf-ordm.ord-no  EQ oe-ord.ord-no
      USE-INDEX oe-misc NO-ERROR.
  li-line = IF AVAIL bf-ordm THEN bf-ordm.line ELSE 0.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-new-recid = RECID(oe-ordm).
  FIND CURRENT oe-ordm EXCLUSIVE-LOCK.
  ASSIGN
   oe-ordm.company   = oe-ord.company
   oe-ordm.ord-no    = oe-ord.ord-no
   oe-ordm.est-no    = oe-ord.est-no
   oe-ordm.line      = li-line + 1
   oe-ordm.bill      = "Y"
   oe-ordm.s-man[1]  = oe-ord.sman[1]
   oe-ordm.s-pct[1]  = oe-ord.s-pct[1]
   oe-ordm.s-comm[1] = oe-ord.s-comm[1]
   oe-ordm.s-man[2]  = oe-ord.sman[2]
   oe-ordm.s-pct[2]  = oe-ord.s-pct[2]
   oe-ordm.s-comm[2] = oe-ord.s-comm[2]
   oe-ordm.s-man[3]  = oe-ord.sman[3]
   oe-ordm.s-pct[3]  = oe-ord.s-pct[3]
   oe-ordm.s-comm[3] = oe-ord.s-comm[3] .

  IF AVAIL oe-ctrl AND oe-ctrl.prep-comm EQ NO THEN DO:             /*Task# 11271302*/  
      ASSIGN
          oe-ordm.s-comm[1] = 0
          oe-ordm.s-comm[2] = 0     
          oe-ordm.s-comm[3] = 0.
  END.

  FIND FIRST ar-ctrl WHERE ar-ctrl.company = oe-ord.company NO-LOCK NO-ERROR.
  IF AVAIL ar-ctrl THEN oe-ordm.actnum = ar-ctrl.sales.
  FIND FIRST cust OF oe-ord NO-LOCK.

  oe-ordm.tax = cust.sort = "Y" AND oe-ord.tax-gr <> "".
  
  FIND FIRST oe-ctrl NO-LOCK
       WHERE oe-ctrl.company = oe-ord.company
      NO-ERROR.
  FIND FIRST shipto NO-LOCK
       WHERE shipto.company EQ cocode
         AND shipto.cust-no EQ oe-ord.cust-no
         NO-ERROR.
   
  IF AVAIL oe-ctrl AND oe-ctrl.prep-chrg THEN
      ASSIGN oe-ordm.spare-char-1 = IF AVAIL shipto AND shipto.tax-code NE "" THEN shipto.tax-code
                                    ELSE IF AVAIL cust AND cust.spare-char-1 <> "" THEN cust.spare-char-1 
                                    ELSE oe-ord.tax-gr
                oe-ordm.tax = TRUE .

  
  i = 0 .
  FOR EACH bf-ordl OF oe-ord NO-LOCK:
  i = i + 1.
  END.
  
  IF i = 1 THEN DO:
      IF AVAIL oe-ord THEN
          FIND FIRST bf-ordl OF oe-ord NO-LOCK NO-ERROR.
      IF AVAIL bf-ordl THEN
          ASSIGN
          oe-ordm.spare-char-2 = bf-ordl.i-no 
          oe-ordm.ord-i-no  = bf-ordl.job-no
          oe-ordm.ord-line  = bf-ordl.job-no2 .
  END.
  ELSE DO:
      RUN cec/mis-ordfg.w (RECID(oe-ord),OUTPUT v-fgitem,OUTPUT lv-error ) NO-ERROR.
      ASSIGN
          oe-ordm.spare-char-2 = v-fgitem .
      IF AVAIL oe-ord THEN
          FIND FIRST bf-ordl OF oe-ord 
          WHERE bf-ordl.i-no EQ v-fgitem NO-LOCK NO-ERROR.
        IF AVAIL bf-ordl THEN
            ASSIGN
            oe-ordm.ord-i-no  = bf-ordl.job-no
            oe-ordm.ord-line  = bf-ordl.job-no2 .
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
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) EXCLUSIVE.

  RUN oe/oe-comm.p.

  RELEASE xoe-ord.

  RUN oe/calcordt.p (ROWID(oe-ord)).

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"oemisc-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN Redisplay IN WIDGET-HANDLE(CHAR-hdl).

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
  IF NOT v-oecomm-log THEN RUN show-comm (NO).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO oe-ordm.charge IN BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  /* ======== validation ===============*/
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-charge (oe-ordm.charge:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    RUN valid-est (oe-ordm.est-no:HANDLE IN BROWSE {&browse-name}) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.

  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  RUN valid-tax NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-tax-gr NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ord-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
  RUN valid-ord-line NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-po-no-po NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
  RUN valid-s-man (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-s-pct (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-bill NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  FIND CURRENT oe-ordm EXCLUSIVE-LOCK NO-ERROR.
  oe-ordm.spare-char-1 = oe-ordm.spare-char-1:SCREEN-VALUE .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */  
  

  ASSIGN
     lv-new-recid = ?
     lv-valid-charge = NO.

  /*RUN oe/sman-upd.p (ROWID(oe-ordm)).*/
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"oemisc-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN Redisplay IN WIDGET-HANDLE(CHAR-hdl).
      
  IF NOT v-oecomm-log THEN RUN show-comm (NO).

  RUN reopen-query(ROWID(oe-ordm)).

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
  {methods/winReSizeLocInit.i}

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
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  RUN dispatch ('open-query').

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ('row-changed').
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
  {src/adm/template/sndkycas.i "company" "oe-ordm" "company"}

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
  {src/adm/template/snd-list.i "oe-ord"}
  {src/adm/template/snd-list.i "oe-ordm"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-comm B-table-Win 
PROCEDURE show-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-visible AS LOGICAL NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     oe-ordm.s-pct[1]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     oe-ordm.s-pct[2]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     oe-ordm.s-pct[3]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     oe-ordm.s-comm[1]:VISIBLE IN BROWSE {&browse-name} = ip-visible
     oe-ordm.s-comm[2]:VISIBLE IN BROWSE {&browse-name} = ip-visible
     oe-ordm.s-comm[3]:VISIBLE IN BROWSE {&browse-name} = ip-visible.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord V-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
   IF AVAIL oe-ordm THEN do:
       RUN oe/d-ordm.w (ROWID(oe-ordm),ROWID(oe-ord), "update", OUTPUT lv-rowid) .
       RUN reopen-query (lv-rowid).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyRecord V-table-Win 
PROCEDURE pCopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE BUFFER bf-oe-ordm FOR oe-ordm.
   DEFINE BUFFER bf-ordm FOR oe-ordm .
   DEFINE VARIABLE li-line AS INTEGER NO-UNDO.
   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
   DEFINE VARIABLE lv-rowid2 AS ROWID NO-UNDO.
   DEFINE VARIABLE iL AS INTEGER INITIAL 0 EXTENT 2 NO-UNDO.

   IF AVAILABLE oe-ordm THEN
   DO:
       ASSIGN lv-rowid2 = ROWID(oe-ordm) .
      FOR EACH bf-oe-ordm OF oe-ord NO-LOCK
          WHERE bf-oe-ordm.company EQ oe-ord.company
            AND bf-oe-ordm.line NE 0
            BY bf-oe-ordm.line:
          iL[1] = iL[1] + 1.
      END.
     
      FIND LAST bf-ordm NO-LOCK
      WHERE bf-ordm.company EQ oe-ord.company
        AND bf-ordm.ord-no  EQ oe-ord.ord-no
      USE-INDEX oe-misc NO-ERROR.
      li-line = IF AVAIL bf-ordm THEN bf-ordm.line ELSE 0.
      
      CREATE bf-oe-ordm.
      BUFFER-COPY oe-ordm EXCEPT rec_key line TO bf-oe-ordm.
      bf-oe-ordm.line = li-line + 1 .
     
      RUN oe/d-ordm.w (ROWID(bf-oe-ordm), ROWID(oe-ord), "Copy",OUTPUT lv-rowid).
     
      FOR EACH bf-oe-ordm OF oe-ord NO-LOCK
          WHERE bf-oe-ordm.company EQ oe-ord.company
            AND bf-oe-ordm.line NE 0
            BY bf-oe-ordm.line:
          ASSIGN
              iL[2]    = iL[2] + 1.
      END.
     
      IF iL[2] GT 0 AND (iL[1] NE iL[2] OR iL[2] EQ 1) THEN DO:
       RUN reopen-query (lv-rowid).
      END.
      ELSE do: 
         RUN reopen-query (lv-rowid2).
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord V-table-Win 
PROCEDURE pViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
   IF AVAIL oe-ordm THEN do:
       RUN oe/d-ordm.w (ROWID(oe-ordm),ROWID(oe-ord), "view", OUTPUT lv-rowid) .
       RUN reopen-query (lv-rowid).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord V-table-Win 
PROCEDURE pAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE BUFFER bf-oe-ordm FOR oe-ordm.
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  
  IF AVAILABLE oe-ord THEN
  DO:
     
     RUN oe/d-ordm.w (?, ROWID(oe-ord), "add",OUTPUT lv-rowid).
     
     FIND FIRST bf-oe-ordm  NO-LOCK
          WHERE bf-oe-ordm.company EQ oe-ord.company
            AND ROWID(bf-oe-ordm) EQ  lv-rowid NO-ERROR .
        
     IF AVAIL bf-oe-ordm THEN DO:
        RUN reopen-query (lv-rowid).
     END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


