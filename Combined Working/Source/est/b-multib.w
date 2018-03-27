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
{sys/inc/var.i NEW SHARED}
ASSIGN
 cocode = g_company
 locode = g_loc.

DEF BUFFER xest FOR est.
DEF BUFFER xef FOR ef.
DEF BUFFER xeb FOR eb.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR ll-new-form AS LOG NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR hld-yld-qty LIKE eb.yld-qty NO-UNDO.
DEF VAR hld-num-up  LIKE eb.num-up NO-UNDO.
DEF VAR ll-change AS LOG NO-UNDO.
DEF VAR v-form-no LIKE eb.form-no NO-UNDO.
DEF VAR v-blank-no LIKE eb.blank-no NO-UNDO.
DEF VAR v-num-up LIKE eb.num-up NO-UNDO.

&SCOPED-DEFINE sortby-phrase BY eb.form-no BY eb.blank-no

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb ef

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table eb.form-no eb.blank-no eb.part-no ~
display-bl-qty () @ eb.bl-qty eb.bl-qty display-bl-qty () @ eb.bl-qty ~
display-yld-qty () @ eb.yld-qty eb.yld-qty display-yld-qty () @ eb.yld-qty ~
eb.yrprice eb.num-wid eb.num-len eb.num-up 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table eb.form-no eb.blank-no ~
eb.part-no eb.bl-qty eb.yld-qty eb.yrprice eb.num-wid eb.num-len 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table eb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table eb
&Scoped-define QUERY-STRING-br_table FOR EACH eb WHERE eb.company = est.company ~
  AND eb.est-no = est.est-no NO-LOCK, ~
      EACH ef WHERE ef.company = eb.company ~
  AND ef.est-no = eb.est-no ~
  AND ef.form-no = eb.form-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH eb WHERE eb.company = est.company ~
  AND eb.est-no = est.est-no NO-LOCK, ~
      EACH ef WHERE ef.company = eb.company ~
  AND ef.est-no = eb.est-no ~
  AND ef.form-no = eb.form-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table eb ef
&Scoped-define FIRST-TABLE-IN-QUERY-br_table eb
&Scoped-define SECOND-TABLE-IN-QUERY-br_table ef


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
company||y|ASI.oe-boll.company
r-no||y|ASI.oe-boll.r-no
b-no||y|ASI.oe-boll.b-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,r-no,b-no"':U).

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-bl-qty B-table-Win 
FUNCTION display-bl-qty RETURNS INTEGER
  ( )   FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-yld-qty B-table-Win 
FUNCTION display-yld-qty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      eb, 
      ef SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      eb.form-no COLUMN-LABEL "Form#" FORMAT ">>>>>>>>>>":U WIDTH 7
      eb.blank-no COLUMN-LABEL "Blank#" FORMAT ">>>>>>>>>>":U WIDTH 7
      eb.part-no COLUMN-LABEL "Customer Part#" FORMAT "x(15)":U
            WIDTH 20
      display-bl-qty () @ eb.bl-qty
      eb.bl-qty COLUMN-LABEL "Request Qty" FORMAT "->>>,>>>,>>>":U
            WIDTH 13
      display-bl-qty () @ eb.bl-qty
      display-yld-qty () @ eb.yld-qty
      eb.yld-qty FORMAT "->>>,>>>,>>>":U WIDTH 13
      display-yld-qty () @ eb.yld-qty
      eb.yrprice COLUMN-LABEL "Priced By" FORMAT "Yield/Request":U
      eb.num-wid FORMAT ">>>":U
      eb.num-len FORMAT ">>>":U
      eb.num-up COLUMN-LABEL "# Up" FORMAT ">>>,>>9":U
  ENABLE
      eb.form-no
      eb.blank-no
      eb.part-no
      eb.bl-qty
      eb.yld-qty
      eb.yrprice
      eb.num-wid
      eb.num-len
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 108 BY 5.95
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         HEIGHT             = 6.19
         WIDTH              = 108.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.eb WHERE ASI.est  ...,ASI.ef WHERE ASI.eb  ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.eb.company = ASI.est.company
  AND ASI.eb.est-no = ASI.est.est-no"
     _JoinCode[2]      = "ASI.ef.company = ASI.eb.company
  AND ASI.ef.est-no = ASI.eb.est-no
  AND ASI.ef.form-no = ASI.eb.form-no"
     _FldNameList[1]   > ASI.eb.form-no
"eb.form-no" "Form#" ">>>>>>>>>>" "integer" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > ASI.eb.blank-no
"eb.blank-no" "Blank#" ">>>>>>>>>>" "integer" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[3]   > ASI.eb.part-no
"eb.part-no" "Customer Part#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"display-bl-qty () @ eb.bl-qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.eb.bl-qty
"eb.bl-qty" "Request Qty" "->>>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"display-bl-qty () @ eb.bl-qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"display-yld-qty () @ eb.yld-qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.eb.yld-qty
"eb.yld-qty" ? "->>>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" ""
     _FldNameList[9]   > "_<CALC>"
"display-yld-qty () @ eb.yld-qty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > ASI.eb.yrprice
"eb.yrprice" "Priced By" "Yield/Request" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > ASI.eb.num-wid
"eb.num-wid" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > ASI.eb.num-len
"eb.num-len" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > ASI.eb.num-up
"eb.num-up" "# Up" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
ON HELP OF br_table IN FRAME F-Main
DO:
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR char-val AS CHAR NO-UNDO.
 

  /*CASE FOCUS:NAME:
    WHEN "loc" THEN DO:
      RUN windows/l-loc.w (eb.company, FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val <> "" THEN DO:
        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
        APPLY "leave" TO FOCUS.
      END.      
    END.
  END.*/

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
   /*{src/adm/template/brsleave.i}*/
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


&Scoped-define SELF-NAME eb.form-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.form-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.form-no IN BROWSE br_table /* Form# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-form-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.blank-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.blank-no IN BROWSE br_table /* Blank# */
DO:
  IF ll-new-form THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.blank-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.blank-no IN BROWSE br_table /* Blank# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.bl-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.bl-qty br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.bl-qty IN BROWSE br_table /* Request Qty */
DO:
  IF est.est-type EQ 2 OR
     est.est-type EQ 5 OR
     est.est-type EQ 6 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.bl-qty br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.bl-qty IN BROWSE br_table /* Request Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bl-yld-up NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.yld-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.yld-qty br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.yld-qty IN BROWSE br_table /* Yield Qty */
DO:
  IF est.est-type EQ 2 OR
     est.est-type EQ 5 OR
     est.est-type EQ 6 THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.yld-qty br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.yld-qty IN BROWSE br_table /* Yield Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bl-yld-up NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.num-wid IN BROWSE br_table /* # on Width */
DO:
  DEF BUFFER b-eb FOR eb.


  IF (est.est-type EQ 2 OR
      est.est-type EQ 5 OR
      est.est-type EQ 6)                     AND
     CAN-FIND(b-eb OF est
              WHERE b-eb.form-no EQ eb.form-no
                AND b-eb.eqty    EQ eb.eqty) THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.num-wid IN BROWSE br_table /* # on Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bl-yld-up NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.num-wid IN BROWSE br_table /* # on Width */
DO:
  RUN calc-#up.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF eb.num-len IN BROWSE br_table /* # on Length */
DO:
  DEF BUFFER b-eb FOR eb.


  IF (est.est-type EQ 2 OR
      est.est-type EQ 5 OR
      est.est-type EQ 6)                     AND
     CAN-FIND(b-eb OF est
              WHERE b-eb.form-no EQ eb.form-no
                AND b-eb.eqty    EQ eb.eqty) THEN DO:
    APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.num-len IN BROWSE br_table /* # on Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bl-yld-up NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.num-len IN BROWSE br_table /* # on Length */
DO:
  RUN calc-#up.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-#up B-table-Win 
PROCEDURE calc-#up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(DEC(eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}) *
               DEC(eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name})).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE finish-assign B-table-Win 
PROCEDURE finish-assign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.
  DEF VAR ll-die-changed AS LOG NO-UNDO.
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR li-qty AS INT NO-UNDO.


  RUN est/resetf&b.p (ROWID(est), NO).

  /*FOR EACH b-ef
      WHERE b-ef.company EQ est.company
        AND b-ef.est-no  EQ est.est-no:

    b-ef.die-in = 0.

    FOR EACH xeb
        WHERE xeb.company EQ b-ef.company
          AND xeb.est-no  EQ b-ef.est-no
          AND xeb.form-no EQ b-ef.form-no
        NO-LOCK:

      b-ef.die-in = b-ef.die-in + xeb.die-in.
    END.
  END.*/

  FIND CURRENT eb.
  FIND CURRENT est.

  lv-die-in = eb.die-in.
  IF eb.die-in NE 0 THEN eb.die-in = (eb.die-in / v-num-up) * eb.num-up.
  IF lv-die-in NE eb.die-in THEN ll-die-changed = YES.

  RELEASE xef.
  IF ll-change         AND
     est.est-type NE 2 AND
     est.est-type NE 5 AND
     est.est-type NE 6 THEN
  FIND FIRST xef
      WHERE xef.company EQ eb.company
        AND xef.est-no  EQ eb.est-no
        AND xef.form-no EQ eb.form-no
      NO-LOCK NO-ERROR.

  IF AVAIL xef THEN DO:
    v-qty = eb.yld-qty / eb.num-up.
    {sys/inc/roundup.i v-qty}
    ASSIGN
     eb.yld-qty = v-qty * eb.num-up
     /*xef.die-in = eb.die-in*/.

    ll-ans = NO.

    FOR EACH xeb
        WHERE xeb.company EQ xef.company
          AND xeb.est-no  EQ xef.est-no
          AND xeb.form-no EQ xef.form-no
          AND RECID(xeb)  NE RECID(eb)
          AND xeb.yld-qty NE v-qty * xeb.num-up:
      ll-ans = YES.
      MESSAGE "For all other Blanks on this Form..." SKIP
              "Click the YES button to calculate the layout based on the Request Qty" SKIP
              "Click the NO  button to calculate the layout based on the Yield Qty"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-ans.
      LEAVE.
    END.
    RELEASE xeb.

    FOR EACH xeb
        WHERE xeb.company EQ xef.company
          AND xeb.est-no  EQ xef.est-no
          AND xeb.form-no EQ xef.form-no
          AND ROWID(xeb)  NE ROWID(eb)
          AND xeb.yld-qty NE v-qty * xeb.num-up
        BY xeb.blank-no:
      /*IF xeb.yld-qty LT xeb.bl-qty THEN xeb.yld-qty = xeb.bl-qty.*/
      IF xeb.yld-qty EQ 0 THEN xeb.yld-qty = xeb.bl-qty.
        
      ASSIGN
       li-qty      = IF ll-ans THEN xeb.bl-qty ELSE xeb.yld-qty
       lv-die-in   = xeb.die-in
       xeb.die-in  = xeb.die-in / xeb.num-up
       xeb.num-up  = TRUNC(li-qty / v-qty,0) + INT(li-qty MODULO v-qty GT 0)
       xeb.die-in  = xeb.die-in * xeb.num-up
       xeb.yld-qty = v-qty * xeb.num-up
       /*xef.die-in  = xef.die-in + xeb.die-in*/.

      IF lv-die-in NE xeb.die-in THEN ll-die-changed = YES.

      IF xeb.num-wid * xeb.num-len NE xeb.num-up THEN
        ASSIGN
         xeb.num-wid = xeb.num-up
         xeb.num-len = 1.
    END.
    RELEASE xeb.
  END.

  FIND CURRENT eb  NO-LOCK.
  FIND CURRENT est NO-LOCK.

  IF ll-die-changed THEN RUN est/updefdie.p (ROWID(ef)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-eb-rowid B-table-Win 
PROCEDURE get-eb-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.

  op-rowid = ROWID(eb).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER xeb-form-ef FOR ef.

  DEF VAR i AS INT NO-UNDO.
  DEF VAR x AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  FIND CURRENT est.

  DO WITH FRAME {&frame-name}:
    ll-change = eb.bl-qty  NE DEC(eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name})  OR
                eb.yld-qty NE DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                eb.num-up  NE DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF ll-new-form THEN DO:
      FIND FIRST xef
          WHERE xef.company EQ est.company
            AND xef.est-no  EQ est.est-no
            AND xef.form-no EQ INT(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK NO-ERROR.

      IF NOT AVAIL xef THEN DO:
        est.form-qty = est.form-qty + 1.

        CREATE xef.
        BUFFER-COPY ef TO xef
        ASSIGN
         xef.form-no   = est.form-qty
         xef.blank-qty = 1.

        FIND FIRST xeb
            WHERE xeb.company EQ eb.company
              AND xeb.est-no  EQ eb.est-no
              AND xeb.form-no EQ v-form-no
              AND ROWID(xeb)  NE ROWID(eb)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL xeb THEN DO:
          FIND CURRENT ef NO-ERROR.
          IF AVAIL ef THEN ef.form-no = 9999999.
        END.
      END.
    END.

    IF DEC(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})  NE v-form-no  OR
       DEC(eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE v-blank-no THEN DO:

      {sys/inc/xeb-form.i "eb." "0" "DEC(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})" "0"}

      eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} =
            STRING((DEC(eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) * 1000) +
                   1 * (IF DEC(eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) LT v-blank-no THEN -1 ELSE 1)).

      {sys/inc/xeb-form.i "eb." "v-blank-no" "DEC(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})" "DEC(eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})"}
       
      FOR EACH xeb
          WHERE xeb.company EQ ef.company
            AND xeb.est-no  EQ ef.est-no
            AND xef.form-no EQ DEC(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND ROWID(xeb)  NE ROWID(eb)
          BY xeb.blank-no:

        {sys/inc/xeb-form.i "xeb." "xeb.blank-no" "xeb.form-no" "xeb.blank-no * 1000"}

        xeb.blank-no = xeb.blank-no * 1000.
      END.      
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-bl-qty LIKE eb.bl-qty NO-UNDO.
  DEF VAR lv-yld-qty LIKE eb.yld-qty NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   lv-bl-qty  = eb.bl-qty
   lv-yld-qty = eb.yld-qty.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    eb.num-up = INT(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF est.est-type EQ 2 OR
       est.est-type EQ 5 OR
       est.est-type EQ 6 THEN
      ASSIGN
       eb.bl-qty  = lv-bl-qty
       eb.yld-qty = lv-yld-qty.
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
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO eb.form-no IN BROWSE {&browse-name}.
    ll-new-form = NO.

    ASSIGN
     hld-yld-qty = eb.yld-qty
     hld-num-up  = eb.num-up.
       
    IF hld-yld-qty EQ 0 THEN DO:
      FIND FIRST xeb
          WHERE xeb.company EQ eb.company
            AND xeb.est-no  EQ eb.est-no
            AND xeb.form-no EQ eb.form-no
            AND RECID(xeb)  NE RECID(eb)
          NO-LOCK NO-ERROR.
      IF AVAIL xeb THEN DO:
        v-qty = xeb.yld-qty / xeb.num-up.
        {sys/inc/roundup.i v-qty}
      END.
      ELSE v-qty = 0.
    END.
         
    ELSE DO:
      v-qty = hld-yld-qty / hld-num-up.
      {sys/inc/roundup.i v-qty}
    END.
  END.

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
  IF est.est-type EQ 2 OR
     est.est-type EQ 5 OR
     est.est-type EQ 6 THEN
  DO WITH FRAME {&FRAME-NAME}:
    eb.yld-qty:LABEL IN BROWSE {&browse-name} = "Qty/Set".
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
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-first THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-source", OUTPUT char-hdl).

    RUN get-eb-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).

    RUN repo-query (lv-rowid).

    ll-first = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rowid = ROWID(eb).

  RUN calc-#up.

  RUN valid-form-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-blank-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-bl-yld-up NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  ASSIGN
   ll-change  = NO
   v-form-no  = eb.form-no
   v-blank-no = eb.blank-no
   v-num-up   = eb.num-up.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN finish-assign.

  RUN dispatch ("open-query").

  RUN repo-query (lv-rowid).

  ll-new-form = NO.

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
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  
  DO WITH FRAME {&frame-name}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
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
  {src/adm/template/sndkycas.i "company" "oe-boll" "company"}
  {src/adm/template/sndkycas.i "r-no" "oe-boll" "r-no"}
  {src/adm/template/sndkycas.i "b-no" "oe-boll" "b-no"}

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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "eb"}
  {src/adm/template/snd-list.i "ef"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bl-yld-up B-table-Win 
PROCEDURE valid-bl-yld-up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-ans AS LOG NO-UNDO.


  IF est.est-type NE 2 AND
     est.est-type NE 5 AND
     est.est-type NE 6 THEN DO WITH FRAME {&FRAME-NAME}:
    IF DEC(eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "Request Qty may not be zero..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.bl-qty IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    /*IF DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) LT
       DEC(eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name})  THEN DO:
      eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
          eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name}.
      IF DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
         DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) GT v-qty THEN DO:
        v-qty = DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) /
                DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).
        {sys/inc/roundup.i v-qty}
      END.
    END.*/

    IF DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) LT 1 THEN
      ASSIGN
       eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
       eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
       eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}  = "1".

    IF DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) NE hld-yld-qty AND
       DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) MODULO
         DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0          AND
       v-qty NE 0                                                           THEN
      eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING(TRUNC(DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) / v-qty,0) +
                 INT(DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) MODULO v-qty GT 0)).
                    
    IF DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) NE hld-num-up THEN DO:
      ll-ans = NO.
      IF DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) MODULO
           DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN
        MESSAGE "Recalculate Yield Qty?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-ans.
      IF ll-ans THEN DO:
        v-qty = DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) / DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).
        {sys/inc/roundup.i v-qty}
        eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
            STRING(v-qty * DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name})).
      END.
    END.
          
    ASSIGN
     hld-yld-qty = DEC(eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name})
     hld-num-up  = DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF DEC(eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}) *
       DEC(eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name}) NE
        DEC(eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
      ASSIGN
       eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name} =
                              eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}
       eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
          
    v-qty = hld-yld-qty / hld-num-up.
    {sys/inc/roundup.i v-qty}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-no B-table-Win 
PROCEDURE valid-blank-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "Blank Number may not be zero..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.blank-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-form-no B-table-Win 
PROCEDURE valid-form-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST xef
        WHERE xef.company EQ est.company
          AND xef.est-no  EQ est.est-no
          AND xef.form-no EQ INT(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
    IF NOT AVAIL xef THEN DO:
      IF NOT ll-new-form THEN
        MESSAGE "Form# does not exist on this estimate, add a new one?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE ll-new-form.

      IF ll-new-form THEN
        ASSIGN
         eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
         /*eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(est.form-qty + 1)*/.

      ELSE DO:
        APPLY "entry" TO eb.form-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    ELSE
    IF INT(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE eb.form-no THEN DO:
      FOR EACH xeb
          WHERE xeb.company EQ est.company
            AND xeb.est-no  EQ est.est-no  
            AND xeb.form-no EQ INT(eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})
          BY xeb.blank-no DESC:
        LEAVE.
      END.

      eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} =
                            STRING((IF AVAIL xeb THEN xeb.blank-no ELSE 0) + 1).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-bl-qty B-table-Win 
FUNCTION display-bl-qty RETURNS INTEGER
  ( )  :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld-part-qty AS DEC NO-UNDO.
  DEF VAR lv-bl-qty LIKE eb.bl-qty NO-UNDO.
  DEF VAR lv-yld-qty LIKE eb.yld-qty NO-UNDO.


  IF est.est-type EQ 2 OR est.est-type EQ 6 THEN DO:
    IF est.est-type EQ 2 THEN
      ASSIGN
       lv-bl-qty  = eb.bl-qty
       lv-yld-qty = eb.cust-%.
    ELSE
      ASSIGN
       lv-bl-qty  = est.est-qty[1]
       lv-yld-qty = eb.yld-qty.

    {sys/inc/partqty1.i ld-part-qty lv-yld-qty}

    lv-bl-qty = lv-bl-qty * ld-part-qty.
  END.

  ELSE lv-bl-qty = eb.bl-qty.
  
  RETURN lv-bl-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-yld-qty B-table-Win 
FUNCTION display-yld-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lv-yld-qty LIKE eb.yld-qty NO-UNDO.


  lv-yld-qty = IF est.est-type EQ 2 THEN eb.cust-%  ELSE
               IF est.est-type EQ 6 THEN eb.yld-qty ELSE eb.yld-qty.
  
  RETURN lv-yld-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

