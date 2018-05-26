&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-eb NO-UNDO LIKE eb
       FIELD yieldQtyOverride AS INTEGER
       FIELD sheetsRequired AS DECIMAL
       FIELD maxSheetsPerForm AS DECIMAL
       FIELD calcYieldQty AS DECIMAL
       FIELD effectiveYldQty AS DECIMAL
       FIELD surplusQty AS DECIMAL
       FIELD yld-qty-orig AS DECIMAL /* as loaded */
       FIELD dec-recid AS DECIMAL
       FIELD ebRowid AS ROWID
       INDEX est-qty company est-no eqty form-no blank-no.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
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

/* Returned from window */
DEFINE VARIABLE /* INPUT-OUTPUT PARAMETER */ io-rowid      AS ROWID NO-UNDO.
DEFINE VARIABLE /* OUTPUT PARAMETER */ op-fb-changed AS LOG   NO-UNDO.


{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
ASSIGN
    cocode = g_company
    locode = g_loc.

DEFINE BUFFER xest  FOR est.
DEFINE BUFFER xef   FOR ef.
DEFINE BUFFER xeb   FOR eb.

DEFINE BUFFER b-ref FOR reftable.

DEFINE VARIABLE ll-first      AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE ll-new-form   AS LOG       NO-UNDO.
DEFINE VARIABLE v-qty         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE hld-yld-qty   LIKE eb.yld-qty NO-UNDO.
DEFINE VARIABLE hld-num-up    LIKE eb.num-up NO-UNDO.
DEFINE VARIABLE ll-change     AS LOG       NO-UNDO.
DEFINE VARIABLE v-form-no     LIKE eb.form-no NO-UNDO.
DEFINE VARIABLE v-blank-no    LIKE eb.blank-no NO-UNDO.
DEFINE VARIABLE v-num-up      LIKE eb.num-up NO-UNDO.
DEFINE VARIABLE lv-prev-val-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-rowid      AS ROWID     NO-UNDO.


/* gdm - 07310904 */
&SCOPED-DEFINE yellowColumnsName b-multbl

&SCOPED-DEFINE noSortByField 1
DEFINE VARIABLE CHAR-hdl      AS cha       NO-UNDO.


DEFINE BUFFER bf-tt-eb    FOR tt-eb.

DEFINE BUFFER multbl      FOR reftable.
DEFINE BUFFER xeb-form-ef FOR ef.
DEFINE BUFFER b-ef        FOR ef.
DEFINE BUFFER b-eb        FOR eb.
DEFINE BUFFER bf2-tt-eb   FOR tt-eb.
DEFINE VARIABLE lUseAlternateQty AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lManualRecalc    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCEGOTOCALC      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO .
RUN sys/ref/nk1look.p (INPUT cocode, "CEGOTOCALC", "C" /* Char */, NO, NO /* check by cust */, "", "" , 
    OUTPUT cCEGOTOCALC, OUTPUT lRecFound).
lManualRecalc = (IF cCEGOTOCALC EQ "AUTOCALC" THEN NO ELSE YES).

&SCOPED-DEFINE where-multbl WHERE multbl.reftable EQ "est\d-multbl.w" ~
                              AND multbl.company  EQ est.company      ~
                              AND multbl.loc      EQ est.loc          ~
                              AND multbl.code     EQ est.est-no

&SCOPED-DEFINE SORTBY-PHRASE BY tt-eb.form-no BY tt-eb.blank-no

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
&Scoped-define INTERNAL-TABLES tt-eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-eb.form-no tt-eb.blank-no ~
tt-eb.part-no tt-eb.num-wid tt-eb.num-len tt-eb.num-up tt-eb.bl-qty ~
tt-eb.calcYieldQty tt-eb.yld-qty tt-eb.yrprice tt-eb.sheetsRequired ~
tt-eb.maxSheetsPerForm tt-eb.effectiveYldQty tt-eb.surplusQty ~
tt-eb.yld-qty-orig 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-eb.form-no ~
tt-eb.blank-no tt-eb.part-no tt-eb.num-wid tt-eb.num-len tt-eb.bl-qty ~
tt-eb.yld-qty tt-eb.yrprice 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-eb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-eb
&Scoped-define QUERY-STRING-br_table FOR EACH tt-eb WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH tt-eb WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-eb
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-eb


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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD comma B-table-Win 
FUNCTION comma RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-bl-qty B-table-Win 
FUNCTION display-bl-qty RETURNS DECIMAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tt-eb.form-no COLUMN-LABEL "Form" FORMAT ">>9":U
      tt-eb.blank-no COLUMN-LABEL "Blank" FORMAT ">>9":U
      tt-eb.part-no FORMAT "x(20)":U
      tt-eb.num-wid COLUMN-LABEL "# on!Width" FORMAT ">9":U
      tt-eb.num-len COLUMN-LABEL "# on! Length" FORMAT ">9":U
      tt-eb.num-up FORMAT ">>9":U
      tt-eb.bl-qty COLUMN-LABEL "Request Qty" FORMAT "->>>,>>>,>>>":U
      tt-eb.calcYieldQty COLUMN-LABEL "Calculated!Yield Qty" FORMAT "->>>,>>>,>>>":U
      tt-eb.yld-qty COLUMN-LABEL "Yield Qty!Override" FORMAT "->>>,>>>,>>>":U
      tt-eb.yrprice FORMAT "Yield/Request":U
      tt-eb.sheetsRequired COLUMN-LABEL "Sheets!Required" FORMAT "->>>,>>>,>>>":U
      tt-eb.maxSheetsPerForm COLUMN-LABEL "Max Sheets!Per Form" FORMAT "->>>,>>>,>>>":U
      tt-eb.effectiveYldQty COLUMN-LABEL "Effective!Yield Qty" FORMAT "->>>,>>>,>>>":U
      tt-eb.surplusQty COLUMN-LABEL "Surplus Qty" FORMAT "->>>,>>>,>>>":U
      tt-eb.yld-qty-orig COLUMN-LABEL "Yield Qty Loaded" FORMAT "->>>,>>>,>>>":U
  ENABLE
      tt-eb.form-no
      tt-eb.blank-no
      tt-eb.part-no
      tt-eb.num-wid
      tt-eb.num-len
      tt-eb.bl-qty
      tt-eb.yld-qty
      tt-eb.yrprice
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 125 BY 16.91 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-eb T "?" NO-UNDO asi eb
      ADDITIONAL-FIELDS:
          FIELD yieldQtyOverride AS INTEGER
          FIELD sheetsRequired AS DECIMAL
          FIELD maxSheetsPerForm AS DECIMAL
          FIELD calcYieldQty AS DECIMAL
          FIELD effectiveYldQty AS DECIMAL
          FIELD surplusQty AS DECIMAL
          FIELD yld-qty-orig AS DECIMAL /* as loaded */
          FIELD dec-recid AS DECIMAL
          FIELD ebRowid AS ROWID
          INDEX est-qty company est-no eqty form-no blank-no
      END-FIELDS.
   END-TABLES.
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
         HEIGHT             = 17.1
         WIDTH              = 125.8.
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
       tt-eb.yld-qty:VISIBLE IN BROWSE br_table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.tt-eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.tt-eb.form-no
"tt-eb.form-no" "Form" ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-eb.blank-no
"tt-eb.blank-no" "Blank" ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt-eb.part-no
"tt-eb.part-no" ? "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tt-eb.num-wid
"tt-eb.num-wid" "# on!Width" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tt-eb.num-len
"tt-eb.num-len" "# on! Length" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.tt-eb.num-up
     _FldNameList[7]   > Temp-Tables.tt-eb.bl-qty
"tt-eb.bl-qty" "Request Qty" "->>>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"tt-eb.calcYieldQty" "Calculated!Yield Qty" "->>>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tt-eb.yld-qty
"tt-eb.yld-qty" "Yield Qty!Override" "->>>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tt-eb.yrprice
"tt-eb.yrprice" ? "Yield/Request" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"tt-eb.sheetsRequired" "Sheets!Required" "->>>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"tt-eb.maxSheetsPerForm" "Max Sheets!Per Form" "->>>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"tt-eb.effectiveYldQty" "Effective!Yield Qty" "->>>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"tt-eb.surplusQty" "Surplus Qty" "->>>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"tt-eb.yld-qty-orig" "Yield Qty Loaded" "->>>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
        DEFINE VARIABLE lv-rowid AS ROWID     NO-UNDO.
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
 

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
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:   
        DEFINE VARIABLE bl-qty  AS INTEGER NO-UNDO .
        DEFINE VARIABLE yld-qty AS INTEGER NO-UNDO .



    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}
  
        lv-prev-val-1 = tt-eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}.

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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

ON LEAVE OF tt-eb.form-no IN BROWSE br_table /* Form# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-form-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

ON ENTRY OF tt-eb.blank-no IN BROWSE br_table /* Blank# */
    DO:
        IF ll-new-form THEN 
        DO:
            APPLY "tab" TO tt-eb.blank-no IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

ON LEAVE OF tt-eb.blank-no IN BROWSE br_table /* Blank# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-blank-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

ON ENTRY OF tt-eb.bl-qty IN BROWSE br_table /* Request Qty */
    DO:
        IF est.est-type EQ 2 OR
            est.est-type EQ 5 OR
            est.est-type EQ 6 THEN 
        DO:
            APPLY "tab" TO tt-eb.bl-qty IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

ON LEAVE OF tt-eb.bl-qty IN BROWSE br_table /* Request Qty */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-bl-yld-up NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

ON ENTRY OF tt-eb.num-wid IN BROWSE br_table /* # on Width */
    DO:
        DEFINE BUFFER b-eb FOR tt-eb.


        IF (est.est-type EQ 2 OR
            est.est-type EQ 5 OR
            est.est-type EQ 6)                     AND
            CAN-FIND(b-eb OF est
            WHERE b-eb.form-no EQ tt-eb.form-no
            AND b-eb.eqty    EQ tt-eb.eqty) THEN 
        DO:
            APPLY "tab" TO tt-eb.num-wid IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

ON LEAVE OF tt-eb.num-wid IN BROWSE br_table /* # on Width */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-bl-yld-up NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            RUN recalcCurrent.
        END.
    END.


ON ENTRY OF tt-eb.num-len IN BROWSE br_table /* # on Length */
    DO:
        DEFINE BUFFER b-eb FOR tt-eb.


        IF (est.est-type EQ 2 OR
            est.est-type EQ 5 OR
            est.est-type EQ 6)                     AND
            CAN-FIND(b-eb OF est
            WHERE b-eb.form-no EQ tt-eb.form-no
            AND b-eb.eqty    EQ tt-eb.eqty) THEN 
        DO:
            APPLY "tab" TO tt-eb.num-len IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
        END.
    END.

ON LEAVE OF tt-eb.num-len IN BROWSE br_table /* # on Length */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-bl-yld-up NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            RUN recalcCurrent.
        END.
    END.


ON LEAVE OF tt-eb.yld-qty IN BROWSE br_table /* Yield Qty */
    DO:
        /* RUN calc-#up. */
        RUN recalcCurrent.
    END.

ON LEAVE OF tt-eb.yrprice IN BROWSE br_table 
    DO:

        /* user must press save otherwise browse loses focus*/
/*        RUN dispatch('update-record':U).*/
/*        IF RETURN-VALUE = "ADM-ERROR":U THEN*/
/*           RETURN NO-APPLY.                 */
       
    END.


FIND eb NO-LOCK WHERE ROWID(eb) EQ lv-rowid NO-ERROR.


IF AVAILABLE eb THEN FIND FIRST est OF eb NO-LOCK.

IF AVAILABLE est THEN 
DO:


    RELEASE eb.

    RUN del-ref-records.

    FOR EACH ef FIELDS(est-no form-no board brd-dscr) NO-LOCK
        WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no,
        EACH eb FIELDS(form-no blank-no) NO-LOCK
        WHERE eb.company EQ est.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no:
        CREATE multbl.
        ASSIGN
            multbl.reftable = "est\d-multbl.w"
            multbl.company  = est.company
            multbl.loc      = est.loc
            multbl.code     = est.est-no
            multbl.code2    = ef.board
            multbl.dscr     = ef.brd-dscr
            multbl.val[1]   = eb.form-no
            multbl.val[2]   = eb.blank-no
            multbl.val[3]   = DEC(RECID(eb)).
        RELEASE multbl.
    END.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-go B-table-Win 
PROCEDURE apply-go :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-fb-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE op-fb-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE io-rowid      AS ROWID     NO-UNDO.
    FOR EACH multbl NO-LOCK {&where-multbl} USE-INDEX reftable,
      FIRST eb NO-LOCK
      WHERE RECID(eb)    EQ INT(multbl.val[3])
        AND (eb.form-no  NE multbl.val[1] OR
             eb.blank-no NE multbl.val[2]):
    ll-fb-changed = YES.
    LEAVE.
END.

op-fb-changed = ll-fb-changed.

IF ll-fb-changed THEN 
DO:
    RUN new-forms.

    FOR EACH multbl {&where-multbl} USE-INDEX reftable,
        FIRST eb
        WHERE RECID(eb)    EQ INT(multbl.val[3])
          AND eb.blank-no  LT 999
          AND (eb.form-no  NE multbl.val[1] OR
               eb.blank-no NE multbl.val[2]):

    IF eb.form-no NE multbl.val[1] THEN 
    DO:
    {sys/inc/xeb-form.i "eb." "0" "multbl.val[1]" "0"}
    END.

    multbl.val[2] = (multbl.val[2] * 1000) +
        (1 * (IF multbl.val[2] LT eb.blank-no THEN -1 ELSE 1)).

    {sys/inc/xeb-form.i "eb." "eb.blank-no" "multbl.val[1]" "multbl.val[2] * 1000"}

    ASSIGN
        eb.form-no  = multbl.val[1]
        eb.blank-no = multbl.val[2] * 1000.

    RUN update-ef-board-proc.
END.

RUN del-ref-records.

RUN est/resetf&b.p (ROWID(est), NO).

li = 0.
FOR EACH est-op
    WHERE est-op.company EQ est.company
    AND est-op.est-no  EQ est.est-no
    AND est-op.line    LT 500
    BY est-op.qty
    BY est-op.s-num
    BY est-op.b-num
    BY est-op.d-seq
    BY est-op.op-pass
    BY est-op.rec_key:
      
      {sys/inc/outstrPL.i est-op SHARE}
    ASSIGN
        li          = li + 1
        est-op.line = li
        .
     
    IF AVAILABLE reftable THEN reftable.loc = STRING(est-op.line,"9999999999"). 
END.
END.

io-rowid = ROWID(tt-eb).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-ttrel B-table-Win 
PROCEDURE assign-ttrel :
/*------------------------------------------------------------------------------
      Purpose:     Created due to limit on code in one section
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        DISPLAY tt-eb.part-no
            tt-eb.bl-qty 
            tt-eb.yld-qty
            tt-eb.num-wid           
            tt-eb.calcYieldQty
            tt-eb.num-len
            tt-eb.yrprice
            WITH BROWSE {&browse-name}.

    END.
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
        tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name} =
            STRING(DEC(tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}) *
            DEC(tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name})).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkAllSaved B-table-Win 
PROCEDURE checkAllSaved :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplAllSaved AS LOG NO-UNDO.
    DEFINE VARIABLE lSaved AS LOG NO-UNDO.

    lSaved = TRUE.
    FOR EACH bf-tt-eb:  
        FIND eb EXCLUSIVE-LOCK WHERE ROWID(eb) EQ bf-tt-eb.ebRowid
            NO-ERROR. 

        IF AVAILABLE eb THEN 
        DO:

            lSaved = (eb.form-no EQ bf-tt-eb.form-no
                AND eb.blank-no EQ bf-tt-eb.blank-no
                AND eb.num-wid EQ (IF est.est-type GE 5 THEN bf-tt-eb.num-len ELSE bf-tt-eb.num-wid)
                AND eb.num-len EQ (IF est.est-type GE 5 THEN bf-tt-eb.num-wid ELSE bf-tt-eb.num-len)                  
                AND eb.num-up EQ bf-tt-eb.num-up)
                .

            /* For type 2, cust-% is displayed at yld-qty, but not updateable */
            /* Using tt-eb.yld-qty as the override, comparing that to calcYieldQty */
            IF lSaved EQ TRUE AND est.est-type NE 2 THEN
                lSaved = eb.yld-qty EQ (IF bf-tt-eb.yld-qty GT 0 THEN bf-tt-eb.yld-qty ELSE bf-tt-eb.calcYieldQty).

    
            /* bl-qty has a different meaning for type 2 and 6, so can't assign directly */
            IF lSaved AND NOT (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                lSaved = eb.bl-qty EQ bf-tt-eb.bl-qty.


        END.
        IF NOT lSaved THEN
            LEAVE.
    END. 
    oplAllSaved = lSaved.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createReftable B-table-Win 
PROCEDURE createReftable :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-tt-eb FOR tt-eb.

    FOR EACH multbl {&where-multbl} USE-INDEX reftable:
    DELETE multbl.
END.

    
FOR EACH ef FIELDS(est-no form-no board brd-dscr) NO-LOCK 
    WHERE ef.company EQ est.company
      AND ef.est-no  EQ est.est-no,
    EACH  bf-tt-eb NO-LOCK
    WHERE bf-tt-eb.company EQ est.company
      AND bf-tt-eb.est-no  EQ ef.est-no
    /* AND bf-tt-eb.form-no EQ ef.form-no */:
  
    FIND eb NO-LOCK WHERE ROWID(eb) EQ bf-tt-eb.ebRowid NO-ERROR.
  
    CREATE multbl.
    ASSIGN
        multbl.reftable = "est\d-multbl.w"
        multbl.company  = est.company
        multbl.loc      = est.loc
        multbl.code     = est.est-no
        multbl.code2    = ef.board
        multbl.dscr     = ef.brd-dscr
        multbl.val[1]   = bf-tt-eb.form-no
        multbl.val[2]   = bf-tt-eb.blank-no
        multbl.val[3]   = DEC(RECID(eb)).
    RELEASE multbl.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE del-ref-records B-table-Win 
PROCEDURE del-ref-records :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FOR EACH multbl {&where-multbl} USE-INDEX reftable:
    DELETE multbl.
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
    DEFINE VARIABLE lv-die-in      LIKE ef.die-in NO-UNDO.
    DEFINE VARIABLE ll-die-changed AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-ans         AS LOG     NO-UNDO.
    DEFINE VARIABLE li-qty         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-frm         LIKE tt-eb.form-no INIT 0 NO-UNDO.
    DEFINE VARIABLE lv-blk         LIKE tt-eb.blank-no INIT 0 NO-UNDO.

    FIND CURRENT tt-eb.
    IF NOT AVAILABLE est THEN
        FIND FIRST est NO-LOCK WHERE est.company EQ tt-eb.company
            AND est.est-no EQ tt-eb.est-no
            NO-ERROR.
    /* Made decision to take these updates out pending further review */
    /* for ticket 19568                                               */
    /*   FIND CURRENT est EXCLUSIVE.                                                                  */
    /*                                                                                                */
    /*   lv-die-in = tt-eb.die-in.                                                                    */
    /*   IF tt-eb.die-in NE 0 THEN tt-eb.die-in = (tt-eb.die-in / v-num-up) * tt-eb.num-up.           */
    /*   IF lv-die-in NE tt-eb.die-in THEN ll-die-changed = YES.                                      */
    /*                                                                                                */
    /*   RELEASE xef.                                                                                 */
    /*   IF ll-change         AND                                                                     */
    /*      est.est-type NE 2 AND                                                                     */
    /*      est.est-type NE 5 AND                                                                     */
    /*      est.est-type NE 6 THEN                                                                    */
    /*   FIND FIRST xef                                                                               */
    /*       WHERE xef.company EQ tt-eb.company                                                       */
    /*         AND xef.est-no  EQ tt-eb.est-no                                                        */
    /*         AND xef.form-no EQ tt-eb.form-no                                                       */
    /*       NO-LOCK NO-ERROR.                                                                        */
    /*                                                                                                */
    /*   IF AVAIL xef THEN DO:                                                                        */
    /*     v-qty = tt-eb.yld-qty / tt-eb.num-up.                                                      */
    /*     {sys/inc/roundup.i v-qty}                                                                  */
    /*     ASSIGN                                                                                     */
    /*      tt-eb.yld-qty = v-qty * tt-eb.num-up                                                      */
    /*      /*xef.die-in = tt-eb.die-in*/.                                                            */
    /*                                                                                                */
    /*     ll-ans = NO.                                                                               */
    /*                                                                                                */
    /*     FOR EACH xeb                                                                               */
    /*         WHERE xeb.company EQ xef.company                                                       */
    /*           AND xeb.est-no  EQ xef.est-no                                                        */
    /*           AND xeb.form-no EQ xef.form-no                                                       */
    /*           AND decimal(RECID(xeb))  NE tt-eb.dec-recid                                          */
    /*           AND xeb.yld-qty NE v-qty * xeb.num-up:                                               */
    /*       ll-ans = lUseAlternateQty.                                                               */
    /* /*      ll-ans = YES.                                                                       */ */
    /* /*      MESSAGE "For all other Blanks on this Form..." SKIP                                 */ */
    /* /*              "Click the YES button to calculate the layout based on the Request Qty" SKIP*/ */
    /* /*              "Click the NO  button to calculate the layout based on the Yield Qty"       */ */
    /* /*          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                                        */ */
    /* /*          UPDATE ll-ans.                                                                  */ */
    /*       LEAVE.                                                                                   */
    /*     END.                                                                                       */
    /*     RELEASE xeb.                                                                               */
    /*                                                                                                */
    /*     FOR EACH xeb                                                                               */
    /*         WHERE xeb.company EQ xef.company                                                       */
    /*           AND xeb.est-no  EQ xef.est-no                                                        */
    /*           AND xeb.form-no EQ xef.form-no                                                       */
    /*           AND ROWID(xeb)  NE ROWID(tt-eb)                                                      */
    /*           AND xeb.yld-qty NE v-qty * xeb.num-up                                                */
    /*         BY xeb.blank-no:                                                                       */
    /*       /*IF xeb.yld-qty LT xeb.bl-qty THEN xeb.yld-qty = xeb.bl-qty.*/                          */
    /*       IF xeb.yld-qty EQ 0 THEN xeb.yld-qty = xeb.bl-qty.                                       */
    /*                                                                                                */
    /*       ASSIGN                                                                                   */
    /*        li-qty      = IF ll-ans THEN xeb.bl-qty ELSE xeb.yld-qty                                */
    /*        lv-die-in   = xeb.die-in                                                                */
    /*        xeb.die-in  = xeb.die-in / xeb.num-up                                                   */
    /*        xeb.num-up  = TRUNC(li-qty / v-qty,0) + INT(li-qty MODULO v-qty GT 0)                   */
    /*        xeb.die-in  = xeb.die-in * xeb.num-up                                                   */
    /*        xeb.yld-qty = v-qty * xeb.num-up                                                        */
    /*        /*xef.die-in  = xef.die-in + xeb.die-in*/.                                              */
    /*                                                                                                */
    /*       IF lv-die-in NE xeb.die-in THEN ll-die-changed = YES.                                    */
    /*                                                                                                */
    /*       IF xeb.num-wid * xeb.num-len NE xeb.num-up THEN                                          */
    /*         ASSIGN                                                                                 */
    /*          xeb.num-wid = xeb.num-up                                                              */
    /*          xeb.num-len = 1.                                                                      */
    /*     END.                                                                                       */
    /*     RELEASE xeb.                                                                               */
    /*   END.                                                                                         */
    /*                                                                                                */
    /*   FIND CURRENT tt-eb  NO-LOCK.                                                                 */
    /*   FIND CURRENT est NO-LOCK.                                                                    */
    /*                                                                                                */
    /*   IF ll-die-changed THEN RUN est/updefdie.p (ROWID(ef)).                                       */

    FOR EACH multbl
        WHERE multbl.reftable EQ "est\d-multbl.w"
          AND multbl.company  EQ est.company
          AND multbl.loc      EQ est.loc
          AND multbl.code     EQ est.est-no
        BY multbl.val[1] DESCENDING
        BY multbl.val[2] DESCENDING:

        ASSIGN
            multbl.val[1] = (multbl.val[1] * 1000) +
                     (1 * (IF multbl.val[1] LT v-form-no THEN -1 ELSE 1))
            multbl.val[2] = (multbl.val[2] * 1000) +
                     (1 * (IF multbl.val[2] LT v-blank-no THEN -1 ELSE 1)).
    END.

    FOR EACH multbl
        WHERE multbl.reftable EQ "est\d-multbl.w"
          AND multbl.company  EQ est.company
          AND multbl.loc      EQ est.loc
          AND multbl.code     EQ est.est-no
        BREAK BY multbl.val[1]
        BY multbl.val[2]:

        IF FIRST-OF(multbl.val[1]) THEN lv-frm = lv-frm + 1.

        ASSIGN
            lv-blk        = lv-blk + 1
            multbl.val[1] = lv-frm
            multbl.val[2] = lv-blk.

        IF LAST-OF(multbl.val[1]) THEN lv-blk = 0.
    END.

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
    DEFINE OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.

    op-rowid = ROWID(tt-eb).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeMulti B-table-Win 
PROCEDURE initializeMulti :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    FOR EACH ef FIELDS(est-no form-no board brd-dscr) NO-LOCK
        WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no,
        EACH eb FIELDS(form-no blank-no) NO-LOCK
        WHERE eb.company EQ est.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no
        BY eb.form-no BY eb.blank-no:

        CREATE multbl.
        ASSIGN
            multbl.reftable = "est\d-multbl.w"
            multbl.company  = est.company
            multbl.loc      = est.loc
            multbl.code     = est.est-no
            multbl.code2    = ef.board
            multbl.dscr     = ef.brd-dscr
            multbl.val[1]   = eb.form-no
            multbl.val[2]   = eb.blank-no
            multbl.val[3]   = DEC(RECID(eb)).
        RELEASE multbl.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadTempTable B-table-Win 
PROCEDURE loadTempTable :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo LIKE est.est-no.

    DEFINE VARIABLE cEstNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

    cEstNo = ipcEstNo.
    cCompany = ipcCompany.

    EMPTY TEMP-TABLE tt-eb.
    FOR EACH eb NO-LOCK WHERE eb.company EQ cCompany
        AND eb.est-no EQ cEstNo  :
        FIND FIRST est OF eb NO-LOCK.

        CREATE tt-eb.
        BUFFER-COPY eb TO tt-eb.
        ASSIGN 
            tt-eb.dec-recid = DECIMAL(RECID(eb))
            tt-eb.ebRowid   = ROWID(eb)
            .

        /* Save to compare with calculated value */
        tt-eb.yld-qty-orig = tt-eb.yld-qty.

        /* Setting this way on create of temp-table, write it back to same field */
        tt-eb.calcYieldQty = IF est.est-type EQ 2 THEN  eb.cust-%  
        ELSE  IF est.est-type EQ 6 THEN eb.yld-qty 
        ELSE eb.yld-qty.



        /* Setting this way on create of temp-table, write it back to same field */
        tt-eb.num-wid = IF est.est-type GE 5 THEN eb.num-len ELSE eb.num-wid.

        /* Setting this way on create of temp-table, write it back to same field */
        tt-eb.num-len = IF est.est-type GE 5 THEN eb.num-wid ELSE eb.num-len.

        /* Setting this way on create of temp-table, write it back to same field */

        tt-eb.bl-qty = display-bl-qty().

        /* Yld-qty is the override quantity on the screen - calcYieldQty will be written to yld-qty on save */
        IF tt-eb.calcYieldQty EQ eb.yld-qty THEN
            tt-eb.yld-qty = 0.

    END.
    FIND FIRST eb NO-LOCK WHERE ROWID(eb) = lv-rowid NO-ERROR.
    IF AVAILABLE eb THEN 
      FIND FIRST tt-eb WHERE tt-eb.ebRowid   = ROWID(eb) NO-ERROR.

    RUN recalcAll.

    FOR EACH tt-eb:
        /* If calculated value is different, assume stored value was an override */
        IF tt-eb.effectiveYldQty NE tt-eb.yld-qty-orig THEN
            tt-eb.yld-qty = tt-eb.yld-qty-orig.
        ELSE 
            tt-eb.yld-qty = 0.
      
    END.

    RUN createReftable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-bl-qty  LIKE tt-eb.bl-qty NO-UNDO.
    DEFINE VARIABLE lv-yld-qty LIKE tt-eb.yld-qty NO-UNDO.
    DEFINE VARIABLE lv-field   AS CHARACTER NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN
        lv-bl-qty  = tt-eb.bl-qty
        lv-yld-qty = tt-eb.yld-qty.

    DO WITH FRAME {&FRAME-NAME}:
        IF est.est-type GE 5 THEN
            ASSIGN
                lv-field                                            = tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}
                tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name} = tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name}
                tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name} = lv-field.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        tt-eb.num-up = INT(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).

        IF est.est-type EQ 2 OR
            est.est-type EQ 5 OR
            est.est-type EQ 6 THEN
            ASSIGN
                tt-eb.bl-qty  = lv-bl-qty
                tt-eb.yld-qty = lv-yld-qty.
    END.

    FOR EACH b-ref NO-LOCK
        WHERE b-ref.reftable EQ reftable.reftable
        AND b-ref.company  EQ reftable.company
        AND b-ref.loc      EQ reftable.loc
        AND b-ref.code     EQ reftable.code
        AND b-ref.val[1]   EQ reftable.val[1]
        AND b-ref.val[2]   GE reftable.val[2]
        AND ROWID(b-ref)   NE ROWID(reftable)
        BY b-ref.val[2] DESCENDING:
        b-ref.val[2] = b-ref.val[2] + 1.
    END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    /*{custom/askdel.i}  */

    /* Code placed here will execute PRIOR to standard behavior. */

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
        APPLY "entry" TO tt-eb.form-no IN BROWSE {&browse-name}.
        ll-new-form = NO.

        ASSIGN
            hld-yld-qty = tt-eb.yld-qty
            hld-num-up  = tt-eb.num-up.
       
        IF hld-yld-qty EQ 0 THEN 
        DO:
            FIND FIRST xeb NO-LOCK
                WHERE xeb.company EQ tt-eb.company
                AND xeb.est-no  EQ tt-eb.est-no
                AND xeb.form-no EQ tt-eb.form-no
                /* AND decimal(RECID(xeb))  NE tt-eb.dec-recid */
                AND ROWID(xeb) NE ROWID(tt-eb) /* Since xeb is now a buffer for tt-eb */
                 NO-ERROR.
            IF AVAILABLE xeb THEN 
            DO:
                v-qty = xeb.yld-qty / xeb.num-up.
                {sys/inc/roundup.i v-qty}
            END.
            ELSE v-qty = 0.
        END.
         
        ELSE 
        DO:
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
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID     NO-UNDO.
  
    /* Code placed here will execute PRIOR to standard behavior. */

  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */


    DEFINE VARIABLE cEstNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"release-source",OUTPUT char-hdl). 
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
        RUN getEstRow IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).
 
    FIND FIRST eb WHERE ROWID(eb) = lv-rowid NO-LOCK NO-ERROR. 

    IF AVAILABLE eb THEN 
    DO:
        ASSIGN 
            cEstNo   = eb.est-no
            cCompany = eb.company
            .
/*        FIND FIRST est NO-LOCK  OF eb NO-ERROR.                      */
/*        IF AVAILABLE est AND (est.est-type EQ 2 OR                   */
/*            est.est-type EQ 5 OR                                     */
/*            est.est-type EQ 6) THEN                                  */
/*        DO WITH FRAME {&FRAME-NAME}:                                 */
/*            tt-eb.yld-qty:LABEL IN BROWSE {&browse-name} = "Qty/Set".*/
/*        END.                                                         */
    END.

    RUN loadTempTable (INPUT cCompany, INPUT cEstNo).
    /*          END. */
    RUN recalcAll.

/* to implement */
/* APPLY 'choose' TO btRecalc. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */
    lv-rowid = ROWID(tt-eb).

    RUN calc-#up.

    RUN valid-form-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-blank-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-bl-yld-up NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN
        ll-change  = NO
        v-form-no  = tt-eb.form-no
        v-blank-no = tt-eb.blank-no
        v-num-up   = tt-eb.num-up.

    DO WITH FRAME {&frame-name}:
        ll-change = tt-eb.bl-qty  NE DEC(tt-eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name})  OR
            tt-eb.yld-qty NE DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) OR
            tt-eb.num-up  NE DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
  
    /* Save calculated fields that have been updated */
    RUN saveRowValues.
  
    RUN finish-assign.

    RUN dispatch ("open-query").

    RUN repo-query (lv-rowid).
     
    RUN recalcAll.

    ll-new-form = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-forms B-table-Win 
PROCEDURE new-forms :
/*------------------------------------------------------------------------------
      Purpose:     Create ef records for new forms
      Parameters:  <none>
      Notes:       Run on submit
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-ref FOR reftable.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.

  
    FOR EACH multbl {&where-multbl} USE-INDEX reftable,
      FIRST eb EXCLUSIVE-LOCK
      WHERE RECID(eb) EQ INT(multbl.val[3])
        AND NOT CAN-FIND(FIRST ef
                         WHERE ef.company EQ eb.company
                           AND ef.est-no  EQ eb.est-no
                           AND ef.form-no EQ multbl.val[1]),
      FIRST ef EXCLUSIVE-LOCK
      WHERE ef.company EQ eb.company
        AND ef.est-no  EQ eb.est-no
        AND ef.form-no EQ eb.form-no
      BREAK BY multbl.val[1]
            BY multbl.val[2]:

    IF FIRST-OF(multbl.val[1]) THEN
        IF CAN-FIND(FIRST b-ref
            WHERE b-ref.reftable EQ multbl.reftable
            AND b-ref.company  EQ multbl.company
            AND b-ref.loc      EQ multbl.loc
            AND b-ref.code     EQ multbl.code
            AND b-ref.val[1]   EQ ef.form-no
            AND ROWID(b-ref)   NE ROWID(multbl)) THEN 
        DO:
            CREATE b-ef.
            BUFFER-COPY ef EXCEPT rec_key TO b-ef
                ASSIGN
                b-ef.form-no   = multbl.val[1]
                b-ef.blank-qty = 1.

        {sys/inc/xeb-form.i "eb." "0" "multbl.val[1]" "0"}
        END.

        ELSE 
        DO:


        {sys/inc/xeb-form.i "eb." "0" "multbl.val[1]" "0"}

            ef.form-no  = multbl.val[1].

        END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-new-forms B-table-Win 
PROCEDURE process-new-forms :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-fb-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE li            AS INTEGER   NO-UNDO.

    FOR EACH multbl NO-LOCK {&where-multbl} USE-INDEX reftable,
      FIRST eb NO-LOCK
      WHERE RECID(eb)    EQ INT(multbl.val[3])
        AND (eb.form-no  NE multbl.val[1] OR
             eb.blank-no NE multbl.val[2]):
      ll-fb-changed = YES.
      LEAVE.
    END.
    
    op-fb-changed = ll-fb-changed.
    
    IF ll-fb-changed THEN 
    DO:
        RUN new-forms.
    
        FOR EACH multbl {&where-multbl} USE-INDEX reftable,
            FIRST eb EXCLUSIVE-LOCK
            WHERE RECID(eb)    EQ INT(multbl.val[3])
              AND eb.blank-no  LT 999
              AND (eb.form-no  NE multbl.val[1] OR
                   eb.blank-no NE multbl.val[2]):
    
            IF eb.form-no NE multbl.val[1] THEN 
            DO:
                {sys/inc/xeb-form.i "eb." "0" "multbl.val[1]" "0"}
            END.
        
            multbl.val[2] = (multbl.val[2] * 1000) +
                (1 * (IF multbl.val[2] LT eb.blank-no THEN -1 ELSE 1)).
        
              {sys/inc/xeb-form.i "eb." "eb.blank-no" "multbl.val[1]" "multbl.val[2] * 1000"}
        
            ASSIGN
                eb.form-no  = multbl.val[1]
                eb.blank-no = multbl.val[2] * 1000.
        
            RUN update-ef-board-proc.
        END.
    
        RUN del-ref-records.
        
        RUN est/resetf&b.p (ROWID(est), NO).
        
        li = 0.
        FOR EACH est-op EXCLUSIVE-LOCK
            WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.line    LT 500
            BY est-op.qty
            BY est-op.s-num
            BY est-op.b-num
            BY est-op.d-seq
            BY est-op.op-pass
            BY est-op.rec_key:
              
              {sys/inc/outstrPL.i est-op SHARE}
            ASSIGN
                li          = li + 1
                est-op.line = li.
             
            IF AVAILABLE reftable THEN reftable.loc = STRING(est-op.line,"9999999999"). 
        END. /* each est-op */
    END. /* If changed */

RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-target", OUTPUT char-hdl).

RUN get-eb-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT io-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcAll B-table-Win 
PROCEDURE recalcAll :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dMaxSheets AS DECIMAL NO-UNDO.
    DEFINE VARIABLE rtt-ebRow AS ROWID NO-UNDO.
    
    rtt-ebRow = ?.
    IF AVAIL tt-eb THEN
      rtt-ebRow = ROWID(tt-eb).

    FOR EACH tt-eb BREAK BY tt-eb.form-no:
    
        IF FIRST-OF(tt-eb.form-no) THEN 
        DO:
            /* Calculate maximum sheets requested for this form */
            dMaxSheets = 0.
            FOR EACH bf-tt-eb WHERE bf-tt-eb.form-no EQ 
                tt-eb.form-no:
                /* bl-qty is requested sheets quantity */
                IF bf-tt-eb.bl-qty GT dMaxSheets THEN 
                    dMaxSheets = bf-tt-eb.bl-qty / bf-tt-eb.num-up.
            END.
            {sys/inc/roundup.i dMaxSheets}
        END.
    
        tt-eb.sheetsRequired   = tt-eb.bl-qty / tt-eb.num-up.
    
        {sys/inc/roundup.i tt-eb.sheetsRequired} /* sheets req'd / num up, rounded up */
        
        ASSIGN 
            tt-eb.maxSheetsPerForm = dMaxSheets 
            tt-eb.calcYieldQty     = tt-eb.num-up * tt-eb.maxSheetsPerForm        
            tt-eb.effectiveYldQty  = (IF tt-eb.yld-qty GT 0 THEN  tt-eb.yld-qty ELSE 
                                  tt-eb.calcYieldQty)
            tt-eb.surplusQty       = (IF tt-eb.bl-qty GT 0 THEN tt-eb.effectiveYldQty - tt-eb.bl-qty ELSE 0)
            .

      
    END. 
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
    IF rtt-ebRow NE ? THEN DO:

      RUN repo-query (INPUT rtt-ebRow ).

    END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcAllNumUp B-table-Win 
PROCEDURE recalcAllNumUp :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       -Not yet implemented (ticket 19568), code preserved from original
                     version of the program
                   -Original logic from finish-assign to recalc num-up
                   -Dependency on reftable record, est record, eb record, ef rec
                   -If this entire procedure is not run, at least the update to 
                    die must occur when num-up changes for data integrity
                   -Run from saveFormBlankChanges so that reftable exists
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-die-in      LIKE ef.die-in NO-UNDO.
    DEFINE VARIABLE ll-die-changed AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-ans         AS LOG     NO-UNDO.
    DEFINE VARIABLE li-qty         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-frm         LIKE eb.form-no INIT 0 NO-UNDO.
    DEFINE VARIABLE lv-blk         LIKE eb.blank-no INIT 0 NO-UNDO.
    DEFINE VARIABLE li-num-up      AS INTEGER NO-UNDO.
    DEFINE BUFFER multbl FOR reftable.


    FIND CURRENT eb.
    FIND CURRENT est.
    FIND FIRST ef NO-LOCK WHERE ef.company = eb.company 
        AND ef.loc = eb.loc
        AND ef.est-no = eb.est-no 
        AND ef.form-no = eb.form-no 
        NO-ERROR.
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
            AND xef.form-no EQ reftable.val[1]
            NO-LOCK NO-ERROR.

    IF AVAILABLE xef THEN 
    DO:
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
                li-qty    = IF ll-ans THEN xeb.bl-qty ELSE xeb.yld-qty
                lv-die-in = xeb.die-in
                li-num-up = xeb.num-up.

            ASSIGN 
                xeb.num-up  = TRUNC(li-qty / v-qty,0) + INT(li-qty MODULO v-qty GT 0)
                xeb.yld-qty = v-qty * xeb.num-up
                .
       
            /* Update die-in in response to change in num-up */
            RUN updateDieIn (INPUT li-num-up, INPUT xeb.num-up).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcCurrent B-table-Win 
PROCEDURE recalcCurrent :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dMaxSheets AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSheetReqd AS DECIMAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name} =
            STRING(DEC(tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}) *
            DEC(tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name})).
    END.
  

    /* Calculate mraximum sheets requested for this form */
    dMaxSheets = 0.
    FOR EACH bf2-tt-eb WHERE bf2-tt-eb.form-no EQ 
        tt-eb.form-no
        AND bf2-tt-eb.blank-no NE int(tt-eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}):
        /* bl-qty is requested sheets quantity */
        IF bf2-tt-eb.bl-qty GT dMaxSheets THEN 
            dMaxSheets = bf2-tt-eb.bl-qty / bf2-tt-eb.num-up.
    END.

   {sys/inc/roundup.i dMaxSheets}
    
    tt-eb.sheetsRequired:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(DECIMAL(tt-eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name}) 
        / DECIMAL(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name})).
    dSheetReqd = DECIMAL(tt-eb.sheetsRequired:SCREEN-VALUE IN BROWSE {&browse-name}).
    {sys/inc/roundup.i dSheetReqd} /* sheets req'd / num up, rounded up */

    IF dSheetReqd GT dMaxSheets THEN
        dMaxSheets = dSheetReqd.
    tt-eb.sheetsRequired:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dSheetReqd).

    ASSIGN 
        tt-eb.maxSheetsPerForm:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dMaxSheets)
        tt-eb.calcYieldQty:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(DECIMAL(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) * dMaxSheets)
        /* tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}          = STRING(DECIMAL(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) * dMaxSheets) */
        tt-eb.effectiveYldQty:SCREEN-VALUE IN BROWSE {&browse-name}  = (IF INTEGER(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN  
                                                                          tt-eb.yld-Qty:SCREEN-VALUE IN BROWSE {&browse-name} 
                                                                        ELSE 
                                                                          tt-eb.calcYieldQty:SCREEN-VALUE IN BROWSE {&browse-name})
        tt-eb.surplusQty:SCREEN-VALUE IN BROWSE {&browse-name}       = (IF INTEGER(tt-eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN 
                                                                           STRING( INTEGER ( tt-eb.effectiveYldQty:SCREEN-VALUE IN BROWSE {&browse-name} ) - INTEGER (tt-eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name})) 
          
                                                                        ELSE "")
        .


    RUN saveRowValues.
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
    IF ip-rowid <> ? THEN
        REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
    
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
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  
    DO WITH FRAME {&frame-name}:
        REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
        RUN dispatch ("row-changed").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetAll B-table-Win 
PROCEDURE resetAll :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rowid AS ROWID     NO-UNDO.

    DEFINE VARIABLE cEstNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"release-source",OUTPUT char-hdl). 
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
        RUN getEstRow IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).

    FIND FIRST eb WHERE ROWID(eb) = lv-rowid NO-LOCK NO-ERROR. 

    IF AVAILABLE eb THEN 
    DO:
        ASSIGN 
            cEstNo   = eb.est-no
            cCompany = eb.company
            .

        RUN loadTempTable (cCompany, cEstNo).
        RUN recalcAll.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAll B-table-Win 
PROCEDURE saveAll :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrigNumUp        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNewNumUp         AS INTEGER NO-UNDO.
    DEFINE VARIABLE isFormBlankChange AS LOGICAL NO-UNDO.

    RUN valid-frm-bl.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    isFormBlankChange = NO.
    FOR EACH tt-eb:  
        FIND eb EXCLUSIVE-LOCK WHERE ROWID(eb) EQ tt-eb.ebRowid
            NO-ERROR. 

        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST ef NO-LOCK WHERE ef.company = eb.company 
                AND ef.loc = eb.loc
                AND ef.est-no = eb.est-no 
                AND ef.form-no = eb.form-no 
                NO-ERROR.

            iOrigNumUp = eb.num-up.
            IF eb.form-no NE tt-eb.form-no OR eb.blank-no  NE tt-eb.blank-no  THEN
                isFormBlankChange = TRUE.

            ASSIGN             
                eb.num-wid = IF est.est-type GE 5 THEN tt-eb.num-len ELSE tt-eb.num-wid
                eb.num-len = IF est.est-type GE 5 THEN tt-eb.num-wid ELSE tt-eb.num-len             
                eb.num-up  = tt-eb.num-up
                eb.yrprice = tt-eb.yrprice
                eb.part-no = tt-eb.part-no
                .
            iNewNumUp = eb.num-up.

            /* For type 2, cust-% is displayed at yld-qty, but not updateable */
            IF est.est-type NE 2 THEN
                eb.yld-qty = (IF tt-eb.yld-qty GT 0 THEN tt-eb.yld-qty ELSE tt-eb.calcYieldQty).
  
            /* bl-qty has a different meaning for type 2 and 6, so can't assign directly */
            IF NOT (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                eb.bl-qty = tt-eb.bl-qty.                      
           

            /* Handle any change to num-up, uses buffer xeb */
            IF iOrigNumUp NE iNewNumUp THEN 
            DO:
                FIND xeb EXCLUSIVE-LOCK WHERE ROWID(xeb) EQ ROWID(eb) 
                    NO-ERROR.
                RUN updateDieIn (INPUT iOrigNumUp, INPUT iNewNumUp).
                FIND CURRENT xeb NO-LOCK.
            END.
           
        END.
        RELEASE eb.
    END. 
    
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"release-source",OUTPUT char-hdl). 
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
        RUN setChangesMade IN WIDGET-HANDLE(char-hdl) (INPUT YES).
 
    /* Handle any form/blank changes for all tt-eb */
    IF isFormBlankChange THEN
        RUN saveFormBlankChanges.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveFormBlankChanges B-table-Win 
PROCEDURE saveFormBlankChanges :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-frm        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-blk        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-fb-changed AS LOGICAL   NO-UNDO.

    /* Create multbl records to perform update */



  
    /* Create a multbl for each eb of estimate to process form and blank changes */                              
    FOR EACH multbl {&where-multbl} USE-INDEX reftable:
    DELETE multbl.
END.
  
/* Note: a new form-no had previously been added to ef during entry of a line */
FOR EACH ef FIELDS(est-no form-no board brd-dscr) NO-LOCK 
    WHERE ef.company EQ est.company
      AND ef.est-no  EQ est.est-no,
    EACH  tt-eb NO-LOCK
    WHERE tt-eb.company EQ est.company
      AND tt-eb.est-no  EQ ef.est-no
    /* AND tt-eb.form-no EQ ef.form-no */:

    FIND eb NO-LOCK WHERE ROWID(eb) EQ tt-eb.ebRowid NO-ERROR.

    CREATE multbl.
    ASSIGN
        multbl.reftable = "est\d-multbl.w"
        multbl.company  = est.company
        multbl.loc      = est.loc
        multbl.code     = est.est-no
        multbl.code2    = ef.board
        multbl.dscr     = ef.brd-dscr
        multbl.val[1]   = tt-eb.form-no
        multbl.val[2]   = tt-eb.blank-no
        multbl.val[3]   = DEC(RECID(eb))
        .
        
    RELEASE multbl.
END.

FOR EACH tt-eb:

    /* Code Prior to Finish Assign Logic */
    FIND eb NO-LOCK WHERE ROWID(eb) EQ tt-eb.ebRowid NO-ERROR.

    ASSIGN
        ll-change  = NO
        v-form-no  = eb.form-no
        v-blank-no = eb.blank-no
        v-num-up   = eb.num-up
        .

    ll-change = tt-eb.bl-qty  NE eb.bl-qty  OR
                tt-eb.yld-qty NE eb.yld-qty OR
                tt-eb.num-up  NE eb.num-up.

    /* Finish Assign - Reorder multbl */
    /* Renumbers form & blank compared to original */
  
    IF ll-change THEN 
    DO:
        /* If form or blank was changed, renumber other lines */

        /* Lines below the current have negative values, Lines above the current have positive values */
        FOR EACH multbl EXCLUSIVE-LOCK
            WHERE multbl.reftable EQ "est\d-multbl.w"
            AND multbl.company  EQ est.company
            AND multbl.loc      EQ est.loc
            AND multbl.code     EQ est.est-no
            BY multbl.val[1] DESCENDING
            BY multbl.val[2] DESCENDING:
    
            ASSIGN
                multbl.val[1] = (multbl.val[1] * 1000) +
                         (1 * (IF multbl.val[1] LT v-form-no THEN -1 ELSE 1))
                multbl.val[2] = (multbl.val[2] * 1000) +
                         (1 * (IF multbl.val[2] LT v-blank-no THEN -1 ELSE 1)).
        END.
    
      
        /* Renumber form and blank starting from 1 */
        ASSIGN 
            lv-frm = 0
            lv-blk = 0
            .
        FOR EACH multbl EXCLUSIVE-LOCK
            WHERE multbl.reftable EQ "est\d-multbl.w"
            AND multbl.company  EQ est.company
            AND multbl.loc      EQ est.loc
            AND multbl.code     EQ est.est-no
            BREAK BY multbl.val[1]
            BY multbl.val[2]:
    
            IF FIRST-OF(multbl.val[1]) THEN lv-frm = lv-frm + 1.
    
            ASSIGN
                lv-blk        = lv-blk + 1
                multbl.val[1] = lv-frm
                multbl.val[2] = lv-blk.
    
            IF LAST-OF(multbl.val[1]) THEN lv-blk = 0.
        END. /* Each multbl to renumber form and blank */
    END. /* If a change was made */
END.


/* Save Form/Blank changes to all tables */

FOR EACH multbl NO-LOCK {&where-multbl} USE-INDEX reftable,
      FIRST eb NO-LOCK
      WHERE RECID(eb)    EQ INT(multbl.val[3])
        AND (eb.form-no  NE multbl.val[1] OR
             eb.blank-no NE multbl.val[2]):
ll-fb-changed = YES.
LEAVE.
END.

op-fb-changed = ll-fb-changed.

IF ll-fb-changed THEN 
DO:
    RUN new-forms.

    FOR EACH multbl EXCLUSIVE-LOCK {&where-multbl} USE-INDEX reftable,
        FIRST eb EXCLUSIVE-LOCK
        WHERE RECID(eb)    EQ INT(multbl.val[3])
          AND eb.blank-no  LT 999
          AND (eb.form-no  NE multbl.val[1] OR
               eb.blank-no NE multbl.val[2]):

        IF eb.form-no NE multbl.val[1] THEN 
        DO:
            {sys/inc/xeb-form.i "eb." "0" "multbl.val[1]" "0"}
        END.
    
        multbl.val[2] = (multbl.val[2] * 1000) +
            (1 * (IF multbl.val[2] LT eb.blank-no THEN -1 ELSE 1)).
    
         /* Assigns blank-no below, but this gets reset by est/resetf&b */
         /* Updates est-flm and xeb-form-ef */
          {sys/inc/xeb-form.i "eb." "eb.blank-no" "multbl.val[1]" "multbl.val[2] * 1000"}
          
          
        ASSIGN
            eb.form-no  = multbl.val[1]
            eb.blank-no = multbl.val[2] * 1000
            .
        
        RUN update-ef-board-proc.
    END.

RUN del-ref-records.

RUN est/resetf&b.p (ROWID(est), NO).

li = 0.
FOR EACH est-op EXCLUSIVE-LOCK
    WHERE est-op.company EQ est.company
    AND est-op.est-no  EQ est.est-no
    AND est-op.line    LT 500
    BY est-op.qty
    BY est-op.s-num
    BY est-op.b-num
    BY est-op.d-seq
    BY est-op.op-pass
    BY est-op.rec_key:
      
      {sys/inc/outstrPL.i est-op SHARE}
    ASSIGN
        li          = li + 1
        est-op.line = li.
     
    IF AVAILABLE reftable THEN reftable.loc = STRING(est-op.line,"9999999999"). 
END.
END. /* If form or blank changed */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRowValues B-table-Win 
PROCEDURE saveRowValues :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    ASSIGN 
        tt-eb.form-no          = INTEGER(tt-eb.form-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        tt-eb.blank-no         = INTEGER(tt-eb.blank-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        tt-eb.bl-qty           = INTEGER(tt-eb.bl-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        tt-eb.yld-qty          = INTEGER(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        tt-eb.num-len          = INTEGER(tt-eb.num-len:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        tt-eb.num-wid          = INTEGER(tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        tt-eb.maxSheetsPerForm = INTEGER(tt-eb.maxSheetsPerForm:SCREEN-VALUE IN BROWSE {&browse-name})
        tt-eb.calcYieldQty     = INTEGER(tt-eb.calcYieldQty:SCREEN-VALUE IN BROWSE {&browse-name})       
        tt-eb.effectiveYldQty  = INTEGER(tt-eb.effectiveYldQty:SCREEN-VALUE IN BROWSE {&browse-name})
        tt-eb.surplusQty       = INTEGER(tt-eb.surplusQty:SCREEN-VALUE IN BROWSE {&browse-name})
        tt-eb.sheetsRequired   = INTEGER(tt-eb.sheetsRequired:SCREEN-VALUE IN BROWSE {&browse-name})
        .
/* RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) . */
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
  {src/adm/template/snd-list.i "tt-eb"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCalcBasis B-table-Win 
PROCEDURE setCalcBasis :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplUseAlternateQty AS LOGICAL NO-UNDO.
    lUseAlternateQty = iplUseAlternateQty.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ef-board-proc B-table-Win 
PROCEDURE update-ef-board-proc :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF ef.

    FIND FIRST ef EXCLUSIVE-LOCK WHERE
        ef.company EQ cocode AND
        ef.est-no EQ eb.est-no AND
        ef.eqty EQ eb.eqty AND
        ef.form-no EQ eb.form-no.

    IF AVAILABLE ef THEN
    DO:
        ASSIGN
            ef.board    = multbl.code2
            ef.brd-dscr = multbl.dscr.
        RELEASE ef.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateDieIn B-table-Win 
PROCEDURE updateDieIn :
/*------------------------------------------------------------------------------
      Purpose:     Update die-in in response to change in eb.num-up
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiOldNumUp AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNewNumUp AS INTEGER     NO-UNDO.

    ASSIGN 
        xeb.die-in = xeb.die-in / ipiOldNumUp  
        xeb.die-in = xeb.die-in * ipiNewNumUp
        .
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
    DEFINE VARIABLE ll-ans             AS LOG     NO-UNDO.
    DEFINE VARIABLE ll-one-bl-per-form AS LOG     NO-UNDO.
    DEFINE VARIABLE li-first-bl        AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-cnt             AS INTEGER NO-UNDO.
    DEFINE BUFFER b-ref FOR reftable.
    
    ll-one-bl-per-form = YES.

    /* Check if one board per form */
    FOR EACH b-ref NO-LOCK
        WHERE b-ref.reftable EQ "est\d-multbl.w"
        AND b-ref.company  EQ est.company
        AND b-ref.loc      EQ est.loc
        AND b-ref.code     EQ est.est-no:  

        IF b-ref.val[2] GT 1 THEN 
        DO:
            ll-one-bl-per-form = NO.
            LEAVE.
        END.

    END.

    IF est.est-type NE 2 AND
        est.est-type NE 5 AND
        est.est-type NE 6 THEN 
    DO WITH FRAME {&FRAME-NAME}:

        IF DEC(tt-eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN 
        DO:
            MESSAGE "Request Qty may not be zero..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-eb.bl-qty IN BROWSE {&browse-name}.
            RETURN ERROR.
        END.


        IF DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) LT 1 THEN
            ASSIGN
                tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
                tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
                tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}  = "1".

        IF DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) NE hld-yld-qty AND
            DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) MODULO
            DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0          AND
            v-qty NE 0                                                           AND
            NOT ll-one-bl-per-form                                               THEN 
        DO:
            tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name} =
                STRING(TRUNC(DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) / v-qty,0) +
                INT(DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) MODULO v-qty GT 0)).
        END.
                    
        IF DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) NE hld-num-up THEN 
        DO:
            /*       ll-ans = lManualRecalc.                                                                  */
            /*       IF DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) MODULO                       */
            /*            DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 AND lManualRecalc THEN */
            /*         MESSAGE "Recalculate Yield Qty?"                                                       */
            /*             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                                           */
            /*             UPDATE ll-ans.                                                                     */
            ll-ans = TRUE.
            IF ll-ans THEN 
            DO:
                v-qty = DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name}) / DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).
        {sys/inc/roundup.i v-qty}
                tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
                    STRING(v-qty * DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name})).
            END.
        END.
          
        ASSIGN
            hld-yld-qty = DEC(tt-eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name})
            hld-num-up  = DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}).
        /* taking out, was changing num-wid entered 
            IF DEC(tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}) *
               DEC(tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                DEC(tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
              ASSIGN
               tt-eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name} =
                                      tt-eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}
               tt-eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name} = "1".
        */
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
        IF INT(tt-eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN 
        DO:
            MESSAGE "Blank Number may not be zero..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tt-eb.blank-no IN BROWSE {&browse-name}.
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

    DEFINE VARIABLE lRefExists AS LOGICAL     NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:

  
        lRefExists = FALSE.
        FIND FIRST b-ref NO-LOCK NO-ERROR.
        IF AVAIL b-ref THEN
          lRefExists = TRUE.
        FIND FIRST b-ref NO-LOCK
            WHERE b-ref.reftable EQ "est\d-multbl.w"
            AND b-ref.company  EQ est.company
            AND b-ref.loc      EQ est.loc
            AND b-ref.code     EQ est.est-no
            AND b-ref.val[1]   EQ INT(tt-eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}) 
            NO-ERROR.
        IF NOT AVAILABLE b-ref AND lRefExists THEN 
        DO:
            IF NOT ll-new-form THEN
                MESSAGE "Form# does not exist on this estimate, add a new one?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll-new-form.
         
            IF ll-new-form THEN
                ASSIGN
                    tt-eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
                    /*tt-eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(est.form-qty + 1)*/.

            ELSE 
            DO:
                APPLY "entry" TO tt-eb.form-no IN BROWSE {&browse-name}.
                RETURN ERROR.
            END.
      
        END.
    
        ELSE
            IF INT(tt-eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE
                INT(lv-prev-val-1) THEN 
            DO:
                FOR EACH b-ref NO-LOCK
                    WHERE b-ref.reftable EQ "est\d-multbl.w"
                    AND b-ref.company  EQ est.company
                    AND b-ref.loc      EQ est.loc
                    AND b-ref.code     EQ est.est-no
                    AND b-ref.val[1]   EQ INT(tt-eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name})
                    AND ROWID(b-ref)   NE ROWID(reftable)  
                    BY b-ref.val[2] DESCENDING:
                    LEAVE.
                END.

                tt-eb.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} =
                    STRING((IF AVAILABLE b-ref THEN b-ref.val[2] ELSE 0) + 1).
            END.  

        lv-prev-val-1 = tt-eb.form-no:SCREEN-VALUE IN BROWSE {&browse-name}. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frm-bl B-table-Win 
PROCEDURE valid-frm-bl :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iFormCnt  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankCnt AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    cMessage = "".
    iFormCnt = 0.
    FOR EACH bf-tt-eb BREAK BY bf-tt-eb.form-no:


        IF FIRST-OF(bf-tt-eb.form-no) THEN 
        DO: 
            /* Form may start at # 0, so don't make iFormCnt 1 in that case */
            IF NOT (iFormCnt EQ 0 AND bf-tt-eb.form-no EQ 0) THEN 
            iFormCnt = iFormCnt + 1.
            IF bf-tt-eb.form-no NE iFormCnt THEN 
            DO:
                cMessage = "Form #" + STRING(bf-tt-eb.form-no) + " is out of order and should be #" + STRING(iFormCnt).
                LEAVE.
            END.
            iBlankCnt = 0.
            FOR EACH bf2-tt-eb WHERE bf2-tt-eb.form-no EQ bf-tt-eb.form-no
                BY bf2-tt-eb.blank-no.
                iBlankCnt = iBlankCnt + 1.

                IF bf2-tt-eb.blank-no NE iBlankCnt THEN 
                DO:
                    cMessage = "Form #: " + STRING(bf2-tt-eb.form-no) + ", blank #" + STRING(bf2-tt-eb.blank-no) + " is out of order and should be #" + STRING(iBlankCnt).
                    LEAVE.
                END.

            END. /* each blank of form */

        END. /* First-of form */
  
    END. /* each tt-eb */

    IF cMessage GT "" THEN 
    DO:
        MESSAGE cMessage
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN ERROR.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION comma B-table-Win 
FUNCTION comma RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-bl-qty B-table-Win 
FUNCTION display-bl-qty RETURNS DECIMAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ld-part-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-bl-qty   LIKE eb.bl-qty NO-UNDO.
    DEFINE VARIABLE lv-yld-qty  LIKE eb.yld-qty NO-UNDO.


    IF est.est-type EQ 2 OR est.est-type EQ 6 THEN 
    DO:
        IF est.est-type EQ 2 THEN
            ASSIGN
                lv-bl-qty  = tt-eb.bl-qty
                lv-yld-qty = tt-eb.cust-%.
        ELSE
            ASSIGN
                lv-bl-qty  = est.est-qty[1]
                lv-yld-qty = tt-eb.calcYieldQty.

        {sys/inc/partqty1.i ld-part-qty lv-yld-qty}

        lv-bl-qty = lv-bl-qty * ld-part-qty.
    END.

    ELSE lv-bl-qty = tt-eb.bl-qty.
  
    RETURN lv-bl-qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

