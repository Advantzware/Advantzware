&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: est\b-qtchg.w

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
{sys/inc/var.i "new shared"}
{custom/globdefs.i}
    
cocode = g_company.

{sys/inc/ceprepprice.i}

DEF VAR lv-update-est AS LOG NO-UNDO.

DEF BUFFER b-quotechg FOR quotechg.
DEF BUFFER b-quotehd FOR quotehd.

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
&Scoped-define EXTERNAL-TABLES quoteqty
&Scoped-define FIRST-EXTERNAL-TABLE quoteqty


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR quoteqty.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES quotechg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table quotechg.s-num quotechg.b-num ~
quotechg.bill quotechg.code quotechg.charge quotechg.prep-qty quotechg.cost ~
quotechg.mkup quotechg.spare-dec-1 quotechg.amtz quotechg.amt quotechg.matf ~
quotechg.matm quotechg.labf quotechg.labm quotechg.simon 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table quotechg.s-num ~
quotechg.b-num quotechg.bill quotechg.charge quotechg.prep-qty ~
quotechg.cost quotechg.mkup quotechg.spare-dec-1 quotechg.amtz quotechg.amt ~
quotechg.matf quotechg.matm quotechg.labf quotechg.labm quotechg.simon 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table quotechg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table quotechg
&Scoped-define QUERY-STRING-br_table FOR EACH quotechg WHERE quotechg.company eq quoteqty.company ~
  AND quotechg.loc eq quoteqty.loc ~
  AND quotechg.q-no eq quoteqty.q-no ~
  AND ((ASI.quotechg.line eq quoteqty.line AND quotechg.qty eq quoteqty.qty) OR ~
       (ASI.quotechg.LINE eq 0                 AND quotechg.qty eq 0               )) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH quotechg WHERE quotechg.company eq quoteqty.company ~
  AND quotechg.loc eq quoteqty.loc ~
  AND quotechg.q-no eq quoteqty.q-no ~
  AND ((ASI.quotechg.line eq quoteqty.line AND quotechg.qty eq quoteqty.qty) OR ~
       (ASI.quotechg.LINE eq 0                 AND quotechg.qty eq 0               )) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table quotechg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table quotechg


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
q-no||y|ASI.quotechg.q-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "q-no"':U).

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
      quotechg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      quotechg.s-num COLUMN-LABEL "S" FORMAT ">9":U COLUMN-FONT 0
      quotechg.b-num COLUMN-LABEL "B" FORMAT ">9":U COLUMN-FONT 0
      quotechg.bill FORMAT "!":U COLUMN-FONT 0
      quotechg.code FORMAT "x(5)":U WIDTH 7
      quotechg.charge COLUMN-LABEL "Charge Description" FORMAT "x(20)":U
            COLUMN-FONT 0
      quotechg.prep-qty FORMAT ">>>,>>>,>>9.9<":U WIDTH 21
      quotechg.cost FORMAT ">>,>>9.99":U COLUMN-FONT 0
      quotechg.mkup FORMAT ">>9.99<<":U WIDTH 11 COLUMN-FONT 0
      quotechg.spare-dec-1 COLUMN-LABEL "Price" FORMAT "->>,>>9.99":U
      quotechg.amtz FORMAT ">>9.99":U
      quotechg.amt COLUMN-LABEL "Total" FORMAT ">>,>>9.99":U COLUMN-FONT 0
      quotechg.matf COLUMN-LABEL "Mat'l SU" FORMAT ">>>>9.99":U
            COLUMN-FONT 0
      quotechg.matm FORMAT ">>>>9.99":U COLUMN-FONT 0
      quotechg.labf COLUMN-LABEL "Labor SU" FORMAT ">>>>9.99":U
            COLUMN-FONT 0
      quotechg.labm COLUMN-LABEL "Labor/M" FORMAT ">>>>9.99":U
            COLUMN-FONT 0
      quotechg.simon FORMAT "!":U COLUMN-FONT 0
  ENABLE
      quotechg.s-num
      quotechg.b-num
      quotechg.bill HELP "(L)abor, (M)aterials, (N)o Charge, (T)ime, or (W)ill Advise"
      quotechg.charge
      quotechg.prep-qty
      quotechg.cost
      quotechg.mkup
      quotechg.spare-dec-1
      quotechg.amtz
      quotechg.amt
      quotechg.matf
      quotechg.matm
      quotechg.labf
      quotechg.labm
      quotechg.simon HELP "(S)eparate, (I)ntegrate, (M)aintenance, (O)ther, (N)o charge"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 10.95
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
   External Tables: ASI.quoteqty
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
         HEIGHT             = 11.14
         WIDTH              = 149.6.
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
     _TblList          = "ASI.quotechg WHERE ASI.quoteqty ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.quotechg.company eq ASI.quoteqty.company
  AND ASI.quotechg.loc eq ASI.quoteqty.loc
  AND ASI.quotechg.q-no eq ASI.quoteqty.q-no
  AND ((ASI.quotechg.line eq ASI.quoteqty.line AND ASI.quotechg.qty eq ASI.quoteqty.qty) OR
       (ASI.quotechg.LINE eq 0                 AND ASI.quotechg.qty eq 0               ))"
     _FldNameList[1]   > ASI.quotechg.s-num
"quotechg.s-num" "S" ? "integer" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.quotechg.b-num
"quotechg.b-num" "B" ? "integer" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.quotechg.bill
"quotechg.bill" ? "!" "character" ? ? 0 ? ? ? yes "(L)abor, (M)aterials, (N)o Charge, (T)ime, or (W)ill Advise" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.quotechg.code
"quotechg.code" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.quotechg.charge
"quotechg.charge" "Charge Description" ? "character" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.quotechg.prep-qty
"quotechg.prep-qty" ? ">>>,>>>,>>9.9<" "decimal" ? ? ? ? ? ? yes ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.quotechg.cost
"quotechg.cost" ? ">>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.quotechg.mkup
"quotechg.mkup" ? ">>9.99<<" "decimal" ? ? 0 ? ? ? yes ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.quotechg.spare-dec-1
"quotechg.spare-dec-1" "Price" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.quotechg.amtz
"quotechg.amtz" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.quotechg.amt
"quotechg.amt" "Total" ">>,>>9.99" "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.quotechg.matf
"quotechg.matf" "Mat'l SU" ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.quotechg.matm
"quotechg.matm" ? ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.quotechg.labf
"quotechg.labf" "Labor SU" ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.quotechg.labm
"quotechg.labm" "Labor/M" ? "decimal" ? ? 0 ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.quotechg.simon
"quotechg.simon" ? "!" "character" ? ? 0 ? ? ? yes "(S)eparate, (I)ntegrate, (M)aintenance, (O)ther, (N)o charge" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DEF VAR char-val AS cha NO-UNDO.
    
  CASE FOCUS:NAME:
    WHEN "charge" THEN DO:
      RUN windows/l-prep.w (quoteqty.company, quotechg.charge:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
      IF char-val NE "" THEN 
          ASSIGN 
            quotechg.charge:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val)
            quotechg.CODE:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
    END.
  END.

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


&Scoped-define SELF-NAME quotechg.bill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.bill br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF quotechg.bill IN BROWSE br_table /* Bill */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bill NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotechg.prep-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.prep-qty br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quotechg.prep-qty IN BROWSE br_table /* Qty */
DO:
    RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotechg.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.cost br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quotechg.cost IN BROWSE br_table /* Cost */
DO:
    RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotechg.mkup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.mkup br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quotechg.mkup IN BROWSE br_table /* Mkup */
DO:
    RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotechg.spare-dec-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.spare-dec-1 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF quotechg.spare-dec-1 IN BROWSE br_table /* Price */
DO:
  RUN UpdateMarkup.
  RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotechg.amtz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.amtz br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF quotechg.amtz IN BROWSE br_table /* Amtz */
DO:
  RUN calc-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME quotechg.simon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL quotechg.simon br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF quotechg.simon IN BROWSE br_table /* SIMON */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-simon NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "quoteqty"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "quoteqty"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-amt B-table-Win 
PROCEDURE calc-amt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN UpdatePrice.
    quotechg.amt:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(DEC(quotechg.prep-qty:SCREEN-VALUE IN BROWSE {&browse-name}) *
               DEC(quotechg.spare-dec-1:SCREEN-VALUE IN BROWSE {&browse-name}) *
               (IF DEC(quotechg.amtz:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
                  DEC(quotechg.amtz:SCREEN-VALUE IN BROWSE {&browse-name}) / 100 else 1)).
/*     IF ceprepprice-chr EQ "Profit" THEN                                                    */
/*        quotechg.amt:SCREEN-VALUE IN BROWSE {&browse-name} =                                */
/*         STRING(DEC(quotechg.prep-qty:SCREEN-VALUE IN BROWSE {&browse-name}) *              */
/*                DEC(quotechg.cost:SCREEN-VALUE     IN BROWSE {&browse-name}) /              */
/*                (1 - (DEC(quotechg.mkup:SCREEN-VALUE IN BROWSE {&browse-name}) / 100)) *    */
/*                (IF DEC(quotechg.amtz:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN      */
/*                   DEC(quotechg.amtz:SCREEN-VALUE IN BROWSE {&browse-name}) / 100 else 1)). */
/*     ELSE                                                                                   */
/*        quotechg.amt:SCREEN-VALUE IN BROWSE {&browse-name} =                                */
/*         STRING(DEC(quotechg.prep-qty:SCREEN-VALUE IN BROWSE {&browse-name}) *              */
/*                DEC(quotechg.cost:SCREEN-VALUE     IN BROWSE {&browse-name}) *              */
/*                (1 + (DEC(quotechg.mkup:SCREEN-VALUE IN BROWSE {&browse-name}) / 100)) *    */
/*                (IF DEC(quotechg.amtz:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN      */
/*                   DEC(quotechg.amtz:SCREEN-VALUE IN BROWSE {&browse-name}) / 100 else 1)). */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR curr-widget AS WIDGET-HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  /*IF NOT lv-update-est THEN DO:
     ASSIGN curr-widget = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
     DO WHILE VALID-HANDLE (curr-widget):
        IF NOT curr-widget:READ-ONLY AND curr-widget:MODIFIED THEN
        DO:
            lv-update-est = YES.
            LEAVE.
        END.
     END.
  END.*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF lv-update-est THEN DO:
     /* need to complete to update estimate */
  END.

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
  ASSIGN
   quotechg.company = quoteqty.company
   quotechg.loc     = quoteqty.loc
   quotechg.q-no    = quoteqty.q-no
   quotechg.line    = quoteqty.line 
   quotechg.qty     = quoteqty.qty
   quotechg.quote-date = quoteqty.quote-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-charge AS CHAR NO-UNDO.
  DEF VAR lv-s-num AS INT NO-UNDO.
  DEF VAR lv-b-num AS INT NO-UNDO.
  DEF VAR lv-company AS CHAR NO-UNDO.
  DEF VAR lv-loc AS CHAR NO-UNDO.
  DEF VAR lv-q-no AS INT NO-UNDO.
  DEF VAR lv-est-no AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  ASSIGN
    lv-charge = quotechg.charge
    lv-s-num = quotechg.s-num
    lv-b-num = quotechg.b-num
    lv-company = quotechg.company
    lv-loc     = quotechg.loc
    lv-q-no    = quotechg.q-no.

  FIND FIRST b-quotehd WHERE
       b-quotehd.company EQ lv-company AND
       b-quotehd.loc EQ lv-loc AND
       b-quotehd.q-no EQ lv-q-no
       NO-LOCK NO-ERROR.

  IF AVAIL b-quotehd THEN
  DO:
     lv-est-no = b-quotehd.est-no.
     RELEASE b-quotehd.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF lv-est-no NE "" AND
     CAN-FIND(FIRST est-prep WHERE
     est-prep.company EQ lv-company AND
     est-prep.est-no EQ lv-est-no AND
     est-prep.s-num EQ lv-s-num AND
     est-prep.b-num EQ lv-b-num AND
     est-prep.dscr EQ lv-charge) THEN

    FOR EACH b-quotechg WHERE
        b-quotechg.company = lv-company AND
        b-quotechg.loc = lv-loc AND
        b-quotechg.q-no = lv-q-no AND
        b-quotechg.charge = lv-charge AND
        b-quotechg.s-num = lv-s-num AND
        b-quotechg.b-num = lv-b-num
        EXCLUSIVE-LOCK:
   
        DELETE b-quotechg.
    END.

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
    APPLY "entry" TO quotechg.s-num IN BROWSE {&browse-name}.
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
  RUN valid-bill NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-simon NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF adm-new-record THEN  lv-update-est = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lv-update-est AND AVAIL quotechg THEN DO:     
      FIND FIRST quotehd OF quotechg NO-LOCK NO-ERROR.
      IF AVAIL quotehd THEN DO:
          FIND FIRST est-prep
              WHERE est-prep.company EQ quotechg.company
                AND est-prep.e-num EQ quotehd.e-num
                AND est-prep.est-no EQ quotehd.est-no
                AND est-prep.s-num EQ quotechg.s-num
                AND est-prep.b-num EQ quotechg.b-num
                AND (IF quotechg.CODE NE "" THEN est-prep.CODE EQ quotechg.CODE ELSE
                quotechg.charge EQ est-prep.dscr)  /*quotechg.code initially blank for some reason*/
              EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL est-prep THEN
              ASSIGN
                  est-prep.cost = quotechg.cost
                  est-prep.mkup = quotechg.mkup
                  est-prep.spare-dec-1 = quotechg.spare-dec-1
                  est-prep.amtz = quotechg.amtz
                  .
            RELEASE est-prep.
      END.
  END.
  lv-update-est = NO.
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
  {src/adm/template/sndkycas.i "q-no" "quotechg" "q-no"}

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
  {src/adm/template/snd-list.i "quoteqty"}
  {src/adm/template/snd-list.i "quotechg"}

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
        dCost =  DEC(quotechg.cost:SCREEN-VALUE IN BROWSE {&browse-name})
        dPrice =  DEC(quotechg.spare-dec-1:SCREEN-VALUE IN BROWSE {&browse-name}).
    IF dPrice GE dCost THEN DO:
        IF ceprepprice-chr EQ "Profit" THEN
            dMkup = (1 - dCost / dPrice) * 100. 
        ELSE
            dMkup = (dPrice / dCost - 1) * 100.
        quotechg.mkup:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dMkup).
    END.
    lv-update-est = YES.
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
        dCost =  DEC(quotechg.cost:SCREEN-VALUE IN BROWSE {&browse-name})
        dMkup =  DEC(quotechg.mkup:SCREEN-VALUE IN BROWSE {&browse-name}).
    IF ceprepprice-chr EQ "Profit" THEN
        dPrice = dCost / (1 - (dMkup / 100)).
    ELSE
        dPrice = dCost * (1 + (dMkup / 100)).
    quotechg.spare-dec-1:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(dPrice).
    lv-update-est = YES.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bill B-table-Win 
PROCEDURE valid-bill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INDEX("LMNTW",quotechg.bill:SCREEN-VALUE IN BROWSE {&browse-name}) LE 0 THEN DO:
      MESSAGE "Must be " + TRIM(quotechg.bill:HELP IN BROWSE {&browse-name}) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO quotechg.bill IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-simon B-table-Win 
PROCEDURE valid-simon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INDEX("SIMON",quotechg.simon:SCREEN-VALUE IN BROWSE {&browse-name}) LE 0 THEN DO:
      MESSAGE "Must be " + TRIM(quotechg.simon:HELP IN BROWSE {&browse-name}) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO quotechg.simon IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

