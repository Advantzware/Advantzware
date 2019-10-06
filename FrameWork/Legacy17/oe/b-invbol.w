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
&Scoped-define EXTERNAL-TABLES inv-line oe-bolh
&Scoped-define FIRST-EXTERNAL-TABLE inv-line


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-line, oe-bolh.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-boll

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table oe-boll.job-no oe-boll.job-no2 ~
oe-boll.loc oe-boll.loc-bin oe-boll.tag oe-boll.cases oe-boll.qty-case ~
oe-boll.partial oe-boll.qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table oe-boll.job-no ~
oe-boll.job-no2 oe-boll.loc oe-boll.loc-bin oe-boll.tag oe-boll.cases ~
oe-boll.partial 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table oe-boll
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table oe-boll
&Scoped-define QUERY-STRING-br_table FOR EACH oe-boll WHERE oe-boll.company = oe-bolh.company ~
  AND oe-boll.b-no = oe-bolh.b-no ~
  AND oe-boll.i-no = inv-line.i-no ~
  AND oe-boll.line = inv-line.line NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH oe-boll WHERE oe-boll.company = oe-bolh.company ~
  AND oe-boll.b-no = oe-bolh.b-no ~
  AND oe-boll.i-no = inv-line.i-no ~
  AND oe-boll.line = inv-line.line NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table oe-boll
&Scoped-define FIRST-TABLE-IN-QUERY-br_table oe-boll


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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      oe-boll SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      oe-boll.job-no COLUMN-LABEL "   Job#" FORMAT "x(6)":U WIDTH 9
      oe-boll.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 3
      oe-boll.loc COLUMN-LABEL "Whs" FORMAT "x(5)":U WIDTH 8
      oe-boll.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 11
      oe-boll.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U WIDTH 25
      oe-boll.cases COLUMN-LABEL "Units" FORMAT "->>>,>>>":U WIDTH 10
      oe-boll.qty-case COLUMN-LABEL "Qty/Unit" FORMAT ">>>,>>>":U
            WIDTH 10
      oe-boll.partial COLUMN-LABEL "Partial" FORMAT "->>,>>>,>>>":U
            WIDTH 14
      oe-boll.qty COLUMN-LABEL "Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 14
  ENABLE
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.loc
      oe-boll.loc-bin
      oe-boll.tag
      oe-boll.cases
      oe-boll.partial
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 114 BY 5.95
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
   External Tables: ASI.inv-line,ASI.oe-bolh
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
         WIDTH              = 114.8.
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
     _TblList          = "ASI.oe-boll WHERE ASI.inv-line <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.oe-boll.company = ASI.oe-bolh.company
  AND ASI.oe-boll.b-no = ASI.oe-bolh.b-no
  AND ASI.oe-boll.i-no = ASI.inv-line.i-no
  AND ASI.oe-boll.line = ASI.inv-line.line"
     _FldNameList[1]   > ASI.oe-boll.job-no
"oe-boll.job-no" "   Job#" ? "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-boll.job-no2
"oe-boll.job-no2" "" ? "integer" ? ? ? ? ? ? yes ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-boll.loc
"oe-boll.loc" "Whs" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-boll.loc-bin
"oe-boll.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-boll.tag
"oe-boll.tag" "Tag" "x(20)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-boll.cases
"oe-boll.cases" "Units" "->>>,>>>" "integer" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-boll.qty-case
"oe-boll.qty-case" "Qty/Unit" ">>>,>>>" "integer" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-boll.partial
"oe-boll.partial" "Partial" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-boll.qty
"oe-boll.qty" "Quantity" "->>,>>>,>>>" "integer" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  CASE FOCUS:NAME:
    WHEN "job-no"  THEN RUN fgbin-help.

    WHEN "job-no2" THEN RUN fgbin-help.

    WHEN "loc"     THEN RUN fgbin-help.

    WHEN "loc-bin" THEN RUN fgbin-help.

    WHEN "tag"     THEN RUN fgbin-help.
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
   /*{src/adm/template/brsleave.i}*/
   {est/brsleave.i}
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


&Scoped-define SELF-NAME oe-boll.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.job-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.job-no IN BROWSE br_table /*    Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.job-no2 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.job-no2 IN BROWSE br_table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.loc br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.loc IN BROWSE br_table /* Whs */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.loc-bin br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.loc-bin IN BROWSE br_table /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (4) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.tag br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF oe-boll.tag IN BROWSE br_table /* Tag */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (5) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.cases br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.cases IN BROWSE br_table /* Units */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-boll.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-boll.partial br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF oe-boll.partial IN BROWSE br_table /* Partial */
DO:
  RUN calc-qty.
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
  {src/adm/template/row-list.i "inv-line"}
  {src/adm/template/row-list.i "oe-bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-line"}
  {src/adm/template/row-find.i "oe-bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty B-table-Win 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name} =
      STRING((DEC(oe-boll.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
              DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
             DEC(oe-boll.partial:SCREEN-VALUE IN BROWSE {&browse-name})).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help B-table-Win 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-fgibn4.w (oe-boll.company, oe-boll.i-no, oe-boll.job-no:screen-value in browse {&browse-name}, INT(oe-boll.job-no2:screen-value in browse {&browse-name}), oe-boll.loc:screen-value in browse {&browse-name}, oe-boll.loc-bin:screen-value in browse {&browse-name}, oe-boll.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.job-no  OR
                         INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.loc     OR
                         oe-boll.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE fg-bin.loc-bin OR
                         oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.tag)
    THEN DO:
      ASSIGN
       oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag.

      RUN new-bin.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   oe-boll.qty-case = DEC(oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})
   oe-boll.qty      = DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name}).

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
   oe-boll.company = oe-bolh.company
   oe-boll.loc     = oe-bolh.loc
   oe-boll.bol-no  = oe-bolh.bol-no
   oe-boll.ord-no  = oe-bolh.ord-no
   oe-boll.b-no    = oe-bolh.b-no
   oe-boll.r-no    = oe-bolh.r-no
   oe-boll.i-no    = inv-line.i-no
   oe-boll.line    = inv-line.line.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-boll FOR oe-boll.

  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  FIND FIRST b-oe-boll
      WHERE b-oe-boll.company EQ oe-boll.company
        AND b-oe-boll.b-no    EQ oe-boll.b-no
        AND b-oe-boll.i-no    EQ oe-boll.i-no
        AND b-oe-boll.LINE    EQ oe-boll.LINE
        AND ROWID(b-oe-boll)  NE ROWID(oe-boll)
      NO-LOCK NO-ERROR.
  IF NOT AVAIL b-oe-boll THEN DO:
    MESSAGE "You must have at least one FG Bin..." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
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
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
    APPLY "entry" TO oe-boll.job-no IN BROWSE {&browse-name}.
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
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN calc-qty.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin B-table-Win 
PROCEDURE new-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST fg-bin 
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.job-no  EQ oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE old-valid-job-loc-bin-tag B-table-Win 
PROCEDURE old-valid-job-loc-bin-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR lv-fields AS CHAR INIT "job-no,job-no2,loc,loc-bin,tag" NO-UNDO.
  DEF VAR li-field# AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    li-field# = LOOKUP(FOCUS:NAME IN BROWSE {&browse-name},lv-fields).

    IF li-field# EQ 0 THEN li-field# = 9999.

    FIND FIRST fg-bin
        WHERE fg-bin.company  EQ cocode
          AND fg-bin.i-no     EQ oe-boll.i-no
          AND fg-bin.job-no   EQ oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (fg-bin.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               (li-field#     LT 2 AND INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0))
          AND (fg-bin.loc     EQ oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               (li-field#     LT 3 AND oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""))
          AND (fg-bin.loc-bin EQ oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               (li-field#     LT 4 AND oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""))
          AND (fg-bin.tag     EQ oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               (li-field#     LT 5 AND oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""))
          /*AND fg-bin.qty      NE 0*/
        USE-INDEX co-ino NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND li-field# NE 9999 THEN DO:
      ASSIGN
       oe-boll.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag).
      RUN calc-qty.
    END.

    IF NOT AVAIL fg-bin                                                     /*OR 
       fg-bin.qty LT DEC(oe-boll.qty:SCREEN-VALUE IN BROWSE {&browse-name})*/ THEN DO:
      IF AVAIL fg-bin THEN DO:
        MESSAGE "Insufficient qty in bin..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO oe-boll.cases IN BROWSE {&browse-name}.
      END.

      ELSE DO:
        MESSAGE "Invalid Bin, try help..." VIEW-AS ALERT-BOX.
        IF li-field# LE 5 THEN
          APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO oe-boll.loc-bin IN BROWSE {&browse-name}.
      END.

      RETURN ERROR.
    END.
  END.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-bintags B-table-Win 
PROCEDURE select-bintags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN oe/d-selbn1.w (RECID(inv-line)).
  RUN dispatch ('open-query').

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
  {src/adm/template/snd-list.i "inv-line"}
  {src/adm/template/snd-list.i "oe-bolh"}
  {src/adm/template/snd-list.i "oe-boll"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-loc-bin-tag B-table-Win 
PROCEDURE valid-job-loc-bin-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ip-int LT 99 THEN
      oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
          FILL(" ",6 - LENGTH(TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
          TRIM(oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST fg-bin 
                    WHERE fg-bin.company  EQ cocode
                      AND fg-bin.i-no     EQ oe-boll.i-no
                      AND (fg-bin.job-no  EQ oe-boll.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       OR ip-int LT 1)
                      AND (fg-bin.job-no2 EQ INT(oe-boll.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-int LT 2)
                      AND (fg-bin.loc     EQ oe-boll.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 3)
                      AND (fg-bin.loc-bin EQ oe-boll.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR ip-int LT 4)
                      AND (fg-bin.tag     EQ oe-boll.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 5))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.

      IF ip-int EQ 99 THEN
        APPLY "entry" TO oe-boll.job-no IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.

      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-all B-table-Win 
PROCEDURE validate-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN valid-job-loc-bin-tag (99) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

