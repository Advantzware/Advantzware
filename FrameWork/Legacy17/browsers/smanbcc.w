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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
/*
DEF TEMP-TABLE tt-cst LIKE smanbcst.
DEF TEMP-TABLE tt-cat LIKE smanbcat.
*/
DEF TEMP-TABLE tt-bud FIELD procat AS cha
                      FIELD cust-no AS cha
                      FIELD budget-amt AS DEC
                      FIELD msf AS INT  
                      FIELD tons AS DEC 
                      INDEX bud cust-no procat.

DEF VAR v-prd AS INT NO-UNDO.

DEF VAR v-cust-changed AS LOG NO-UNDO.
DEF VAR v-procat-changed AS LOG NO-UNDO.
DEF VAR char-hdl AS cha NO-UNDO.

DEF VAR lv-sort-by AS CHAR INIT "cust-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Customer" NO-UNDO.
DEF VAR ll-sort-asc AS LOG INIT YES NO-UNDO.

&SCOPED-DEFINE sortby-log                             ~
    IF lv-sort-by EQ "cust-no"  THEN tt-bud.cust-no ELSE     ~
    IF lv-sort-by EQ "procat"  THEN tt-bud.procat  ELSE ""

&SCOPED-DEFINE sortby BY tt-bud.cust-no BY tt-bud.procat

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

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
&Scoped-define EXTERNAL-TABLES smanbugt
&Scoped-define FIRST-EXTERNAL-TABLE smanbugt


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR smanbugt.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-bud

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-bud.cust-no tt-bud.procat tt-bud.budget-amt tt-bud.msf tt-bud.tons   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-bud.cust-no ~
   tt-bud.procat ~
   tt-bud.budget-amt ~
   tt-bud.msf ~
   tt-bud.tons   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-bud
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-bud
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-bud     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-bud     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-bud
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-bud


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rd_cust-procat br_table cb_cust cb_procat 
&Scoped-Define DISPLAYED-OBJECTS v-sortby rd_cust-procat cb_procat 

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
</FOREIGN-KEYS
><EXECUTING-CODE>
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
DEFINE VARIABLE cb_cust AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cb_procat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Category" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY .71 NO-UNDO.

DEFINE VARIABLE rd_cust-procat AS CHARACTER INITIAL "C" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "C",
"Category", "P"
     SIZE 42 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-bud SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-bud.cust-no LABEL "Customer" LABEL-BGCOLOR 14
      tt-bud.procat LABEL "Category" WIDTH 14  LABEL-BGCOLOR 14    
      tt-bud.budget-amt LABEL "Budget $" FORM "->>,>>>,>>9"
      tt-bud.msf LABEL "Budget MSF" FORM "->,>>>,>>9"      
      tt-bud.tons LABEL "Budget Tons" FORM "->,>>>,>>9.9"
      ENABLE tt-bud.cust-no
             tt-bud.procat
             tt-bud.budget-amt
             tt-bud.msf
             tt-bud.tons
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 80 BY 15.71
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     v-sortby AT ROW 1.24 COL 49 COLON-ALIGNED NO-LABEL
     rd_cust-procat AT ROW 1.95 COL 2 NO-LABEL
     br_table AT ROW 2.19 COL 1
     cb_cust AT ROW 2.91 COL 19 COLON-ALIGNED
     cb_procat AT ROW 2.91 COL 22 COLON-ALIGNED
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.29 COL 42
     "Budget per Customer/Category" VIEW-AS TEXT
          SIZE 32 BY .95 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: asi.smanbugt
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
         HEIGHT             = 16.91
         WIDTH              = 80.
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
/* BROWSE-TAB br_table rd_cust-procat F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cb_cust IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       cb_cust:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       cb_procat:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       rd_cust-procat:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN v-sortby IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-bud
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "asi.smanbcat.company = asi.smanbugt.company
  AND asi.smanbcat.sman = asi.smanbugt.sman
  AND asi.smanbcat.budget-yr = asi.smanbugt.budget-yr
  "
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
        when "cust-no" then do:
             run windows/l-cust.w (g_company,focus:screen-value, output char-val).
             if char-val <> "" AND AVAIL tt-bud then DO:
                assign tt-bud.cust-no:screen-value IN BROWSE {&browse-name} = entry(1,char-val)
                       /*ocat-desc:screen-value = entry(2,char-val)*/
                       .
                APPLY 'tab' TO tt-bud.cust-no.
             END.
        end.
        when "procat" then do:
             IF rd_cust-procat = "C" THEN
                run windows/l-fgcat.w (g_company,focus:screen-value, output char-val).
             ELSE run windows/l-cust.w (g_company,focus:screen-value, output char-val).
             if char-val <> "" AND AVAIL tt-bud then DO:
                assign tt-bud.procat:SCREEN-VALUE IN BROWSE {&browse-name} = entry(1,char-val)
                       /*ocat-desc:screen-value = entry(2,char-val)*/
                       .
                APPLY 'tab' TO tt-bud.procat.
             END.
        end.
   END CASE.
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
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.
 
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
  ELSE ASSIGN lv-sort-by     = lv-column-nam
              lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
  RUN dispatch ('open-query').

  /*APPLY "choose" TO btn_go.*/
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


&Scoped-define SELF-NAME cb_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_cust B-table-Win
ON VALUE-CHANGED OF cb_cust IN FRAME F-Main /* Customer */
DO:
   v-cust-changed = YES.
   RUN dispatch ('open-query').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_procat B-table-Win
ON VALUE-CHANGED OF cb_procat IN FRAME F-Main /* Category */
DO:
   v-procat-changed = YES.
   RUN dispatch ('open-query').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_cust-procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_cust-procat B-table-Win
ON VALUE-CHANGED OF rd_cust-procat IN FRAME F-Main
DO:
  ASSIGN {&self-name}.
  RUN dispatch ('open-query').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

ON 'entry':U OF  tt-bud.cust-no
DO:
    IF NOT adm-new-record THEN DO:
       APPLY 'tab' TO SELF.
       RETURN NO-APPLY.
    END.
    RETURN.
END.
ON 'leave':U OF tt-bud.cust-no
DO:
   IF AVAILABLE tt-bud THEN DO:
   tt-bud.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(tt-bud.cust-no:SCREEN-VALUE).
   IF LASTKEY <>  -1 AND
      NOT CAN-FIND(FIRST cust WHERE cust.company = g_company 
                    AND cust.cust-no = tt-bud.cust-no:SCREEN-VALUE IN BROWSE {&browse-name})
   THEN DO:
        MESSAGE "Invalid customer.  Try help." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
   END.
   RETURN.
   END.
END.

ON 'entry':U OF  tt-bud.procat
DO:
    IF NOT adm-new-record THEN DO:
       APPLY 'tab' TO SELF.
       RETURN NO-APPLY.
    END.
    RETURN.
END.
   
ON 'leave':U OF tt-bud.procat 
DO:
    IF AVAIL tt-bud THEN DO:
    tt-bud.procat:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(tt-bud.procat:SCREEN-VALUE).
    IF LASTKEY <>  -1 AND
       rd_cust-procat = "C" AND
       NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = g_company 
                    AND fgcat.procat = tt-bud.procat:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
        MESSAGE "Invalid category.  Try help." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF LASTKEY <>  -1 AND
       rd_cust-procat = "P" AND
       NOT CAN-FIND(FIRST cust WHERE cust.company = g_company 
                    AND cust.cust-no = tt-bud.procat:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
        MESSAGE "Invalid customer.  Try help." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    END.

    RETURN.

END.

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
  {src/adm/template/row-list.i "smanbugt"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "smanbugt"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-procat-cnt AS INT NO-UNDO.
  DEF VAR v-cust-cnt AS INT NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR v-log AS LOG NO-UNDO.

  /*FOR EACH tt-cst:
      DELETE tt-cst.
  END.
  FOR EACH tt-cat:
      DELETE tt-cat.
  END.
  */

  FOR EACH tt-bud:
      DELETE tt-bud.
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).
  
IF rd_cust-procat = "C" THEN DO: /*by customer*/
  /*cb_cust:HIDDEN IN FRAME {&FRAME-NAME} = NO.*/
  cb_procat:HIDDEN IN FRAME {&FRAME-NAME} = YES.
  tt-bud.procat:LABEL IN BROWSE {&browse-name} = "Category".
  IF AVAIL smanbugt THEN DO:
     /*
     v-procat-cnt = 0.
     v-cust-cnt = 0.

     IF NOT v-cust-changed THEN DO:
        cb_cust = "".
        FOR EACH cust NO-LOCK WHERE cust.company = g_company BY cust.cust-no:
            cb_cust:ADD-LAST(cust.cust-no) IN FRAME {&FRAME-NAME}.
        END.
     /*   cb_cust:SCREEN-VALUE = cb_cust:ENTRY(1).      */
     END.
     */

     FOR EACH smanbcst NO-LOCK WHERE smanbcst.company = smanbugt.company
                                 AND smanbcst.sman = smanbugt.sman
                                 AND smanbcst.budget-yr = smanbugt.budget-yr
                                 AND smanbcst.budget-period = v-prd
                                /* AND smanbcst.cust-no = cb_cust:SCREEN-VALUE IN FRAME {&FRAME-NAME} */
                                 BREAK BY smanbcst.procat :
         /*IF FIRST-OF(smanbcst.procat) THEN*/ DO:
            CREATE tt-bud.
            ASSIGN tt-bud.procat = smanbcst.procat
                   tt-bud.cust-no = smanbcst.cust-no
                   v-cust-cnt = 0.
         END.
         ASSIGN tt-bud.budget-amt = /*tt-bud.budget-amt +*/  smanbcst.budget-amt
                tt-bud.msf = /*tt-bud.msf +*/ smanbcst.msf
                tt-bud.tons = /*tt-bud.tons +*/ smanbcst.tons.
     END.
  END.
END.
ELSE DO: /* by category*/
  IF AVAIL smanbugt THEN DO:
     cb_cust:HIDDEN = YES.
     cb_procat:HIDDEN = NO.
     cb_procat:SENSITIVE = YES.
     tt-bud.procat:LABEL IN BROWSE {&browse-name} = "Customer".
     
     IF NOT v-procat-changed THEN DO:
        cb_procat = "".
        FOR EACH  fgcat WHERE fgcat.company = g_company BY fgcat.procat:
            v-log = cb_procat:ADD-LAST(fgcat.procat) IN FRAME {&FRAME-NAME}.            
        END.
        cb_procat:SCREEN-VALUE = cb_procat:ENTRY(1).      
     END.
     FOR EACH smanbcst NO-LOCK WHERE smanbcst.company = smanbugt.company
                                 AND smanbcst.sman = smanbugt.sman
                                 AND smanbcst.budget-yr = smanbugt.budget-yr
                                 AND smanbcst.budget-period = v-prd
                                 AND smanbcst.procat = cb_procat:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                                 BREAK BY smanbcst.cust-no :
         IF FIRST-OF(smanbcst.cust-no) THEN DO:
            CREATE tt-bud.
            ASSIGN tt-bud.procat = smanbcst.cust-no  /*reversed*/
                   tt-bud.cust-no = smanbcst.procat. /*reversed*/
         END.
         ASSIGN tt-bud.budget-amt = tt-bud.budget-amt + smanbcst.budget-amt
                tt-bud.msf = tt-bud.msf + smanbcst.msf
                tt-bud.tons = tt-bud.tons + smanbcst.tons.
     END.
  END.
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
  DEF buffer bf-bugt FOR smanbugt.
  DEF buffer bf-bcat FOR smanbcat.
  DEF VAR v-procat AS cha NO-UNDO.
  DEF VAR v-cust-no AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).

  ASSIGN
    v-procat = IF rd_cust-procat = "C" THEN tt-bud.procat ELSE tt-bud.cust-no
    v-cust-no = IF rd_cust-procat = "C" THEN tt-bud.cust-no ELSE tt-bud.procat.

  FIND FIRST bf-bcat WHERE bf-bcat.company = smanbugt.company
                       AND bf-bcat.sman = smanbugt.sman
                       AND bf-bcat.budget-yr = smanbugt.budget-yr
                       AND bf-bcat.budget-period = v-prd
                       AND bf-bcat.procat = v-procat  NO-ERROR.
  /*IF NOT AVAIL bf-bcat THEN DO:
     CREATE bf-bcat.
     ASSIGN bf-bcat.company = smanbugt.company
            bf-bcat.sman = smanbugt.sman
            bf-bcat.budget-yr = smanbugt.budget-yr
            bf-bcat.budget-period = v-prd
            bf-bcat.procat = v-procat
            bf-bcat.budget-amt = tt-bud.budget-amt
            bf-bcat.msf = tt-bud.msf
            bf-bcat.tons = tt-bud.tons.
  END.*/
  
  /* update smanbugt from smanbcat */
  FIND FIRST bf-bugt WHERE bf-bugt.company = smanbugt.company
                       AND bf-bugt.sman = smanbugt.sman
                       AND bf-bugt.budget-yr = smanbugt.budget-yr NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    v-procat = IF rd_cust-procat = "C" THEN tt-bud.procat ELSE tt-bud.cust-no
    v-cust-no = IF rd_cust-procat = "C" THEN tt-bud.cust-no ELSE tt-bud.procat.

  FIND FIRST smanbcst WHERE smanbcst.company = smanbugt.company
                                AND smanbcst.sman = smanbugt.sman
                                AND smanbcst.budget-yr = smanbugt.budget-yr
                                AND smanbcst.budget-period = v-prd
                                AND smanbcst.procat = v-procat 
                                AND smanbcst.cust-no = v-cust-no NO-ERROR.

  IF NOT AVAIL smanbcst THEN DO:
     CREATE smanbcst.
     ASSIGN smanbcst.company = smanbugt.company
            smanbcst.sman = smanbugt.sman
            smanbcst.budget-yr = smanbugt.budget-yr
            smanbcst.budget-period = v-prd
            smanbcst.procat = v-procat
            smanbcst.cust-no = v-cust-no.
  END.
  ELSE 
    ASSIGN bf-bugt.budget-amt[v-prd] = bf-bugt.budget-amt[v-prd] - smanbcst.budget-amt
           bf-bugt.msf[v-prd] = bf-bugt.msf[v-prd] - smanbcst.msf
           bf-bugt.ton[v-prd] = bf-bugt.ton[v-prd] - smanbcst.ton.

  FIND FIRST bf-bcat WHERE bf-bcat.company = smanbugt.company
                       AND bf-bcat.sman = smanbugt.sman
                       AND bf-bcat.budget-yr = smanbugt.budget-yr
                       AND bf-bcat.budget-period = v-prd
                       AND bf-bcat.procat = v-procat  NO-ERROR.
  IF NOT AVAIL bf-bcat THEN DO:
     CREATE bf-bcat.
     ASSIGN bf-bcat.company = smanbugt.company
            bf-bcat.sman = smanbugt.sman
            bf-bcat.budget-yr = smanbugt.budget-yr
            bf-bcat.budget-period = v-prd
            bf-bcat.procat = v-procat.
  END.
  ELSE
    ASSIGN bf-bcat.budget-amt = bf-bcat.budget-amt - smanbcst.budget-amt
           bf-bcat.msf = bf-bcat.msf - smanbcst.msf
           bf-bcat.tons = bf-bcat.tons - smanbcst.ton.

  ASSIGN smanbcst.budget-amt = tt-bud.budget-amt
         smanbcst.msf = tt-bud.msf
         smanbcst.tons = tt-bud.tons
         bf-bcat.budget-amt = bf-bcat.budget-amt + tt-bud.budget-amt
         bf-bcat.msf = bf-bcat.msf + tt-bud.msf
         bf-bcat.tons = bf-bcat.tons + tt-bud.tons
         bf-bugt.budget-amt[v-prd] = bf-bugt.budget-amt[v-prd] + tt-bud.budget-amt
         bf-bugt.msf[v-prd] = bf-bugt.msf[v-prd] + tt-bud.msf
         bf-bugt.ton[v-prd] = bf-bugt.ton[v-prd] + tt-bud.tons.
  
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
   ASSIGN cb_cust:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          cb_procat:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          rd_cust-procat:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-output-choice AS CHAR NO-UNDO.
   DEF VAR v-from-cust-no AS CHAR NO-UNDO.
   DEF VAR v-to-cust-no AS CHAR NO-UNDO.
   DEF BUFFER tt-bud-1 FOR tt-bud.
   
   {&browse-name}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.

   IF AVAIL tt-bud THEN DO:
   
     RUN windows/d-budcus.w (INPUT tt-bud.cust-no,
                             OUTPUT v-output-choice,
                             OUTPUT v-from-cust-no,
                             OUTPUT v-to-cust-no).
    
     IF v-output-choice NE "" THEN DO:
       
       IF v-output-choice EQ "Line" THEN DO:
       
         /* Dispatch standard ADM method.                             */
         RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
       END.
       ELSE DO:
         
         RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
         RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).
    
         FOR EACH tt-bud WHERE
           tt-bud.cust-no EQ v-from-cust-no:
    
           IF NOT CAN-FIND(FIRST tt-bud-1 WHERE
             tt-bud-1.cust-no = v-to-cust-no AND
             tt-bud-1.procat = tt-bud.procat) THEN DO:
              
             REPOSITION {&browse-name} TO ROWID ROWID(tt-bud) NO-ERROR.
             
             RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ).
             
             {&browse-name}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
             tt-bud.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = v-to-cust-no.
             RUN local-update-record.
           END.
         END.
    
         ASSIGN
           cb_cust:SENSITIVE IN FRAME {&FRAME-NAME}        = YES
           cb_procat:SENSITIVE IN FRAME {&FRAME-NAME}      = YES
           rd_cust-procat:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       END.
     END.
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
  /*IF rd_cust-procat = "C" THEN tt-bud.cust-no = cb_cust:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  ELSE tt-bud.cust-no = cb_procat:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-bugt FOR smanbugt.
  DEF BUFFER bf-bcat FOR smanbcat.
  DEF BUFFER bf-smanbcst FOR smanbcst.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  IF AVAIL tt-bud THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).
     IF rd_cust-procat = "C" THEN
        FIND FIRST bf-smanbcst WHERE bf-smanbcst.company = smanbugt.company
                           AND bf-smanbcst.sman = smanbugt.sman 
                           AND bf-smanbcst.budget-yr = smanbugt.budget-yr
                           AND bf-smanbcst.budget-period = v-prd
                           AND bf-smanbcst.procat = tt-bud.procat
                           AND bf-smanbcst.cust-no = tt-bud.cust-no  NO-ERROR.
     /*ELSE FIND FIRST smanbcst WHERE smanbcst.company = smanbugt.company
                           AND smanbcst.sman = smanbugt.sman 
                           AND smanbcst.budget-yr = smanbugt.budget-yr
                           AND smanbcst.budget-period = v-prd
                           AND smanbcst.procat = tt-bud.cust-no
                           AND smanbcst.cust-no = tt-bud.procat  NO-ERROR.*/

  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF AVAIL bf-smanbcst THEN do:
      FIND FIRST bf-bcat WHERE bf-bcat.company = smanbugt.company
                           AND bf-bcat.sman = smanbugt.sman
                           AND bf-bcat.budget-yr = smanbugt.budget-yr
                           AND bf-bcat.budget-period = bf-smanbcst.budget-period
                           AND bf-bcat.procat = bf-smanbcst.procat  NO-ERROR.
      ASSIGN bf-bcat.budget-amt = bf-bcat.budget-amt - bf-smanbcst.budget-amt
             bf-bcat.msf = bf-bcat.msf - bf-smanbcst.msf
             bf-bcat.tons = bf-bcat.tons - bf-smanbcst.tons.

      FIND FIRST bf-bugt WHERE bf-bugt.company = smanbugt.company
                           AND bf-bugt.sman = smanbugt.sman
                           AND bf-bugt.budget-yr = smanbugt.budget-yr NO-ERROR.
      ASSIGN bf-bugt.budget-amt[v-prd] = bf-bugt.budget-amt[v-prd] - bf-smanbcst.budget-amt
             bf-bugt.msf[v-prd] = bf-bugt.msf[v-prd] - bf-smanbcst.msf
             bf-bugt.ton[v-prd] = bf-bugt.ton[v-prd] - bf-smanbcst.tons.
      
      DELETE bf-smanbcst.
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (recid(smanbugt)).

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
  ASSIGN
   cb_cust:SENSITIVE IN FRAME {&FRAME-NAME} = NO
   cb_procat:SENSITIVE IN FRAME {&FRAME-NAME} = NO
   rd_cust-procat:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  APPLY 'entry' TO tt-bud.budget-amt IN BROWSE {&browse-name}.

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
  {methods/winReSizeLocInit.i}

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
  RUN build-table.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */         
  
  &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
             FOR EACH tt-bud 
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
  ELSE {&open-query} {&sortby-phrase-desc}. 

  ASSIGN v-sortby:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                         TRIM(lv-sort-by-lab)               + " " +
                         TRIM(STRING(ll-sort-asc,"As/Des")) + "cending"
         v-sortby =  TRIM(lv-sort-by-lab)               + " " +
                     TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /*RUN dispatch ('open-query').*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  
  IF adm-new-record AND
     AVAIL tt-bud AND
     CAN-FIND(FIRST smanbcst WHERE smanbcst.company = smanbugt.company
                           AND smanbcst.sman = smanbugt.sman 
                           AND smanbcst.budget-yr = smanbugt.budget-yr
                           AND smanbcst.budget-period = v-prd
                           AND smanbcst.procat = tt-bud.procat:SCREEN-VALUE IN BROWSE {&browse-name}
                           AND smanbcst.cust-no = tt-bud.cust-no:SCREEN-VALUE)
  THEN DO:
        MESSAGE "Budget per Customer/Category already exists.  Try help." 
             VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO tt-bud.cust-no IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   cb_cust:SENSITIVE IN FRAME {&FRAME-NAME}        = YES
   cb_procat:SENSITIVE IN FRAME {&FRAME-NAME}      = YES
   rd_cust-procat:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (Recid(smanbugt)).

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
  {src/adm/template/snd-list.i "smanbugt"}
  {src/adm/template/snd-list.i "tt-bud"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-budget B-table-Win 
PROCEDURE update-budget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 /*
 DEF BUFFER bf-smanbcst FOR smanbcst.
 DEF VAR v-tot-amt AS DEC NO-UNDO.
 DEF VAR v-tot-msf AS INT NO-UNDO.
 DEF VAR v-tot-tons AS INT NO-UNDO.

 RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
 RUN get-period IN WIDGET-HANDLE(char-hdl) (OUTPUT v-prd).

 ASSIGN v-tot-amt = 0
        v-tot-msf = 0
        v-tot-tons = 0.
 FOR each bf-smanbcst NO-LOCK WHERE bf-smanbcst.company = smanbugt.company
                                AND bf-smanbcst.sman = smanbugt.sman
                                AND bf-smanbcst.budget-yr = smanbugt.budget-yr
                                AND bf-smanbcst.budget-period = v-prd
                                AND bf-smanbcst.procat = tt-bud.procat 
                                AND bf-smanbcst.cust-no = tt-bud.cust-no:
     ASSIGN v-tot-amt = v-tot-amt + bf-smanbcst.budget-amt
            v-tot-msf = v-tot-msf + bf-smanbcst.msf
            v-tot-tons = v-tot-tons + bf-smanbcst.tons.
 END.
 
 FIND FIRST smanbcat WHERE smanbcat.company = smanbugt.company
                       AND smanbcat.sman = smanbugt.sman
                       AND smanbcat.budget-yr = smanbugt.budget-yr
                       AND smanbcat.budget-period = v-prd
                       AND smanbcat.procat = tt-bud.procat  NO-ERROR.
  IF NOT AVAIL smanbcat THEN DO:
     CREATE smanbcat.
     ASSIGN smanbcat.company = smanbugt.company
            smanbcat.sman = smanbugt.sman
            smanbcat.budget-yr = smanbugt.budget-yr
            smanbcat.budget-period = v-prd
            smanbcat.procat = tt-bud.procat.
  END.
  
  ASSIGN smanbcat.budget-amt = tt-cat.budget-amt
         smanbcat.msf = tt-cat.msf
         smanbcat.tons = tt-cat.tons.

  /* update smanbugt from smanbcat */
  FIND FIRST bf-bugt WHERE bf-bugt.company = smanbcat.company
                       AND bf-bugt.sman = smanbcat.sman
                       AND bf-bugt.budget-yr = smanbcat.budget-yr NO-ERROR.
  ASSIGN bf-bugt.budget-amt[smanbcat.budget-period] = 0
         bf-bugt.msf[smanbcat.budget-period] = 0
         bf-bugt.ton[smanbcat.budget-period] = 0
         .
  FOR EACH bf-bcat WHERE bf-bcat.company = smanbcat.company
                     AND bf-bcat.sman = smanbcat.sman
                     AND bf-bcat.budget-yr = smanbcat.budget-yr
                     AND bf-bcat.budget-period = smanbcat.budget-period NO-LOCK:

      ASSIGN bf-bugt.budget-amt[bf-bcat.budget-period] = bf-bugt.budget-amt[bf-bcat.budget-period] +
                                                         bf-bcat.budget-amt
             bf-bugt.msf[bf-bcat.budget-period] = bf-bugt.msf[bf-bcat.budget-period] +
                                                         bf-bcat.msf
             bf-bugt.ton[bf-bcat.budget-period] = bf-bugt.ton[bf-bcat.budget-period] +
                                                         bf-bcat.ton
             .

      
  END.
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

