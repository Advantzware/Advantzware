&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: browsers\fgijob.w 

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
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
    cocode = g_company
    locode = g_loc.

DEF VAR ll-show-zero-bins AS LOG NO-UNDO.
DEF VAR lShowRecalcFields AS LOG NO-UNDO.
DEFINE VARIABLE lcReturn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-secure  AS LOGICAL   NO-UNDO.
DEF VAR iConsumeThisTag AS INT NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "FgItemHideCalcFields", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).
lShowRecalcFields = TRUE.
IF llRecFound THEN
    lShowRecalcFields = NOT LOGICAL(lcReturn) NO-ERROR.  
    
{sys/inc/oeinq.i}
 
{fg/w-jobs.i "NEW SHARED"}
{Inventory/ttInventory.i "NEW SHARED"}

DEFINE TEMP-TABLE w-jobs LIKE w-job.

DEFINE TEMP-TABLE hold-job LIKE w-job.

/* Tkt 66887 - old code had single field TT.  Expanded to hold additional data to prevent re-reading tables */
DEF TEMP-TABLE tt-ids 
    FIELD tt-type AS CHAR 
    FIELD tt-rowid AS ROWID
    FIELD tt-tagno AS CHAR 
    FIELD tt-jobno LIKE oe-boll.job-no
    FIELD tt-jobno2 LIKE oe-boll.job-no2
    FIELD tt-qty LIKE oe-boll.qty.

DEF VAR lv-sort-by AS CHAR INIT "tag" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Tag" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR li-qty-pal AS INT NO-UNDO.
DEF VAR lc-pass-loc AS CHAR NO-UNDO.
DEF VAR lv-show-zero-bins AS LOG NO-UNDO.
DEF VAR lv-show-tag-no AS CHAR NO-UNDO.
DEFINE VARIABLE lAccessPro AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAccessClose AS LOGICAL NO-UNDO.
DEFINE VARIABLE cAccessList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatusDEscription AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInventoryProcs   AS HANDLE    NO-UNDO.

RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryprocs.

RUN methods/prgsecur.p
    (INPUT "fgijob.",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT lAccessPro, /* Allowed? Yes/NO */
    OUTPUT lAccessClose, /* used in template/windows.i  */
    OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */
/*-sort-by = NOT oeinq.*/

&SCOPED-DEFINE for-each1    ~
    FOR EACH w-job WHERE (IF (lc-pass-loc EQ "" OR lc-pass-loc = "*ALL") THEN TRUE ~
                           ELSE w-job.loc EQ lc-pass-loc)
     
    /*~
      BY (IF oeinq THEN w-job.job-no ELSE "") DESC
      BY (IF oeinq THEN w-job.job-no2 ELSE 0) DESC
      BY w-job.job-no
      BY w-job.job-no2
      BY w-job.loc
      BY w-job.loc-bin
      BY w-job.tag.                          
    */


&SCOPED-DEFINE sortby-log                                               ~
    IF lv-sort-by EQ "cust-no"              THEN w-job.cust-no                                          ELSE ~
    IF lv-sort-by EQ "job-no-disp"          THEN w-job.job-no-disp                                      ELSE ~
    IF lv-sort-by EQ "po-no"                THEN STRING(w-job.po-no,'>>>>>9')                           ELSE ~
    IF lv-sort-by EQ "loc"                  THEN w-job.loc                                              ELSE ~
    IF lv-sort-by EQ "loc-bin"              THEN w-job.loc-bin                                          ELSE ~
    IF lv-sort-by EQ "tag"                  THEN w-job.tag                                              ELSE ~
    IF lv-sort-by EQ "cases"                THEN string(9999999999 + w-job.cases,"-9999999999")         ELSE ~
    IF lv-sort-by EQ "case-count"           THEN string(9999999999 + w-job.case-count,"-9999999999")    ELSE ~
    IF lv-sort-by EQ "cases-unit"           THEN string(9999999999 + w-job.cases-unit,"-9999999999")    ELSE ~
    IF lv-sort-by EQ "partial-count"        THEN string(9999999999 + w-job.partial-count,"-9999999999") ELSE ~
    IF lv-sort-by EQ "qty"                  THEN STRING(9999999999.99 + w-job.qty,"-9999999999.99")     ELSE ~
    IF lv-sort-by EQ "rel-qty"              THEN STRING(9999999999.99 + w-job.rel-qty,"-9999999999.99") ELSE ~
    IF lv-sort-by EQ "bol-qty"              THEN STRING(9999999999.99 + w-job.bol-qty,"-9999999999.99") ELSE ~
    IF lv-sort-by EQ "avl-qty"              THEN STRING(9999999999.99 + w-job.avl-qty,"-9999999999.99") ELSE ~
    IF lv-sort-by EQ "std-tot-cost"         THEN STRING(w-job.std-tot-cost,"-9999999999.99999")         ELSE ~
    IF lv-sort-by EQ "sell-uom"             THEN w-job.sell-uom                                         ELSE ~
    IF lv-sort-by EQ "std-mat-cost"         THEN STRING(w-job.std-mat-cost,"-9999999999.99999")         ELSE ~
    IF lv-sort-by EQ "std-lab-cost"         THEN STRING(w-job.std-lab-cost,"-9999999999.99999")         ELSE ~
    IF lv-sort-by EQ "std-var-cost"         THEN STRING(w-job.std-var-cost,"-9999999999.99999")         ELSE ~
    IF lv-sort-by EQ "tot-wt"               THEN STRING(w-job.tot-wt,"-999999.99")                      ELSE ~
    IF lv-sort-by EQ "tagStatusID"          THEN w-job.tagStatusID                                      ELSE ~
    IF lv-sort-by EQ "tagStatusDescription" THEN w-job.tagStatusDescription                             ELSE ~
    IF lv-sort-by EQ "onHold"               THEN STRING(w-job.onHold)                                   ELSE ~
    STRING(w-job.std-fix-cost,"-9999999999.99999")     

&SCOPED-DEFINE sortby BY w-job.tag

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

DO TRANSACTION:
    {sys\inc\fgsecur.i}
END.

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
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-job

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table w-job.job-no-disp w-job.po-no w-job.loc w-job.loc-bin w-job.tag w-job.cases w-job.case-count w-job.cases-unit w-job.partial-count w-job.qty w-job.rel-qty w-job.bol-qty w-job.avl-qty w-job.std-tot-cost w-job.sell-uom w-job.std-mat-cost w-job.std-lab-cost w-job.std-var-cost w-job.std-fix-cost w-job.cust-no w-job.tot-wt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table w-job.job-no-disp ~
w-job.po-no ~
w-job.loc ~
 w-job.loc-bin ~
 w-job.tag ~
 w-job.cases ~
 w-job.case-count ~
w-job.cases-unit ~
w-job.partial-count ~
 w-job.qty ~
w-job.rel-qty ~
w-job.bol-qty ~
w-job.avl-qty ~
w-job.std-tot-cost ~
 w-job.sell-uom ~
 w-job.std-mat-cost ~
 w-job.std-lab-cost ~
 w-job.std-var-cost ~
 w-job.std-fix-cost ~
w-job.cust-no ~
w-job.tot-wt   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table w-job
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table w-job
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH w-job WHERE (IF (lc-pass-loc EQ "" OR lc-pass-loc = "*ALL") THEN TRUE       ELSE w-job.loc EQ lc-pass-loc)       BY (IF oeinq THEN w-job.job-no ELSE "") DESC       BY (IF oeinq THEN w-job.job-no2 ELSE 0) DESC       BY w-job.cust-no       BY w-job.job-no       BY w-job.job-no2       BY w-job.loc       BY w-job.loc-bin       BY w-job.tag
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME}   FOR EACH w-job WHERE (IF (lc-pass-loc EQ "" OR lc-pass-loc = "*ALL") THEN TRUE       ELSE w-job.loc EQ lc-pass-loc)       BY (IF oeinq THEN w-job.job-no ELSE "") DESC       BY (IF oeinq THEN w-job.job-no2 ELSE 0) DESC       BY w-job.cust-no       BY w-job.job-no       BY w-job.job-no2       BY w-job.loc       BY w-job.loc-bin       BY w-job.tag.
&Scoped-define TABLES-IN-QUERY-br_table w-job
&Scoped-define FIRST-TABLE-IN-QUERY-br_table w-job


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnLocationDetails br_table 

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
DEFINE BUTTON btnBinDetails 
    LABEL "View Bin Details" 
    SIZE 24 BY 1.

DEFINE BUTTON btnLocationDetails 
    LABEL "View Location Details" 
    SIZE 24 BY 1 TOOLTIP "Click to View Location Details".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
    w-job SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
    QUERY br_table NO-LOCK DISPLAY
    w-job.job-no-disp          LABEL "Job#"                   FORMAT "x(9)"        WIDTH 13   LABEL-BGCOLOR 14
    w-job.po-no                LABEL "PO#"                    FORMAT "x(9)"        WIDTH 13   LABEL-BGCOLOR 14
    w-job.loc                  LABEL "Whse"                                        WIDTH 10   LABEL-BGCOLOR 14
    w-job.loc-bin              LABEL "Bin"                                         WIDTH 14   LABEL-BGCOLOR 14
    w-job.tag                  LABEL "Tag"                    FORMAT "x(20)"       WIDTH 28   LABEL-BGCOLOR 14
    w-job.cases                LABEL "Units"                                                  LABEL-BGCOLOR 14
    w-job.case-count           LABEL "Unit Count"             FORMAT ">>>,>>9"                LABEL-BGCOLOR 14
    w-job.cases-unit           COLUMN-LABEL "Units/!Pallet"   FORMAT ">>>,>>9"                LABEL-BGCOLOR 14
    w-job.partial-count        LABEL "Partial"                FORMAT "->>>,>>9"                LABEL-BGCOLOR 14
    w-job.qty                  LABEL "Total Qty"              FORMAT "->,>>>,>>9"               LABEL-BGCOLOR 14
    w-job.rel-qty              LABEL "Releases"               FORMAT "->,>>>,>>9"               LABEL-BGCOLOR 14
    w-job.bol-qty              LABEL "BOL Qty"                FORMAT "->,>>>,>>9"               LABEL-BGCOLOR 14
    w-job.avl-qty              LABEL "Avail to Release"       FORMAT "->,>>>,>>9"               LABEL-BGCOLOR 14
    w-job.std-tot-cost         COLUMN-LABEL "Standard!Cost"                                   LABEL-BGCOLOR 14
    w-job.sell-uom             LABEL "UOM"                                                    LABEL-BGCOLOR 14
    w-job.std-mat-cost         LABEL "Material"               FORMAT "->>>,>>9.9999"          LABEL-BGCOLOR 14
    w-job.std-lab-cost         LABEL "Labor"                  FORMAT "->>>,>>9.9999"          LABEL-BGCOLOR 14
    w-job.std-var-cost         LABEL "Variable O/H"           FORMAT "->>>,>>9.9999"          LABEL-BGCOLOR 14
    w-job.std-fix-cost         LABEL "Fixed O/H"              FORMAT "->>>,>>9.9999"          LABEL-BGCOLOR 14
    w-job.cust-no              LABEL "Cust#"                  FORMAT "x(8)"        WIDTH 13   LABEL-BGCOLOR 14  
    w-job.tot-wt               LABEL "Lbs / 100"              FORMAT "->,>>>,>>9.99" WIDTH 16   LABEL-BGCOLOR 14
    w-job.tagStatusID          LABEL "Tag Status ID "         FORMAT "X(4)"        WIDTH 18.3 LABEL-BGCOLOR 14
    w-job.tagStatusDescription LABEL "Tag Status Description" FORMAT "X(32)"                  LABEL-BGCOLOR 14
    w-job.onHold               LABEL "On Hold"                FORMAT "Yes/No"                 LABEL-BGCOLOR 14
ENABLE w-job.job-no-disp
       w-job.po-no
       w-job.loc 
       w-job.loc-bin 
       w-job.tag 
       w-job.cases 
       w-job.case-count
       w-job.cases-unit
       w-job.partial-count 
       w-job.qty
       w-job.rel-qty
       w-job.bol-qty
       w-job.avl-qty
       w-job.std-tot-cost 
       w-job.sell-uom 
       w-job.std-mat-cost 
       w-job.std-lab-cost 
       w-job.std-var-cost 
       w-job.std-fix-cost
       w-job.cust-no
       w-job.tot-wt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 156 BY 20.24
         FONT 0
         TITLE "Bin Details".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    btnLocationDetails AT ROW 1 COL 1 HELP
    "View Location Details" WIDGET-ID 10
    btnBinDetails AT ROW 1 COL 25 HELP
    "View Bin Details" WIDGET-ID 12
    br_table AT ROW 1.95 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.itemfg
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
    MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 21.19
         WIDTH              = 156.
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
/* BROWSE-TAB br_table btnBinDetails F-Main */
ASSIGN 
    FRAME F-Main:SCROLLABLE       = FALSE
    FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnBinDetails IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
  FOR EACH w-job WHERE (IF (lc-pass-loc EQ "" OR lc-pass-loc = "*ALL") THEN TRUE
      ELSE w-job.loc EQ lc-pass-loc)
      BY (IF oeinq THEN w-job.job-no ELSE "") DESC
      BY (IF oeinq THEN w-job.job-no2 ELSE 0) DESC
      BY w-job.cust-no
      BY w-job.job-no
      BY w-job.job-no2
      BY w-job.loc
      BY w-job.loc-bin
      BY w-job.tag.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main /* Bin Details */
    DO:
        RUN update-cost.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Bin Details */
    DO:
  /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Bin Details */
    DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main /* Bin Details */
    DO:
		{methods/template/sortindicator.i}
        DEF VAR lh-column AS HANDLE NO-UNDO.
        DEF VAR lv-column-nam AS CHAR NO-UNDO.
        DEF VAR lv-column-lab AS CHAR NO-UNDO.


        ASSIGN
            lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
            lv-column-nam = lh-column:NAME
            lv-column-lab = lh-column:LABEL.

        IF lv-column-nam BEGINS "li-" THEN 
        DO:
            APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
            RETURN NO-APPLY.
        END.

 
        IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

        ELSE
            ASSIGN
                lv-sort-by     = lv-column-nam
                lv-sort-by-lab = lv-column-lab.

        APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

        RUN resort-query.
		{methods/template/sortindicatorend.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Bin Details */
    DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
    {src/adm/template/brschnge.i}

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLocationDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLocationDetails B-table-Win
ON CHOOSE OF btnLocationDetails IN FRAME F-Main /* View Location Details */
    DO:
        DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

        {methods/run_link.i "ViewDetail-TARGET" "pViewDetail" "(5)"}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

RUN set-read-only(YES).

IF fgsecurity-log THEN 
DO:
    FIND FIRST usergrps NO-LOCK
        WHERE usergrps.usergrps EQ fgsecurity-char
        NO-ERROR.
    IF AVAILABLE usergrps AND
        (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
        TRIM(usergrps.users) NE "*") THEN
        ASSIGN
            w-job.std-tot-cost:VISIBLE IN BROWSE {&browse-name} = NO
            w-job.sell-uom:VISIBLE IN BROWSE {&browse-name} = NO
            w-job.std-mat-cost:VISIBLE IN BROWSE {&browse-name} = NO
            w-job.std-lab-cost:VISIBLE IN BROWSE {&browse-name} = NO
            w-job.std-var-cost:VISIBLE IN BROWSE {&browse-name} = NO
            w-job.std-fix-cost:VISIBLE IN BROWSE {&browse-name} = NO
            .
END.
ASSIGN 
    w-job.avl-qty:VISIBLE IN BROWSE {&browse-name} = lShowRecalcFields 
    w-job.rel-qty:VISIBLE IN BROWSE {&browse-name} = lShowRecalcFields
    w-job.bol-qty:VISIBLE IN BROWSE {&browse-name} = lShowRecalcFields
    .

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
    {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
    {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:                 /** BUILD JOB WORK FILE **/
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE w-jobs.
    EMPTY TEMP-TABLE w-job.
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ itemfg.company NO-LOCK NO-ERROR.

    IF lv-show-tag-no EQ "" THEN DO:
        FOR EACH fg-bin
            WHERE fg-bin.company eq itemfg.company
            AND  fg-bin.i-no    eq itemfg.i-no
            NO-LOCK:
            RUN createwjobs.
        END. /* each fg-bin */
    END.
    ELSE IF INDEX(lv-show-tag-no, '*') EQ 0 THEN DO:
        FOR EACH fg-bin
            WHERE fg-bin.company eq itemfg.company
            AND  fg-bin.i-no    eq itemfg.i-no
            AND fg-bin.tag     BEGINS lv-show-tag-no 
            NO-LOCK:
            IF  fg-bin.i-no    eq itemfg.i-no THEN 
                RUN createwjobs.
        END. /* each fg-bin */
    END.
    ELSE DO:
        FOR EACH fg-bin
            WHERE fg-bin.company eq itemfg.company         
            AND  fg-bin.tag MATCHES lv-show-tag-no
            NO-LOCK:
            IF  fg-bin.i-no    eq itemfg.i-no THEN 
                RUN createwjobs. 
        END. /* each fg-bin */
    END.
  
    /* This is true UNLESS NK1 FgItemHideCalcFields exists and has logical TRUE */
    IF ll-show-zero-bins THEN DO:
        FOR EACH job-hdr FIELDS(std-lab-cost std-mat-cost std-var-cost  std-fix-cost) NO-LOCK
            WHERE job-hdr.company EQ itemfg.company
            AND job-hdr.i-no    EQ itemfg.i-no
            AND job-hdr.opened  EQ YES
            USE-INDEX i-no,
            FIRST  job FIELDS(job-no job-no2 loc) NO-LOCK
                WHERE job.company EQ job-hdr.company
                AND job.job     EQ job-hdr.job
                AND job.job-no  EQ job-hdr.job-no
                AND job.job-no2 EQ job-hdr.job-no2:
  
            IF lv-show-tag-no = ""  THEN
                RUN create-table (job-hdr.std-lab-cost,
                    job-hdr.std-mat-cost,
                    job-hdr.std-var-cost,
                    job-hdr.std-fix-cost).

            RELEASE w-jobs.
        END.

        FOR EACH reftable FIELDS(val) NO-LOCK
            WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.code2    EQ itemfg.i-no
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ ""
            USE-INDEX code2,
            FIRST job FIELDS(job-no job-no2 loc) NO-LOCK
            WHERE job.company EQ reftable.company
            AND job.job     EQ INT(reftable.code)
            AND job.opened  EQ YES
            AND NOT CAN-FIND(FIRST job-hdr
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ reftable.code2):
            IF lv-show-tag-no = ""  THEN
                RUN create-table (reftable.val[1],
                    reftable.val[2],
                    reftable.val[3],
                    reftable.val[4]).

            RELEASE w-jobs.
        END.
    END. /* ll-show-zero-bins */

    EMPTY TEMP-TABLE tt-ids.
    IF lShowRecalcFields THEN DO:
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company EQ itemfg.company          
            AND oe-rell.i-no    EQ itemfg.i-no
            AND oe-rell.posted  EQ NO 
            ,
            EACH oe-relh FIELDS() NO-LOCK
            WHERE oe-relh.r-no EQ oe-rell.r-no
            AND oe-relh.deleted EQ NO
            AND oe-relh.posted  EQ NO
            :
            CREATE tt-ids.
            /* Ticket 66887 - we're reading the oe-rell, oe-relh, oe-boll and oe-bolh tables here
                Improvement is to assign more detail in the temp-table so we don't have to read the
                tables again to calculate line-level values */
            ASSIGN 
                tt-type = "REL"
                tt-rowid = ROWID(oe-rell)
                tt-jobno = oe-rell.job-no
                tt-jobno2 = oe-rell.job-no2
                tt-tagno = oe-rell.tag
                tt-qty = oe-rell.qty.
        END.

        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ itemfg.company
            AND oe-boll.posted  EQ NO
            AND oe-boll.i-no    EQ itemfg.i-no
            ,
            FIRST oe-bolh FIELDS() NO-LOCK
            WHERE oe-bolh.b-no    EQ oe-boll.b-no AND 
            oe-bolh.deleted EQ NO AND  
            oe-bolh.posted EQ NO:
            CREATE tt-ids.
                ASSIGN 
                    tt-type = "BOL"
                    tt-rowid = ROWID(oe-boll)
                    tt-jobno = oe-boll.job-no
                    tt-jobno2 = oe-boll.job-no2
                    tt-tagno = oe-boll.tag
                    tt-qty = oe-boll.qty.
        END.
  
        IF AVAIL oe-ctrl AND NOT oe-ctrl.u-inv THEN
        FOR EACH inv-line NO-LOCK
            WHERE inv-line.company EQ itemfg.company
            AND inv-line.i-no    EQ itemfg.i-no,
            EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ inv-line.company
            AND oe-boll.b-no    EQ inv-line.b-no
            AND oe-boll.ord-no  EQ inv-line.ord-no
            AND oe-boll.i-no    EQ inv-line.i-no
            AND oe-boll.po-no   EQ inv-line.po-no:
            CREATE tt-ids.
            ASSIGN 
                tt-type = "BOL"
                tt-rowid = ROWID(oe-boll)
                tt-jobno = oe-boll.job-no
                tt-jobno2 = oe-boll.job-no2
                tt-tagno = oe-boll.tag
                tt-qty = oe-boll.qty.
        END.
    END.
  
    FOR EACH w-jobs BREAK BY w-jobs.job-no BY w-jobs.job-no2:
        CREATE w-job.
        ASSIGN 
            w-job.job-no               = w-jobs.job-no
            w-job.job-no2              = w-jobs.job-no2
            w-job.job-no-disp          = TRIM(w-job.job-no) + "-" + STRING(w-job.job-no2,"99")
            w-job.po-no                = w-jobs.po-no
            w-job.i-no                 = w-jobs.i-no
            w-job.j-no                 = w-jobs.j-no
            w-job.loc                  = w-jobs.loc
            w-job.loc-bin              = w-jobs.loc-bin
            w-job.tag                  = w-jobs.tag
            w-job.cust-no              = w-jobs.cust-no
            w-job.cases                = w-jobs.cases
            w-job.case-count           = w-jobs.case-count
            w-job.cases-unit           = w-jobs.cases-unit
            w-job.partial-count        = w-jobs.partial-count
            w-job.qty                  = w-jobs.qty
            w-job.std-tot-cost         = w-jobs.std-tot-cost
            w-job.std-mat-cost         = w-jobs.std-mat-cost
            w-job.std-lab-cost         = w-jobs.std-lab-cost
            w-job.std-var-cost         = w-jobs.std-var-cost
            w-job.std-fix-cost         = w-jobs.std-fix-cost
            w-job.last-cost            = w-jobs.last-cost
            w-job.sell-uom             = w-jobs.sell-uom
            w-job.tot-wt               = w-jobs.tot-wt
            w-job.tagStatusID          = w-jobs.tagStatusID
            w-job.tagStatusDescription = w-jobs.tagStatusDescription
            w-job.onHold               = w-jobs.onHold
            w-job.ship-default         = w-jobs.ship-default
            .

        IF w-job.job-no-disp EQ "-00" THEN w-job.job-no-disp = "".
       
        DELETE w-jobs.

        /* Ticket 66887 - and here, in the old code, we re-read the oe-relx and oe-bolx tables again, once per tag-line
            Now, since we have the qty info in the temp-table, we just use that instead.  For Premier DB, this saves
            about 500,000 reads */
        FOR EACH tt-ids WHERE 
            tt-jobno EQ w-job.job-no AND 
            tt-jobno2 EQ w-job.job-no2 AND 
            tt-tagno EQ w-job.tag
            BY tt-ids.tt-type
            BY tt-ids.tt-jobno 
            BY tt-ids.tt-jobno2 
            BY tt-ids.tt-tagno:
            IF tt-ids.tt-type EQ "REL" THEN ASSIGN 
                w-job.rel-qty = w-job.rel-qty + tt-ids.tt-qty.
            ELSE ASSIGN 
                w-job.bol-qty = w-job.bol-qty + tt-ids.tt-qty.
        END. /* each tt-ids */
        
        ASSIGN 
            w-job.avl-qty = w-job.qty - w-job.rel-qty - w-job.bol-qty.

    END. /* each w-jobs */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-table B-table-Win 
PROCEDURE create-table :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-lab LIKE job-hdr.std-lab-cost NO-UNDO.
    DEF INPUT PARAM ip-mat LIKE job-hdr.std-mat-cost NO-UNDO.
    DEF INPUT PARAM ip-var LIKE job-hdr.std-var-cost NO-UNDO.
    DEF INPUT PARAM ip-fix LIKE job-hdr.std-fix-cost NO-UNDO.

    IF NOT CAN-FIND(FIRST w-jobs
        WHERE w-jobs.job-no  EQ job.job-no
        AND w-jobs.job-no2 EQ job.job-no2) THEN 
    DO:
    
        CREATE w-jobs.
        ASSIGN
            w-jobs.job-no       = job.job-no
            w-jobs.job-no2      = job.job-no2
            w-jobs.i-no         = itemfg.i-no
            w-jobs.loc          = job.loc
            w-jobs.std-lab-cost = ip-lab
            w-jobs.std-mat-cost = ip-mat
            w-jobs.std-var-cost = ip-var
            w-jobs.std-fix-cost = ip-fix
            w-jobs.std-tot-cost = w-jobs.std-lab-cost + w-jobs.std-mat-cost +
                           w-jobs.std-var-cost + w-jobs.std-fix-cost
            w-jobs.sell-uom     = "M"  .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWJobs B-table-Win 
PROCEDURE createWJobs :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    
    IF lv-show-tag-no <> ""  THEN
        IF fg-bin.tag = "" THEN RETURN .

    IF NOT (fg-bin.qty ne 0 or (ll-show-zero-bins AND lv-show-zero-bins)) THEN
        RETURN.
    
    cStatusDescription = "".
    
    IF fg-bin.StatusID NE "" THEN
        RUN Inventory_GetStatusDescription IN hdInventoryProcs (
            INPUT  fg-bin.StatusID,
            OUTPUT cStatusDescription
            ).
    
    CREATE w-jobs.
    ASSIGN 
        w-jobs.job-no               = fg-bin.job-no
        w-jobs.job-no2              = fg-bin.job-no2
        w-jobs.i-no                 = itemfg.i-no
        w-jobs.loc                  = fg-bin.loc
        w-jobs.loc-bin              = fg-bin.loc-bin
        w-jobs.tag                  = fg-bin.tag
        w-jobs.cust-no              = fg-bin.cust-no
        w-jobs.cases                = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
        w-jobs.case-count           = fg-bin.case-count
        w-jobs.cases-unit           = fg-bin.cases-unit
        w-jobs.partial-count        = fg-bin.partial-count
        w-jobs.qty                  = fg-bin.qty
        w-jobs.std-tot-cost         = fg-bin.std-tot-cost
        w-jobs.std-mat-cost         = fg-bin.std-mat-cost
        w-jobs.std-lab-cost         = fg-bin.std-lab-cost
        w-jobs.std-var-cost         = fg-bin.std-var-cost
        w-jobs.std-fix-cost         = fg-bin.std-fix-cost
        w-jobs.last-cost            = fg-bin.last-cost
        w-jobs.sell-uom             = fg-bin.pur-uom
        w-jobs.tot-wt               = fg-bin.tot-wt 
        w-jobs.po-no                = fg-bin.po-no
        w-jobs.tagStatusID          = fg-bin.statusID
        w-jobs.tagStatusDescription = cStatusDescription
        w-jobs.onHold               = fg-bin.onHold
        w-jobs.ship-default         = fg-bin.ship-default
        .
      
    FIND FIRST job-hdr NO-LOCK
        WHERE job-hdr.company EQ fg-bin.company
        AND job-hdr.i-no    EQ fg-bin.i-no
        AND job-hdr.job-no  EQ fg-bin.job-no
        AND job-hdr.job-no2 EQ fg-bin.job-no2
        USE-INDEX i-no NO-ERROR.
    IF AVAILABLE job-hdr THEN 
        w-jobs.j-no = job-hdr.j-no.

    RELEASE w-jobs.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterTagBins B-table-Win 
PROCEDURE filterTagBins :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplShowZeroBins AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplTagBins      AS CHARACTER NO-UNDO.
    
    ASSIGN
        lv-show-zero-bins = iplShowZeroBins
        lv-show-tag-no    = iplTagBins 
        .
    RUN local-open-query.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterZeroBins B-table-Win 
PROCEDURE filterZeroBins :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplShowZeroBins AS LOGICAL NO-UNDO.
    
    lv-show-zero-bins = iplShowZeroBins.
    RUN local-open-query.
    
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
        w-job.job-no-disp:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.loc:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.tag:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.cases:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.case-count:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.cases-unit:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.partial-count:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.qty:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.rel-qty:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.bol-qty:READ-ONLY IN BROWSE {&browse-name} = YES
        w-job.avl-qty:READ-ONLY IN BROWSE {&browse-name} = YES  
        w-job.std-tot-cost:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.sell-uom:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.std-mat-cost:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.std-lab-cost:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.std-var-cost:READ-ONLY IN BROWSE {&browse-name} = YES 
        w-job.std-fix-cost:READ-ONLY IN BROWSE {&browse-name} = YES.

    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
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
    DEF VAR char-hdl AS CHAR NO-UNDO.
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEF VAR ll-zero AS LOG INIT YES NO-UNDO.


    /* Code placed here will execute PRIOR to standard behavior. */
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"inquiry-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN get-ip-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid, OUTPUT ll-zero).

    ll-show-zero-bins = lv-rowid NE ROWID(itemfg) OR ll-zero.

    RUN build-table.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

/* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resort-query B-table-Win 
PROCEDURE resort-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 

  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               
             
  
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
    ELSE {&open-query} {&sortby-phrase-desc}.

    RUN dispatch ("row-changed").

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
  {src/adm/template/snd-list.i "itemfg"}
  {src/adm/template/snd-list.i "w-job"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-pass-loc B-table-Win 
PROCEDURE set-pass-loc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ip-pass-loc AS CHAR NO-UNDO.
    
    lc-pass-loc = ip-pass-loc.
    RUN local-open-query.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only B-table-Win 
PROCEDURE set-read-only :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-log AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        w-job.sell-uom:READ-ONLY IN BROWSE {&browse-name} = ip-log.
        w-job.cust-no:READ-ONLY IN BROWSE {&browse-name} = ip-log.
        w-job.tot-wt:READ-ONLY IN BROWSE {&browse-name} = ip-log.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-cost B-table-Win 
PROCEDURE update-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEF BUFFER bf-w-job FOR w-job.

    DEFINE VARIABLE char-hdl     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hPgmSecurity AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lResult      AS LOGICAL   NO-UNDO.
    
    IF NOT AVAILABLE w-job THEN RETURN.

    /*RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.                    */
    /*RUN epCanAccess IN hPgmSecurity ("viewers/p-fg-bj-l.w", "", OUTPUT lResult).*/
    /*DELETE OBJECT hPgmSecurity.                                                 */
    /*IF lResult THEN                                                             */
    /*    ASSIGN ll-secure = YES.                                                 */
    /*                                                                            */
    /*IF NOT ll-secure THEN DO:                                                   */
    /*    RUN sys/ref/d-passwd.w (1, OUTPUT ll-secure).                           */
    /*    IF NOT ll-secure THEN RETURN.                                           */
    /*END.                                                                        */    

    IF NOT lAccessPro THEN RETURN .
 
    FOR EACH hold-job:
        DELETE hold-job.
    END.

    CREATE hold-job.
    BUFFER-COPY w-job TO hold-job.
  
    RUN fg/fgbjobu.w (RECID(w-job)).

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(itemfg)).

    RUN dispatch ("open-query").

    IF AVAIL hold-job THEN
        FIND FIRST bf-w-job 
            WHERE bf-w-job.job-no  EQ hold-job.job-no
            AND bf-w-job.job-no2 EQ hold-job.job-no2
            AND bf-w-job.loc     EQ hold-job.loc
            AND bf-w-job.loc-bin EQ hold-job.loc-bin
            AND bf-w-job.tag     EQ hold-job.tag
            NO-ERROR.

    IF AVAIL bf-w-job THEN 
    DO:
        lv-rowid = ROWID(bf-w-job).
        REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record B-table-Win 
PROCEDURE update-record :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplResort AS LOGICAL NO-UNDO.

    DEF BUFFER b-w-job FOR w-job.
    DEF BUFFER bf-fg-bin FOR fg-bin.
  
    DO WITH FRAME {&FRAME-NAME}:
        FIND b-w-job WHERE ROWID(b-w-job) EQ ROWID(w-job).
    
        b-w-job.cust-no = w-job.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}.
        b-w-job.sell-uom = w-job.sell-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
        b-w-job.tot-wt = DEC(w-job.tot-wt:SCREEN-VALUE IN BROWSE {&browse-name}).

        FIND b-w-job WHERE ROWID(b-w-job) EQ ROWID(w-job) NO-LOCK NO-ERROR.
        FIND FIRST bf-fg-bin 
            WHERE bf-fg-bin.company EQ cocode
            AND bf-fg-bin.i-no EQ b-w-job.i-no
            AND bf-fg-bin.loc EQ b-w-job.loc
            AND bf-fg-bin.loc-bin EQ b-w-job.loc-bin
            AND bf-fg-bin.tag EQ b-w-job.tag
            AND bf-fg-bin.job-no EQ b-w-job.job-no
            AND bf-fg-bin.job-no2 EQ b-w-job.job-no2
            AND bf-fg-bin.cust-no EQ b-w-job.cust-no
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL bf-fg-bin THEN 
        DO:
            ASSIGN 
                bf-fg-bin.tot-wt = b-w-job.tot-wt.
            RELEASE bf-fg-bin.
        END.
        IF w-job.tot-wt:MODIFIED THEN 
        DO:
            FIND FIRST loadtag EXCLUSIVE-LOCK
                WHERE loadtag.company EQ cocode
                AND loadtag.i-no      EQ b-w-job.i-no
                AND loadtag.job-no    EQ b-w-job.job-no
                AND loadtag.job-no2   EQ b-w-job.job-no2 
                AND loadtag.tag-no    EQ b-w-job.tag 
                NO-ERROR.

            IF AVAIL loadtag THEN 
            DO:
                ASSIGN 
                    loadtag.misc-dec[1] = (b-w-job.case-count * b-w-job.tot-wt) / 100
                    loadtag.misc-dec[2] = loadtag.misc-dec[1] * loadtag.qty-case. 
            END.
            FIND CURRENT loadtag NO-LOCK NO-ERROR.
        END.

        IF iplResort THEN 
        DO:
            RUN set-read-only (YES).
            RUN resort-query.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-spec B-table-Win 
PROCEDURE update-spec :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    def var char-hdl as cha no-undo.
  
    run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
    if valid-handle(widget-handle(char-hdl)) then run select-page in widget-handle(char-hdl) (7).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

