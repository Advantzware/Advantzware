&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: gl\b-glinq.w

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
{methods/template/brwcustomdef.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE        VARIABLE v-max-dscr-length AS INTEGER   NO-UNDO. 
DEFINE SHARED VARIABLE g_company         AS cha       NO-UNDO.
DEFINE SHARED VARIABLE g_loc             AS cha       NO-UNDO.
DEFINE SHARED VARIABLE g_period          AS INTEGER   NO-UNDO.
{methods/defines/hndldefs.i}               
    
{sys/inc/VAR.i NEW SHARED}

DEFINE TEMP-TABLE tt-glinq NO-UNDO
    FIELD tr-date     LIKE glhist.tr-date LABEL "Date"
    FIELD jrnl        LIKE glhist.jrnl LABEL "Ref#"
    FIELD tr-dscr     AS CHARACTER FORMAT "X(60)" LABEL "Description"
    FIELD tr-amt      LIKE glhist.tr-amt LABEL "Amount"
    FIELD db-amt      LIKE glhist.tr-amt LABEL "Total Debit"
    FIELD cr-amt      LIKE glhist.tr-amt LABEL "Total Credit"
    FIELD net-amt     LIKE glhist.tr-amt LABEL "Net Amount"
    FIELD tr-num      LIKE glhist.tr-num FORMAT "9999999" LABEL "Run #" 
    FIELD tr-from     AS cha       FORM "x(30)" LABEL "Inquiry From" 
    FIELD actnum      LIKE glhist.actnum LABEL "Account#"
    FIELD createdBy   LIKE glhist.createdBy LABEL "Created By"
    FIELD createdDate LIKE glhist.createdDate LABEL "Created Date"
    FIELD posted      LIKE glhist.posted LABEL "Posted"
    FIELD tr-period   LIKE glhist.period LABEL "Pd"
    FIELD tr-yr       LIKE glhist.yr LABEL "Year"
    FIELD documentID  LIKE glhist.documentID LABEL "Document Id"
    FIELD sourceDate  LIKE glhist.sourceDate LABEL "Source Date" 
    FIELD riRowid     AS ROWID 
    INDEX tr-date IS PRIMARY tr-date.

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE            VARIABLE lv-first       AS LOG     NO-UNDO.
DEFINE            VARIABLE lv-sort-by     AS cha     NO-UNDO.
DEFINE            VARIABLE ll-sort-asc    AS LOG     NO-UNDO.
DEFINE            VARIABLE lv-sort-by-lab AS cha     NO-UNDO.
DEFINE NEW SHARED VARIABLE uperiod        AS INTEGER NO-UNDO.  /* for gl-open.p */
DEFINE            VARIABLE v-count        AS INTEGER NO-UNDO.
DEFINE            VARIABLE v-acc-length   AS INTEGER NO-UNDO.
DEFINE            VARIABLE v-col-move     AS LOG     INIT YES NO-UNDO.
DEFINE            VARIABLE lv-close-bal   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE lv-open-bal    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.       
DEFINE            VARIABLE dTotalBalance  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO. 

&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING

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
&Scoped-define INTERNAL-TABLES tt-glinq

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-glinq.tr-date tt-glinq.jrnl tt-glinq.db-amt tt-glinq.cr-amt tt-glinq.net-amt tt-glinq.sourceDate tt-glinq.createdBy tt-glinq.createdDate tt-glinq.posted tt-glinq.tr-num  tt-glinq.tr-period tt-glinq.tr-yr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-glinq.tr-date tt-glinq.jrnl    
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-glinq
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-glinq
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-glinq
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq.
&Scoped-define TABLES-IN-QUERY-br_table tt-glinq
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-glinq
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-glinq


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-go btn-all lv-period-fr lv-period-to ~
iRunFrom iRunTo dtDateFrom dtDateTo lv-year br_table 
&Scoped-Define DISPLAYED-OBJECTS lv-period-fr lv-period-to iRunFrom iRunTo ~
dtDateFrom dtDateTo lv-year   
//FI_moveCol
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 lv-period-fr lv-period-to iRunFrom iRunTo dtDateFrom ~
dtDateTo lv-year 

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
DEFINE BUTTON btn-all 
    LABEL "&Show All" 
    SIZE 12 BY 1.

DEFINE BUTTON btn-go 
    LABEL "&Go" 
    SIZE 12 BY 1.

DEFINE VARIABLE dtDateFrom   AS DATE      FORMAT "99/99/9999":U 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE dtDateTo     AS DATE      FORMAT "99/99/9999":U 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1
    BGCOLOR 15 NO-UNDO.  

DEFINE VARIABLE iRunFrom     AS INTEGER   FORMAT ">>>>>>9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE iRunTo       AS INTEGER   FORMAT ">>>>>>9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE lv-period-fr AS INTEGER   FORMAT ">9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 6 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE lv-period-to AS INTEGER   FORMAT ">9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE lv-year      AS INTEGER   FORMAT ">>>9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 8.8 BY 1
    BGCOLOR 15 NO-UNDO.
     
DEFINE RECTANGLE RECT-9
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 167 BY 2.9.         
 

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
    tt-glinq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
    QUERY br_table NO-LOCK DISPLAY
    tt-glinq.tr-num FORMAT "9999999" LABEL-BGCOLOR 14  WIDTH 12       
    tt-glinq.tr-date LABEL-BGCOLOR 14
    tt-glinq.jrnl LABEL-BGCOLOR 14      
    tt-glinq.db-amt FORM "->>,>>>,>>9.99" LABEL-BGCOLOR 14
    tt-glinq.cr-amt FORM "->>,>>>,>>9.99" LABEL-BGCOLOR 14
    tt-glinq.net-amt FORM "->>,>>>,>>9.99" LABEL-BGCOLOR 14      
    tt-glinq.sourceDate LABEL-BGCOLOR 14
    tt-glinq.createdBy  LABEL-BGCOLOR 14
    tt-glinq.createdDate LABEL-BGCOLOR 14
    tt-glinq.posted LABEL-BGCOLOR 14
    
    tt-glinq.tr-period LABEL-BGCOLOR 14
    tt-glinq.tr-yr LABEL-BGCOLOR 14
     ENABLE  tt-glinq.tr-date  tt-glinq.jrnl  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 167 BY 16.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main    
     lv-year AT ROW 2.1 COL 2.2 COLON-ALIGNED NO-LABEL
     lv-period-fr AT ROW 2.05 COL 13.8 COLON-ALIGNED NO-LABEL
     lv-period-to AT ROW 2.05 COL 23.6 COLON-ALIGNED NO-LABEL
     iRunFrom AT ROW 2.05 COL 31.8 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     iRunTo AT ROW 2.05 COL 44.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     dtDateFrom AT ROW 2.05 COL 59.4 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     dtDateTo AT ROW 2.05 COL 76.4 COLON-ALIGNED NO-LABEL WIDGET-ID 68       
     btn-go AT ROW 2 COL 138.2
     btn-all AT ROW 2 COL 154.6
     br_table AT ROW 4.05 COL 1
     "__" VIEW-AS TEXT
          SIZE 1.6 BY .67 AT ROW 1.91 COL 76.4 WIDGET-ID 70
     "Year" VIEW-AS TEXT
          SIZE 8.6 BY .71 AT ROW 1.19 COL 4.2 WIDGET-ID 52
          FGCOLOR 9 FONT 6
     "Run #" VIEW-AS TEXT
          SIZE 15.6 BY .71 AT ROW 1.19 COL 34 WIDGET-ID 60
          FGCOLOR 9 FONT 6
     "__" VIEW-AS TEXT
          SIZE 1.6 BY .67 AT ROW 1.95 COL 23 WIDGET-ID 62
     "__" VIEW-AS TEXT
          SIZE 1.6 BY .67 AT ROW 1.91 COL 45 WIDGET-ID 64
     "Period Range" VIEW-AS TEXT
          SIZE 15.6 BY .71 AT ROW 1.19 COL 15.6 WIDGET-ID 54
          FGCOLOR 9 FONT 6
     "Date" VIEW-AS TEXT
          SIZE 15.6 BY .71 AT ROW 1.19 COL 61.4 WIDGET-ID 72
          FGCOLOR 9 FONT 6
     RECT-9 AT ROW 1 COL 1 WIDGET-ID 4     
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    FGCOLOR 1 .


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
         HEIGHT             = 18.71
         WIDTH              = 169.2.
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
/* BROWSE-TAB br_table lv-close-bal F-Main */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.
       
ASSIGN 
    br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main = 2.       

/* SETTINGS FOR FILL-IN dtDateFrom IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN dtDateTo IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iRunFrom IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN iRunTo IN FRAME F-Main
   1                                                                    */      
/* SETTINGS FOR FILL-IN lv-period-fr IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-period-to IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-year IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME F-Main
   NO-ENABLE                                                            */   
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq.
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
    DO:   
        RUN pUpdate.    
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
        {methods/template/sortindicator.i} 
        DEFINE VARIABLE lh-column     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lv-column-nam AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-column-lab AS CHARACTER NO-UNDO.

  
        ASSIGN
            lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
            lv-column-nam = lh-column:NAME
            lv-column-lab = lh-column:LABEL.

        IF lv-sort-by = lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

        ASSIGN
            lv-sort-by = lv-column-nam.
        /*    lv-sort-by-lab = lv-column-lab.
       */
  

        APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

        /*APPLY "choose" TO btn-inq. */
        CASE lv-column-nam:            
            WHEN "tr-date" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date {&sortby-des}.           
                END.
            WHEN "jrnl" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl {&sortby-des}.
                END.            
            WHEN "db-amt" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.db-amt.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.db-amt {&sortby-des}.
                END.      
            WHEN "net-amt" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.net-amt.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.net-amt {&sortby-des}.
                END.
            WHEN "cr-amt" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.cr-amt.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.cr-amt {&sortby-des}.
                END.
            WHEN "createdBy" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdBy.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdBy {&sortby-des}.
                END.
            WHEN "sourceDate" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.sourceDate.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.sourceDate {&sortby-des}.
                END.              
            WHEN "createdDate" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdDate.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.createdDate {&sortby-des}.
                END.
            WHEN "posted" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.posted.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.posted {&sortby-des}.
                END.
            WHEN "tr-num" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-num.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-num {&sortby-des}.
                END.  
            WHEN "tr-period" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-period.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-period {&sortby-des}.
                END.
            WHEN "tr-yr" THEN 
                DO:
                    IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-yr.
                    ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-yr {&sortby-des}.
                END.
        END CASE.
        {methods/template/sortindicatorend.i} 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
    DO:
        DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE phandle       AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lEnableButton AS LOGICAL   NO-UNDO.


        /* This ADM trigger code must be preserved in order to notify other
           objects when the browser's current row changes. */
        {src/adm/template/brschnge.i}
                                           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
    DO:   
    &scoped-define exclude-row-display true 
        {methods/template/brwrowdisplay.i}  
        IF AVAILABLE tt-glinq AND MONTH(tt-glinq.tr-date) NE tt-glinq.tr-period  THEN
            ASSIGN   tt-glinq.tr-date:BGCOLOR IN BROWSE {&BROWSE-NAME} = 3  .
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-all B-table-Win
ON CHOOSE OF btn-all IN FRAME F-Main /* Show All */
    DO:
        ASSIGN             
            lv-period-fr = 1
            lv-period-to = 12
            iRunFrom     = 0
            iRunTo       = 9999999.
        ASSIGN lv-year.
      
        IF lv-year:SCREEN-VALUE EQ "0" THEN 
            lv-year = YEAR(TODAY).
      
        RUN GetDefaultValue.                 
      
        DISPLAY lv-year lv-period-fr lv-period-to iRunFrom iRunTo WITH FRAME {&FRAME-NAME}.
      
        ASSIGN {&list-1}.
      
        RUN build-inquiry.
        {&open-query-{&browse-name}}
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go B-table-Win
ON CHOOSE OF btn-go IN FRAME F-Main /* Go */
    DO:
        ASSIGN {&list-1}.
                  
        RUN build-inquiry.
        {&open-query-{&browse-name}}
          
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dtDateFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dtDateFrom B-table-Win
ON LEAVE OF dtDateFrom IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.  
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dtDateTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dtDateTo B-table-Win
ON LEAVE OF dtDateTo IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iRunFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iRunFrom B-table-Win
ON LEAVE OF iRunFrom IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.   
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iRunTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iRunTo B-table-Win
ON LEAVE OF iRunTo IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.

   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME lv-year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-year B-table-Win
ON VALUE-CHANGED OF lv-year IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}. 
  
        APPLY "value-changed" TO  lv-period-fr .
        APPLY "value-changed" TO  lv-period-to .     
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-period-fr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-period-fr B-table-Win
ON LEAVE OF lv-period-fr IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.      
  
        FIND FIRST period WHERE period.company = g_company
            AND period.yr = lv-year
            AND period.pnum = lv-period-fr NO-LOCK NO-ERROR.
        dtDateFrom:SCREEN-VALUE = STRING(IF AVAILABLE period AND period.pst NE ? THEN period.pst ELSE 01/01/01) /*12/31/9999*/ .   
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME lv-period-fr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-period-fr B-table-Win
ON VALUE-CHANGED OF lv-period-fr IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.      
  
        FIND FIRST period WHERE period.company = g_company
            AND period.yr = lv-year
            AND period.pnum = lv-period-fr NO-LOCK NO-ERROR.
        dtDateFrom:SCREEN-VALUE = STRING(IF AVAILABLE period AND period.pst NE ? THEN period.pst ELSE 01/01/01) /*12/31/9999*/ .   
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-period-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-period-to B-table-Win
ON LEAVE OF lv-period-to IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.
   
        FIND FIRST period WHERE period.company = g_company
            AND period.yr = lv-year
            AND period.pnum = lv-period-to NO-LOCK NO-ERROR.
        dtDateto:SCREEN-VALUE = STRING(IF AVAILABLE period AND period.pend NE ? THEN period.pend  ELSE 12/31/9999) /*01/01/0001*/ .    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME lv-period-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-period-to B-table-Win
ON VALUE-CHANGED OF lv-period-to IN FRAME F-Main
    DO:
        IF LASTKEY = -1 THEN RETURN.
        ASSIGN {&self-name}.
   
        FIND FIRST period WHERE period.company = g_company
            AND period.yr = lv-year
            AND period.pnum = lv-period-to NO-LOCK NO-ERROR.
        dtDateto:SCREEN-VALUE = STRING(IF AVAILABLE period AND period.pend NE ? THEN period.pend  ELSE 12/31/9999) /*01/01/0001*/ .    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

FIND FIRST company WHERE
    company.company EQ cocode
    NO-LOCK NO-ERROR.

IF AVAILABLE company THEN
DO:
    DO v-count = 1 TO 5:
        v-acc-length = v-acc-length + company.acc-dig[v-count].
        IF company.acc-dig[v-count] NE 0 THEN
            v-acc-length = v-acc-length + 1.
    END.    
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}


&SCOPED-DEFINE cellColumnDat b-glunbal
{methods/browsers/setCellColumns.i}

/*FI_moveCol = "Sort".
DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-inquiry B-table-Win 
PROCEDURE build-inquiry :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE tmp-start    AS DATE      NO-UNDO.
    DEFINE VARIABLE tmp-end      AS DATE      NO-UNDO.
    DEFINE VARIABLE iRecordLimit AS INTEGER   INIT 1000 NO-UNDO.
    DEFINE VARIABLE iTotalCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTitle       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lResponse    AS LOGICAL   NO-UNDO.
  
    
    EMPTY TEMP-TABLE tt-glinq.
    ASSIGN
        v-max-dscr-length = 0
        uperiod           = lv-period-fr
        lv-close-bal      = 0
        lv-open-bal       = 0
        dTotalBalance     = 0 .
    
    FIND FIRST period WHERE period.company = g_company
        AND period.yr = lv-year
        AND (period.pnum = lv-period-fr OR lv-period-fr EQ 0 ) NO-LOCK NO-ERROR.
    tmp-start = IF AVAILABLE period THEN period.pst ELSE 01/01/01 /*12/31/9999*/ .
    FIND FIRST period WHERE period.company = g_company
        AND period.yr = lv-year
        AND (period.pnum = lv-period-to OR lv-period-to EQ 0) NO-LOCK NO-ERROR.
    tmp-end = IF AVAILABLE period THEN period.pend ELSE 12/31/9999 /*01/01/0001*/ .
                
    IF lv-year EQ 0 THEN
        tmp-start = dtDateFrom .                                
     
    MainLoop:  
    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ cocode        
        AND (glhist.period  GE lv-period-fr OR lv-period-fr EQ 0)
        AND (glhist.period  LE lv-period-to  OR lv-period-to EQ 0)        
        /* AND (glhist.glYear EQ  lv-year OR lv-year EQ 0) */
        AND glhist.tr-date GE dtDateFrom 
        AND glhist.tr-date LE dtDateTo 
        AND (glhist.tr-num GE iRunFrom  OR iRunFrom EQ 0)
        AND (glhist.tr-num LE iRunTo  OR  iRunTo EQ 0)
          
        BREAK BY glhist.tr-num 
        BY glhist.tr-date :  
            
        IF FIRST-OF(glhist.tr-num) THEN 
            ASSIGN
                lv-open-bal  = 0
                lv-close-bal = 0.
      
        IF glhist.tr-amt GT 0 THEN
            ASSIGN           
                lv-open-bal = lv-open-bal + glhist.tr-amt.
        IF glhist.tr-amt LT 0 THEN
            ASSIGN           
                lv-close-bal = lv-close-bal + ( glhist.tr-amt * - 1) .
           
        IF (LAST-OF(glhist.tr-num) AND lv-open-bal NE lv-close-bal) OR (MONTH(glhist.tr-date) NE glhist.period) THEN
        DO:
            FIND FIRST tt-glinq EXCLUSIVE-LOCK 
                WHERE tt-glinq.tr-num EQ glhist.tr-num NO-ERROR.
            IF NOT AVAILABLE tt-glinq THEN
            DO:
         
                CREATE tt-glinq.
                ASSIGN 
                    tt-glinq.tr-date     = glhist.tr-date
                    tt-glinq.jrnl        = glhist.jrnl
                    tt-glinq.tr-dscr     = glhist.tr-dscr
                    tt-glinq.tr-amt      = glhist.tr-amt
                    tt-glinq.actnum      = glhist.actnum
                    tt-glinq.tr-num      = glhist.tr-num               
                    tt-glinq.createdBy   = glhist.createdBy
                    tt-glinq.createdDate = glhist.createdDate
                    tt-glinq.posted      = glhist.posted
                    tt-glinq.tr-num      = glhist.tr-num
                    tt-glinq.tr-period   = glhist.period
                    tt-glinq.tr-yr       = glhist.glYear
                    tt-glinq.documentID  = glhist.documentID
                    tt-glinq.sourceDate  = glhist.sourceDate
                    tt-glinq.riRowid     = ROWID(glhist).
            END.        
            ASSIGN
                tt-glinq.db-amt  = lv-open-bal
                tt-glinq.cr-amt  = lv-close-bal 
                tt-glinq.net-amt = lv-open-bal - lv-close-bal .                
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-fields B-table-Win 
PROCEDURE get-fields :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-company LIKE glhist.company NO-UNDO.
    DEFINE OUTPUT PARAMETER op-tr-num  LIKE glhist.tr-num  NO-UNDO.

    ASSIGN
        op-company = g_company
        op-tr-num  = IF AVAILABLE tt-glinq THEN tt-glinq.tr-num ELSE 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
        NO-LOCK NO-ERROR.
    ASSIGN
        lv-year      = IF AVAILABLE period THEN period.yr ELSE YEAR(TODAY)
        lv-period-fr = g_period
        lv-period-to = g_period
        uperiod      = g_period.
    RUN GetDefaultValue.
    dtDateFrom = TODAY . 
    dtDateFrom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY) .
    dtDateTo = TODAY . 
    dtDateTo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)  .
    RUN build-inquiry.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    RUN setCellColumns.
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN 
        tt-glinq.tr-date:READ-ONLY IN BROWSE {&browse-name} = YES
        tt-glinq.jrnl:READ-ONLY IN BROWSE {&browse-name}    = YES        
        .
    
    APPLY 'ENTRY':U TO lv-year IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            br_table:COLUMN-MOVABLE   = v-col-move
            br_table:COLUMN-RESIZABLE = v-col-move
            v-col-move                = NOT v-col-move.
       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDefaultValue B-table-Win 
PROCEDURE GetDefaultValue :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
  
        FIND FIRST period WHERE period.company = g_company
            AND period.yr = lv-year
            AND period.pnum = lv-period-fr NO-LOCK NO-ERROR.
        dtDateFrom = (IF AVAILABLE period THEN period.pst ELSE 01/01/01) /*12/31/9999*/ . 
        dtDateFrom:screen-value = STRING(IF AVAILABLE period AND period.pst NE ? THEN period.pst ELSE 01/01/01) /*12/31/9999*/ .
        
        FIND FIRST period WHERE period.company = g_company
            AND period.yr = lv-year
            AND period.pnum = lv-period-to NO-LOCK NO-ERROR.
        dtDateto = (IF AVAILABLE period THEN period.pend ELSE 12/31/9999) /*01/01/0001*/ . 
        dtDateto:SCREEN-VALUE = STRING(IF AVAILABLE period AND period.pend NE ? THEN period.pend ELSE 12/31/9999) /*01/01/0001*/ .
        IF iRunTo EQ 0 THEN 
            ASSIGN
                iRunTo              = 9999999
                iRunTo:SCREEN-VALUE = "9999999".   
   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallAudit B-table-Win
PROCEDURE pCallAudit:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cMsgResponse AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hTable AS HANDLE NO-UNDO.

    IF AVAILABLE tt-glinq THEN DO:
        IF NOT CAN-FIND(FIRST AuditTbl
                        WHERE AuditTbl.AuditTable   EQ "glhist"
                          AND (AuditTbl.AuditCreate EQ YES
                           OR  AuditTbl.AuditDelete EQ YES
                           OR  AuditTbl.AuditUpdate EQ YES)) THEN DO:
            RUN displayMessageQuestion ("17", OUTPUT cMsgResponse).
            IF cMsgResponse EQ "no" THEN DO:
                RETURN NO-APPLY.
            END. /* if no */
        END. /* if audittble */
        FIND FIRST glhist NO-LOCK
             WHERE ROWID(glhist) EQ tt-glinq.riRowid
             NO-ERROR.
        IF AVAILABLE glhist THEN DO:
            hTable = BUFFER glhist:HANDLE.
            RUN system/CallAudit.p ("glhist", hTable, ipcType, PROGRAM-NAME(1)).
        END. /* avail glhist */
    END. /* avail tt-glinq */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdate B-table-Win 
PROCEDURE pUpdate :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE phandle AS HANDLE NO-UNDO.
    DEFINE VARIABLE riRowid AS ROWID  NO-UNDO.
    DEFINE VARIABLE riRecid AS RECID  NO-UNDO.

    IF AVAILABLE tt-glinq THEN
    DO:
        riRecid = RECID(tt-glinq).
        RUN gl/GLHistPop.w (INPUT tt-glinq.tr-num) .
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
    {src/adm/template/snd-list.i "tt-glinq"}

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

