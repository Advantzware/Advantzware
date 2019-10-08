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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR v-max-dscr-length AS INT NO-UNDO.               

DEF SHARED VAR g_company AS cha NO-UNDO.
DEF SHARED VAR g_loc AS cha NO-UNDO.
DEF SHARED VAR g_period AS INT NO-UNDO.
{methods/defines/hndldefs.i}               
    
{sys/inc/VAR.i NEW SHARED}

DEF TEMP-TABLE tt-glinq NO-UNDO
    FIELD tr-date LIKE gltrans.tr-date LABEL "Date"
    FIELD jrnl LIKE gltrans.jrnl       LABEL "Ref#"
    FIELD tr-dscr AS CHAR FORMAT "X(60)" LABEL "Description"
    FIELD tr-amt LIKE gltrans.tr-amt   LABEL "Amount"
    FIELD tr-num LIKE gltrans.trnum
    FIELD tr-from AS cha      FORM "x(30)"    LABEL "Inquiry From" 
    FIELD actnum LIKE gltrans.actnum LABEL "Account#"
    INDEX tr-date IS PRIMARY tr-date.

ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-first AS LOG NO-UNDO.
DEF VAR lv-sort-by AS cha NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-sort-by-lab AS cha NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.  /* for gl-open.p */
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-acc-length AS INT NO-UNDO.
DEF VAR v-col-move AS LOG INIT YES NO-UNDO.

&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-glinq

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-glinq.actnum tt-glinq.tr-date tt-glinq.jrnl tt-glinq.tr-dscr tt-glinq.tr-amt tt-glinq.tr-from   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-glinq.actnum tt-glinq.tr-date tt-glinq.jrnl tt-glinq.tr-dscr tt-glinq.tr-amt   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-glinq
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-glinq
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-glinq
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq.
&Scoped-define TABLES-IN-QUERY-br_table tt-glinq
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-glinq


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_acct lv-year lv-period-fr lv-period-to ~
btn-go btn-all btn-print br_table 
&Scoped-Define DISPLAYED-OBJECTS begin_acct lv-actname lv-year lv-open-bal ~
lv-close-bal lv-period-fr lv-period-to FI_moveCol

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 begin_acct lv-year lv-period-fr lv-period-to 

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

DEFINE BUTTON btn-print 
     LABEL "&Print" 
     SIZE 12 BY 1.

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(25)":U 
     LABEL "Account Number" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-actname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.


DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-close-bal AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Closing Balance" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE lv-open-bal AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Opening Balance" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE lv-period-fr AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Period Range" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-period-to AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-year AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-glinq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-glinq.actnum LABEL-BGCOLOR 14 tt-glinq.tr-date LABEL-BGCOLOR 14 tt-glinq.jrnl LABEL-BGCOLOR 14
      tt-glinq.tr-dscr FORM "X(60)" LABEL-BGCOLOR 14 tt-glinq.tr-amt FORM "->>,>>>,>>9.99" LABEL-BGCOLOR 14
      tt-glinq.tr-from FORM "x(30)" LABEL-BGCOLOR 14
     ENABLE tt-glinq.actnum tt-glinq.tr-date  tt-glinq.jrnl tt-glinq.tr-dscr tt-glinq.tr-amt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 12.86
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     begin_acct AT ROW 1.48 COL 34 COLON-ALIGNED
     lv-actname AT ROW 1.48 COL 65 COLON-ALIGNED NO-LABEL
     lv-year AT ROW 2.67 COL 34 COLON-ALIGNED
     lv-open-bal AT ROW 2.67 COL 88 COLON-ALIGNED
     lv-close-bal AT ROW 3.62 COL 88 COLON-ALIGNED
     lv-period-fr AT ROW 4.81 COL 34 COLON-ALIGNED
     lv-period-to AT ROW 4.81 COL 47 COLON-ALIGNED NO-LABEL
     btn-go AT ROW 5.29 COL 60
     btn-all AT ROW 5.29 COL 78
     btn-print AT ROW 5.29 COL 96
     "Click on Yellow Field to " VIEW-AS TEXT
          SIZE 28 BY .95 AT ROW 5.29 COL 112 WIDGET-ID 14
     FI_moveCol AT ROW 5.29 COL 133 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     br_table AT ROW 6.71 COL 1
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
         HEIGHT             = 18.71
         WIDTH              = 143.8.
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
/* BROWSE-TAB br_table btn-print F-Main */
/* BROWSE-TAB br_table FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */

/* SETTINGS FOR FILL-IN begin_acct IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-actname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-close-bal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-open-bal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-period-fr IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-period-to IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-year IN FRAME F-Main
   1                                                                    */
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

&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct B-table-Win
ON ENTRY OF begin_acct IN FRAME F-Main /* Account Number */
DO:
  RUN new-begin_acct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct B-table-Win
ON HELP OF begin_acct IN FRAME F-Main /* Account Number */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.


  RUN windows/l-acct.w (cocode, "", {&self-name}:SCREEN-VALUE, OUTPUT char-val).
  IF char-val NE "" AND ENTRY(1,char-val) NE {&self-name}:SCREEN-VALUE THEN DO:
    {&self-name}:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-begin_acct.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct B-table-Win
ON LEAVE OF begin_acct IN FRAME F-Main /* Account Number */
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

   IF NOT CAN-FIND(FIRST account WHERE account.company = g_company
                                   AND account.actnum = begin_acct) THEN DO:
      MESSAGE "Invalid Account Number. Try Help." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   RUN new-begin_acct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct B-table-Win
ON VALUE-CHANGED OF begin_acct IN FRAME F-Main /* Account Number */
DO:
  RUN new-begin_acct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-period-to B-table-Win
ON LEAVE OF lv-period-to IN FRAME F-Main /* Account Number */
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

   IF lv-period-to:SCREEN-VALUE < lv-period-fr:SCREEN-VALUE THEN DO:
       MESSAGE "Ending Period must be same or later period..." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.

   IF lv-period-fr:SCREEN-VALUE EQ "0" OR lv-period-to:SCREEN-VALUE EQ "0" THEN DO:
       MESSAGE "Period should not be zero..." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-period-fr B-table-Win
ON LEAVE OF lv-period-fr IN FRAME F-Main /* Account Number */
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

   IF  lv-period-fr:SCREEN-VALUE EQ "0" THEN DO:
       MESSAGE "Period should not be zero..." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
  DEF VAR phandle AS HANDLE NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.


  {methods/run_link.i "container-source" "select-page" "(2)"}
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

  IF lv-sort-by = lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ASSIGN
     lv-sort-by     = lv-column-nam.
 /*    lv-sort-by-lab = lv-column-lab.
*/
  

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*APPLY "choose" TO btn-inq. */
  CASE lv-column-nam:
      WHEN "actnum" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.actnum.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.actnum {&sortby-des}.           
      END.
      WHEN "tr-date" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-date {&sortby-des}.           
      END.
      WHEN "jrnl" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.jrnl {&sortby-des}.
      END.
      WHEN "tr-dscr" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-dscr.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-dscr {&sortby-des}.
      END.
      WHEN "tr-amt" THEN DO:
           IF ll-sort-asc THEN OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-amt.
           ELSE OPEN QUERY {&SELF-NAME} FOR EACH tt-glinq BY tt-glinq.tr-amt {&sortby-des}.
      END.
  END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'run-no-target':U,OUTPUT char-hdl).
  DO li = 1 TO NUM-ENTRIES(char-hdl):
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
      RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ("open-query"). 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-all B-table-Win
ON CHOOSE OF btn-all IN FRAME F-Main /* Show All */
DO:
      ASSIGN begin_acct = ""
             lv-period-fr = 1
             lv-period-to = 12.
      ASSIGN lv-year.
      
      IF lv-year:SCREEN-VALUE EQ "0" THEN 
                 lv-year = YEAR(TODAY).
      
      DISPLAY begin_acct lv-year lv-period-fr lv-period-to WITH FRAME {&FRAME-NAME}.
      
      RUN build-inquiry.
      {&open-query-{&browse-name}}
      DISPLAY lv-open-bal lv-close-bal WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go B-table-Win
ON CHOOSE OF btn-go IN FRAME F-Main /* Go */
DO:
      ASSIGN {&list-1}.
      
      IF lv-period-to:SCREEN-VALUE < lv-period-fr:SCREEN-VALUE THEN DO:
          MESSAGE "Ending Period must be same or later period" VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF lv-period-fr:SCREEN-VALUE EQ "0" OR lv-period-to:SCREEN-VALUE EQ "0" THEN DO:
          MESSAGE "Period should not be zero..." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      RUN build-inquiry.
      {&open-query-{&browse-name}}
          DISPLAY lv-open-bal lv-close-bal WITH FRAME {&FRAME-NAME}.
      
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print B-table-Win
ON CHOOSE OF btn-print IN FRAME F-Main /* Print */
DO:
{sys/form/r-top.f}

DEF VAR lv-acct   LIKE account.actnum                   NO-UNDO.
DEF VAR lv-dscr   LIKE account.dscr                     NO-UNDO.        
DEF VAR lv-yr     LIKE period.yr                        NO-UNDO.
DEF VAR lv-per-fr LIKE period.pnum                      NO-UNDO.
DEF VAR lv-per-to LIKE lv-per-fr                        NO-UNDO.
DEF VAR lv-open   AS   DEC FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR lv-close  LIKE lv-open                          NO-UNDO.

FORM SKIP(1)
     lv-acct    COLON 30 LABEL "GL Account#"
     lv-dscr    AT 32    NO-LABEL
     lv-yr      COLON 30 LABEL "Year"
     lv-per-fr  COLON 30 LABEL "Beginning Period"
     lv-per-to  COLON 30 LABEL "Ending Period"
     SKIP(1)
     lv-open    COLON 30 LABEL "Opening Balance"
     SKIP(1)

    WITH STREAM-IO WIDTH 80 FRAME gl-inq SIDE-LABELS NO-UNDERLINE PAGE-TOP
         TITLE "  A C C O U N T   A C T I V I T Y  ".

format space(4)
       tt-glinq.tr-date column-label "Date"  space(2)
       tt-glinq.jrnl    column-label "Ref#"  space(2)
       tt-glinq.tr-dscr label "Description" format "x(33)" space(2)
       tt-glinq.tr-amt  label "Amount" space(3)
      with no-box no-attr-space frame g2lines row 8 12 down stream-io WIDTH 80 centered.

     
  FIND FIRST tt-glinq NO-ERROR.

  IF AVAIL tt-glinq THEN DO WITH FRAME gl-inq:
    SESSION:SET-WAIT-STATE ("general").
        
    {sys/inc/print1.i}
    {sys/inc/outprint.i 56}

    VIEW FRAME r-top.

    DISPLAY begin_acct   @ lv-acct
            lv-actname   @ lv-dscr
            lv-year      @ lv-yr
            lv-period-fr @ lv-per-fr
            lv-period-to @ lv-per-to
            lv-open-bal  @ lv-open.

    FOR EACH tt-glinq:
      DISPLAY tt-glinq.tr-date
              tt-glinq.jrnl
              tt-glinq.tr-dscr
              tt-glinq.tr-amt
          WITH FRAME g2lines.
      DOWN WITH FRAME g2lines.
    END.

    DISPLAY SKIP(1)
            lv-close-bal @ lv-close COLON 30 LABEL "Closing Balance"
            SKIP(2)
        WITH STREAM-IO WIDTH 80 FRAME gl-inq1 SIDE-LABELS NO-UNDERLINE.

    OUTPUT CLOSE.

    SESSION:SET-WAIT-STATE ("").

    RUN scr-rpt.w (list-name,TRIM(FRAME gl-inq:TITLE),11,"P"). /* open file-name, title */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

FIND FIRST company WHERE
     company.company EQ cocode
     NO-LOCK NO-ERROR.

IF AVAIL company THEN
DO:
   DO v-count = 1 TO 5:
      v-acc-length = v-acc-length + company.acc-dig[v-count].
      IF company.acc-dig[v-count] NE 0 THEN
         v-acc-length = v-acc-length + 1.
   END.

   IF v-acc-length GT 1 THEN
      tt-glinq.actnum:WIDTH-CHARS IN BROWSE {&browse-name} = v-acc-length + 3.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}


&SCOPED-DEFINE cellColumnDat b-glinq
{methods/browsers/setCellColumns.i}

FI_moveCol = "Sort".
DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

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
  DEF VAR tmp-start AS DATE NO-UNDO.
  DEF VAR tmp-end AS DATE NO-UNDO.

  EMPTY TEMP-TABLE tt-glinq.
  ASSIGN
     v-max-dscr-length = 0
     uperiod = lv-period-fr.
  FIND first account WHERE account.company = g_company
                       AND account.actnum = begin_acct NO-LOCK NO-ERROR.

  FIND FIRST period WHERE period.company = g_company
                      AND period.yr = lv-year
                      AND period.pnum = lv-period-fr NO-LOCK NO-ERROR.
  tmp-start = IF AVAIL period THEN period.pst ELSE 01/01/01 /*12/31/9999*/ .
  FIND FIRST period WHERE period.company = g_company
                      AND period.yr = lv-year
                      AND period.pnum = lv-period-to NO-LOCK NO-ERROR.
  tmp-end = IF AVAIL period THEN period.pend ELSE 12/31/9999 /*01/01/0001*/ .
 RUN gl/gl-opend.p (ROWID(account), tmp-start, OUTPUT lv-open-bal).

  lv-close-bal = lv-open-bal.

  for each glhist NO-LOCK
      where glhist.company eq cocode
        and (glhist.actnum  eq begin_acct OR begin_acct = "")
        and glhist.period  ge lv-period-fr
        and glhist.period  le lv-period-to
        and glhist.tr-date ge tmp-start
        and glhist.tr-date le tmp-end
      by glhist.tr-date :
      CREATE tt-glinq.
      ASSIGN tt-glinq.tr-date = glhist.tr-date
             tt-glinq.jrnl = glhist.jrnl
             tt-glinq.tr-dscr = glhist.tr-dscr
             tt-glinq.tr-amt = glhist.tr-amt
             tt-glinq.tr-from = "GL History "
             tt-glinq.actnum = glhist.actnum
             tt-glinq.tr-num = glhist.tr-num
             lv-close-bal = lv-close-bal + glhist.tr-amt.

      IF LENGTH(glhist.tr-dscr) GT v-max-dscr-length THEN
         v-max-dscr-length = LENGTH(glhist.tr-dscr).
  end.

  for each gltrans  NO-LOCK
      where gltrans.company eq cocode
        and (gltrans.actnum  eq begin_acct OR begin_acct = "")
        and gltrans.period  ge lv-period-fr
        and gltrans.period  le lv-period-to
        and gltrans.tr-date ge tmp-start
        and gltrans.tr-date le tmp-end
      by gltrans.tr-date:
      CREATE tt-glinq.
      ASSIGN tt-glinq.tr-date = gltran.tr-date
             tt-glinq.jrnl = gltran.jrnl
             tt-glinq.tr-dscr = gltran.tr-dscr
             tt-glinq.tr-amt = gltran.tr-amt
             tt-glinq.tr-from = "GL Transaction " + string(gltran.trnum)
             tt-glinq.actnum = gltrans.actnum
             tt-glinq.tr-num = gltrans.trnum
             lv-close-bal = lv-close-bal + gltrans.tr-amt.

      IF LENGTH(gltran.tr-dscr) GT v-max-dscr-length THEN
         v-max-dscr-length = LENGTH(gltran.tr-dscr).
  end.
 
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
        br_table:COLUMN-MOVABLE = v-col-move
        br_table:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move
        FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
     DISPLAY FI_moveCol.
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
  DEF OUTPUT PARAM op-company LIKE glhist.company NO-UNDO.
  DEF OUTPUT PARAM op-tr-num  LIKE glhist.tr-num  NO-UNDO.

  ASSIGN
   op-company = g_company
   op-tr-num  = IF AVAIL tt-glinq THEN tt-glinq.tr-num ELSE 0.

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
  find first period
      where period.company eq cocode
        and period.pst     le today
        and period.pend    ge today
      no-lock no-error.
  assign
   lv-year      = if avail period then period.yr else year(today)
   lv-period-fr = g_period
   lv-period-to = g_period
   uperiod      = g_period.

  RUN build-inquiry.
  tt-glinq.tr-dscr:WIDTH-CHARS IN BROWSE {&browse-name} = v-max-dscr-length + 20.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    RUN setCellColumns.
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN tt-glinq.tr-date:READ-ONLY IN BROWSE {&browse-name} = YES
           tt-glinq.jrnl:READ-ONLY IN BROWSE {&browse-name} = YES
           tt-glinq.tr-dscr:READ-ONLY IN BROWSE {&browse-name} = YES
           tt-glinq.tr-amt:READ-ONLY IN BROWSE {&browse-name} = YES   
           tt-glinq.actnum:READ-ONLY = YES
           FI_moveCol = "Sort"
           .
        DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
        APPLY 'ENTRY':U TO begin_acct IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-begin_acct B-table-Win 
PROCEDURE new-begin_acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND account
        WHERE account.company EQ g_company
          AND account.actnum  EQ begin_acct:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN
      ASSIGN
       lv-actname:SCREEN-VALUE = account.dscr
       lv-actname.
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

