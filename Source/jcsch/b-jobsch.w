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
DEF VAR lv-die-no LIKE eb.die-no NO-UNDO.
DEF VAR lv-board LIKE ef.board NO-UNDO.
DEF VAR lv-po-no LIKE po-ord.po-no NO-UNDO.
DEF VAR lv-color AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-due-date AS DATE NO-UNDO.
DEF VAR lv-mr-stime AS cha NO-UNDO.
DEF VAR lv-mr-etime AS cha NO-UNDO.
DEF VAR lv-run-stime AS cha NO-UNDO.
DEF VAR lv-run-etime AS cha NO-UNDO.
DEF VAR lv-mr-hr AS cha NO-UNDO.
DEF VAR lv-run-hr AS cha NO-UNDO.
DEF VAR lv-col-bgcolor AS INT NO-UNDO.
DEF VAR lv-brs-col-hd AS WIDGET-HANDLE NO-UNDO.
DEF VAR lv-brs-col-hd-list AS cha NO-UNDO.
DEF VAR lv-mr-edate AS DATE NO-UNDO.
DEF VAR lv-run-edate AS DATE NO-UNDO.
DEF VAR lv-accum-hr AS DEC NO-UNDO.
DEF VAR lv-mr-tot AS INT NO-UNDO.  /* for pink color */
DEF VAR lv-run-tot AS INT NO-UNDO.        /* for pink color */
DEF VAR lv-prev-st-date AS DATE NO-UNDO.  /* for start-date update */
DEF VAR lv-first-time AS LOG NO-UNDO. /* don't high-light first time */
DEF BUFFER bf-job-mch FOR job-mch.
DEF BUFFER bf-job-hdr FOR job-hdr.
DEF BUFFER bf-job FOR job.
DEF VAR ll-enabled AS LOG NO-UNDO.
DEF VAR lv-seq-color AS INT NO-UNDO.
DEF VAR lv-corr AS LOG NO-UNDO.
DEF VAR v-dim-fmt AS cha NO-UNDO.

DEF TEMP-TABLE tt-sch FIELD rec-id AS RECID
                      FIELD seq-no LIKE job-mch.seq-no
                      FIELD board LIKE ef.board
                      FIELD die LIKE   eb.die-no
                      FIELD plate LIKE eb.plate-no
                      FIELD colour AS cha EXTENT 10
                      FIELD color-list AS cha FORM "x(50)"
                      FIELD Start-date AS DATE FORM "99/99/9999"
                      FIELD Start-time AS INT 
                      FIELD due-date AS DATE FORM "99/99/9999"
                      FIELD po-no AS INT LABEL "PO#"
                      FIELD cust-no LIKE oe-ord.cust-no
                      FIELD anchored AS log LABEL "Lock"  VIEW-AS TOGGLE-BOX 
                      FIELD i-no LIKE job-hdr.i-no 
                      FIELD len LIKE eb.len
                      FIELD wid LIKE eb.wid
                      FIELD dep LIKE eb.dep
                      FIELD style LIKE eb.style
                      INDEX tsch IS PRIMARY  start-date ASCENDING
                                             seq-no ASCENDING 
                      .

DEF TEMP-TABLE tt-color FIELD i-code AS cha.
DEF TEMP-TABLE tt-seq FIELD rec-id AS RECID
                      FIELD seq AS INT.
DEF TEMP-TABLE tt-reseq FIELD rec-id AS RECID
                        FIELD seq-no AS INT.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "start-date"  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Start Date"  NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
/*
&SCOPED-DEFINE sortby-log                                          ~
    IF lv-sort-by EQ "due-date" THEN STRing(year(tt-sch.due-date)) + STRING(MONTH(tt-sch.due-date),"99") + STRING(DAY(tt-sch.due-date),"99")   ELSE ~
    IF lv-sort-by EQ "board"    THEN tt-sch.board           ELSE ~
    IF lv-sort-by EQ "Die"     THEN tt-sch.die                 ELSE ~
    IF lv-sort-by EQ "seq-no"   THEN tt-sch.seq-no              ELSE ~
    IF lv-sort-by EQ "Cust-no"  THEN tt-sch.cust-no            ELSE ~
    IF lv-sort-by EQ "Plate"   THEN tt-sch.plate               ELSE ~
                 STRING(YEAR(tt-sch.start-date),"9999") + STRING(MONTH(tt-sch.start-date),"99") + STRING(DAY(tt-sch.start-date),"99")

     /*
    IF lv-sort-by EQ "job-no"    THEN STRING(oe-ordl.job-no,"x(6)") + STRING(oe-ordl.job-no2,"99")                                                     ELSE ~
                                      STRING(YEAR(oe-ordl.req-date),"9999") + STRING(MONTH(oe-ordl.req-date),"99") + STRING(DAY(oe-ordl.req-date),"99")
    */

*/                           /*     string(tt-sch.len,">>>>9.99<<<")*/

&SCOPED-DEFINE sortby-log                                          ~
    IF lv-sort-by EQ "due-date" THEN STRing(year(tt-sch.due-date)) + STRING(MONTH(tt-sch.due-date),"99") + STRING(DAY(tt-sch.due-date),"99")   ELSE ~
    IF lv-sort-by EQ "board"    THEN tt-sch.board           ELSE ~
    IF lv-sort-by EQ "Die"     THEN tt-sch.die                 ELSE ~
    IF lv-sort-by EQ "seq-no"     THEN fill(" ", 3 - LENGTH(string(tt-sch.seq-no))) + trim(string(tt-sch.seq-no))              ELSE ~
    IF lv-sort-by EQ "Cust-no"  THEN tt-sch.cust-no            ELSE ~
    IF lv-sort-by EQ "Plate"   THEN tt-sch.plate               ELSE ~
    IF lv-sort-by EQ "i-no"   THEN tt-sch.i-no               ELSE ~
    IF lv-sort-by EQ "Style"   THEN tt-sch.style               ELSE ~
    IF lv-sort-by EQ "Len"   THEN   string(tt-sch.len,v-dim-fmt)  ELSE ~
    IF lv-sort-by EQ "Wid"   THEN string(tt-sch.wid,v-dim-fmt)       ELSE ~
    IF lv-sort-by EQ "Dep"   THEN string(tt-sch.dep,v-dim-fmt)       ELSE ~
    IF lv-sort-by EQ "job-no"   THEN job-mch.job-no            ELSE ~
                 STRING(YEAR(tt-sch.start-date),"9999") + STRING(MONTH(tt-sch.start-date),"99") + STRING(DAY(tt-sch.start-date),"99")

&SCOPED-DEFINE sortby-log2                                         ~
    IF lv-sort-by EQ "due-date" THEN STRing(year(bf-tsch.due-date)) + STRING(MONTH(bf-tsch.due-date),"99") + STRING(DAY(bf-tsch.due-date),"99")   ELSE ~
    IF lv-sort-by EQ "board"    THEN bf-tsch.board           ELSE ~
    IF lv-sort-by EQ "Die"     THEN bf-tsch.die                 ELSE ~
    IF lv-sort-by EQ "seq-no"     THEN fill(" ", 3 - LENGTH(string(bf-tsch.seq-no))) + trim(string(bf-tsch.seq-no))              ELSE ~
    IF lv-sort-by EQ "Cust-no"  THEN bf-tsch.cust-no            ELSE ~
    IF lv-sort-by EQ "Plate"   THEN bf-tsch.plate               ELSE ~
    IF lv-sort-by EQ "i-no"   THEN tt-sch.i-no               ELSE ~
    IF lv-sort-by EQ "Style"   THEN tt-sch.style               ELSE ~
    IF lv-sort-by EQ "Len"   THEN string(tt-sch.len,v-dim-fmt)     ELSE ~
    IF lv-sort-by EQ "Wid"   THEN string(tt-sch.wid,v-dim-fmt)     ELSE ~
    IF lv-sort-by EQ "Dep"   THEN string(tt-sch.dep,v-dim-fmt)     ELSE ~
    IF lv-sort-by EQ "job-no"   THEN bf-mch.job-no               ELSE ~
                 STRING(YEAR(bf-tsch.start-date),"9999") + STRING(MONTH(bf-tsch.start-date),"99") + STRING(DAY(bf-tsch.start-date),"99")
/*
&SCOPED-DEFINE sortby BY tt-sch.start-date 
&SCOPED-DEFINE sortby2 BY bf-tsch.start-date 
*/
&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})           
    /*{&sortby} */

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        
    /*{&sortby} */

&SCOPED-DEFINE sortby-phrase-asc2  ~
    BY ({&sortby-log2})            
    /*{&sortby2}  */

&SCOPED-DEFINE sortby-phrase-desc2  ~
    BY ({&sortby-log2}) DESC        
    /*{&sortby2}*/

DEF VAR lv-old-seq AS INT NO-UNDO.  /* for override */

{custom/globdefs.i}
{methods/defines/hndldefs.i}

DEF VAR v-do-ontime AS LOG INIT YES NO-UNDO.
DEF VAR v-do-overbk AS LOG INIT YES NO-UNDO.
DEF VAR v-do-board AS LOG NO-UNDO.
DEF VAR v-do-prep AS LOG NO-UNDO.
DEF VAR v-do-late AS LOG INIT YES NO-UNDO.

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

def var k_frac as dec init 6.25 no-undo.
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES mach
&Scoped-define FIRST-EXTERNAL-TABLE mach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sch job-mch job-hdr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-sch.anchored tt-sch.seq-no job-mch.job-no job-mch.job-no2 job-mch.frm job-mch.blank-no job-mch.pass tt-sch.cust-no tt-sch.start-date /* display-due-date() @ lv-due-date display-board() @ lv-board display-eb() @ lv-die-no display-color() @ lv-color display-po-no() @ lv-po-no */ tt-sch.due-date tt-sch.board tt-sch.die tt-sch.plate tt-sch.po-no tt-sch.i-no tt-sch.style display-cw-dim(lv-corr,tt-sch.len) @ tt-sch.len display-cw-dim(lv-corr,tt-sch.wid) @ tt-sch.wid display-cw-dim(lv-corr,tt-sch.dep) @ tt-sch.dep tt-sch.color-list job-mch.start-date-su cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-mr-stime cvt-time-to-string('END',job-mch.end-time-su,0.00) @ lv-mr-etime job-mch.end-date-su cvt-hour-to-string(job-mch.mr-hr) @ lv-mr-hr /* job-mch.start-date */ cvt-time-to-string('',job-mch.start-time,0.00) @ lv-run-stime cvt-time-to-string('END',job-mch.end-time,0.00) @ lv-run-etime job-mch.end-date cvt-hour-to-string(job-mch.run-hr) @ lv-run-hr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-sch.anchored ~
tt-sch.seq-no ~
tt-sch.start-date ~
/* ~
tt-sch.due-date ~
tt-sch.board ~
tt-sch.die ~
tt-sch.plate ~
   job-mch.start-date-su ~
job-mch.end-date-su ~
job-mch.start-date ~
job-mch.end-date  */   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table tt-sch job-mch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table tt-sch
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table job-mch
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-sch, ~
               EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK, ~
               FIRST job-hdr WHERE job-hdr.company = job-mch.company                       AND job-hdr.job = job-mch.job NO-LOCK                       BY tt-sch.anchored DESC BY tt-sch.start-date BY tt-sch.seq-no                       BY job-mch.job-no BY job-mch.job-no2 BY job-mch.frm BY job-mch.blank-no
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME}     FOR EACH tt-sch, ~
               EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK, ~
               FIRST job-hdr WHERE job-hdr.company = job-mch.company                       AND job-hdr.job = job-mch.job NO-LOCK                       BY tt-sch.anchored DESC BY tt-sch.start-date BY tt-sch.seq-no                       BY job-mch.job-no BY job-mch.job-no2 BY job-mch.frm BY job-mch.blank-no.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-sch job-mch job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-sch
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table job-mch
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table job-hdr


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-go lv-start-date lv-end-date v-cust-no ~
v-board v-die v-plate v-color Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS lv-start-date lv-end-date v-cust-no ~
v-board v-die v-plate v-color 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-hour-to-string B-table-Win 
FUNCTION cvt-hour-to-string RETURNS CHARACTER
  ( INPUT ip-hour AS dec  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time-to-string B-table-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
  (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-board B-table-Win 
FUNCTION display-board RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-color B-table-Win 
FUNCTION display-color RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-die B-table-Win 
FUNCTION display-die RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-due-date B-table-Win 
FUNCTION display-due-date RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-po-no B-table-Win 
FUNCTION display-po-no RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-col-bgcolor B-table-Win 
FUNCTION get-col-bgcolor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-job-bgcolor B-table-Win 
FUNCTION get-job-bgcolor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-go 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-end-date AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-mach AS CHARACTER FORMAT "X(256)":U 
     LABEL "Machine" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-start-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date From" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE v-board AS CHARACTER FORMAT "X(15)":U 
     LABEL "Board" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-color AS CHARACTER FORMAT "X(50)":U 
     LABEL "Color" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-cust-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-die AS CHARACTER FORMAT "X(15)":U 
     LABEL "Die#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-plate AS CHARACTER FORMAT "X(15)":U 
     LABEL "Plate#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-sch, 
      job-mch, 
      job-hdr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-sch.anchored COLUMN-LABEL "Lock"  LABEL-BGCOLOR 14
      tt-sch.seq-no COLUMN-LABEL "Seq#" FORMAT ">>9":U LABEL-BGCOLOR 14       
      job-mch.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U  LABEL-BGCOLOR 14
      job-mch.job-no2 COLUMN-LABEL "" FORMAT ">9":U        LABEL-BGCOLOR 14
      job-mch.frm COLUMN-LABEL "S" FORM ">9" LABEL-BGCOLOR 14
      job-mch.blank-no COLUMN-LABEL "B" FORM ">9" LABEL-BGCOLOR 14
      job-mch.pass COLUMN-LABEL "P" FORM ">9" LABEL-BGCOLOR 14
      tt-sch.cust-no LABEL "Customer" LABEL-BGCOLOR 14
      tt-sch.start-date LABEL "Start Date" FORMAT "99/99/9999":U              LABEL-BGCOLOR 14
  /*    display-due-date() @ lv-due-date COLUMN-LABEL "Due Date"
      display-board() @ lv-board COLUMN-LABEL "Board"
      display-eb() @ lv-die-no COLUMN-LABEL "Die#"
      display-color() @ lv-color COLUMN-LABEL "Colors"
      display-po-no() @ lv-po-no COLUMN-LABEL "PO#"
   */         
      tt-sch.due-date   LABEL "Due Date"  LABEL-BGCOLOR 14
      tt-sch.board LABEL "Board" LABEL-BGCOLOR 14
      tt-sch.die LABEL "Die#"    LABEL-BGCOLOR 14
      tt-sch.plate LABEL "Plate#"   LABEL-BGCOLOR 14
      tt-sch.po-no LABEL "PO#"      LABEL-BGCOLOR 14
      tt-sch.i-no LABEL "FG Item" LABEL-BGCOLOR 14
      tt-sch.style LABEL "Style " LABEL-BGCOLOR 14
      display-cw-dim(lv-corr,tt-sch.len) @ tt-sch.len label "Len" LABEL-BGCOLOR 14
      display-cw-dim(lv-corr,tt-sch.wid) @ tt-sch.wid LABEL "Wid" LABEL-BGCOLOR 14
      display-cw-dim(lv-corr,tt-sch.dep) @ tt-sch.dep LABEL "Dep" LABEL-BGCOLOR 14
      tt-sch.color-list LABEL "Color" LABEL-BGCOLOR 14

      job-mch.start-date-su COLUMN-LABEL "Setup!Start Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14 

      cvt-time-to-string('',job-mch.start-time-su,0.00) @ lv-mr-stime COLUMN-LABEL "Setup!Start Time" LABEL-BGCOLOR 14      
      cvt-time-to-string('END',job-mch.end-time-su,0.00) @ lv-mr-etime COLUMN-LABEL "Setup!End Time" LABEL-BGCOLOR 14

      job-mch.end-date-su COLUMN-LABEL "Setup!End Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14 

      cvt-hour-to-string(job-mch.mr-hr) @ lv-mr-hr COLUMN-LABEL "MR Hour" LABEL-BGCOLOR 14
     /* job-mch.start-date COLUMN-LABEL "Run!Start Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      */
      cvt-time-to-string('',job-mch.start-time,0.00) @ lv-run-stime COLUMN-LABEL "Run!Start Time" LABEL-BGCOLOR 14   
      cvt-time-to-string('END',job-mch.end-time,0.00) @ lv-run-etime COLUMN-LABEL "Run!End Time" LABEL-BGCOLOR 14
      
      job-mch.end-date COLUMN-LABEL "Run!End Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
    
      cvt-hour-to-string(job-mch.run-hr) @ lv-run-hr COLUMN-LABEL "Run Hour" LABEL-BGCOLOR 14
  ENABLE
      tt-sch.anchored
      tt-sch.seq-no
      tt-sch.start-date 
     /* 
      tt-sch.due-date
      tt-sch.board
      tt-sch.die
      tt-sch.plate      
      job-mch.start-date-su
      job-mch.end-date-su
      job-mch.start-date
      job-mch.end-date    */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 13.1
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn-go AT ROW 1 COL 116
     lv-mach AT ROW 1.24 COL 12 COLON-ALIGNED
     lv-start-date AT ROW 1.24 COL 61 COLON-ALIGNED
     lv-end-date AT ROW 1.24 COL 89 COLON-ALIGNED
     v-cust-no AT ROW 2.43 COL 13 COLON-ALIGNED DEBLANK  NO-TAB-STOP 
     v-board AT ROW 2.43 COL 40 COLON-ALIGNED
     v-die AT ROW 2.43 COL 65 COLON-ALIGNED
     v-plate AT ROW 2.43 COL 92 COLON-ALIGNED
     v-color AT ROW 2.43 COL 116 COLON-ALIGNED
     Browser-Table AT ROW 3.62 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: asi.mach
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
         HEIGHT             = 16.14
         WIDTH              = 132.6.
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
/* BROWSE-TAB Browser-Table v-color F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-mach IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-mach:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH tt-sch,
        EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK,
        FIRST job-hdr WHERE job-hdr.company = job-mch.company
                      AND job-hdr.job = job-mch.job NO-LOCK
                      BY tt-sch.anchored DESC BY tt-sch.start-date BY tt-sch.seq-no
                      BY job-mch.job-no BY job-mch.job-no2 BY job-mch.frm BY job-mch.blank-no.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST USED, FIRST USED"
     _OrdList          = "ASI.job.start-date|yes,ASI.job-mch.seq-no|yes"
     _JoinCode[1]      = "ASI.job-mch.company = ASI.mach.company
  AND ASI.job-mch.m-code = ASI.mach.m-code"
     _JoinCode[2]      = "ASI.job-hdr.company = ASI.job-mch.company
  AND ASI.job-hdr.job = ASI.job-mch.job"
     _JoinCode[3]      = "ASI.job.company = ASI.job-hdr.company
  AND ASI.job.job = ASI.job-hdr.job"
     _Where[3]         = "(ASI.job.start-date >= lv-start-date or lv-start-date = ?)"
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state in phandle ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
  /*  lv-col-bgcolor = get-col-bgcolor(). */   

   /* tt-sch.seq-no:COLUMN-BGCOLOR IN BROWSE {&browse-name} = 10.  error */
    RUN set-col-bgcolor.
    FIND style WHERE style.company = g_company AND
                     style.style = tt-sch.style NO-LOCK NO-ERROR.
    lv-corr = IF AVAIL style AND style.industry = "2" THEN YES ELSE NO.
    
    IF lv-corr THEN
         ASSIGN tt-sch.len:FORMAT IN BROWSE {&browse-name} = "->>9.99"
                tt-sch.wid:FORMAT IN BROWSE {&browse-name} = "->>9.99"
                tt-sch.dep:FORMAT IN BROWSE {&browse-name} = "->>9.99"
                v-dim-fmt = "->>9.99".
    ELSE ASSIGN tt-sch.len:FORMAT IN BROWSE {&browse-name} = ">9.99999"
                tt-sch.wid:FORMAT IN BROWSE {&browse-name} = ">9.99999"
                tt-sch.dep:FORMAT IN BROWSE {&browse-name} = ">9.99999"
                v-dim-fmt = ">9.99999".
                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
     {brsleave.i}

         APPLY "value-changed" TO BROWSE {&browse-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
   DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.


  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
  
  /*RUN dispatch ("open-query").*/
  RUN resort-query.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i} 

    FIND job WHERE job.company = job-hdr.company
               AND job.job = job-hdr.job NO-LOCK NO-ERROR.
    IF AVAIL job THEN DO:
      {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
        "(job.rec_key,{methods/headers/job.i})"} 
      {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
        "(CAN-FIND(FIRST notes WHERE notes.rec_key = job.rec_key))"}
      /*{methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
        (CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = job.rec_key))"}     */

    END.
    ELSE DO:
         {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header" "('','')"} 
         {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message" "(no)"}
    END.

     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-go B-table-Win
ON CHOOSE OF btn-go IN FRAME F-Main /* Go */
DO:
    ASSIGN lv-START-date lv-end-date.
    /*{&OPEN-QUERY-{&browse-name}} */
    RUN dispatch ('open-query').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-end-date B-table-Win
ON ANY-KEY OF lv-end-date IN FRAME F-Main /* To */
DO:
   IF KEYLABEL(LASTKEY) = "DEL" THEN do:
      APPLY "?" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-end-date B-table-Win
ON LEAVE OF lv-end-date IN FRAME F-Main /* To */
DO:
   ASSIGN lv-end-date.
   /*APPLY "choose" TO btn-go. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-end-date B-table-Win
ON VALUE-CHANGED OF lv-end-date IN FRAME F-Main /* To */
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 5 THEN DO:
       MESSAGE SELF:SCREEN-VALUE VIEW-AS ALERT-BOX.
      self:SCREEN-VALUE = SELF:SCREEN-VALUE + STRING(YEAR(TODAY)).
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-start-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-start-date B-table-Win
ON ANY-KEY OF lv-start-date IN FRAME F-Main /* Start Date From */
DO:
   IF KEYLABEL(LASTKEY) = "DEL" THEN do:
      APPLY "?" TO SELF.
      RETURN NO-APPLY.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-start-date B-table-Win
ON LEAVE OF lv-start-date IN FRAME F-Main /* Start Date From */
DO:
   ASSIGN lv-start-date.
  /* APPLY "choose" TO btn-go. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-start-date B-table-Win
ON VALUE-CHANGED OF lv-start-date IN FRAME F-Main /* Start Date From */
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 5 THEN DO:
       MESSAGE SELF:SCREEN-VALUE VIEW-AS ALERT-BOX.
      self:SCREEN-VALUE = SELF:SCREEN-VALUE + STRING(YEAR(TODAY)).
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-board B-table-Win
ON LEAVE OF v-board IN FRAME F-Main /* Board */
DO:
   ASSIGN v-board.
 /*  APPLY "choose" TO btn-go. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-color
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-color B-table-Win
ON LEAVE OF v-color IN FRAME F-Main /* Color */
DO:
   ASSIGN v-color.
   /*APPLY "choose" TO btn-go.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-cust-no B-table-Win
ON LEAVE OF v-cust-no IN FRAME F-Main /* Customer */
DO:
   ASSIGN v-cust-no.
  /* APPLY "choose" TO btn-go. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-die
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-die B-table-Win
ON LEAVE OF v-die IN FRAME F-Main /* Die# */
DO:
   ASSIGN v-die.
   /*APPLY "choose" TO btn-go.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-plate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-plate B-table-Win
ON LEAVE OF v-plate IN FRAME F-Main /* Plate# */
DO:
   ASSIGN v-plate.
  /* APPLY "choose" TO btn-go.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* triggers for browser's cell */
ON 'any-key' OF tt-sch.start-date IN BROWSE browser-table
DO:
    IF KEYLABEL(LASTKEY) = "DEL" THEN do:
      APPLY "?" TO SELF.
      RETURN NO-APPLY.
   END.
END.

ON MOUSE-SELECT-CLICK OF tt-sch.anchored IN BROWSE Browser-Table /* Locked? */
DO:   
   IF adm-brs-in-update THEN DO: 
      if SELF:screen-value = "Yes" then self:screen-value = "No" .
      else self:screen-value = "Yes".
      RUN disp-next-seq (INPUT SELF:screen-value = "Yes") .
      return no-apply. 
   END. 
END.

ASSIGN lv-start-date = TODAY
       lv-end-date = TODAY.

/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "mach"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mach"}

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
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR ii AS INT NO-UNDO.
  DEF VAR tmp-color AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.


  FOR EACH tt-color:
    DELETE tt-color.
  END.

  FOR EACH tt-sch.
    DELETE tt-sch.
  END.

  FOR EACH tt-seq.
    DELETE tt-seq.
  END.

  FOR EACH bf-job
      WHERE bf-job.company EQ mach.company
        AND bf-job.opened     EQ YES NO-LOCK,
      FIRST bf-job-hdr
      WHERE bf-job-hdr.company EQ bf-job.company
        AND bf-job-hdr.job     EQ bf-job.job
        AND bf-job-hdr.job-no  EQ bf-job.job-no
        AND bf-job-hdr.job-no2 EQ bf-job.job-no2
        AND (bf-job-hdr.cust-no BEGINS v-cust-no OR v-cust-no = "") NO-LOCK,
      EACH bf-job-mch
      WHERE bf-job-mch.company    EQ bf-job-hdr.company
        AND bf-job-mch.job        EQ bf-job-hdr.job
        AND bf-job-mch.job-no     EQ bf-job-hdr.job-no
        AND bf-job-mch.job-no2    EQ bf-job-hdr.job-no2
        AND bf-job-mch.m-code     EQ mach.m-code
        AND bf-job-mch.run-complete EQ FALSE  /* rdb 02/13/07 12150601 */
        AND bf-job-mch.start-date GE lv-start-date 
        AND bf-job-mch.start-date LE lv-end-date NO-LOCK
      BREAK BY bf-job-mch.start-date 
            BY bf-job-mch.seq-no:

      IF bf-job-mch.mr-complete AND bf-job-mch.run-complete THEN NEXT.

      IF v-board <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-board )
      THEN NEXT.
          
      IF v-color <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-color )
      THEN NEXT.

      IF v-plate <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.code BEGINS v-plate)
      THEN next.     
      IF v-die <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.CODE BEGINS v-die)
      THEN NEXT.
      
      CREATE tt-sch.
      ASSIGN tt-sch.rec-id = RECID(bf-job-mch)
             tt-sch.seq-no =  bf-job-mch.seq-no 
             tt-sch.start-date = bf-job-mch.start-date
             tt-sch.anchored = bf-job-mch.anchored
             .
      CREATE tt-seq.
      ASSIGN tt-seq.rec-id = RECID(bf-job-mch).

      FIND FIRST oe-ord WHERE oe-ord.company EQ bf-job-mch.company
                          AND oe-ord.job-no  EQ bf-job-mch.job-no
                          AND oe-ord.job-no2 EQ bf-job-mch.job-no2 no-lock no-error.
      tt-sch.due-date =     IF AVAIL oe-ord THEN oe-ord.due-date ELSE ?.
      FIND FIRST oe-ordl WHERE oe-ordl.company EQ bf-job-mch.company
                          AND oe-ordl.job-no  EQ bf-job-mch.job-no
                          AND oe-ordl.job-no2 EQ bf-job-mch.job-no2 
                          AND oe-ordl.i-no = bf-job-hdr.i-no  no-lock no-error.
      tt-sch.due-date =     IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?. /* oe-ordl.prom-date */
      IF NOT AVAIL oe-ord AND NOT AVAIL oe-ordl AND tt-sch.due-date = ? 
      THEN tt-sch.due-date = bf-job.due-date.

      find first po-ordl where po-ordl.company eq bf-job-hdr.company            
                           and po-ordl.job-no  eq bf-job-mch.job-no
                           and po-ordl.job-no2 eq bf-job-mch.job-no2
                           and po-ordl.s-num   eq bf-job-mch.frm
                           and po-ordl.i-no    eq bf-job-mch.i-no
                           NO-LOCK NO-ERROR.
      tt-sch.po-no = IF AVAIL po-ordl THEN po-ordl.po-no ELSE 0.

      FIND FIRST ef WHERE ef.company = bf-job-hdr.company
                       AND ef.est-no = bf-job-hdr.est-no
                       AND ef.form-no = bf-job-mch.frm
                       NO-LOCK NO-ERROR.
      tt-sch.board = IF AVAIL ef THEN ef.board ELSE ''.

      FIND FIRST eb WHERE eb.company = bf-job-hdr.company
                  AND eb.est-no = bf-job-hdr.est-no
                  AND eb.form-no = bf-job-mch.frm
                  /*AND eb.blank-no = job-mch.blank-no */
                  NO-LOCK NO-ERROR.
       tt-sch.colour = "".
       tt-sch.color-list = "".
       tmp-color = "".

       IF AVAIL eb THEN do:       
          FOR EACH tt-color:
              DELETE tt-color.
          END.
          DO ii = 1 TO eb.i-col:
             CREATE tt-color.
             ASSIGN tt-color.i-code = eb.i-code[ii].
             IF ii < 11 THEN tt-sch.colour[ii] = eb.i-code[ii]. 
          END.
          ii = 0.
          FOR EACH tt-color BREAK BY tt-color.i-code:
              ii = ii + 1.
              IF first-OF(tt-color.i-code) THEN DO:
                 tmp-color = tmp-color +
                             IF tmp-color <> "" THEN ("," + tt-color.i-code)
                                                ELSE tt-color.i-code.             
              END.
          END.
          ASSIGN tt-sch.style = eb.style
                 tt-sch.len = eb.len
                 tt-sch.wid = eb.wid
                 tt-sch.dep = eb.dep
                 tt-sch.i-no = bf-job-hdr.i-no.
       END.
       tt-sch.die = IF AVAIL eb THEN eb.die-no ELSE "".
       tt-sch.plate = IF AVAIL eb THEN eb.plate-no ELSE "".
       tt-sch.cust-no = IF AVAIL eb THEN eb.cust-no 
                        ELSE IF AVAIL oe-ord THEN oe-ord.cust-no
                        ELSE "".
       tt-sch.color-list = tmp-color.
       lv-start-time = lv-start-time + bf-job-mch.run-hr.
       tt-sch.start-time = bf-job-mch.start-time.

       FIND FIRST job-sch WHERE job-sch.company = mach.company 
                             AND job-sch.m-code = mach.m-code
                             AND job-sch.m-date = tt-sch.start-date
                             AND job-sch.seq = tt-sch.seq NO-ERROR.
       IF NOT AVAIL job-sch THEN CREATE job-sch.
       ASSIGN job-sch.company = mach.company
              job-sch.m-code = mach.m-code
              job-sch.m-date = tt-sch.start-date
              job-sch.job-no = bf-job-mch.job-no
              job-sch.job-no2 = bf-job-mch.job-no2
              job-sch.seq = tt-sch.seq-no.
  END.

  FOR EACH job-sch WHERE job-sch.company = mach.company 
                     AND job-sch.m-code = mach.m-code
                     AND job-sch.m-date = job-sch.m-date:

      FIND FIRST tt-sch  WHERE tt-sch.start-date = job-sch.m-date
                           AND tt-sch.seq = job-sch.seq NO-ERROR.
      IF NOT AVAIL tt-sch THEN delete job-sch.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disp-next-seq B-table-Win 
PROCEDURE disp-next-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ll-lock-seq AS LOG NO-UNDO.

 IF NOT ll-lock-seq THEN DO:
    tt-sch.seq-no:SCREEN-VALUE IN BROWSE {&browse-name} = string("000").
 END.
 ELSE DO:
     DEF BUFFER bx-sch FOR tt-sch.
     DEF VAR lv-last-seq AS INT NO-UNDO.

     FOR EACH bx-sch BY seq-no DESC.
         lv-last-seq = bx-sch.seq-no.
         LEAVE.
     END.
     tt-sch.seq-no:SCREEN-VALUE IN BROWSE {&browse-name} = string(lv-last-seq + 10,">>9").
 END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-board B-table-Win 
PROCEDURE do-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-calc-sch B-table-Win 
PROCEDURE do-calc-sch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-date-wkst AS DATE NO-UNDO.
  DEF VAR lv-date-wkend AS DATE NO-UNDO.
  DEF VAR lv-time-wkst AS INT NO-UNDO.
  DEF VAR lv-time-wkend AS INT NO-UNDO.
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mcode AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-x-start-time AS INT NO-UNDO.
  DEF VAR lv-x-mr-time AS INT NO-UNDO.
  DEF VAR lv-x-run-time AS INT NO-UNDO.

  DEF BUFFER bt-sch FOR tt-sch.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bx-mch FOR job-mch.

  ASSIGN lv-date-wkst = lv-start-date
         lv-date-wkend = lv-end-date.
/*
MESSAGE "Are you sure you want to Re-Sequence?" 
    lv-date-wkst lv-date-wkend
    /*string(lv-time-wkst,"hh:mm") string(lv-time-wkend,"hh:mm")*/
    VIEW-AS ALERT-BOX WARNING
          BUTTON YES-NO UPDATE ll-ans AS LOG.
          
IF ll-ans THEN */
do:
 i = 0.
 SESSION:SET-WAIT-STATE("general").
 FOR EACH tt-reseq:
     DELETE tt-reseq.
 END.
 lv-seq = 1.

 FOR EACH /*bt-sch WHERE bt-sch.start-date >= lv-date-wkst AND
                        bt-sch.start-date <= lv-date-wkend    , */
      bf-mch WHERE RECID(bf-mch) = tt-sch.rec-id AND NOT bf-mch.anchored 
                        BREAK BY tt-sch.start-date  :

      IF FIRST-OF(tt-sch.start-date) THEN do:
         FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                            AND mach-calendar.m-code = mach.m-code
                            AND mach-calendar.m-date = tt-sch.start-date
                            NO-LOCK NO-ERROR.
         IF NOT AVAIL mach-calendar AND tt-sch.start-date <> ? THEN DO:
            MESSAGE "Need Machine Capacity Setup..." VIEW-AS ALERT-BOX ERROR.
            RETURN .
         END.

         ASSIGN lv-seq = 1.
      END.
      CREATE tt-reseq.
      ASSIGN tt-reseq.rec-id = RECID(tt-sch)
             tt-reseq.seq = lv-seq * 10.

      lv-seq = lv-seq + 1.
      
 END.
 
 FOR EACH tt-reseq,
     FIRST bt-sch WHERE /*bt-sch.start-date >= lv-date-wkst AND
                        bt-sch.start-date <= lv-date-wkend   */
                        RECID(bt-sch) = tt-reseq.rec-id ,
      FIRST bf-mch WHERE RECID(bf-mch) = bt-sch.rec-id AND NOT bf-mch.anchored 
                    BREAK BY bt-sch.start-date BY tt-reseq.seq-no   :
      
      IF FIRST-OF(bt-sch.start-date) THEN DO:
         FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                            AND mach-calendar.m-code = mach.m-code
                            AND mach-calendar.m-date = bt-sch.start-date
                            NO-LOCK NO-ERROR.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time
         ELSE 0.
      END.


       lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
       lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

       FIND FIRST job-sch WHERE job-sch.company = mach.company 
                             AND job-sch.m-code = mach.m-code
                             AND job-sch.m-date = bt-sch.start-date
                             AND job-sch.seq = bt-sch.seq NO-ERROR.
       IF AVAIL job-sch THEN delete job-sch.


       ASSIGN bt-sch.seq-no = tt-reseq.seq-no
              bf-mch.seq-no = bt-sch.seq-no
              bf-mch.start-time-su = lv-start-time 
              bt-sch.start-time = lv-start-time + lv-mr-time
              bf-mch.start-time = lv-start-time + lv-mr-time
              bf-mch.start-date-su = bt-sch.start-date               
              .
     
       ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.

     
       ASSIGN bf-mch.end-time = lv-start-time
              bf-mch.end-time-su = lv-start-time - lv-run-time
              bf-mch.end-date = bt-sch.start-date + 
                                 IF bf-mch.end-time < bf-mch.start-time THEN 1 ELSE 0
              bf-mch.end-date-su = bt-sch.start-date +
                                 IF bf-mch.end-time-su < bf-mch.start-time-su THEN 1 ELSE 0
              .

        FIND FIRST job-sch WHERE job-sch.company = mach.company 
                             AND job-sch.m-code = mach.m-code
                             AND job-sch.m-date = bt-sch.start-date
                             AND job-sch.seq = bt-sch.seq NO-ERROR.
        IF NOT AVAIL job-sch THEN CREATE job-sch.

        ASSIGN job-sch.company = mach.company
               job-sch.m-code = mach.m-code
               job-sch.m-date = bt-sch.start-date
               job-sch.seq = bt-sch.seq-no
               job-sch.job-no = bf-job-mch.job-no
               job-sch.job-no2 = bf-job-mch.job-no2.
/*
        MESSAGE "do-calc bf-mch"
        bf-mch.job-no bf-mch.job-no2
              bf-mch.seq-no 
              bf-mch.m-code
              string(bf-mch.start-time-su,"hh:mm")
              string(bf-mch.end-time-su,"hh:mm")
              string(lv-mr-time,"hh:mm")
              string(lv-run-time,"hh:mm")
              string(bf-mch.start-time,"hh:mm")
              string(bf-mch.end-time,"hh:mm")
              VIEW-AS ALERT-BOX.
 */

       /* Resequence other following machines for the job of selected machine */
       lv-x-start-time = lv-start-time.
       FOR EACH bx-mch WHERE bx-mch.company = bf-mch.company
                    AND bx-mch.job-no = bf-mch.job-no
                    AND bx-mch.job-no2 = bf-mch.job-no2
                    AND bx-mch.frm >= bf-mch.frm
                    AND bx-mch.blank-no >= bf-mch.blank-no
                    AND bx-mch.pass >= bf-mch.pass
                    AND NOT bf-mch.anchored
                    AND RECID(bx-mch) <> RECID(bf-mch)
               BREAK BY bx-mch.frm BY bx-mch.blank-no 
                     by bx-mch.pass BY bx-mch.m-code:

           lv-x-mr-time = IF bx-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bx-mch.mr-hr,0) * 3600 +
                   ((bx-mch.mr-hr - truncate(bx-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
           lv-x-run-time = IF bx-mch.run-hr = 0 THEN 0 ELSE
                     truncate(bx-mch.run-hr,0) * 3600 +
                   ((bx-mch.run-hr - truncate(bx-mch.run-hr,0)) * 100 * 60 / 100) * 60.

           ASSIGN bx-mch.start-time-su = lv-x-start-time 
                  bx-mch.start-time = lv-x-start-time + lv-x-mr-time
                  bx-mch.start-date-su = bt-sch.start-date               
                  .

           ASSIGN lv-x-start-time = lv-x-start-time + lv-x-mr-time + lv-x-run-time.

           ASSIGN bx-mch.end-time = lv-x-start-time
                  bx-mch.end-time-su = lv-x-start-time - lv-x-run-time
                  bx-mch.end-date = bt-sch.start-date + 
                             IF bx-mch.end-time < bx-mch.start-time THEN 1 ELSE 0
                  bx-mch.end-date-su = bt-sch.start-date +
                             IF bx-mch.end-time-su < bx-mch.start-time-su THEN 1 ELSE 0
                  .
   
       /* === end of re-scheduling for the following machines of selected job */
       END.
  END.
  /* need here if bt-sch.start-date <> lv-date-wkst/lv-date-wkend */
  /*RUN dispatch ('assign-record'). */

  RUN resort-query. 
END.
/*
IF lv-mcode <> mach.m-code THEN DO:
   DEF VAR char-hdl AS cha NO-UNDO.
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
   RUN dispatch IN WIDGET-HANDLE(char-hdl) ('open-query').

END.
*/
SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-late B-table-Win 
PROCEDURE do-late :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-lock B-table-Win 
PROCEDURE do-lock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


MESSAGE "Are you sure you want to LOCK all jobs?" VIEW-AS ALERT-BOX WARNING
       BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:
   SESSION:SET-WAIT-STATE("general").
   FOR EACH tt-sch:
       tt-sch.anchored = YES.
       FIND FIRST bf-job-mch WHERE RECID(bf-job-mch) = tt-sch.rec-id.
       bf-job-mch.anchored = YES.
   END.
   RUN dispatch ("open-query").
   SESSION:SET-WAIT-STATE("").
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-ontime B-table-Win 
PROCEDURE do-ontime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-overbook B-table-Win 
PROCEDURE do-overbook :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-prep B-table-Win 
PROCEDURE do-prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-print B-table-Win 
PROCEDURE do-print :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN jcsch/r-jobsch.w(mach.m-code,lv-start-date,lv-end-date).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-release B-table-Win 
PROCEDURE do-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


MESSAGE "Are you sure you want to Unlock all jobs?" VIEW-AS ALERT-BOX WARNING
       BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:
   SESSION:SET-WAIT-STATE("general").
   FOR EACH tt-sch:
       tt-sch.anchored = NO.
       tt-sch.seq-no = 0.       
       FIND FIRST bf-job-mch WHERE RECID(bf-job-mch) = tt-sch.rec-id.
       ASSIGN bf-job-mch.anchored = no
              bf-job-mch.seq-no = 0
              bf-job-mch.start-time = 0
              bf-job-mch.end-time = 0
              bf-job-mch.start-time-su = 0
              bf-job-mch.end-time-su = 0.

   END.
   RUN dispatch ("open-query").
   SESSION:SET-WAIT-STATE("").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-schedule B-table-Win 
PROCEDURE do-schedule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-date-wkst AS DATE NO-UNDO.
  DEF VAR lv-date-wkend AS DATE NO-UNDO.
  DEF VAR lv-due-sdate AS DATE NO-UNDO.
  DEF VAR lv-due-edate AS DATE NO-UNDO.
  DEF VAR lv-time-wkst AS INT NO-UNDO.
  DEF VAR lv-time-wkend AS INT NO-UNDO.
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mcode AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-x-start-time AS INT NO-UNDO.
  DEF VAR lv-x-mr-time AS INT NO-UNDO.
  DEF VAR lv-x-run-time AS INT NO-UNDO.
  DEF VAR lv-sch-date AS DATE NO-UNDO.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-x-start-date AS DATE NO-UNDO.
  DEF VAR lv-last-end-date AS DATE NO-UNDO.

  DEF BUFFER bt-sch FOR tt-sch.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bx-mch FOR job-mch.

  ASSIGN lv-date-wkst = lv-start-date
         lv-date-wkend = lv-end-date.

  RUN jcsch/d-jobsch.w (mach.m-code,INPUT-OUTPUT lv-date-wkst, INPUT-OUTPUT lv-date-wkend, OUTPUT lv-time-wkst, OUTPUT lv-time-wkend,OUTPUT lv-mcode,OUTPUT lv-due-sdate,OUTPUT lv-due-edate,OUTPUT lv-sch-date).
  IF lv-time-wkst = 0 AND lv-time-wkend = 0 THEN RETURN.

MESSAGE "Are you sure you want to Re-Sequence?" 
    lv-date-wkst lv-date-wkend
    /*string(lv-time-wkst,"hh:mm") string(lv-time-wkend,"hh:mm")*/
    VIEW-AS ALERT-BOX WARNING
          BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN do:
 i = 0.
 SESSION:SET-WAIT-STATE("general").
 FOR EACH tt-reseq:
     DELETE tt-reseq.
 END.
 lv-seq = 1.
 /*
 IF lv-sort-by = "due-date" THEN
 FOR EACH bt-sch NO-LOCK WHERE bt-sch.start-date >= lv-date-wkst AND
                         bt-sch.start-date <= lv-date-wkend AND
                         bt-sch.due-date >= lv-due-sdate AND
                         bt-sch.due-date <= lv-due-edate  ,
      FIRST bf-mch NO-LOCK WHERE RECID(bf-mch) = bt-sch.rec-id AND NOT bf-mch.anchored 
                          BREAK BY bt-sch.due-date :
                         

      IF FIRST-OF(bt-sch.start-date) THEN do:
         ASSIGN lv-seq = 1.
      END.
      CREATE tt-reseq.
      ASSIGN tt-reseq.rec-id = RECID(bt-sch)
             tt-reseq.seq = lv-seq * 10.
      lv-seq = lv-seq + 1.
 END.
 else */
 

 FOR EACH bt-sch WHERE bt-sch.start-date >= lv-date-wkst AND
                         bt-sch.start-date <= lv-date-wkend AND
                         bt-sch.due-date >= lv-due-sdate AND
                         bt-sch.due-date <= lv-due-edate  ,
      FIRST bf-mch NO-LOCK WHERE RECID(bf-mch) = bt-sch.rec-id AND NOT bf-mch.anchored 
                           BREAK BY bt-sch.start-date :
     
      IF FIRST-OF(bt-sch.start-date) THEN do:

         /*========= check any prior machine starts later */
         FIND FIRST bx-mch WHERE bx-mch.company = bf-mch.company
                              AND bx-mch.job = bf-mch.job
                              AND bx-mch.job-no = bf-mch.job-no
                              AND bx-mch.job-no2 = bf-mch.job-no2
                              AND bx-mch.frm = bf-mch.frm
                              AND bx-mch.LINE < bf-mch.line
                              AND bx-mch.m-code <> bf-mch.m-code
                              AND (bx-mch.start-date > lv-sch-date OR
                                  (bx-mch.start-date = lv-sch-date AND
                                   bx-mch.end-time > lv-time-wkst))
                   /*      AND bx-mch.blank-no >= bf-mch.blank-no
                     AND bx-mch.pass >= bf-mch.pass
                     AND NOT bf-mch.anchored
                     AND RECID(bx-mch) <> RECID(bf-mch)
                     BREAK BY bx-mch.frm BY bx-mch.blank-no 
                     by bx-mch.pass :*/
                     USE-INDEX line-idx NO-LOCK NO-ERROR.
         IF AVAIL bx-mch THEN DO:
            lv-sch-date = bx-mch.end-date.
            lv-time-wkst = bx-mch.end-time.
            lv-time-wkend = 86400.
            MESSAGE "There is Prior Machine ending later date/time - " bx-mch.start-date " "
                STRING(bx-mch.end-time,"hh:mm")  "." SKIP
                "Schedule Start Date should be later than " bx-mch.start-date VIEW-AS ALERT-BOX.
         END.
          /*==============*/
         ASSIGN lv-seq = 1.
      END.
      IF bt-sch.start-date = ? THEN bt-sch.start-date = lv-sch-date.
      CREATE tt-reseq.
      ASSIGN tt-reseq.rec-id = RECID(bt-sch)
             tt-reseq.seq = lv-seq * 10.
      lv-seq = lv-seq + 1.
 END.
 lv-start-date = lv-sch-date.
 FOR EACH tt-reseq,
     FIRST bt-sch WHERE /*bt-sch.start-date >= lv-date-wkst AND
                        bt-sch.start-date <= lv-date-wkend   */
                        RECID(bt-sch) = tt-reseq.rec-id ,
      FIRST bf-mch WHERE RECID(bf-mch) = bt-sch.rec-id AND NOT bf-mch.anchored 
                    BREAK BY bt-sch.start-date BY tt-reseq.seq-no   :
      
      IF FIRST-OF(bt-sch.start-date) THEN DO:
         FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                            AND mach-calendar.m-code = mach.m-code
                            AND mach-calendar.m-date = bt-sch.start-date
                            NO-LOCK NO-ERROR.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time
         ELSE 0.
      END.


       lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
       lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

       FIND FIRST job-sch WHERE job-sch.company = mach.company 
                             AND job-sch.m-code = mach.m-code
                             AND job-sch.m-date = bt-sch.start-date
                             AND job-sch.seq = bt-sch.seq NO-ERROR.
       IF AVAIL job-sch THEN delete job-sch.


       ASSIGN bt-sch.seq-no = tt-reseq.seq-no
              bf-mch.seq-no = bt-sch.seq-no
              bf-mch.start-time-su = lv-start-time 
              bf-mch.end-time-su = lv-start-time + lv-mr-time
              bt-sch.start-time = lv-start-time + lv-mr-time
              bf-mch.start-time = lv-start-time + lv-mr-time
              bf-mch.start-date-su = lv-start-date .              
       
/*IF bf-mch.start-date = ? THEN bf-mch.start-date = lv-start-date            .*/

       ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
       IF lv-start-time > 86400 THEN lv-start-time = lv-start-time - 86400.

       ASSIGN bf-mch.end-time = lv-start-time
              bf-mch.end-date-su = bf-mch.start-date-su +
                                   IF bf-mch.end-time-su < bf-mch.start-time-su THEN 1 ELSE 0
              bf-mch.start-date = bf-mch.end-date-su
              bf-mch.end-date = bf-mch.start-date + 
                                 IF (bf-mch.end-time < bf-mch.start-time OR
                                     bf-mch.end-time-su < bf-mch.start-time-su)
                                 THEN 1 ELSE 0
              
              lv-start-date = MAXIMUM(bf-mch.end-date-su,bf-mch.end-date).

        FIND FIRST job-sch WHERE job-sch.company = mach.company 
                             AND job-sch.m-code = mach.m-code
                             AND job-sch.m-date = bt-sch.start-date
                             AND job-sch.seq = bt-sch.seq NO-ERROR.
        IF NOT AVAIL job-sch THEN CREATE job-sch.

        ASSIGN job-sch.company = mach.company
               job-sch.m-code = mach.m-code
               job-sch.m-date = bt-sch.start-date
               job-sch.seq = bt-sch.seq-no
               job-sch.job-no = bf-job-mch.job-no
               job-sch.job-no2 = bf-job-mch.job-no2.     

       /* Resequence other following machines for the job of selected machine */
       lv-x-start-time = lv-start-time.
       lv-x-start-date = lv-start-date.
  
       FOR EACH bx-mch USE-INDEX line-idx WHERE bx-mch.company = bf-mch.company
                    AND bx-mch.job = bf-mch.job   
                    AND bx-mch.job-no = bf-mch.job-no
                    AND bx-mch.job-no2 = bf-mch.job-no2
                    AND (bx-mch.frm > bf-mch.frm OR
                         (bx-mch.frm = bf-mch.frm AND bx-mch.LINE > bf-mch.LINE)) 
                    /*AND bx-mch.frm >= bf-mch.frm
                    AND bx-mch.blank-no >= bf-mch.blank-no
                    AND bx-mch.pass >= bf-mch.pass */
                    AND NOT bf-mch.anchored
                    AND RECID(bx-mch) <> RECID(bf-mch)
             /*  BREAK BY bx-mch.frm BY bx-mch.blank-no 
                     by bx-mch.pass BY bx-mch.m-code*/ BY bx-mch.job BY bx-mch.LINE :

           lv-x-mr-time = IF bx-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bx-mch.mr-hr,0) * 3600 +
                   ((bx-mch.mr-hr - truncate(bx-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
           lv-x-run-time = IF bx-mch.run-hr = 0 THEN 0 ELSE
                     truncate(bx-mch.run-hr,0) * 3600 +
                   ((bx-mch.run-hr - truncate(bx-mch.run-hr,0)) * 100 * 60 / 100) * 60.

           ASSIGN bx-mch.start-time-su = lv-x-start-time 
                  bx-mch.start-time = lv-x-start-time + lv-x-mr-time
                  bx-mch.end-time-su = lv-x-start-time + lv-x-mr-time
                  bx-mch.start-date-su = lv-x-start-date               
                  bx-mch.start-date = lv-x-start-date               
                  .

           ASSIGN lv-x-start-time = lv-x-start-time + lv-x-mr-time + lv-x-run-time.
           IF lv-x-start-time >= 86400 THEN lv-x-start-time = lv-x-start-time - 86400.

           ASSIGN bx-mch.end-time = lv-x-start-time
                  bx-mch.end-date = bx-mch.start-date + 
                             IF (bx-mch.end-time < bx-mch.start-time or
                                 bx-mch.end-time-su < bx-mch.start-time-su) THEN 1 ELSE 0
                  bx-mch.end-date-su = bx-mch.start-date-su +
                             IF bx-mch.end-time-su < bx-mch.start-time-su THEN 1 ELSE 0
                  lv-x-start-date = MAXIMUM(bx-mch.end-date,bx-mch.end-date-su).         
/*MESSAGE "resche for other machine for the job:"  bx-mch.m-code bx-mch.frm bx-mch.LINE
         bx-mch.start-date-su bx-mch.end-date
         bx-mch.start-time-su bx-mch.end-time VIEW-AS ALERT-BOX. */
       END.
       
       /* === end of re-scheduling for the following machines of selected job */

 END. /* each tt-reseq */
  /* need here if bt-sch.start-date <> lv-date-wkst/lv-date-wkend */
  /*RUN dispatch ('assign-record'). */
  
  /* update due-date*/
 FOR EACH tt-reseq,
     FIRST bt-sch WHERE /*bt-sch.start-date >= lv-date-wkst AND
                        bt-sch.start-date <= lv-date-wkend   */
                        RECID(bt-sch) = tt-reseq.rec-id ,
      FIRST bf-mch NO-LOCK WHERE RECID(bf-mch) = bt-sch.rec-id AND NOT bf-mch.anchored 
                       BREAK BY bf-mch.job-no BY bf-mch.job-no2   :
      IF FIRST-OF(bf-mch.job-no2) THEN DO:
         FIND FIRST bf-job WHERE bf-job.company EQ bf-mch.company
                    AND bf-job.job     EQ bf-mch.job
                    AND bf-job.job-no  EQ bf-mch.job-no
                    AND bf-job.job-no2 EQ bf-mch.job-no2 NO-ERROR.
         IF AVAIL bf-job-hdr AND bf-job-hdr.due-date = ? THEN DO:
             FOR EACH bx-mch NO-LOCK WHERE bx-mch.company = bf-mch.company
                      AND bx-mch.job-no = bf-mch.job-no
                      AND bx-mch.job-no2 = bf-mch.job-no2
                      BREAK BY bx-mch.frm BY bx-mch.blank-no by bx-mch.pass :
                 lv-last-end-date = bx-mch.end-date. 
             END.
             bf-job.due-date = lv-last-end-date + 1 .   
             bt-sch.due-date = bf-job.due-date.
         END.
      END.
   
 END.
END. /* ll-ans */


FOR EACH tt-reseq,
     FIRST bt-sch WHERE /*bt-sch.start-date >= lv-date-wkst AND
                        bt-sch.start-date <= lv-date-wkend   */
                        RECID(bt-sch) = tt-reseq.rec-id ,
      FIRST bf-mch WHERE RECID(bf-mch) = bt-sch.rec-id AND NOT bf-mch.anchored :
    bt-sch.start-date = bf-mch.start-date.
    RUN resort-query.
END.
IF lv-mcode <> mach.m-code THEN DO:
   DEF VAR char-hdl AS cha NO-UNDO.
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
   RUN dispatch IN WIDGET-HANDLE(char-hdl) ('open-query').
END.


SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-sort B-table-Win 
PROCEDURE do-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bt-sch FOR tt-sch.
 DEF BUFFER bf-jmch FOR job-mch.
 DEF VAR lv-date-wkst AS DATE NO-UNDO.
  DEF VAR lv-date-wkend AS DATE NO-UNDO.
  DEF VAR lv-time-wkst AS INT NO-UNDO.
  DEF VAR lv-time-wkend AS INT NO-UNDO.
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR ll-ans AS LOG NO-UNDO.

/*
 MESSAGE "Are you sure you want to Reset Sequence?" skip(1)
       "This will number th jobs exactly in the order shown on the scheduling board." SKIP
       "You can sort the planning board by clicking on any yellow field. Click again to reverse the sort"
     VIEW-AS ALERT-BOX WARNING
        BUTTON YES-NO UPDATE ll-ans AS LOG. */
  RUN jcsch/d-reseq.w (OUTPUT ll-ans).
 IF ll-ans THEN do:
     RUN resort-query.
     /* assign values from screen */
 FOR EACH bt-sch,
     FIRST bf-job-mch WHERE RECID(bf-job-mch) = bt-sch.rec-id
                        AND NOT bf-job-mch.anchored,
     FIRST tt-seq WHERE tt-seq.rec-id = bt-sch.REC-ID
              BREAK BY bt-sch.start-date BY tt-seq.seq:
  /*
      FIND FIRST job-sch WHERE job-sch.company = mach.company 
                           AND job-sch.m-code = mach.m-code
                           AND job-sch.m-date = bt-sch.start-date
                           AND job-sch.seq = tt-seq.seq  
                           NO-ERROR.
      IF AVAIL job-sch THEN DELETE job-sch.
*/

      IF FIRST-OF(bt-sch.start-date) THEN DO:
         FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                            AND mach-calendar.m-code = mach.m-code
                            AND mach-calendar.m-date = bt-sch.start-date
                            NO-LOCK NO-ERROR.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.         
         
      END.

      FIND FIRST job-sch WHERE job-sch.company = mach.company 
                           AND job-sch.m-code = mach.m-code
                           AND job-sch.m-date = bt-sch.start-date
                           AND job-sch.seq = tt-seq.seq  
                           NO-ERROR.
      IF NOT AVAIL job-sch THEN CREATE job-sch.
      ASSIGN job-sch.company = mach.company
             job-sch.m-code = mach.m-code
             job-sch.m-date = bt-sch.start-date
             job-sch.seq = tt-seq.seq /*bt-sch.seq-no*/
             job-sch.job-no = bf-job-mch.job-no
             job-sch.job-no2 = bf-job-mch.job-no2.

      /* check anchored start and end time */
      find first bf-jmch use-index start-seq WHERE bf-jmch.company = mach.company
                             and bf-jmch.m-code = mach.m-code
                             and bf-jmch.start-date = bt-sch.start-date                             
                             and bf-jmch.start-time < lv-start-time 
                             and bf-jmch.end-time > lv-start-time
                             AND bf-jmch.anchored
                             no-lock no-error.
        if avail bf-jmch then repeat:
           lv-start-time = bf-jmch.end-time.
           find next bf-jmch use-index start-seq WHERE bf-jmch.company = mach.company
                             and bf-jmch.m-code = mach.m-code
                             and bf-jmch.start-date = bt-sch.start-date                             
                             and bf-jmch.start-time < lv-start-time 
                             and bf-jmch.end-time > lv-start-time
                             AND bf-jmch.anchored
                             no-lock no-error.
           if not avail bf-jmch then leave.                  
                            
        end.
      
      
      ASSIGN bf-job-mch.start-date = bt-sch.start-date
             bf-job-mch.start-time = bt-sch.start-time
             bf-job-mch.seq-no = job-sch.seq
             bt-sch.seq-no = job-sch.seq.

      /*
      FIND FIRST mch-act WHERE mch-act.company = mach.company
                           AND mch-act.m-code = mach.m-code  NO-ERROR.
      lv-time-wkst = IF AVAIL mch-act THEN mch-act.START ELSE 0.
      */

      
       lv-mr-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-job-mch.mr-hr,0) * 3600 +
                    ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
       lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-job-mch.run-hr,0) * 3600 +
                    ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.

       ASSIGN bt-sch.seq = tt-seq.seq /*lv-seq * 10*/
              bf-job-mch.seq-no = bt-sch.seq
              bf-job-mch.start-time-su = lv-start-time 
              bt-sch.start-time = lv-start-time + lv-mr-time
              bf-job-mch.start-time = lv-start-time + lv-mr-time
              bf-job-mch.start-date-su = bt-sch.start-date               
              .
 
       ASSIGN lv-seq = lv-seq + 1
              lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
       ASSIGN bf-job-mch.end-time = lv-start-time
              bf-job-mch.end-time-su = lv-start-time - lv-run-time
              bf-job-mch.end-date = bt-sch.start-date + 
                                 IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
              bf-job-mch.end-date-su = bt-sch.start-date +
                                 IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0
              .

  END.

  RUN resort-query.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-browse-col-hd B-table-Win 
PROCEDURE get-browse-col-hd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF VAR lv-col-num AS INT NO-UNDO.
     
     ASSIGN lv-brs-col-hd = ?
            lv-brs-col-hd-list = ""
            lv-col-num = 0.

     
     lv-brs-col-hd = BROWSE {&browse-name}:FIRST-COLUMN.

     DO WHILE VALID-HANDLE(lv-brs-col-hd):
        lv-col-num = lv-col-num + 1.
        IF lv-col-num = 1 OR lv-col-num = 3 THEN
           lv-brs-col-hd-list = lv-brs-col-hd-list + 
                             IF lv-brs-col-hd-list <> "" THEN "," + STRING(lv-brs-col-hd)
                             ELSE STRING(lv-brs-col-hd).
  /*
MESSAGE lv-brs-col-hd-list SKIP
        string(lv-brs-col-hd) lv-brs-col-hd lv-brs-col-hd:LABEL VIEW-AS ALERT-BOX.
    */
        
        lv-brs-col-hd = lv-brs-col-hd:NEXT-COLUMN.

     END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry B-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* not to browser's row get focused */
  
  IF ll-enabled THEN do:
      APPLY "entry" TO tt-sch.anchored IN BROWSE {&BROWSE-name} .
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
  ASSIGN lv-old-seq = tt-sch.seq
         lv-prev-st-date = tt-sch.start-date.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND bf-job-mch WHERE RECID(bf-job-mch) = tt-sch.rec-id.
  ASSIGN bf-job-mch.seq = tt-sch.seq
         bf-job-mch.anchored = tt-sch.anchored
         .


  IF tt-sch.start-date <> lv-prev-st-date THEN do:
      /* reset same as not scheduled yet */
     ASSIGN bf-job-mch.start-date = tt-sch.start-date
            bf-job-mch.start-date-su = tt-sch.start-date
            bf-job-mch.end-date = bf-job-mch.start-date 
            bf-job-mch.end-date-su = bf-job-mch.start-date-su
            bf-job-mch.seq-no = 0
            bf-job-mch.anchored = NO
            bf-job-mch.start-time = 0
            bf-job-mch.end-time = 0
            bf-job-mch.start-time-su = 0
            bf-job-mch.end-time-su  = 0
            .

     RUN reschedule-job-mch. /* reschedule for the job */
     RUN reschedule-stdate. /* reschedule for the machine and late seq */


  END.

  ELSE IF tt-sch.seq <> lv-old-seq THEN RUN override-seq.

  RUN do-calc-sch .
 
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

  IF lv-first-time THEN do:
     BROWSE {&browse-name}:DESELECT-focused-ROW() NO-ERROR.
     lv-first-time = NO.
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
  ll-enabled = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO tt-sch.anchored IN BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-browse-col-hd.
  APPLY 'ENTRY' TO LV-START-date IN FRAME {&FRAME-NAME}.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
     RUN GET-ATTRIBUTE IN WIDGET-HANDLE(char-hdl) ('display-machine').
     IF RETURN-VALUE = "YES" THEN DO:
        lv-mach = mach.m-code.
        lv-mach:HIDDEN = NO.
        DISPLAY lv-mach WITH FRAME {&FRAME-NAME}.
     END.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN lv-mr-tot = 0
         lv-run-tot = 0.

  RUN build-table.
  lv-first-time = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "value-changed" TO BROWSE {&browse-name}.

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
  DO WITH FRAME {&FRAME-NAME}:
     IF date(tt-sch.start-date:SCREEN-VALUE IN BROWSE {&browse-name})  < TODAY THEN DO:
        MESSAGE "Start Date can not be earlier than today..." VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO tt-sch.start-date.
        RETURN ERROR.
     END.
    
     IF CAN-FIND(FIRST bf-job-mch WHERE bf-job-mch.company = mach.company                           
                           AND bf-job-mch.job = job-mch.job
                           AND bf-job-mch.job-no = job-mch.job-no 
                           AND bf-job-mch.job-no2 = job-mch.job-no2
                           AND bf-job-mch.m-code < job-mch.m-code
                           AND bf-job-mch.frm <= job-mch.frm
                           AND bf-job-mch.blank-no <= job-mch.blank-no
                           AND bf-job-mch.start-date > date(tt-sch.start-date:SCREEN-VALUE))
     THEN DO:                           
          MESSAGE "Invalid Start Date. Make sure Start Date is appropriate for JOB and MACHINE."
                  VIEW-AS ALERT-BOX ERROR.
          APPLY 'entry' TO tt-sch.start-date.
          RETURN error.
     END.
     IF tt-sch.anchored:SCREEN-VALUE = "YES" AND INT(tt-sch.seq-no:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Assign Sequence Before get Anchored." VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO tt-sch.anchored.
        RETURN error.
     END.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-enabled  = NO.
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
  RUN end-update IN WIDGET-HANDLE(char-hdl).





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE override-seq B-table-Win 
PROCEDURE override-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-tsch FOR tt-sch.
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF VAR lv-date-wkst AS DATE NO-UNDO.
  DEF VAR lv-date-wkend AS DATE NO-UNDO.
  DEF VAR lv-time-wkst AS INT NO-UNDO.
  DEF VAR lv-time-wkend AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF BUFFER bf-tmp-sch FOR tt-sch.
  DEF BUFFER bf2-job-mch FOR job-mch.
  DEF BUFFER bf-tmp-jmch FOR job-mch.
  DEF VAR lv-seq-anchored AS LOG NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  lv-seq = 10.
  FIND LAST bf-tmp-sch USE-INDEX tsch WHERE bf-tmp-sch.seq-no < tt-sch.seq 
                                        AND bf-tmp-sch.seq-no <> 0
                                        AND recid(bf-tmp-sch) <> recid(tt-sch) NO-LOCK NO-ERROR.

  IF NOT AVAIL bf-tmp-sch THEN DO:  
     FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                               AND mach-calendar.m-code = mach.m-code 
                               AND mach-calendar.m-date = lv-start-date
                               NO-LOCK NO-ERROR.
             lv-time-wkst = IF AVAIL mach-calendar THEN mach-calendar.START-time ELSE 0.      
             
  END.
  ELSE DO:
      FIND bf2-job-mch WHERE RECID(bf2-job-mch) = bf-tmp-sch.rec-id.
      lv-time-wkst = bf2-job-mch.end-time.
      lv-seq = bf-tmp-sch.seq + 10.
  END.

  IF lv-old-seq > tt-sch.seq THEN DO:
      /* re-sequence */
      FOR EACH bf-tsch WHERE bf-tsch.seq >= tt-sch.seq AND bf-tsch.seq <= lv-old-seq
                        /* AND RECID(bf-tsch) <> RECID(tt-sch) */ ,
          FIRST bf-job-mch WHERE RECID(bf-job-mch) = bf-tsch.rec-id 
                           /*  AND NOT bf-job-mch.anchored changed*/
                          AND bf-job-mch.seq-no > 0
                       BREAK BY bf-tsch.start-date BY bf-tsch.seq DESC.

          lv-seq = IF RECID(bf-tsch) <> RECID(tt-sch) THEN bf-job-mch.seq-no + 10
                   ELSE bf-job-mch.seq-no.
          bf-tsch.seq = lv-seq .
          /*
          FIND FIRST job-sch WHERE job-sch.company = mach.company
                               AND job-sch.m-code = mach.m-code
                               AND job-sch.m-date = bf-tsch.start-date
                               AND job-sch.seq = lv-old-seq /*bf-tsch.seq */ NO-ERROR.          
          IF AVAIL JOB-SCH THEN       job-sch.seq = bf-tsch.seq.
           ??? */
          ASSIGN bf-job-mch.start-date = bf-tsch.start-date              
                 bf-job-mch.seq-no = bf-tsch.seq.
          

         lv-seq = lv-seq + 10.      
      END.

      FOR EACH bf-tsch WHERE bf-tsch.seq >= tt-sch.seq AND bf-tsch.seq <= lv-old-seq
                        /* AND RECID(bf-tsch) <> RECID(tt-sch) */ ,
          FIRST bf-job-mch WHERE RECID(bf-job-mch) = bf-tsch.rec-id 
                           /*  AND NOT bf-job-mch.anchored changed*/
                          AND bf-job-mch.seq-no > 0
                       BREAK BY bf-tsch.start-date BY bf-tsch.seq .
/*
          IF FIRST-OF(bf-tsch.start-date) AND NOT first(bf-tsch.start-date)
                         THEN ASSIGN lv-seq = 10.
*/
          IF FIRST-OF(bf-tsch.start-date) THEN ASSIGN lv-start-time = lv-time-wkst
                                                      .        
/*
          lv-seq-anchored = YES.
          DO WHILE NOT lv-seq-anchored:
             FIND FIRST bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                               /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                                AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                                AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                                AND bf-tmp-jmch.start-date = bf-tsch.start-date
                                AND bf-tmp-jmch.seq-no = lv-seq 
                                AND bf-tmp-jmch.anchored NO-LOCK NO-ERROR. 
              IF AVAIL bf-tmp-jmch THEN lv-seq = lv-seq + 10.
              ELSE lv-seq-anchored = NO.
          END.
 */

          ASSIGN bf-job-mch.start-date = bf-tsch.start-date              
                 .
          
          lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bf-job-mch.mr-hr,0) * 3600 +
                    ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
          lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                       truncate(bf-job-mch.run-hr,0) * 3600 +
                    ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.
       
          ASSIGN bf-job-mch.start-time-su = lv-start-time 
              bf-job-mch.end-time-su = lv-start-time + lv-mr-time
              bf-job-mch.start-time = lv-start-time + lv-mr-time
              .
 
          ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
          ASSIGN bf-job-mch.end-time = lv-start-time
                 bf-job-mch.end-date = bf-tsch.start-date + 
                                 IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
                 bf-job-mch.end-date-su = bf-tsch.start-date +
                                 IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0
                 .
      END.



  END.  /* seq# lowered */
  ELSE IF lv-old-seq < tt-sch.seq THEN DO:  /* seq# highered */
      IF lv-old-seq = 0 THEN DO: /* new locked */
         lv-start-time = lv-time-wkst.
         FIND FIRST bf-job-mch WHERE RECID(bf-job-mch) = tt-sch.rec-id .
         lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bf-job-mch.mr-hr,0) * 3600 +
                    ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
         lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                       truncate(bf-job-mch.run-hr,0) * 3600 +
                    ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.

         ASSIGN bf-job-mch.start-time-su = lv-start-time 
                bf-job-mch.start-time = lv-start-time + lv-mr-time
                .
 
       ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
       ASSIGN bf-job-mch.end-time = lv-start-time
              bf-job-mch.end-time-su = lv-start-time - lv-run-time
              bf-job-mch.end-date = tt-sch.start-date + 
                                 IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
              bf-job-mch.end-date-su = tt-sch.start-date +
                                 IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0              
              .
      DISPLAY cvt-time-to-string('',bf-job-mch.start-time-su,0.00) @ lv-mr-stime 
              cvt-time-to-string('END',bf-job-mch.end-time-su,0.00) @ lv-mr-etime
              cvt-hour-to-string(bf-job-mch.mr-hr) @ lv-mr-hr 
              cvt-time-to-string('',bf-job-mch.start-time,0.00) @ lv-run-stime 
              cvt-time-to-string('END',bf-job-mch.end-time,0.00) @ lv-run-etime
              cvt-hour-to-string(bf-job-mch.run-hr) @ lv-run-hr 
          WITH BROWSE {&browse-name}.
       RETURN.
      END.
      

      /* re-sequence */
      FOR EACH bf-tsch WHERE bf-tsch.seq >= tt-sch.seq 
                        /* AND RECID(bf-tsch) <> RECID(tt-sch) */ ,
          FIRST bf-job-mch WHERE RECID(bf-job-mch) = bf-tsch.rec-id 
                           /*  AND NOT bf-job-mch.anchored changed*/
                          AND bf-job-mch.seq-no > 0
                       BREAK BY bf-tsch.start-date BY bf-tsch.seq DESC.

          lv-seq = IF RECID(bf-tsch) <> RECID(tt-sch) THEN bf-job-mch.seq-no + 10
                   ELSE bf-job-mch.seq-no.
          bf-tsch.seq = lv-seq .
          /*
          FIND FIRST job-sch WHERE job-sch.company = mach.company
                               AND job-sch.m-code = mach.m-code
                               AND job-sch.m-date = bf-tsch.start-date
                               AND job-sch.seq = lv-old-seq /*bf-tsch.seq */ NO-ERROR.          
          IF AVAIL JOB-SCH THEN       job-sch.seq = bf-tsch.seq.
           ??? */
          ASSIGN bf-job-mch.start-date = bf-tsch.start-date              
                 bf-job-mch.seq-no = bf-tsch.seq.
          
         lv-seq = lv-seq + 10.      
      END.


      FOR EACH bf-tsch WHERE bf-tsch.seq >= tt-sch.seq 
                         /*AND RECID(bf-tsch) <> RECID(tt-sch)*/,
          FIRST bf-job-mch WHERE RECID(bf-job-mch) = bf-tsch.rec-id 
                           /*  AND NOT bf-job-mch.anchored*/
                      BREAK BY bf-tsch.start-date BY bf-tsch.seq.
      /*
         IF FIRST-OF(bf-tsch.start-date) AND NOT first(bf-tsch.start-date)
                         THEN ASSIGN lv-seq = 10.
      */                   
/*
         IF tt-sch.seq = 10 THEN DO:
            FIND FIRST mch-act WHERE mch-act.company = mach.company
                              AND mch-act.m-code = mach.m-code  NO-ERROR.
            lv-time-wkst = IF AVAIL mch-act THEN mch-act.START ELSE 0.                  
         END.
         ELSE DO:
            FIND LAST bf-tmp-sch USE-INDEX tsch WHERE bf-tmp-sch.seq-no <= tt-sch.seq
                                                      AND RECID(bf-tmp-sch) <> RECID(tt-sch).
            FIND bf2-job-mch WHERE RECID(bf2-job-mch) = bf-tmp-sch.rec-id.
            lv-time-wkst = bf2-job-mch.end-time.
            lv-seq = bf-tmp-sch.seq.
         END.
*/         
         IF FIRST-OF(bf-tsch.start-date) THEN ASSIGN lv-start-time = lv-time-wkst.
/* changed
         lv-seq-anchored = YES.
         DO WHILE NOT lv-seq-anchored:
            FIND FIRST bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                              /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                               AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                               AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                               AND bf-tmp-jmch.start-date = bf-tsch.start-date
                               AND bf-tmp-jmch.seq-no = lv-seq 
                               AND bf-tmp-jmch.anchored NO-LOCK NO-ERROR. 
             IF AVAIL bf-tmp-jmch THEN lv-seq = lv-seq + 10.
             ELSE lv-seq-anchored = NO.
         END.
*/
         bf-tsch.seq = lv-seq.         
         /*
         FIND FIRST job-sch WHERE job-sch.company = mach.company
                               AND job-sch.m-code = mach.m-code
                               AND job-sch.m-date = bf-tsch.start-date
                               AND job-sch.seq = bf-tsch.seq  NO-ERROR.
           
         IF AVAIL job-sch THEN job-sch.seq = bf-tsch.seq.
         */
         ASSIGN bf-job-mch.start-date = bf-tsch.start-date              
                bf-job-mch.seq-no = bf-tsch.seq
                .

         lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
                    truncate(bf-job-mch.mr-hr,0) * 3600 +
                   ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
         lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-job-mch.run-hr,0) * 3600 +
                   ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.

      ASSIGN bf-job-mch.start-time-su = lv-start-time 
             bf-tsch.start-time = lv-start-time + lv-mr-time
             bf-job-mch.start-time = lv-start-time + lv-mr-time
             bf-job-mch.start-date-su = bf-tsch.start-date               
             .

      ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
      ASSIGN bf-job-mch.end-time = lv-start-time
             bf-job-mch.end-time-su = lv-start-time - lv-run-time
             bf-job-mch.end-date = bf-tsch.start-date + 
                                IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
             bf-job-mch.end-date-su = bf-tsch.start-date +
                                IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0
             .

     END. /* for each */

  END.  /* seq# highered */
  RUN dispatch ('open-query').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reschedule-job-mch B-table-Win 
PROCEDURE reschedule-job-mch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-tmp-jmch FOR job-mch.
DEF VAR lv-seq AS INT NO-UNDO.
DEF var lv-seq-anchored AS LOG NO-UNDO.
DEF VAR lv-date-wkst AS DATE NO-UNDO.
DEF VAR lv-date-wkend AS DATE NO-UNDO.
DEF VAR lv-time-wkst AS INT NO-UNDO.
DEF VAR lv-time-wkend AS INT NO-UNDO.
DEF VAR lv-start-time AS INT NO-UNDO.
DEF VAR lv-mr-time AS INT NO-UNDO.
DEF VAR lv-run-time AS INT NO-UNDO.
DEF VAR lv-mcode AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.

lv-seq = lv-old-seq.

/*===== reschedule job-mch for previous date =======*/
FOR EACH bf-job-mch USE-INDEX line-idx 
         WHERE bf-job-mch.company = ASI.mach.company
                      AND bf-job-mch.job-no = job-mch.job-no
                      AND bf-job-mch.job-no2 = job-mch.job-no2
                      AND bf-job-mch.LINE >= job-mch.LINE
                      AND NOT bf-job-mch.anchored 
                      ,
      FIRST bf-job-hdr WHERE bf-job-hdr.company = bf-job-mch.company
                          AND bf-job-hdr.job = bf-job-mch.job 
                          AND (bf-job-hdr.cust-no BEGINS v-cust-no OR v-cust-no = "")                          
      NO-LOCK,
      FIRST bf-job WHERE bf-job.company = bf-job-hdr.company
                         AND bf-job.job = bf-job-hdr.job NO-LOCK      
               BREAK BY bf-job-mch.start-date 
                    /* BY bf-job-mch.seq-no */:
/*
      IF v-board <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-board )
      THEN NEXT.
          
      IF v-color <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-color )
      THEN NEXT.

      IF v-plate <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.code BEGINS v-plate)
      THEN next.     
      IF v-die <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.CODE BEGINS v-die)
      THEN NEXT.         
 */
      IF FIRST-OF(bf-job-mch.start-date) THEN DO:
         FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                      AND mach-calendar.m-code = mach.m-code
                      AND mach-calendar.m-date = lv-prev-st-date
                      NO-LOCK NO-ERROR.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
         
         FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                                     /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                                     AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                                     AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                                     AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                                     AND RECID(bf-tmp-jmch) <> RECID(bf-job-mch)
                                     /*AND bf-tmp-jmch.seq-no < lv-old-seq */
                                     NO-LOCK BY bf-tmp-jmch.start-time DESC:
             lv-start-time = bf-tmp-jmch.end-time.
             LEAVE.
         END.
         
      END.
      
      lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bf-job-mch.mr-hr,0) * 3600 +
                             ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
      lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                truncate(bf-job-mch.run-hr,0) * 3600 +
              ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.

      lv-seq-anchored = YES.
      DO WHILE NOT lv-seq-anchored:
         FIND FIRST bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                               /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                                AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                                AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                                AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                                AND bf-tmp-jmch.seq-no = lv-seq 
                                AND bf-tmp-jmch.anchored NO-LOCK NO-ERROR. 
         IF AVAIL bf-tmp-jmch THEN lv-seq = lv-seq + 10.
         ELSE lv-seq-anchored = NO.
      END.
       
      ASSIGN bf-job-mch.seq-no = lv-seq 
             bf-job-mch.start-time-su = lv-start-time 
             bf-job-mch.start-time = lv-start-time + lv-mr-time
             .

      ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
      IF lv-start-time >= 86400 THEN lv-start-time = lv-start-time - 86400.

      ASSIGN  bf-job-mch.end-time = lv-start-time
              bf-job-mch.end-time-su = lv-start-time - lv-run-time
              bf-job-mch.start-date-su = bf-job-mch.start-date -
                                         IF bf-job-mch.start-time < bf-job-mch.end-time-su THEN 1 ELSE 0
              bf-job-mch.end-date = bf-job-mch.start-date + 
                                    IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
              bf-job-mch.end-date-su = bf-job-mch.start-date-su +
                                    IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0
              .
      lv-seq = lv-seq + 10.
/*
      MESSAGE "resche job-mch"
              bf-job-mch.seq-no 
              bf-job-mch.m-code
              bf-job-mch.LINE job-mch.LINE
              string(bf-job-mch.start-time-su,"hh:mm")
              string(bf-job-mch.end-time-su,"hh:mm")
              string(lv-mr-time,"hh:mm")
              string(lv-run-time,"hh:mm")
              string(bf-job-mch.start-time,"hh:mm")
              string(bf-job-mch.end-time,"hh:mm")
              VIEW-AS ALERT-BOX.
  */    
      

  END.

  /* reset seq, time for the job */
  FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = job-mch.company
                         AND bf-tmp-jmch.job = job-mch.job
                         AND bf-tmp-jmch.job-no =  job-mch.job-no
                         AND bf-tmp-jmch.job-no2 =  job-mch.job-no2
                         AND bf-tmp-jmch.LINE > job-mch.LINE
                        /* AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                         AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                         AND bf-tmp-jmch.seq-no = lv-seq 
                         AND bf-tmp-jmch.anchored*/
                      /*
                         AND bf-tmp-jmch.frm >= job-mch.frm
                         AND bf-tmp-jmch.blank-no >= job-mch.blank-no
                         AND bf-tmp-jmch.m-code > job-mch.m-code */
                         :
      ASSIGN bf-tmp-jmch.start-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time-su
             bf-tmp-jmch.end-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time-su
             bf-tmp-jmch.start-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time
             bf-tmp-jmch.end-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time           
             bf-tmp-jmch.seq-no = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.seq-no
             bf-tmp-jmch.start-date = tt-sch.start-date
             bf-tmp-jmch.start-date-su = tt-sch.start-date
             bf-tmp-jmch.end-date =    tt-sch.start-date
             bf-tmp-jmch.end-date-su = tt-sch.start-date
                 .
      FIND FIRST job-hdr WHERE job-hdr.company = bf-tmp-jmch.company
                           AND job-hdr.job = bf-tmp-jmch.job
                           AND job-hdr.job-no = bf-tmp-jmch.job-no
                           AND job-hdr.job-no2 = bf-tmp-jmch.job-no2.
                           
      job-hdr.start-date = tt-sch.start-date.
      FIND FIRST job OF bf-tmp-jmch.
      job.start-date = tt-sch.start-date.

  END.

  /* reset job.start-date and job-hdr.start-date from job-mch's the earlist start-date */
  FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = job-mch.company
                         AND bf-tmp-jmch.job = job-mch.job
                         AND bf-tmp-jmch.job-no =  job-mch.job-no
                         AND bf-tmp-jmch.job-no2 =  job-mch.job-no2 NO-LOCK
                         BY bf-tmp-jmch.start-date:
      FIND FIRST job-hdr WHERE job-hdr.company = bf-tmp-jmch.company
                           AND job-hdr.job = bf-tmp-jmch.job
                           AND job-hdr.job-no = bf-tmp-jmch.job-no
                           AND job-hdr.job-no2 = bf-tmp-jmch.job-no2.
                           
      job-hdr.start-date = tt-sch.start-date.
      FIND FIRST job OF bf-tmp-jmch.
      job.start-date = tt-sch.start-date.
      LEAVE.
  END.

  IF lv-prev-st-date >= lv-start-date AND lv-prev-st-date <= lv-end-date THEN
     RUN dispatch ('open-query').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reschedule-stdate B-table-Win 
PROCEDURE reschedule-stdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-tmp-jmch FOR job-mch.
DEF VAR lv-seq AS INT NO-UNDO.
DEF var lv-seq-anchored AS LOG NO-UNDO.
DEF VAR lv-date-wkst AS DATE NO-UNDO.
DEF VAR lv-date-wkend AS DATE NO-UNDO.
DEF VAR lv-time-wkst AS INT NO-UNDO.
DEF VAR lv-time-wkend AS INT NO-UNDO.
DEF VAR lv-start-time AS INT NO-UNDO.
DEF VAR lv-mr-time AS INT NO-UNDO.
DEF VAR lv-run-time AS INT NO-UNDO.
DEF VAR lv-mcode AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.

lv-seq = lv-old-seq.

/*===== reschedule job-mch for previous date =======*/
FOR EACH bf-job-mch WHERE bf-job-mch.company = ASI.mach.company
                      AND bf-job-mch.m-code = ASI.mach.m-code
                      AND bf-job-mch.start-date = lv-prev-st-date 
                      AND NOT bf-job-mch.anchored 
                      AND bf-job-mch.seq-no > lv-old-seq 
                       ,
      FIRST bf-job-hdr WHERE bf-job-hdr.company = bf-job-mch.company
                          AND bf-job-hdr.job = bf-job-mch.job 
                          AND (bf-job-hdr.cust-no BEGINS v-cust-no OR v-cust-no = "")                          
      NO-LOCK,
      FIRST bf-job WHERE bf-job.company = bf-job-hdr.company
                         AND bf-job.job = bf-job-hdr.job
                         /*or lv-start-date = ?*/ 
                         NO-LOCK      
               BREAK BY bf-job-mch.start-date 
                     BY bf-job-mch.seq-no:

      IF v-board <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-board )
      THEN NEXT.
          
      IF v-color <> "" AND NOT can-find(FIRST job-mat WHERE job-mat.company = mach.company
                                         AND job-mat.job-no = bf-job-hdr.job-no
                                         AND job-mat.job-no2 = bf-job-hdr.job-no2  
                                         AND job-mat.i-no BEGINS v-color )
      THEN NEXT.

      IF v-plate <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.code BEGINS v-plate)
      THEN next.     
      IF v-die <> "" AND NOT can-find(FIRST job-prep WHERE job-prep.company = mach.company
                                         AND job-prep.job-no = bf-job-hdr.job-no
                                         AND job-prep.job-no2 = bf-job-hdr.job-no2  
                                         AND job-prep.CODE BEGINS v-die)
      THEN NEXT.         

      IF FIRST-OF(bf-job-mch.start-date) THEN DO:
         FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                      AND mach-calendar.m-code = mach.m-code
                      AND mach-calendar.m-date = lv-prev-st-date
                      NO-LOCK NO-ERROR.
         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.

         FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                                     /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                                     AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                                     AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                                     AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                                     AND bf-tmp-jmch.seq-no < lv-old-seq 
                                     NO-LOCK BY bf-tmp-jmch.start-time DESC:
             lv-start-time = bf-tmp-jmch.end-time.
             LEAVE.
         END.
      END.
      
      lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bf-job-mch.mr-hr,0) * 3600 +
                             ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
      lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
                truncate(bf-job-mch.run-hr,0) * 3600 +
              ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.

      lv-seq-anchored = YES.
      DO WHILE NOT lv-seq-anchored:
         FIND FIRST bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                               /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                                AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                                AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                                AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                                AND bf-tmp-jmch.seq-no = lv-seq 
                                AND bf-tmp-jmch.anchored NO-LOCK NO-ERROR. 
         IF AVAIL bf-tmp-jmch THEN lv-seq = lv-seq + 10.
         ELSE lv-seq-anchored = NO.
      END.
       
      ASSIGN bf-job-mch.seq-no = lv-seq 
             bf-job-mch.start-time-su = lv-start-time 
             bf-job-mch.start-time = lv-start-time + lv-mr-time
             .

      ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.
      IF lv-start-time >= 86400 THEN lv-start-time = lv-start-time - 86400.

      ASSIGN  bf-job-mch.start-date-su = bf-job-mch.start-date
              bf-job-mch.end-time = lv-start-time
              bf-job-mch.end-time-su = lv-start-time - lv-run-time
              bf-job-mch.end-date = bf-job-mch.start-date + 
                                    IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
              bf-job-mch.end-date-su = bf-job-mch.start-date-su +
                                    IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0
              .
      lv-seq = lv-seq + 10.

  END.

  /* reset seq, time for the job */
  FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = job-mch.company
                         AND bf-tmp-jmch.job = job-mch.job
                         AND bf-tmp-jmch.job-no =  job-mch.job-no
                         AND bf-tmp-jmch.job-no2 =  job-mch.job-no2
                         AND bf-tmp-jmch.LINE > job-mch.LINE
                        /* AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                         AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                         AND bf-tmp-jmch.seq-no = lv-seq 
                         AND bf-tmp-jmch.anchored*/
                      /*
                         AND bf-tmp-jmch.frm >= job-mch.frm
                         AND bf-tmp-jmch.blank-no >= job-mch.blank-no
                         AND bf-tmp-jmch.m-code > job-mch.m-code */
                         :
      ASSIGN bf-tmp-jmch.start-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time-su
             bf-tmp-jmch.end-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time-su
             bf-tmp-jmch.start-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time
             bf-tmp-jmch.end-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time           
             bf-tmp-jmch.seq-no = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.seq-no
             bf-tmp-jmch.start-date = tt-sch.start-date
             bf-tmp-jmch.start-date-su = tt-sch.start-date
             bf-tmp-jmch.end-date =    tt-sch.start-date
             bf-tmp-jmch.end-date-su = tt-sch.start-date
                 .
      FIND FIRST job-hdr WHERE job-hdr.company = bf-tmp-jmch.company
                           AND job-hdr.job = bf-tmp-jmch.job
                           AND job-hdr.job-no = bf-tmp-jmch.job-no
                           AND job-hdr.job-no2 = bf-tmp-jmch.job-no2.
                           
      job-hdr.start-date = tt-sch.start-date.
      FIND FIRST job OF bf-tmp-jmch.
      job.start-date = tt-sch.start-date.

  END.
  /* reset job.start-date and job-hdr.start-date from job-mch's the earlist start-date */
  FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = job-mch.company
                         AND bf-tmp-jmch.job = job-mch.job
                         AND bf-tmp-jmch.job-no =  job-mch.job-no
                         AND bf-tmp-jmch.job-no2 =  job-mch.job-no2 NO-LOCK
                         BY bf-tmp-jmch.start-date:
      FIND FIRST job-hdr WHERE job-hdr.company = bf-tmp-jmch.company
                           AND job-hdr.job = bf-tmp-jmch.job
                           AND job-hdr.job-no = bf-tmp-jmch.job-no
                           AND job-hdr.job-no2 = bf-tmp-jmch.job-no2.
                           
      job-hdr.start-date = tt-sch.start-date.
      FIND FIRST job OF bf-tmp-jmch.
      job.start-date = tt-sch.start-date.
      LEAVE.
  END.

  IF lv-prev-st-date >= lv-start-date AND lv-prev-st-date <= lv-end-date THEN
     RUN dispatch ('open-query').

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
DEF VAR li-tmp-seq AS INT NO-UNDO.
DEF BUFFER bf-mch FOR job-mch.
DEF BUFFER bf-jhdr FOR job-hdr.
DEF BUFFER bf-tsch FOR tt-sch.
DEF VAR v-bf-tsch-rowid AS ROWID NO-UNDO.


IF ll-sort-asc THEN DO:
    /*&SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              USE-INDEX cust NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

   */

    OPEN QUERY {&Browse-NAME}
    FOR EACH tt-sch ,
        EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK,
        FIRST job-hdr WHERE job-hdr.company = job-mch.company
                      AND job-hdr.job = job-mch.job NO-LOCK 
                      BY tt-sch.anchored DESC
                     /* {&sortby}  */
                      {&sortby-phrase-asc}
                       BY tt-sch.seq-no 
                      .

     li-tmp-seq = 0.
     FOR EACH bf-tsch,
         EACH bf-mch WHERE RECID(bf-mch) = bf-tsch.rec-id NO-LOCK,
         FIRST bf-jhdr WHERE bf-jhdr.company = bf-mch.company
                      AND bf-jhdr.job = bf-mch.job NO-LOCK
                     BREAK BY tt-sch.anchored DESC
                      /*{&sortby2}*/
                      {&sortby-phrase-asc2}
                       BY bf-tsch.seq-no
                      :
       /*  IF first-of(bf-tsch.start-date) THEN li-tmp-seq = 0.*/
         li-tmp-seq = li-tmp-seq + 1.
         FIND FIRST tt-seq WHERE tt-seq.rec-id = bf-tsch.rec-id.
         tt-seq.seq = li-tmp-seq * 10.
    END.
   
END.
ELSE do:
    OPEN QUERY {&Browse-NAME}
    FOR EACH tt-sch,
        EACH job-mch WHERE RECID(job-mch) = tt-sch.rec-id NO-LOCK,
        FIRST job-hdr WHERE job-hdr.company = job-mch.company
                      AND job-hdr.job = job-mch.job NO-LOCK     
                      BY tt-sch.anchored DESC
                       /*{&sortby}*/
                       {&sortby-phrase-desc}
                       BY tt-sch.seq-no  .
    
     li-tmp-seq = 0.
     FOR EACH bf-tsch,
        EACH bf-mch WHERE RECID(bf-mch) = bf-tsch.rec-id NO-LOCK,
        FIRST bf-jhdr WHERE bf-jhdr.company = bf-mch.company
                      AND  bf-jhdr.job = bf-mch.job NO-LOCK                  
                      BREAK BY tt-sch.anchored DESC
                      /*{&sortby2}*/
                     {&sortby-phrase-desc2}
                      BY bf-tsch.seq-no
                      :
         /*IF first-of(bf-tsch.start-date) THEN li-tmp-seq = 0.*/
         li-tmp-seq = li-tmp-seq + 1.
         FIND FIRST tt-seq WHERE tt-seq.rec-id = bf-tsch.rec-id.
         tt-seq.seq = li-tmp-seq * 10.
     END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-schedule B-table-Win 
PROCEDURE run-schedule :
/*------------------------------------------------------------------------------
  Purpose:   reschedure for ip-sch-date
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT param ip-sch-date AS DATE NO-UNDO.

  DEF VAR lv-date-wkend AS DATE NO-UNDO.
  DEF VAR lv-time-wkst AS INT NO-UNDO.
  DEF VAR lv-time-wkend AS INT NO-UNDO.
  DEF VAR lv-seq AS INT NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mcode AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  DEF BUFFER bt-sch FOR tt-sch.
  DEF BUFFER bf-mch FOR job-mch.

  SESSION:SET-WAIT-STATE("general").

  FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                             AND mach-calendar.m-code = mach.m-code
                             AND mach-calendar.m-date = lv-prev-st-date
                             NO-LOCK NO-ERROR.
  lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.

  
 i = 0.
 
 FOR EACH tt-reseq:
     DELETE tt-reseq.
 END.

 lv-seq = 1.
 
 FOR EACH bf-mch WHERE bf-mch.company = mach.company
                   AND bf-mch.m-code = mach.m-code
                   AND bf-mch.start-date = ip-sch-date 
                   AND NOT bf-mch.anchored 
                       :


      lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                     truncate(bf-mch.mr-hr,0) * 3600 +
                   ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
      lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                     truncate(bf-mch.run-hr,0) * 3600 +
                   ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.


      FIND FIRST job-sch WHERE job-sch.company = mach.company 
                            AND job-sch.m-code = mach.m-code
                            AND job-sch.m-date = ip-sch-date
                            AND job-sch.seq = lv-seq * 10 NO-ERROR.
      IF AVAIL job-sch THEN delete job-sch.


      ASSIGN bf-mch.seq-no = lv-seq * 10
             bf-mch.start-time-su = lv-start-time 
             bf-mch.start-time = lv-start-time + lv-mr-time
             bf-mch.start-date-su = ip-sch-date               
             .

      ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.


      ASSIGN bf-mch.end-time = lv-start-time
             bf-mch.end-time-su = lv-start-time - lv-run-time
             bf-mch.end-date = ip-sch-date + 
                                IF bf-mch.end-time < bf-mch.start-time THEN 1 ELSE 0
             bf-mch.end-date-su = ip-sch-date +
                                IF bf-mch.end-time-su < bf-mch.start-time-su THEN 1 ELSE 0
             .

       FIND FIRST job-sch WHERE job-sch.company = mach.company 
                            AND job-sch.m-code = mach.m-code
                            AND job-sch.m-date = ip-sch-date
                            AND job-sch.seq = bf-mch.seq-no NO-ERROR.
       IF NOT AVAIL job-sch THEN CREATE job-sch.

       ASSIGN job-sch.company = mach.company
              job-sch.m-code = mach.m-code
              job-sch.m-date = ip-sch-date
              job-sch.seq = bf-mch.seq-no
              job-sch.job-no = bf-mch.job-no
              job-sch.job-no2 = bf-mch.job-no2.

      
      lv-seq = lv-seq + 1.
 END.
/*
 RUN resort-query.
*/


SESSION:SET-WAIT-STATE("").

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
  {src/adm/template/snd-list.i "mach"}
  {src/adm/template/snd-list.i "tt-sch"}
  {src/adm/template/snd-list.i "job-mch"}
  {src/adm/template/snd-list.i "job-hdr"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-col-bgcolor B-table-Win 
PROCEDURE set-col-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF VAR lv-new-bgcolor AS INT NO-UNDO.
     DEF VAR i AS INT NO-UNDO.
     DEF VAR lv-new-col-hd AS WIDGET-HANDLE NO-UNDO.
     DEF VAR lv-new-col-hd-list AS cha NO-UNDO.
     DEF VAR li-num-of-col AS INT NO-UNDO.
     DEF VAR lv-job-bgcolor AS INT NO-UNDO.

     lv-new-bgcolor = get-col-bgcolor().
     lv-job-bgcolor = get-job-bgcolor().

     li-num-of-col = NUM-ENTRIES(lv-brs-col-hd-list).  /* list is from get-browse-col-hd */

     DO i = 1 TO NUM-ENTRIES(lv-brs-col-hd-list):
        lv-new-col-hd = WIDGET-HANDLE(ENTRY(i,lv-brs-col-hd-list)).
        IF i = 1 THEN lv-new-col-hd:bgcolor = lv-new-bgcolor NO-ERROR.     
        ELSE IF i = 2 THEN lv-new-col-hd:BGCOLOR = lv-job-bgcolor NO-ERROR.
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-hour-to-string B-table-Win 
FUNCTION cvt-hour-to-string RETURNS CHARACTER
  ( INPUT ip-hour AS dec  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   RETURN STRING( int(ip-hour * 3600),"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-time-to-string B-table-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
  (INPUT ip-type AS CHAR, INPUT ip-stime AS INT, INPUT ip-hour AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF ip-type = "END" THEN DO:
     DEF VAR li-end-time AS INT NO-UNDO.
     li-end-time = ip-stime + ip-hour * 3600.
     RETURN STRING(li-end-time,"HH:MM").
  END.
  ELSE
  RETURN STRING(ip-stime,"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-board B-table-Win 
FUNCTION display-board RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST ef WHERE ef.company = job-hdr.company
                  AND ef.est-no = job-hdr.est-no
                  AND ef.form-no = job-mch.frm
                  NO-LOCK NO-ERROR.

  IF AVAIL ef THEN RETURN ef.board.
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-color B-table-Win 
FUNCTION display-color RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND FIRST eb WHERE eb.company = job-hdr.company
                  AND eb.est-no = job-hdr.est-no
                  AND eb.form-no = job-mch.frm
                  /*AND eb.blank-no = job-mch.blank-no */
                  NO-LOCK NO-ERROR.
  IF AVAIL eb THEN do:
     DEF VAR ii AS INT NO-UNDO.
     DEF VAR tmp-color AS cha NO-UNDO.
     DO ii = 1 TO eb.i-col:
        tmp-color = tmp-color + IF tmp-color <> "" THEN ("," + eb.i-code[ii] )
                                ELSE eb.i-code[ii].
     END.
     RETURN tmp-color.
  END.
  ELSE RETURN "".   /* Function return value. */

   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-cw-dim B-table-Win 
FUNCTION display-cw-dim RETURNS DECIMAL
  ( input ip-is-corr-style as log, input  ip-dim as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var out-dim as dec no-undo.
  
  
  if ip-is-corr-style and ip-dim <> 0 then 
     /*round(trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC),2)   sys/inc/k16.i */
     out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
  else out-dim = ip-dim.

  RETURN out-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-die B-table-Win 
FUNCTION display-die RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND FIRST eb WHERE eb.company = job-hdr.company
                  AND eb.est-no = job-hdr.est-no
                  AND eb.form-no = job-mch.frm
                  /*AND eb.blank-no = job-mch.blank-no */
                  NO-LOCK NO-ERROR.
  IF AVAIL eb THEN RETURN eb.die-no.
  ELSE RETURN "".   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-due-date B-table-Win 
FUNCTION display-due-date RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST oe-ord WHERE oe-ord.company EQ job-mch.company
          AND oe-ord.job-no  EQ job-mch.job-no
          AND oe-ord.job-no2 EQ job-mch.job-no2 no-lock no-error.
   IF AVAIL oe-ord THEN   RETURN oe-ord.due-date.
   ELSE RETURN ?.
   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-po-no B-table-Win 
FUNCTION display-po-no RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   find first po-ordl
          where po-ordl.company eq job-hdr.company            
            and po-ordl.job-no  eq job-mch.job-no
            and po-ordl.job-no2 eq job-mch.job-no2
            and po-ordl.s-num   eq job-mch.frm
            and po-ordl.i-no    eq job-mch.i-no
            NO-LOCK NO-ERROR.
 


  IF AVAIL po-ordl THEN RETURN po-ordl.po-no.
  ELSE RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-col-bgcolor B-table-Win 
FUNCTION get-col-bgcolor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lt-mr-time AS INT NO-UNDO.
  DEF VAR lt-run-time AS INT NO-UNDO.
  DEF VAR lv-tmp-date AS DATE NO-UNDO.
  DEF VAR v-tot-rm AS INT NO-UNDO.
  DEF VAR ll-no-board AS LOG NO-UNDO.

  lv-tmp-date = IF AVAIL tt-sch THEN tt-sch.start-date ELSE lv-start-date.
 
  FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                             AND mach-calendar.m-code = mach.m-code
                             AND mach-calendar.m-date = lv-tmp-date NO-LOCK NO-ERROR.
  
  lv-col-bgcolor = 10.                  /* Green */
  IF NOT AVAIL tt-sch THEN lv-col-bgcolor = 10.  
  
  /* ===doen for job-no column
  IF NOT AVAIL tt-sch THEN lv-col-bgcolor = 10.  
  ELSE IF tt-sch.due-date < tt-sch.start-date OR tt-sch.due-date < TODAY 
          THEN lv-col-bgcolor = 12. /* RED*/ 
  */

  /*
  ELSE do:

      ASSIGN lt-mr-time = IF job-mch.run-hr = 0 THEN 0 ELSE
                      truncate(job-mch.mr-hr,0) * 3600 +
                    ((job-mch.mr-hr - truncate(job-mch.mr-hr,0)) * 100 * 60 / 100) * 60
             lt-run-time = IF job-mch.run-hr = 0 THEN 0 ELSE
                      truncate(job-mch.run-hr,0) * 3600 +
                    ((job-mch.run-hr - truncate(job-mch.run-hr,0)) * 100 * 60 / 100) * 60.
      lv-mr-tot = lv-mr-tot + lt-mr-time.
      lv-run-tot = lv-run-tot + lt-run-time.
             
      IF (lv-run-tot + lv-mr-tot) > 86400 THEN  lv-col-bgcolor = 6. /* gold gray : more than 24 hours a day*/ 

  END.
  */

    /*=== blue color for waiting for board */
  ll-no-board = YES.
  for each job-mat
        where job-mat.company eq g_company
          and job-mat.job     eq job-mch.job
          and job-mat.job-no  eq job-mch.job-no
          and job-mat.job-no2 eq job-mch.job-no2
          and job-mat.frm     eq job-mch.frm
        no-lock,
        first item
        where item.company    eq g_company
          and item.i-no       eq job-mat.i-no
          and item.mat-type   eq "B"
        no-lock:

      FIND first mat-act
          where mat-act.company eq g_company
            and mat-act.job     eq job-mch.job
            and mat-act.job-no  eq job-mch.job-no
            and mat-act.job-no2 eq job-mch.job-no2
            and mat-act.i-no    eq job-mat.i-no
            and mat-act.s-num   eq job-mat.frm
            and mat-act.b-num   eq job-mat.blank-no
          use-index job NO-LOCK NO-ERROR.

      IF AVAIL mat-act THEN DO:  
         ll-no-board = NO.
         LEAVE.
      END.
    END.
    IF ll-no-board THEN lv-col-bgcolor = 20 /* light purple */. /* blue - 9 */
  /*=== end logic for blue color */
  /* need for pink w/f prep */

    ELSE IF (AVAIL mach-calendar AND job-mch.end-time > mach-calendar.end-time )
           OR job-mch.end-time < job-mch.start-time
         THEN lv-col-bgcolor = 14. /* YELLOW */

    RETURN lv-col-bgcolor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-job-bgcolor B-table-Win 
FUNCTION get-job-bgcolor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR lt-mr-time AS INT NO-UNDO.
  DEF VAR lt-run-time AS INT NO-UNDO.
  DEF VAR lv-tmp-date AS DATE NO-UNDO.
  DEF VAR v-tot-rm AS INT NO-UNDO.
  DEF VAR ll-no-board AS LOG NO-UNDO.

  lv-tmp-date = IF AVAIL tt-sch THEN tt-sch.start-date ELSE lv-start-date.
 
  FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                             AND mach-calendar.m-code = mach.m-code
                             AND mach-calendar.m-date = lv-tmp-date NO-LOCK NO-ERROR.
  lv-col-bgcolor = 10.                  /* Green */


  IF NOT AVAIL tt-sch THEN lv-col-bgcolor = 10.  
  ELSE IF tt-sch.due-date < tt-sch.start-date OR tt-sch.due-date < TODAY 
          THEN lv-col-bgcolor = 12. /* RED*/ 
/*
  ELSE IF (AVAIL mach-calendar AND job-mch.end-time > mach-calendar.end-time )
           OR job-mch.end-time < job-mch.start-time
      THEN lv-col-bgcolor = 14. /* YELLOW */

*/

  RETURN lv-col-bgcolor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

