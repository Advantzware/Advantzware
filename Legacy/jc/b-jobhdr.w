&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  jc\b-jobhdr.w

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
{sys/inc/VAR.i "new shared" }
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO. /* for jc/jc-calc.p */
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF NEW SHARED VAR nufile AS LOG NO-UNDO.
DEF VAR lv-recalced AS LOG NO-UNDO.
DEF VAR v-job-rec-key AS CHAR NO-UNDO.
DEF VAR v-job-header AS CHAR NO-UNDO.
DEF VAR lv-qty-changed AS LOG NO-UNDO.
DEF BUFFER b-job-hdr FOR job-hdr.
DEF VAR OEJobHold-log AS LOG  NO-UNDO.
DEF VAR lcReturn      AS CHAR NO-UNDO.
DEF VAR llRecFound    AS LOG  NO-UNDO.
DEFINE VARIABLE ld-total-cost AS DECIMAL NO-UNDO.
RUN sys/ref/nk1look.p (g_company, "OEJobHold", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

IF llRecFound THEN
    OEJobHold-log = LOGICAL(lcReturn) NO-ERROR.  
    
&SCOPED-DEFINE sortby-phrase BY job-hdr.frm BY job-hdr.blank-no

/* don't send job-hdr.rec_key to notes. send job.rec_key instead */
&SCOPED-DEFINE SETVALUE NO

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-hdr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table job-hdr.frm job-hdr.blank-no ~
job-hdr.cust-no job-hdr.i-no job-hdr.qty job-hdr.sq-in job-hdr.ord-no ~
job-hdr.po-no job-hdr.due-date job-hdr.std-mat-cost job-hdr.std-lab-cost ~
job-hdr.std-fix-cost job-hdr.std-var-cost get-total-cost () @ ld-total-cost job-hdr.j-no job-hdr.job-no ~
job-hdr.job-no2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table job-hdr.frm ~
job-hdr.blank-no job-hdr.cust-no job-hdr.i-no job-hdr.qty job-hdr.ord-no ~
job-hdr.po-no job-hdr.due-date 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table job-hdr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table job-hdr
&Scoped-define QUERY-STRING-Browser-Table FOR EACH job-hdr OF job WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH job-hdr OF job WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table job-hdr


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


 &ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-total-cost B-table-Win 
FUNCTION get-total-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 9.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      job-hdr
    FIELDS(job-hdr.frm
      job-hdr.blank-no
      job-hdr.cust-no
      job-hdr.i-no
      job-hdr.qty
      job-hdr.sq-in
      job-hdr.ord-no
      job-hdr.po-no
      job-hdr.due-date
      job-hdr.std-mat-cost
      job-hdr.std-lab-cost
      job-hdr.std-fix-cost
      job-hdr.std-var-cost
      job-hdr.j-no
      job-hdr.job-no
      job-hdr.job-no2) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      job-hdr.frm COLUMN-LABEL "S" FORMAT ">>>":U WIDTH 4
      job-hdr.blank-no COLUMN-LABEL "B" FORMAT ">>>":U WIDTH 4
      job-hdr.cust-no FORMAT "x(8)":U
      job-hdr.i-no FORMAT "x(15)":U
      job-hdr.qty FORMAT ">>>,>>>,>>9":U
      job-hdr.sq-in FORMAT ">>9.99":U
      job-hdr.ord-no FORMAT ">>>>>9":U WIDTH 9
      job-hdr.po-no FORMAT "x(15)":U
      job-hdr.due-date FORMAT "99/99/9999":U WIDTH 15
      job-hdr.std-mat-cost COLUMN-LABEL "Material" FORMAT "->>>,>>9.99<<":U
      job-hdr.std-lab-cost COLUMN-LABEL "D.L." FORMAT "->>>,>>9.99<<":U
      job-hdr.std-fix-cost COLUMN-LABEL "Fix OH" FORMAT "->>>,>>9.99<<":U
      job-hdr.std-var-cost COLUMN-LABEL "Var OH" FORMAT "->>>,>>9.99<<":U
      get-total-cost () @ ld-total-cost COLUMN-LABEL "Total Cost" FORMAT "->,>>>,>>>,>>9.99<<":U
      job-hdr.j-no COLUMN-LABEL "" FORMAT ">>>>>>9":U
      job-hdr.job-no COLUMN-LABEL "" FORMAT "x(6)":U
      job-hdr.job-no2 COLUMN-LABEL "" FORMAT ">9":U
  ENABLE
      job-hdr.frm
      job-hdr.blank-no
      job-hdr.cust-no
      job-hdr.i-no
      job-hdr.qty
      job-hdr.ord-no
      job-hdr.po-no
      job-hdr.due-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 7.62
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1.24 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 9.1 COL 8 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 9.1 COL 74 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 9.1 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 9.1 COL 3
     RECT-4 AT ROW 8.86 COL 1
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.job
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
         HEIGHT             = 19.33
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4.

ASSIGN 
       job-hdr.j-no:VISIBLE IN BROWSE Browser-Table = FALSE
       job-hdr.job-no:VISIBLE IN BROWSE Browser-Table = FALSE
       job-hdr.job-no2:VISIBLE IN BROWSE Browser-Table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.job-hdr OF ASI.job"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, FIRST OUTER"
     _FldNameList[1]   > ASI.job-hdr.frm
"job-hdr.frm" "S" ">>>" "integer" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-hdr.blank-no
"job-hdr.blank-no" "B" ">>>" "integer" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-hdr.cust-no
"job-hdr.cust-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.job-hdr.i-no
"job-hdr.i-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.job-hdr.qty
"job-hdr.qty" ? ">>>,>>>,>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = ASI.job-hdr.sq-in
     _FldNameList[7]   > ASI.job-hdr.ord-no
"job-hdr.ord-no" ? ? "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job-hdr.po-no
"job-hdr.po-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.job-hdr.due-date
"job-hdr.due-date" ? ? "date" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.job-hdr.std-mat-cost
"job-hdr.std-mat-cost" "Material" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.job-hdr.std-lab-cost
"job-hdr.std-lab-cost" "D.L." ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.job-hdr.std-fix-cost
"job-hdr.std-fix-cost" "Fix OH" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.job-hdr.std-var-cost
"job-hdr.std-var-cost" "Var OH" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.job-hdr.j-no
"get-total-cost () @ ld-total-cost" "Total Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"  
 "job-hdr.j-no" "" ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.job-hdr.job-no
"job-hdr.job-no" "" ? "character" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.job-hdr.job-no2
"job-hdr.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEF VAR phandle AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-hdl AS cha NO-UNDO.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state IN phandle ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR look-recid AS RECID NO-UNDO.
    DEF VAR lv-fg-list AS CHAR NO-UNDO.


    CASE FOCUS:NAME:
      WHEN "ord-no" THEN DO:
        lv-fg-list = job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}.
        IF NOT CAN-FIND(FIRST b-job-hdr
                        WHERE b-job-hdr.company EQ job.company
                          AND b-job-hdr.job     EQ job.job
                          AND b-job-hdr.job-no  EQ job.job-no
                          AND b-job-hdr.job-no2 EQ job.job-no2
                          AND ROWID(b-job-hdr)  NE ROWID(job-hdr)) THEN
        FOR EACH reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job.job,"999999999")
              AND reftable.code2    NE job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            NO-LOCK:
          lv-fg-list = TRIM(lv-fg-list) + "," + TRIM(reftable.code2).
        END.

        RUN windows/l-ordljh.w(cocode, job-hdr.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}, lv-fg-list, job.job-no, job.job-no2, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
        FIND oe-ordl WHERE RECID(oe-ordl) EQ look-recid NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN FOCUS:SCREEN-VALUE = STRING(oe-ordl.ord-no).
      END.
    END CASE.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON RETURN OF Browser-Table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
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
    {est/brsleave.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

   IF AVAIL job THEN DO:
    /* {methods/template/local/setvalue.i} display job's note not job-hdr */
       {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
      "(job.rec_key,{methods/headers/job.i})"}
      {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
      "(CAN-FIND(FIRST notes WHERE notes.rec_key = job.rec_key))"}

      IF AVAIL job-hdr THEN
      DO:
         ASSIGN
            v-job-rec-key = cocode + "|jh" + STRING(job-hdr.j-no)
            v-job-header = " Job: " + job-hdr.job-no + "-" + STRING(job-hdr.job-no2) + " Item#: " + job-hdr.i-no.

         {methods/run_link.i "CONTAINER-SOURCE" "Set-Misc-Rec-Key_Header"
          "(v-job-rec-key,v-job-header)"}

         {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
         "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = v-job-rec-key))"}
      END.
   END.

/*  DEF VAR char-hdl AS CHAR NO-UNDO.
  IF AVAIL job-hdr THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN get-due-date IN WIDGET-HANDLE(char-hdl) (ROWID(job-hdr)).
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-hdr.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.frm Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-hdr.frm IN BROWSE Browser-Table /* S */
DO:
  IF NOT adm-new-record AND job.est-no NE "" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-hdr.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.blank-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-hdr.blank-no IN BROWSE Browser-Table /* B */
DO:
  IF NOT adm-new-record AND job.est-no NE "" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-hdr.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-hdr.cust-no IN BROWSE Browser-Table /* Cust. # */
DO:
  IF NOT adm-new-record AND job.est-no NE "" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-hdr.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-hdr.i-no IN BROWSE Browser-Table /* Item No */
DO:
  IF NOT adm-new-record AND job.est-no NE "" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-hdr.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-hdr.ord-no IN BROWSE Browser-Table /* Order# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF job-hdr.ord-no IN BROWSE Browser-Table /* Order# */
DO:
  FIND FIRST oe-ordl NO-LOCK
      WHERE oe-ordl.company   EQ job.company
        AND oe-ordl.ord-no    EQ INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
        AND oe-ordl.i-no      EQ job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        AND oe-ordl.opened    EQ YES
      NO-ERROR.
  FIND oe-ordl NO-LOCK
      WHERE oe-ordl.company   EQ job.company
        AND oe-ordl.ord-no    EQ INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
        AND oe-ordl.job-no    EQ job.job-no
        AND oe-ordl.job-no2   EQ job.job-no2
        AND oe-ordl.opened    EQ YES
      NO-ERROR.
  IF AVAIL oe-ordl THEN
    job-hdr.due-date:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(oe-ordl.req-date).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-hdr.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.due-date Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF job-hdr.due-date IN BROWSE Browser-Table /* Due Date */
DO:
  IF INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
    RUN last-field-enabled.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-hdr.due-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF job-hdr.due-date IN BROWSE Browser-Table /* Due Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-due-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
ASSIGN cocode = g_company
       locode = g_loc.

DO TRANSACTION:
   {sys\inc\fgsecur.i}
END.

IF fgsecurity-log THEN
DO:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.

   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
      ASSIGN
         job-hdr.std-mat-cost:VISIBLE IN BROWSE {&browse-name} = NO
         job-hdr.std-lab-cost:VISIBLE IN BROWSE {&browse-name} = NO
         job-hdr.std-fix-cost:VISIBLE IN BROWSE {&browse-name} = NO
         job-hdr.std-var-cost:VISIBLE IN BROWSE {&browse-name} = NO.
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
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-hdr B-table-Win 
PROCEDURE get-job-hdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

  op-rowid = IF AVAIL job-hdr THEN ROWID(job-hdr) ELSE ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE last-field-enabled B-table-Win 
PROCEDURE last-field-enabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("update-record").
    RUN repo-query (ROWID(job-hdr)).
    RETURN NO-APPLY.
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
  DEF VAR li-old-qty AS INT NO-UNDO.

  DEF BUFFER job-qty-changed FOR reftable.
   
  /* Code placed here will execute PRIOR to standard behavior. */
  li-old-qty = job-hdr.qty.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */ 
  IF job-hdr.qty NE li-old-qty THEN DO:
     RUN dispatch ('display-fields').
     
     FIND FIRST itemfg
         WHERE itemfg.company EQ job-hdr.company
           AND itemfg.i-no    EQ job-hdr.i-no.
    RUN fg/chkfgloc.p (INPUT job-hdr.i-no, INPUT job-hdr.loc).
    FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ job-hdr.company
          AND itemfg-loc.i-no    EQ job-hdr.i-no
          AND itemfg-loc.loc     EQ job-hdr.loc
    EXCLUSIVE-LOCK NO-ERROR.

     /* gdm - 10060901 */
     IF job-hdr.est-no NE "" THEN 
     DO:
        IF itemfg.isaset EQ NO THEN
           FIND FIRST eb WHERE
                eb.company  EQ job-hdr.company AND
                eb.est-no   EQ job-hdr.est-no AND
                eb.form-no  EQ job-hdr.frm AND
                eb.blank-no EQ job-hdr.blank-no
                NO-LOCK NO-ERROR.

        IF itemfg.isaset EQ YES OR
           (AVAIL eb AND NOT eb.pur-man) OR
           (NOT AVAIL eb AND NOT itemfg.pur-man) THEN DO:
           ASSIGN itemfg.q-ono = itemfg.q-ono - li-old-qty + job-hdr.qty.
           IF AVAIL itemfg-loc THEN
              ASSIGN itemfg-loc.q-ono = itemfg-loc.q-ono - li-old-qty + job-hdr.qty.
        END.


     END.
     ELSE
     /* gdm - 10060901 end */
     IF job-hdr.est-no EQ "" AND NOT itemfg.pur-man THEN DO:
        ASSIGN
         itemfg.q-ono = itemfg.q-ono - li-old-qty + job-hdr.qty.
        IF AVAIL(itemfg-loc) THEN
            ASSIGN
             itemfg-loc.q-ono = itemfg-loc.q-ono - li-old-qty + job-hdr.qty.
     END.

    
     itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
     IF AVAIL itemfg-loc THEN
         itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
     FIND CURRENT itemfg NO-LOCK.
     FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.

     RUN fg/comp-upd.p (RECID(itemfg), li-old-qty * -1, "q-ono", job.est-no).
     RUN fg/comp-upd.p (RECID(itemfg), job-hdr.qty, "q-ono", job.est-no).

       FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR.
           ASSIGN
               job.qty-changed = YES.
       FIND CURRENT job NO-LOCK.        
               

     /* lv-qty-changed needed for correct parameter passed to jc-calc.p */
     lv-qty-changed = TRUE.
     RUN rebuild-stds.
     lv-qty-changed = FALSE.
  END.
  IF adm-new-record THEN DO:
    RUN jc/addJobFarm.p (INPUT job.job).
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
  {&methods/lValidateError.i YES}
   FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ job.company
               AND po-ordl.job-no  EQ job.job-no
               AND po-ordl.job-no2 EQ job.job-no2
               AND po-ordl.opened NO-ERROR .
        IF AVAILABLE po-ordl THEN DO:
            MESSAGE "There is an open Purchase Order for this job. This PO" SKIP
                    "must be closed or deleted before the job can be deleted."
                VIEW-AS ALERT-BOX INFO .
            RETURN ERROR .
        END.
       {&methods/lValidateError.i NO}

  {custom/askdel.i}
  FOR EACH job-farm
      WHERE job-farm.company EQ job-hdr.company
        AND job-farm.job-no  EQ job-hdr.job-no
        AND job-farm.job-no2 EQ job-hdr.job-no2
        AND job-farm.i-no    EQ job-hdr.i-no
      EXCLUSIVE:
    DELETE job-farm.
  END.
  FOR EACH job-farm-rctd
      WHERE job-farm-rctd.company EQ job-hdr.company
        AND job-farm-rctd.job-no  EQ job-hdr.job-no
        AND job-farm-rctd.job-no2 EQ job-hdr.job-no2
        AND job-farm-rctd.i-no    EQ job-hdr.i-no
      EXCLUSIVE:
    DELETE job-farm-rctd.
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
     APPLY "entry" TO job-hdr.frm IN BROWSE {&browse-name}.
     RETURN NO-APPLY.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

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
   DEF BUFFER b-oe-ordl FOR oe-ordl.
   DEF BUFFER bf-oe-ord FOR oe-ord.
   DEF BUFFER bf-job FOR job.
   DEFINE VARIABLE lJobStatWasChanged AS LOGICAL NO-UNDO.
   /* Code placed here will execute PRIOR to standard behavior. */
   lJobStatWasChanged = NO.
   RUN valid-ord-no NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   RUN valid-due-date NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      RUN validate-i-no NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
    
   /* rtc 08/28/08 begin */
   IF job-hdr.ord-no NE INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
         FIND FIRST b-oe-ordl WHERE
                    b-oe-ordl.company   EQ cocode 
                AND b-oe-ordl.opened    EQ YES 
                AND b-oe-ordl.ord-no    EQ INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
                AND b-oe-ordl.i-no      EQ job-hdr.i-no NO-LOCK NO-ERROR.
                
            IF AVAIL b-oe-ordl THEN DO:
               job-hdr.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = b-oe-ordl.po-no.
               
               IF OEJobHold-log THEN DO:
                   FIND FIRST bf-oe-ord WHERE bf-oe-ord.company EQ b-oe-ordl.company
                     AND bf-oe-ord.ord-no EQ b-oe-ordl.ord-no
                     NO-LOCK NO-ERROR.
                     
                   IF bf-oe-ord.stat EQ "H" THEN DO:
                       
                     FIND FIRST bf-job WHERE bf-job.job EQ job-hdr.job NO-LOCK NO-ERROR.
                     IF AVAIL bf-job AND bf-job.stat NE "H" THEN DO:
                         FIND CURRENT bf-job EXCLUSIVE-LOCK NO-ERROR.
                         
                         IF AVAIL bf-job THEN 
                           ASSIGN bf-job.stat = "H"
                                  lJobStatWasChanged = TRUE.
                         FIND CURRENT bf-job NO-LOCK.
                         RELEASE bf-job.
                     END.  
                   END.
               END.
            END.
   END.
   /* rtc 08/28/08 end */

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   IF lv-recalced OR lJobStatWasChanged THEN DO:
      DEF VAR char-hdl AS cha NO-UNDO.
      lv-recalced = NO.
      /*RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
      RUN refresh-browser IN WIDGET-HANDLE(char-hdl). */
      RUN refresh-browser.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebuild-stds B-table-Win 
PROCEDURE rebuild-stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR iQty     AS INTEGER NO-UNDO .
  DEF BUFFER bf-job-hdr FOR job-hdr .
                              
  IF job.est-no NE "" THEN DO:

    IF job.stat = "Z" OR job.stat = "C" THEN DO:
         MESSAGE "Job is closed." SKIP
                 "You must REOPEN job before Rebuilding Job Qty" 
             VIEW-AS ALERT-BOX ERROR.
         UNDO, RETURN.
    END.

    MESSAGE "Recalculate Job Standards? " VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:
      IF AVAIL job-hdr THEN lv-qty = job-hdr.qty.

      /* if nufile is ?, then will not be fully recalculated
         if nufile is NO and the quantity is not changed, 
         jc-calc.p will change the quantity in error */

      ASSIGN
       lv-recalced = YES
        nufile      = IF lv-qty-changed THEN ? ELSE NO.

        FOR EACH bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ job.company 
             AND bf-job-hdr.job     EQ job.job
             AND bf-job-hdr.job-no  EQ job.job-no
             AND bf-job-hdr.job-no2 EQ job.job-no2 :
            iQty = iQty + bf-job-hdr.qty .
        END.
        IF iQty EQ 0 THEN nufile = YES .

      RUN jc/jc-calc.p (RECID(job-hdr), YES).

      ASSIGN
       fil_id = RECID(job)
       nufile = NO.

      RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.

      /*RUN refresh-browser. */
      RUN refresh-RecordSource.

      RUN refresh-browser .

      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
      RUN get-start-date IN WIDGET-HANDLE(char-hdl) (INPUT YES).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-browser B-table-Win 
PROCEDURE refresh-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rowid AS ROWID NO-UNDO.
   DEF BUFFER b-job-hdr FOR job-hdr.


   IF AVAIL job-hdr THEN lv-rowid = ROWID(job-hdr).
   RUN dispatch ("open-query").
   FOR EACH b-job-hdr NO-LOCK
       WHERE b-job-hdr.company EQ job.company
         AND b-job-hdr.job     EQ job.job
         AND b-job-hdr.job-no  EQ job.job-no
         AND b-job-hdr.job-no2 EQ job.job-no2:
     RUN repo-query (ROWID(b-job-hdr)).
   END.
   RUN repo-query (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refresh-RecordSource B-table-Win 
PROCEDURE Refresh-RecordSource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-handle AS HANDLE NO-UNDO.
 DEF VAR lv-rowid AS ROWID NO-UNDO.
 DEF VAR char-hdl AS cha NO-UNDO.

 IF AVAIL job-hdr THEN lv-rowid = ROWID(job-hdr).

 RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
 lv-handle = WIDGET-HANDLE(char-hdl).
 RUN get-link-handle IN adm-broker-hdl (lv-handle,"record-source", OUTPUT char-hdl).

 RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).
 
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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
                                                                
  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
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
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "job-hdr"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-due-date B-table-Win 
PROCEDURE valid-due-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0   AND
       DATE(job-hdr.due-date:SCREEN-VALUE IN BROWSE {&browse-name}) LT
         job.start-date                                                 THEN DO:
      MESSAGE TRIM(job-hdr.due-date:LABEL IN BROWSE {&browse-name}) +
              " may not be before Start Date..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-hdr.due-date IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no B-table-Win 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-fg-list AS CHAR NO-UNDO.
  DEF VAR lv-set-part AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN DO:
      lv-fg-list = job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}.

      IF NOT CAN-FIND(FIRST b-job-hdr
                      WHERE b-job-hdr.company EQ job.company
                        AND b-job-hdr.job     EQ job.job
                        AND b-job-hdr.job-no  EQ job.job-no
                        AND b-job-hdr.job-no2 EQ job.job-no2
                        AND ROWID(b-job-hdr)  NE ROWID(job-hdr)) THEN
      FOR EACH reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    NE job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK:
        lv-fg-list = TRIM(lv-fg-list) + "," + TRIM(reftable.code2).
      END.

      FOR EACH oe-ordl FIELDS(company opened ord-no i-no) WHERE
          oe-ordl.company   EQ job.company AND
          oe-ordl.opened    EQ YES AND
          oe-ordl.ord-no    EQ INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
          NO-LOCK,
          EACH fg-set FIELDS(company set-no part-no) WHERE 
               fg-set.company EQ cocode AND
               fg-set.set-no EQ oe-ordl.i-no
               NO-LOCK:

          IF INDEX(lv-fg-list,fg-set.part-no) GT 0 THEN
          DO:
             lv-set-part = YES.
             LEAVE.
          END.
      END.

      IF (NOT lv-set-part AND NOT CAN-FIND(FIRST oe-ordl
          WHERE oe-ordl.company   EQ job.company
            AND oe-ordl.opened    EQ YES
            AND oe-ordl.ord-no    EQ INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            /*AND oe-ordl.cust-no   EQ job-hdr.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}*/
            AND LOOKUP(oe-ordl.i-no,lv-fg-list) GT 0
                /*AND (oe-ordl.job-no   EQ "" OR
                     (oe-ordl.job-no  EQ job.job-no AND
                      oe-ordl.job-no2 EQ job.job-no2))*/)) OR
         NOT CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company  EQ job.company
                        AND oe-ord.ord-no   EQ INT(job-hdr.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}))
      THEN DO:
        MESSAGE TRIM(job-hdr.ord-no:LABEL IN BROWSE {&browse-name}) +
                " is invalid, try help..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-hdr.ord-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-i-no B-table-Win
PROCEDURE validate-i-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cINo AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                  AND itemfg.prod-uom NE "")
       THEN DO:
       cINo = job-hdr.i-no:SCREEN-VALUE IN BROWSE {&browse-name}.
       MESSAGE cINo + " has no cost UOM. Please correct the item master and try again."
           VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job-hdr.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END. /* not can-find */
  END. /* Do with frame ...*/
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-total-cost B-table-Win 
FUNCTION get-total-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dTotalCost AS DECIMAL FORMAT "->,>>>,>>>,>>>.99<<" NO-UNDO INIT 0.

  IF AVAIL job-hdr THEN
    ASSIGN dTotalCost = job-hdr.std-mat-cost + job-hdr.std-lab-cost + job-hdr.std-fix-cost + job-hdr.std-var-cost.

  RETURN dTotalCost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

