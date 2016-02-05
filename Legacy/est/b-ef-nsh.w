&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: est\b-ef-nsh.w

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

DEF BUFFER b-ef-nsh FOR ef-nsh.

DEF TEMP-TABLE tt-dim NO-UNDO FIELD tt-len AS DEC EXTENT 2
                              FIELD tt-wid AS DEC EXTENT 2
                              FIELD tt-dep AS DEC EXTENT 2
                              FIELD tt-seq AS INT.

DEF VAR lv-dim-list AS CHAR INIT "Length,Width,Depth" NO-UNDO.
DEF VAR lv-val-depts AS CHAR INIT "RC,DC" NO-UNDO.
DEF VAR lv-msg-depts AS CHAR NO-UNDO.
def var K_FRAC as dec init 6.25 no-undo.

{sys/inc/f16to32.i}

RUN sys/inc/lsttomsg.p (lv-val-depts, OUTPUT lv-msg-depts).

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
&Scoped-define EXTERNAL-TABLES ef
&Scoped-define FIRST-EXTERNAL-TABLE ef


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ef.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ef-nsh

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ef-nsh.pass-no ef-nsh.orig-no ~
ef-nsh.sheet-no ef-nsh.dept ef-nsh.wid-in ef-nsh.len-in ef-nsh.dep-in ~
ef-nsh.n-out-w ef-nsh.n-out-l ef-nsh.n-out-d ef-nsh.wid-out ef-nsh.len-out ~
ef-nsh.dep-out 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ef-nsh.pass-no ~
ef-nsh.sheet-no ef-nsh.dept ef-nsh.wid-in ef-nsh.len-in ef-nsh.dep-in ~
ef-nsh.n-out-w ef-nsh.n-out-l ef-nsh.n-out-d ef-nsh.wid-out ef-nsh.len-out ~
ef-nsh.dep-out 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table ef-nsh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table ef-nsh
&Scoped-define QUERY-STRING-br_table FOR EACH ef-nsh OF ef WHERE ~{&KEY-PHRASE} NO-LOCK ~
    BY ef-nsh.pass-no
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH ef-nsh OF ef WHERE ~{&KEY-PHRASE} NO-LOCK ~
    BY ef-nsh.pass-no.
&Scoped-define TABLES-IN-QUERY-br_table ef-nsh
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ef-nsh


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
      ef-nsh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      ef-nsh.pass-no FORMAT ">>>>>>":U
      ef-nsh.orig-no COLUMN-LABEL "Dropped!from Pass#" FORMAT ">>>>>>":U
            WIDTH 12
      ef-nsh.sheet-no FORMAT ">>>>>>":U WIDTH 8
      ef-nsh.dept FORMAT "x(2)":U
      ef-nsh.wid-in COLUMN-LABEL "Width IN" FORMAT ">,>>9.99<<":U
            WIDTH 12
      ef-nsh.len-in COLUMN-LABEL "Length IN" FORMAT ">,>>9.99<<":U
            WIDTH 12
      ef-nsh.dep-in COLUMN-LABEL "Depth IN" FORMAT ">,>>9.99<<":U
            WIDTH 12
      ef-nsh.n-out-w COLUMN-LABEL "#Out W" FORMAT ">>>":U WIDTH 8
      ef-nsh.n-out-l COLUMN-LABEL "#Out L" FORMAT ">>>":U WIDTH 8
      ef-nsh.n-out-d COLUMN-LABEL "#Out D" FORMAT ">>>":U WIDTH 8
      ef-nsh.wid-out COLUMN-LABEL "Width OUT" FORMAT ">,>>9.99<<":U
            WIDTH 12
      ef-nsh.len-out COLUMN-LABEL "Length OUT" FORMAT ">,>>9.99<<":U
            WIDTH 12
      ef-nsh.dep-out COLUMN-LABEL "Depth OUT" FORMAT ">,>>9.99<<":U
            WIDTH 12
  ENABLE
      ef-nsh.pass-no
      ef-nsh.sheet-no
      ef-nsh.dept
      ef-nsh.wid-in
      ef-nsh.len-in
      ef-nsh.dep-in
      ef-nsh.n-out-w
      ef-nsh.n-out-l
      ef-nsh.n-out-d
      ef-nsh.wid-out
      ef-nsh.len-out
      ef-nsh.dep-out
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 5.95
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
   External Tables: asi.ef
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
         WIDTH              = 145.8.
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
     _TblList          = "asi.ef-nsh OF asi.ef"
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "asi.ef-nsh.pass-no|yes"
     _FldNameList[1]   > asi.ef-nsh.pass-no
"ef-nsh.pass-no" ? ">>>>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.ef-nsh.orig-no
"ef-nsh.orig-no" "Dropped!from Pass#" ">>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.ef-nsh.sheet-no
"ef-nsh.sheet-no" ? ">>>>>>" "integer" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.ef-nsh.dept
"ef-nsh.dept" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.ef-nsh.wid-in
"ef-nsh.wid-in" "Width IN" ">,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.ef-nsh.len-in
"ef-nsh.len-in" "Length IN" ">,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.ef-nsh.dep-in
"ef-nsh.dep-in" "Depth IN" ">,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.ef-nsh.n-out-w
"ef-nsh.n-out-w" "#Out W" ">>>" "integer" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.ef-nsh.n-out-l
"ef-nsh.n-out-l" "#Out L" ">>>" "integer" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.ef-nsh.n-out-d
"ef-nsh.n-out-d" "#Out D" ">>>" "integer" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.ef-nsh.wid-out
"ef-nsh.wid-out" "Width OUT" ">,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.ef-nsh.len-out
"ef-nsh.len-out" "Length OUT" ">,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.ef-nsh.dep-out
"ef-nsh.dep-out" "Depth OUT" ">,>>9.99<<" "decimal" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME ef-nsh.pass-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.pass-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef-nsh.pass-no IN BROWSE br_table /* Pass# */
DO:
  IF INT(ef-nsh.pass-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 1 THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.orig-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.orig-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef-nsh.orig-no IN BROWSE br_table /* Dropped!from Pass# */
DO:
  IF INT(ef-nsh.pass-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 1 THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.sheet-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.sheet-no br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef-nsh.sheet-no IN BROWSE br_table /* Sheet# */
DO:
  IF INT(ef-nsh.pass-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 1 THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.sheet-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.sheet-no IN BROWSE br_table /* Sheet# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sheet-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.dept br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.dept IN BROWSE br_table /* Dept */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dept NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.wid-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.wid-in br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef-nsh.wid-in IN BROWSE br_table /* Width IN */
DO:
  IF INT(ef-nsh.pass-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 1 THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.wid-in br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.wid-in IN BROWSE br_table /* Width IN */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim-in (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.wid-in br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.wid-in IN BROWSE br_table /* Width IN */
DO:
  RUN calc-dim-out (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.len-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.len-in br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef-nsh.len-in IN BROWSE br_table /* Length IN */
DO:
  IF INT(ef-nsh.pass-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 1 THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.len-in br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.len-in IN BROWSE br_table /* Length IN */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim-in (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.len-in br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.len-in IN BROWSE br_table /* Length IN */
DO:
  RUN calc-dim-out (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.dep-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.dep-in br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ef-nsh.dep-in IN BROWSE br_table /* Depth IN */
DO:
  IF INT(ef-nsh.pass-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 1 THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.dep-in br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.dep-in IN BROWSE br_table /* Depth IN */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim-in (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.dep-in br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.dep-in IN BROWSE br_table /* Depth IN */
DO:
  RUN calc-dim-out (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.n-out-w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.n-out-w br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.n-out-w IN BROWSE br_table /* #Out W */
DO:
  RUN calc-dim-out (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.n-out-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.n-out-l br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.n-out-l IN BROWSE br_table /* #Out L */
DO:
  RUN calc-dim-out (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.n-out-d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.n-out-d br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.n-out-d IN BROWSE br_table /* #Out D */
DO:
  RUN calc-dim-out (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.wid-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.wid-out br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.wid-out IN BROWSE br_table /* Width OUT */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim-out (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.wid-out br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.wid-out IN BROWSE br_table /* Width OUT */
DO:
  RUN calc-num-out (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.len-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.len-out br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.len-out IN BROWSE br_table /* Length OUT */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim-out (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.len-out br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.len-out IN BROWSE br_table /* Length OUT */
DO:
  RUN calc-num-out (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef-nsh.dep-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.dep-out br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ef-nsh.dep-out IN BROWSE br_table /* Depth OUT */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim-out (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef-nsh.dep-out br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ef-nsh.dep-out IN BROWSE br_table /* Depth OUT */
DO:
  RUN calc-num-out (3).
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
  {src/adm/template/row-list.i "ef"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ef"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-dim-out B-table-Win 
PROCEDURE calc-dim-out :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-dim AS INT NO-UNDO.

  DEF VAR ld-dim AS DEC INIT 1 NO-UNDO.
  DEF VAR ld-out AS DEC INIT 1 NO-UNDO.
  DEF VAR li-out AS INT INIT 1 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ip-dim EQ 1 THEN
      ASSIGN
       ld-dim = DEC(ef-nsh.len-in:SCREEN-VALUE IN BROWSE {&browse-name})   /* Length IN */
       ld-out = DEC(ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name})  /* Length OUT */
       li-out = INT(ef-nsh.n-out-l:SCREEN-VALUE IN BROWSE {&browse-name}). /* #Out L */
    ELSE
    IF ip-dim EQ 2 THEN
      ASSIGN
       ld-dim = DEC(ef-nsh.wid-in:SCREEN-VALUE IN BROWSE {&browse-name})   /* Width IN */
       ld-out = DEC(ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name})  /* Width OUT */
       li-out = INT(ef-nsh.n-out-w:SCREEN-VALUE IN BROWSE {&browse-name}). /* #Out W */
    ELSE
    IF ip-dim EQ 3 THEN
      ASSIGN
       ld-dim = DEC(ef-nsh.dep-in:SCREEN-VALUE IN BROWSE {&browse-name})   /* Depth IN */
       ld-out = DEC(ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name})  /* Depth OUT */
       li-out = INT(ef-nsh.n-out-d:SCREEN-VALUE IN BROWSE {&browse-name}). /* #Out D */

    /* Calculate 'OUT' to equal 'IN' divided by #Out  
       Write trigger logic: TRUNC(ef.gsh-len / ef.nsh-len,0) */

    ASSIGN ld-dim = ld-dim / li-out.        /*trunc(ld-dim / li-out,0)*/  

/*     MESSAGE "current OUT value: " ld-out SKIP                                   */
/*             "calculated OUT: " ld-dim  SKIP                                     */
/* /*             "Is new calulated less than current?" ld-dim LT ld-out SKIP  */  */
/* /*             "Set new value?" ld-dim LT ld-out                            */  */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                      */

/*     IF ld-dim LT ld-out THEN  (SAB: always display the new OUT value)*/
      IF ip-dim EQ 1 THEN
        ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-dim).
      ELSE
      IF ip-dim EQ 2 THEN
        ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-dim).
      ELSE
      IF ip-dim EQ 3 THEN
        ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-dim).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-num-out B-table-Win 
PROCEDURE calc-num-out :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-dim AS INT NO-UNDO.

  DEF VAR ld-dim AS DEC INIT 1 NO-UNDO.
  DEF VAR ld-out AS DEC INIT 1 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF ip-dim EQ 1 THEN
      ASSIGN
       ld-dim = DEC(ef-nsh.len-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-out = DEC(ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
    IF ip-dim EQ 2 THEN
      ASSIGN
       ld-dim = DEC(ef-nsh.wid-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-out = DEC(ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
    IF ip-dim EQ 3 THEN
      ASSIGN
       ld-dim = DEC(ef-nsh.dep-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-out = DEC(ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF ld-out GT ld-dim THEN DO:
      ld-out = ld-dim.
      IF ip-dim EQ 1 THEN
        ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-out).
      ELSE
      IF ip-dim EQ 2 THEN
        ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-out).
      ELSE
      IF ip-dim EQ 3 THEN
        ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-out).
    END.

    ld-out = TRUNC(ld-dim / ld-out,0).

    IF ip-dim EQ 1 THEN
      ef-nsh.n-out-l:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-out).
    ELSE
    IF ip-dim EQ 2 THEN
      ef-nsh.n-out-w:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-out).
    ELSE
    IF ip-dim EQ 3 THEN
      ef-nsh.n-out-d:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ld-out).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE default-new-record B-table-Win 
PROCEDURE default-new-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid1 AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-rowid2 AS ROWID NO-UNDO.

  DEF BUFFER default-ef-nsh FOR ef-nsh.


  FIND default-ef-nsh WHERE ROWID(default-ef-nsh) EQ ip-rowid1 NO-ERROR.

  IF AVAIL default-ef-nsh THEN DO:
      /* stacey */
/*       MESSAGE 'default-new-record: default from ef record.' skip  */
/*               "ef.gsh-wid: " ef.gsh-wid SKIP                      */
/*               "ef.gsh-len: " ef.gsh-len SKIP                      */
/*               "ef.gsh-dep: " ef.gsh-dep                           */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */

    BUFFER-COPY ef EXCEPT rec_key TO default-ef-nsh
    ASSIGN
     default-ef-nsh.pass-no  = 1
     default-ef-nsh.sheet-no = 1
     default-ef-nsh.dept     = "RC"
     default-ef-nsh.len-in   = ef.gsh-len
     default-ef-nsh.wid-in   = ef.gsh-wid
     default-ef-nsh.dep-in   = ef.gsh-dep
     default-ef-nsh.n-out-l  = ef.n-out-l
     default-ef-nsh.n-out-w  = ef.n-out
     default-ef-nsh.n-out-d  = ef.n-out-d
     default-ef-nsh.len-out  = ef.nsh-len
     default-ef-nsh.wid-out  = ef.nsh-wid
     default-ef-nsh.dep-out  = ef.nsh-dep.

    FOR EACH b-ef-nsh OF ef
        WHERE ROWID(b-ef-nsh) NE ip-rowid1
        NO-LOCK BY b-ef-nsh.pass-no DESC:
      default-ef-nsh.pass-no = b-ef-nsh.pass-no + 1.
      LEAVE.
    END.

    RELEASE b-ef-nsh.

    FOR EACH b-ef-nsh OF ef
        WHERE (ip-rowid2 EQ ? AND
               ROWID(b-ef-nsh)  NE ip-rowid1 AND
               b-ef-nsh.orig-no EQ 0)
           OR (ROWID(b-ef-nsh) EQ ip-rowid2)
        NO-LOCK BY b-ef-nsh.pass-no DESC:
        
      ASSIGN
       default-ef-nsh.dept     = b-ef-nsh.dept
       default-ef-nsh.len-in   = b-ef-nsh.len-out
       default-ef-nsh.wid-in   = b-ef-nsh.wid-out
       default-ef-nsh.dep-in   = b-ef-nsh.dep-out
       default-ef-nsh.n-out-l  = 1
       default-ef-nsh.n-out-w  = 1
       default-ef-nsh.n-out-d  = 1
       default-ef-nsh.len-out  = default-ef-nsh.len-in
       default-ef-nsh.wid-out  = default-ef-nsh.wid-in
       default-ef-nsh.dep-out  = default-ef-nsh.dep-in.

/*       MESSAGE "default-ef-nsh.wid-in: " default-ef-nsh.wid-in SKIP  */
/*               "default-ef-nsh.len-in: " default-ef-nsh.len-in SKIP  */
/*               "default-ef-nsh.dep-in: " default-ef-nsh.dep-in       */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */

      LEAVE.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reset-passes (ROWID(ef-nsh)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ef-nsh.pass-no EQ 1 THEN DO:
    MESSAGE "You may not copy Pass# 1..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

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
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rowid = IF AVAIL ef-nsh THEN ROWID(ef-nsh) ELSE ?.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN default-new-record (ROWID(ef-nsh), lv-rowid).

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
  IF ef-nsh.pass-no EQ 1 THEN DO:
    MESSAGE "You may not delete Pass# 1..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reset-passes (?).

  RUN repo-query (IF AVAIL ef-nsh THEN ROWID(ef-nsh) ELSE ?).

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
    APPLY "entry" TO ef-nsh.sheet-no IN BROWSE {&browse-name}.
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
  DO WITH FRAME {&FRAME-NAME}:
    ef-nsh.dept:HELP IN BROWSE {&browse-name} = "Please enter " +
                                                TRIM(lv-msg-depts).
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
  DEF VAR lj AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR ld-dim AS DEC EXTENT 7 NO-UNDO.
  DEF VAR ll-drop AS LOG NO-UNDO.

  DEF BUFFER new-ef-nsh FOR ef-nsh.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-sheet-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-dept NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO li = 1 TO NUM-ENTRIES(lv-dim-list):
    RUN valid-dim-in (li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim-out (li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ef-nsh.dept EQ "RC"                                   AND
     NOT CAN-FIND(FIRST b-ef-nsh OF ef
                  WHERE b-ef-nsh.orig-no EQ ef-nsh.pass-no
                    AND ROWID(b-ef-nsh)  NE ROWID(ef-nsh)) THEN
  FOR EACH eb OF ef NO-LOCK:
    ASSIGN
     ld-dim[1] = ef-nsh.len-in - (ef-nsh.len-out * ef-nsh.n-out-l)
     ld-dim[2] = ef-nsh.wid-in - (ef-nsh.wid-out * ef-nsh.n-out-w)
     ld-dim[3] = ef-nsh.dep-in - (ef-nsh.dep-out * ef-nsh.n-out-d)

     ld-dim[5] = ef-nsh.len-out
     ld-dim[6] = ef-nsh.wid-out
     ld-dim[7] = ef-nsh.dep-out.

    EMPTY TEMP-TABLE tt-dim.

    DO li = 1 TO 2:
      DO lj = 1 TO 3:
        CREATE tt-dim.
        ASSIGN
         tt-seq    = li * lj
         tt-len[1] = ld-dim[1]
         tt-wid[1] = ld-dim[2]
         tt-dep[1] = ld-dim[3]

         tt-len[2] = ld-dim[5]
         tt-wid[2] = ld-dim[6]
         tt-dep[2] = ld-dim[7]

         ld-dim[4] = ld-dim[1]
         ld-dim[1] = ld-dim[2]
         ld-dim[2] = ld-dim[3]
         ld-dim[3] = ld-dim[4]

         ld-dim[4] = ld-dim[5]
         ld-dim[5] = ld-dim[6]
         ld-dim[6] = ld-dim[7]
         ld-dim[7] = ld-dim[4].
      END.
 
      ASSIGN
       ld-dim[4] = ld-dim[2]
       ld-dim[2] = ld-dim[3]
       ld-dim[3] = ld-dim[4]

       ld-dim[4] = ld-dim[6]
       ld-dim[6] = ld-dim[7]
       ld-dim[7] = ld-dim[4].
    END.

    RELEASE tt-dim.

    FOR EACH tt-dim BY tt-seq:
      IF (tt-len[1] GE eb.t-len OR
          tt-wid[1] GE eb.t-wid OR
          tt-dep[1] GE eb.t-dep)    AND
         tt-len[2] GE eb.t-len      AND
         tt-wid[2] GE eb.t-wid      AND
         tt-dep[2] GE eb.t-dep      THEN LEAVE.
    END.


    IF AVAIL tt-dim THEN DO:
      MESSAGE "Create Drop Sheet from this Pass?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

      IF ll THEN DO:
        CREATE new-ef-nsh.

        RUN default-new-record (ROWID(new-ef-nsh), ?).

        FIND CURRENT new-ef-nsh.
        ASSIGN
/*          new-ef-nsh.len-in = IF tt-len[1] GT 0 THEN tt-len[1] ELSE tt-len[2]  */
/*          new-ef-nsh.wid-in = IF tt-wid[1] GT 0 THEN tt-wid[1] ELSE tt-wid[2]  */
/*          new-ef-nsh.dep-in = IF tt-dep[1] GT 0 THEN tt-dep[1] ELSE tt-dep[2]  */
         new-ef-nsh.orig-no = ef-nsh.pass-no
         new-ef-nsh.len-out = new-ef-nsh.len-in
         new-ef-nsh.wid-out = new-ef-nsh.wid-in
         new-ef-nsh.dep-out = new-ef-nsh.dep-in.

        FOR EACH b-ef-nsh OF ef
            WHERE ROWID(b-ef-nsh) NE ROWID(new-ef-nsh)
            NO-LOCK BY b-ef-nsh.sheet-no DESC:

          new-ef-nsh.sheet-no = b-ef-nsh.sheet-no.
          LEAVE.
        END.

        RUN repo-query (ROWID(ef-nsh)).
      END.
    END.

    LEAVE.
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
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-passes B-table-Win 
PROCEDURE reset-passes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER reset-ef-nsh FOR ef-nsh.

  DEF VAR li LIKE ef-nsh.pass-no NO-UNDO.


  IF ip-rowid NE ? THEN DO:
    FOR EACH reset-ef-nsh OF ef
        WHERE ROWID(reset-ef-nsh) NE ip-rowid
        BY reset-ef-nsh.pass-no:
      reset-ef-nsh.pass-no = (reset-ef-nsh.pass-no * 100000) +
                             (IF reset-ef-nsh.pass-no EQ ef-nsh.pass-no THEN 1
                              ELSE 0).
    END.

    ef-nsh.pass-no = ef-nsh.pass-no * 100000.
  END.

  FOR EACH reset-ef-nsh OF ef BY reset-ef-nsh.pass-no:
    ASSIGN
     li = li + 1
     reset-ef-nsh.pass-no = li.
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
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "ef-nsh"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dept B-table-Win 
PROCEDURE valid-dept :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ef-nsh.dept:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(ef-nsh.dept:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NUM-ENTRIES(lv-val-depts) GT 0 AND
       LOOKUP(ef-nsh.dept:SCREEN-VALUE IN BROWSE {&browse-name},lv-val-depts)
                                 LE 0 THEN DO:
      MESSAGE TRIM(ef-nsh.dept:LABEL IN BROWSE {&browse-name}) + " " +
              TRIM(ef-nsh.dept:SCREEN-VALUE IN BROWSE {&browse-name}) +
              " is invalid, must be " + TRIM(lv-msg-depts) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef-nsh.dept IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF ef-nsh.dept:SCREEN-VALUE IN BROWSE {&browse-name} EQ "DC" THEN DO:
      ASSIGN
       ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ef.trim-l).
       ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ef.trim-w).
       ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ef.trim-d).

      DO li = 1 TO 3:
        RUN calc-num-out (li).
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dim-in B-table-Win 
PROCEDURE valid-dim-in :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-dim AS INT NO-UNDO.

  DEF VAR ld-dim-in AS DEC INIT 1 NO-UNDO.
  DEF VAR ld-dim-out AS DEC INIT 1 NO-UNDO.
  DEF VAR lv-dim AS CHAR NO-UNDO.


  lv-dim = ENTRY(ip-dim,lv-dim-list).

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-dim EQ 1 THEN
      ASSIGN
       ld-dim-in  = DEC(ef-nsh.len-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-dim-out = DEC(ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
    IF ip-dim EQ 2 THEN
      ASSIGN
       ld-dim-in  = DEC(ef-nsh.wid-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-dim-out = DEC(ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
    IF ip-dim EQ 3 THEN
      ASSIGN
       ld-dim-in  = DEC(ef-nsh.dep-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-dim-out = DEC(ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    
    IF ld-dim-in LT ld-dim-out THEN DO:
      MESSAGE TRIM(lv-dim) + " In may not be less than " + TRIM(lv-dim) + " Out..."
          VIEW-AS ALERT-BOX ERROR.
      IF ip-dim EQ 1 THEN
        APPLY "entry" TO ef-nsh.len-out IN BROWSE {&browse-name}.
      ELSE
      IF ip-dim EQ 2 THEN
        APPLY "entry" TO ef-nsh.wid-out IN BROWSE {&browse-name}.
      ELSE
      IF ip-dim EQ 3 THEN
        APPLY "entry" TO ef-nsh.dep-out IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dim-out B-table-Win 
PROCEDURE valid-dim-out :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-dim AS INT NO-UNDO.

  DEF VAR lv-msg AS CHAR INIT "" NO-UNDO.
  DEF VAR ld-dim-in AS DEC DECIMALS 10 INIT 1 NO-UNDO.
  DEF VAR ld-dim-out AS DEC DECIMALS 10 INIT 1 NO-UNDO.
  DEF VAR ld-dim-blk AS DEC DECIMALS 10 INIT 1 NO-UNDO.
  DEF VAR lv-dim AS CHAR NO-UNDO.


  lv-dim = ENTRY(ip-dim,lv-dim-list).

  FOR EACH eb
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no
      NO-LOCK
      BY (IF ip-dim EQ 1 THEN eb.t-len ELSE
          IF ip-dim EQ 2 THEN eb.t-wid ELSE eb.t-dep):
    ld-dim-blk = IF ip-dim EQ 1 THEN eb.t-len
                 ELSE
                 IF ip-dim EQ 2 THEN eb.t-wid
                 ELSE                eb.t-dep.
  END.

  IF ip-dim EQ 3 AND
     v-cecscrn-char NE "Decimal" THEN
     ld-dim-blk = ROUND(ld-dim-blk * li-16-32,0) / li-16-32.

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-dim EQ 1 THEN
      ASSIGN
       ld-dim-in  = DEC(ef-nsh.len-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-dim-out = DEC(ef-nsh.len-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
    IF ip-dim EQ 2 THEN
      ASSIGN
       ld-dim-in  = DEC(ef-nsh.wid-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-dim-out = DEC(ef-nsh.wid-out:SCREEN-VALUE IN BROWSE {&browse-name}).
    ELSE
    IF ip-dim EQ 3 THEN
      ASSIGN
       ld-dim-in  = DEC(ef-nsh.dep-in:SCREEN-VALUE IN BROWSE {&browse-name})
       ld-dim-out = DEC(ef-nsh.dep-out:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF ld-dim-out GT ld-dim-in THEN
      lv-msg = TRIM(lv-dim) + " Out may not be greater than " + TRIM(lv-dim) + " In".
    ELSE
    IF ld-dim-out LT ld-dim-blk THEN
      lv-msg = TRIM(lv-dim) + " Out may not be less than the " + TRIM(lv-dim) + " of the Blank".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      IF ip-dim EQ 1 THEN
        APPLY "entry" TO ef-nsh.len-out IN BROWSE {&browse-name}.
      ELSE
      IF ip-dim EQ 2 THEN
        APPLY "entry" TO ef-nsh.wid-out IN BROWSE {&browse-name}.
      ELSE
      IF ip-dim EQ 3 THEN
        APPLY "entry" TO ef-nsh.dep-out IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    LEAVE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sheet-no B-table-Win 
PROCEDURE valid-sheet-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR INIT "" NO-UNDO.
  DEF VAR lv-sheet-no LIKE ef-nsh.sheet-no NO-UNDO.
  DEF VAR lv-field-val LIKE ef-nsh.sheet-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-field-val = INT(ef-nsh.sheet-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FOR EACH b-ef-nsh OF ef
        WHERE ROWID(b-ef-nsh) NE ROWID(ef-nsh)
        NO-LOCK
        BY b-ef-nsh.sheet-no DESC:
      lv-sheet-no = b-ef-nsh.sheet-no.
      LEAVE.
    END.

    IF lv-field-val EQ 0               THEN lv-msg = "may not be zero".
    ELSE
    IF lv-field-val GT lv-sheet-no + 1 THEN lv-msg = "is too high".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(ef-nsh.sheet-no:SCREEN-VALUE IN BROWSE {&browse-name}) + " " +
              TRIM(lv-msg) + "... " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ef-nsh.sheet-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

