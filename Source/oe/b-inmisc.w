&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{sys/inc/VAR.i "new shared"}
{custom/globdefs.i}
ASSIGN cocode = g_company
       locode = g_loc.
{oe/oe-sysct1.i NEW}
 def BUFFER bf-head FOR inv-head.
 DEF BUFFER bf-misc FOR inv-misc.
 DEF VAR v-tax AS DEC NO-UNDO.
 DEFINE VARIABLE rRowidNew AS ROWID NO-UNDO .
 DEFINE VARIABLE lUpdateMiscItem AS LOGICAL NO-UNDO.
 DEFINE VARIABLE lAccessClose AS LOGICAL NO-UNDO.
 DEFINE VARIABLE cAccessList AS CHARACTER NO-UNDO.

 RUN methods/prgsecur.p
    (INPUT "p-invmis.",
     INPUT "Update", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT lUpdateMiscItem, /* Allowed? Yes/NO */
     OUTPUT lAccessClose, /* used in template/windows.i  */
     OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

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
&Scoped-define EXTERNAL-TABLES inv-head
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES inv-misc

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table inv-misc.charge inv-misc.amt ~
inv-misc.actnum inv-misc.dscr inv-misc.po-no inv-misc.cost ~
inv-misc.inv-i-no inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] ~
inv-misc.s-pct[1] inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] ~
inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] ~
inv-misc.tax inv-misc.bill inv-misc.spare-char-1 inv-misc.spare-char-2 ~
inv-misc.est-no inv-misc.spare-int-1 inv-misc.spare-int-2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table inv-misc.charge ~
inv-misc.amt inv-misc.actnum inv-misc.dscr inv-misc.po-no inv-misc.cost ~
inv-misc.inv-i-no inv-misc.inv-line inv-misc.po-no-po inv-misc.s-man[1] ~
inv-misc.s-pct[1] inv-misc.s-comm[1] inv-misc.s-man[2] inv-misc.s-pct[2] ~
inv-misc.s-comm[2] inv-misc.s-man[3] inv-misc.s-pct[3] inv-misc.s-comm[3] ~
inv-misc.tax inv-misc.bill inv-misc.spare-char-2 inv-misc.est-no ~
inv-misc.spare-int-1 inv-misc.spare-int-2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table inv-misc
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table inv-misc
&Scoped-define QUERY-STRING-Browser-Table FOR EACH inv-misc WHERE inv-misc.r-no eq inv-head.r-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH inv-misc WHERE inv-misc.r-no eq inv-head.r-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table inv-misc
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table inv-misc


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find fi_By fi_AutoFindLabel 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find fi_By ~
fi_AutoFindLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE fi_AutoFindLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Find:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fi_By AS CHARACTER FORMAT "X(256)":U INITIAL "By:" 
      VIEW-AS TEXT 
     SIZE 3.6 BY .62 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      inv-misc
    FIELDS(inv-misc.charge
      inv-misc.amt
      inv-misc.actnum
      inv-misc.dscr
      inv-misc.po-no
      inv-misc.cost
      inv-misc.inv-i-no
      inv-misc.inv-line
      inv-misc.po-no-po
      inv-misc.s-man[1]
      inv-misc.s-pct[1]
      inv-misc.s-comm[1]
      inv-misc.s-man[2]
      inv-misc.s-pct[2]
      inv-misc.s-comm[2]
      inv-misc.s-man[3]
      inv-misc.s-pct[3]
      inv-misc.s-comm[3]
      inv-misc.tax
      inv-misc.bill
      inv-misc.spare-char-1
      inv-misc.spare-char-2
      inv-misc.est-no
      inv-misc.spare-int-1
      inv-misc.spare-int-2) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      inv-misc.charge FORMAT "x(20)":U WIDTH 35
      inv-misc.amt COLUMN-LABEL "Sell Price" FORMAT "->>>,>>9.99":U
            WIDTH 15.4
      inv-misc.actnum COLUMN-LABEL "Account#" FORMAT "x(25)":U
            WIDTH 35
      inv-misc.dscr FORMAT "x(30)":U WIDTH 40
      inv-misc.po-no COLUMN-LABEL "Customer PO#" FORMAT "x(15)":U
            WIDTH 20
      inv-misc.cost FORMAT "->>,>>>,>>9.99":U WIDTH 19.6
      inv-misc.inv-i-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 9
      inv-misc.inv-line COLUMN-LABEL "" FORMAT "99":U WIDTH 4
      inv-misc.po-no-po COLUMN-LABEL "Vendor PO#" FORMAT ">>>>>>":U
            WIDTH 14
      inv-misc.s-man[1] COLUMN-LABEL "Slsmn" FORMAT "x(3)":U WIDTH 8
      inv-misc.s-pct[1] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
            WIDTH 13
      inv-misc.s-comm[1] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
            WIDTH 9
      inv-misc.s-man[2] COLUMN-LABEL "Slsmn" FORMAT "x(3)":U WIDTH 8
      inv-misc.s-pct[2] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
      inv-misc.s-comm[2] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
            WIDTH 9
      inv-misc.s-man[3] COLUMN-LABEL "Slsrep" FORMAT "x(3)":U WIDTH 8
      inv-misc.s-pct[3] COLUMN-LABEL "% of Sale" FORMAT ">>9.99":U
            WIDTH 13
      inv-misc.s-comm[3] COLUMN-LABEL "Comm%" FORMAT ">>9.99":U
            WIDTH 9
      inv-misc.tax FORMAT "Y/N":U WIDTH 5.6
      inv-misc.bill FORMAT "X":U WIDTH 6.2
      inv-misc.spare-char-1 COLUMN-LABEL "Tax Prep" FORMAT "x(3)":U
            WIDTH 12
      inv-misc.spare-char-2 COLUMN-LABEL "FG Item Code" FORMAT "x(15)":U
            WIDTH 21
      inv-misc.est-no COLUMN-LABEL "Estimate" FORMAT "x(12)":U
      inv-misc.spare-int-1 COLUMN-LABEL "S" FORMAT "->,>>>,>>9":U
            WIDTH 2
      inv-misc.spare-int-2 COLUMN-LABEL "B" FORMAT "->,>>>,>>9":U
            WIDTH 2
  ENABLE
      inv-misc.charge
      inv-misc.amt
      inv-misc.actnum
      inv-misc.dscr
      inv-misc.po-no
      inv-misc.cost
      inv-misc.inv-i-no
      inv-misc.inv-line
      inv-misc.po-no-po
      inv-misc.s-man[1]
      inv-misc.s-pct[1]
      inv-misc.s-comm[1]
      inv-misc.s-man[2]
      inv-misc.s-pct[2]
      inv-misc.s-comm[2]
      inv-misc.s-man[3]
      inv-misc.s-pct[3]
      inv-misc.s-comm[3]
      inv-misc.tax
      inv-misc.bill
      inv-misc.spare-char-1
      inv-misc.spare-char-2
      inv-misc.est-no
      inv-misc.spare-int-1
      inv-misc.spare-int-2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 13.57
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 14.81 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 14.81 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 14.81 COL 132 HELP
          "CLEAR AUTO FIND Value"
     fi_By AT ROW 14.86 COL 2.4 NO-LABEL
     fi_AutoFindLabel AT ROW 15.05 COL 61.6 NO-LABEL
     RECT-4 AT ROW 14.57 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.inv-head
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
         HEIGHT             = 19.62
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_AutoFindLabel IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_By IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.inv-misc WHERE ASI.inv-head ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "inv-misc.r-no eq inv-head.r-no"
     _FldNameList[1]   > ASI.inv-misc.charge
"inv-misc.charge" ? ? "character" ? ? ? ? ? ? yes ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.inv-misc.amt
"inv-misc.amt" "Sell Price" ? "decimal" ? ? ? ? ? ? yes ? no no "15.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.inv-misc.actnum
"inv-misc.actnum" "Account#" ? "character" ? ? ? ? ? ? yes ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.inv-misc.dscr
"inv-misc.dscr" ? "x(30)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.inv-misc.po-no
"inv-misc.po-no" "Customer PO#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.inv-misc.cost
"inv-misc.cost" ? ? "decimal" ? ? ? ? ? ? yes ? no no "19.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.inv-misc.inv-i-no
"inv-misc.inv-i-no" "Job#" "x(6)" "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.inv-misc.inv-line
"inv-misc.inv-line" "" "99" "integer" ? ? ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.inv-misc.po-no-po
"inv-misc.po-no-po" "Vendor PO#" ">>>>>>" "integer" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.inv-misc.s-man[1]
"inv-misc.s-man[1]" "Slsmn" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.inv-misc.s-pct[1]
"inv-misc.s-pct[1]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.inv-misc.s-comm[1]
"inv-misc.s-comm[1]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.inv-misc.s-man[2]
"inv-misc.s-man[2]" "Slsmn" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.inv-misc.s-pct[2]
"inv-misc.s-pct[2]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.inv-misc.s-comm[2]
"inv-misc.s-comm[2]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.inv-misc.s-man[3]
"inv-misc.s-man[3]" "Slsrep" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.inv-misc.s-pct[3]
"inv-misc.s-pct[3]" "% of Sale" ? "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.inv-misc.s-comm[3]
"inv-misc.s-comm[3]" "Comm%" ? "decimal" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.inv-misc.tax
"inv-misc.tax" ? ? "logical" ? ? ? ? ? ? yes ? no no "5.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.inv-misc.bill
"inv-misc.bill" ? ? "character" ? ? ? ? ? ? yes ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.inv-misc.spare-char-1
"inv-misc.spare-char-1" "Tax Prep" "x(3)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.inv-misc.spare-char-2
"inv-misc.spare-char-2" "FG Item Code" "x(15)" "character" ? ? ? ? ? ? yes ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.inv-misc.est-no
"inv-misc.est-no" "Estimate" "x(12)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.inv-misc.spare-int-1
"inv-misc.spare-int-1" "S" ? "integer" ? ? ? ? ? ? yes ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.inv-misc.spare-int-2
"inv-misc.spare-int-2" "B" ? "integer" ? ? ? ? ? ? yes ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
   IF AVAIL inv-misc AND lUpdateMiscItem THEN do:
       RUN oe/d-inmisc.w (ROWID(inv-misc),ROWID(inv-head), "update", OUTPUT lv-rowid) .
       RUN reopen-query (lv-rowid).
       RUN refresh-value.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
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
   /*{src/adm/template/brsleave.i}*/
    {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
RUN oe/oe-sysct.p.

 IF NOT v-oecomm-log THEN RUN show-comm (NO).

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
  {src/adm/template/row-list.i "inv-head"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE job-help B-table-Win 
PROCEDURE job-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-jobno.w (cocode, inv-misc.inv-i-no:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val, OUTPUT look-recid).
    IF look-recid NE ? THEN DO:
      FIND job-hdr WHERE RECID(job-hdr) EQ look-recid NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN 
        ASSIGN
         inv-misc.inv-i-no:SCREEN-VALUE = job-hdr.job-no
         inv-misc.inv-line:SCREEN-VALUE = STRING(job-hdr.job-no2).                      
    END.
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
    IF NOT AVAIL inv-misc AND adm-new-record AND  adm-adding-record  THEN
      FIND FIRST inv-misc WHERE ROWID(inv-misc) EQ rRowidNew NO-ERROR .
  /* Buttons were made not sensitive during add, so reverse that here */
 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

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

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN refresh-value.

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
  IF NOT v-oecomm-log THEN RUN show-comm (NO).
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
  APPLY "entry" TO inv-misc.charge IN BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-value B-table-Win 
PROCEDURE refresh-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN oe/oe-invup.p (ROWID(inv-head), INPUT NO).

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  RUN get-link-handle IN adm-broker-hdl (WIDGET-HANDLE(char-hdl),"record-source", OUTPUT char-hdl).
  RUN refresh-value IN WIDGET-HANDLE(char-hdl).

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
  {src/adm/template/snd-list.i "inv-head"}
  {src/adm/template/snd-list.i "inv-misc"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ('row-changed').
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-comm B-table-Win 
PROCEDURE show-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-visible AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     inv-misc.s-pct[1]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     inv-misc.s-pct[2]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     inv-misc.s-pct[3]:VISIBLE IN BROWSE {&browse-name}  = ip-visible
     inv-misc.s-comm[1]:VISIBLE IN BROWSE {&browse-name} = ip-visible
     inv-misc.s-comm[2]:VISIBLE IN BROWSE {&browse-name} = ip-visible
     inv-misc.s-comm[3]:VISIBLE IN BROWSE {&browse-name} = ip-visible.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord V-table-Win 
PROCEDURE pUpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
    IF AVAIL inv-misc THEN do:
        RUN oe/d-inmisc.w (ROWID(inv-misc),ROWID(inv-head), "update", OUTPUT lv-rowid) .
        RUN reopen-query (lv-rowid).
        RUN refresh-value.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyRecord V-table-Win 
PROCEDURE pCopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
   DEFINE VARIABLE z AS INTEGER NO-UNDO.
   DEFINE BUFFER bf-misc FOR inv-misc.
   DEFINE BUFFER bf-inv-misc FOR inv-misc.

   IF AVAIL inv-head AND AVAIL inv-misc THEN do:

       FIND LAST bf-misc WHERE bf-misc.r-no = inv-head.r-no NO-LOCK NO-ERROR.
       z = IF AVAIL bf-misc THEN bf-misc.LINE + 1 ELSE 1.

       CREATE bf-inv-misc.
       BUFFER-COPY inv-misc EXCEPT rec_key line TO bf-inv-misc.
       bf-inv-misc.line = z .

       RUN oe/d-inmisc.w (ROWID(bf-inv-misc), ROWID(inv-head), "Copy",OUTPUT lv-rowid).
        FIND FIRST bf-inv-misc NO-LOCK
            WHERE rowid(bf-inv-misc) EQ  lv-rowid  NO-ERROR .
        IF AVAIL bf-inv-misc THEN DO:
            RUN reopen-query (lv-rowid).
            RUN refresh-value.
        END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewRecord V-table-Win 
PROCEDURE pViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
   IF AVAIL inv-misc THEN do:
       RUN oe/d-inmisc.w (ROWID(inv-misc),ROWID(inv-head), "view", OUTPUT lv-rowid) .
       RUN reopen-query (lv-rowid).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddRecord V-table-Win 
PROCEDURE pAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE BUFFER bf-inv-misc FOR inv-misc.
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  
  IF AVAILABLE inv-head THEN DO:

      RUN oe/d-inmisc.w (?, ROWID(inv-head), "add",OUTPUT lv-rowid).

      FIND FIRST bf-inv-misc  NO-LOCK
          WHERE bf-inv-misc.company EQ inv-head.company
          AND ROWID(bf-inv-misc) EQ  lv-rowid NO-ERROR .
           
      IF AVAIL bf-inv-misc THEN DO:
          RUN reopen-query (lv-rowid).
          RUN refresh-value.
      END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckRecord V-table-Win 
PROCEDURE pCheckRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETE oplCheckRecord AS LOGICAL NO-UNDO.
  
  IF AVAILABLE inv-misc THEN oplCheckRecord = YES.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


