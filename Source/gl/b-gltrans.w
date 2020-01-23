&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  gl/b-gltrans.w

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
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}


&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHAR NO-UNDO.

DEFINE VARIABLE char-hdl                AS CHAR NO-UNDO.
DEFINE VARIABLE columnCount             AS INT  NO-UNDO.
DEFINE VARIABLE idx                     AS INT  NO-UNDO.
DEFINE VARIABLE ilogic                  AS LOG  NO-UNDO.
DEFINE VARIABLE ll-show-all             AS LOG  NO-UNDO.
DEFINE VARIABLE ll-sort-asc             AS LOG  INIT NO NO-UNDO.
DEFINE VARIABLE lv-sort-by              AS CHAR INIT "trnum" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab          AS CHAR INIT "Run" NO-UNDO.
DEFINE VARIABLE lvFirstRowID            AS ROWID NO-UNDO.
DEFINE VARIABLE lvLastRowID             AS ROWID NO-UNDO.
DEFINE VARIABLE phandle                 AS HANDLE NO-UNDO.
DEFINE VARIABLE useColors               AS CHAR NO-UNDO.
DEFINE VARIABLE v-called-setCellColumns AS LOG  NO-UNDO.
DEFINE VARIABLE v-col-move              AS LOG  INIT YES NO-UNDO.
DEFINE VARIABLE cAccDscr                  AS CHARACTER NO-UNDO .

DEF TEMP-TABLE tt-gltrans NO-UNDO LIKE gltrans
    FIELD rdRowid AS ROWID 
    .
&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING
&SCOPED-DEFINE yellowColumnsName tt-gltrans

ASSIGN 
    cocode = g_company
    locode = g_loc.


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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES gltrans tt-gltrans

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-gltrans.trnum tt-gltrans.tr-date ~
tt-gltrans.actnum fGetAccDscr() @ cAccDscr tt-gltrans.tr-dscr tt-gltrans.jrnl ~
tt-gltrans.yr tt-gltrans.period tt-gltrans.tr-amt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-gltrans WHERE tt-gltrans.company EQ cocode NO-LOCK , ~
EACH gltrans WHERE gltrans.company EQ cocode AND rowid(gltrans) EQ tt-gltrans.rdRowid NO-LOCK  ~
~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH tt-gltrans WHERE tt-gltrans.company EQ cocode , ~
EACH gltrans WHERE gltrans.company EQ cocode AND rowid(gltrans) EQ tt-gltrans.rdRowid NO-LOCK ~{&SORTBY-PHRASE} .
&Scoped-define TABLES-IN-QUERY-Browser-Table gltrans tt-gltrans
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table gltrans
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table tt-gltrans
/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb_itemType fi_runn fi_journal fi_acc-no ~
fi_year fi_period btn_go btn_show Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS cb_itemType fi_runn fi_journal fi_acc-no ~
fi_year fi_period fi_sortBy FI_moveCol

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetAccDscr B-table-Win 
FUNCTION fGetAccDscr RETURNS CHARACTER
  ( /*ipiLevel AS INTEGER*/ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1
     FONT 6.

DEFINE VARIABLE cb_itemType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Issue" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS " ","All",
                     "Inactive Account","I",
                     "Out of balance transaction","O",
                     "Date outside period","D",
                     "Invalid Period","P"
     DROP-DOWN-LIST
     SIZE 27 BY 1 TOOLTIP "Select Type Filter" NO-UNDO.

DEFINE VARIABLE fi_acc-no AS CHARACTER FORMAT "X(25)":U 
     LABEL "Account" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_journal AS CHARACTER FORMAT "X(10)":U 
     LABEL "Journal" 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_runn AS INTEGER FORMAT ">>>>>":U INITIAL 0 
     LABEL "Run#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sortBy AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34.6 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Year" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR
      tt-gltrans,
      gltrans SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-gltrans.trnum COLUMN-LABEL "Run" FORMAT ">>>>>":U WIDTH 10.2
            LABEL-BGCOLOR 14
      tt-gltrans.tr-date COLUMN-LABEL "Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      tt-gltrans.actnum COLUMN-LABEL "Account" FORMAT "x(25)":U WIDTH 14.4
            LABEL-BGCOLOR 14
      fGetAccDscr() @ cAccDscr COLUMN-LABEL "Account Dscription" FORMAT "x(30)":U
            WIDTH 26.2 
      tt-gltrans.tr-dscr COLUMN-LABEL "Dscription" FORMAT "x(35)":U
            WIDTH 26.2 LABEL-BGCOLOR 14
      tt-gltrans.jrnl FORMAT "x(8)":U WIDTH 12.2 COLUMN-FONT 14 LABEL-BGCOLOR 14
      tt-gltrans.yr FORMAT ">>>>":U WIDTH 7.2 LABEL-BGCOLOR 14
      tt-gltrans.period FORMAT ">>>>":U LABEL-BGCOLOR 14
      tt-gltrans.tr-amt FORMAT "->>,>>>,>>9.99":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 143 BY 16.86
         FONT 2 ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cb_itemType AT ROW 1.00 COL 107 COLON-ALIGNED WIDGET-ID 40
     fi_runn AT ROW 1.00 COL 7 COLON-ALIGNED WIDGET-ID 16
     fi_journal AT ROW 1.00 COL 27 COLON-ALIGNED WIDGET-ID 2
     fi_acc-no AT ROW 1.00 COL 49.8 COLON-ALIGNED
     fi_year AT ROW 1.00 COL 74 COLON-ALIGNED WIDGET-ID 56
     fi_period AT ROW 1.00 COL 92.8 COLON-ALIGNED WIDGET-ID 58
     btn_go AT ROW 2.38 COL 1.8 WIDGET-ID 4
     btn_show AT ROW 2.38 COL 15.2 WIDGET-ID 10
     fi_sortBy AT ROW 2.38 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     Browser-Table AT ROW 3.65 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Click on Column Title to Sort" VIEW-AS TEXT
          SIZE 29 BY .95 AT ROW 2.56 COL 94 WIDGET-ID 14
    FI_moveCol AT ROW 2.38 COL 124 COLON-ALIGNED NO-LABEL WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 1 
         DEFAULT-BUTTON btn_go WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
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
         HEIGHT             = 19.86
         WIDTH              = 143.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}
{custom/yellowColumns.i}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table fi_sortBy FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sortBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-gltrans WHERE tt-gltrans.company EQ cocode NO-LOCK , ~
EACH gltrans WHERE gltrans.company EQ cocode AND rowid(gltrans) EQ tt-gltrans.rdRowid NO-LOCK
     ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > "_<CALC>"
"tt-gltrans.trnum" "Run" ">>>>>" "integer" ? ? ? 14 ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > tt-gltrans.tr-date
"tt-gltrans.tr-date" "Date" ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > tt-gltrans.actnum
"tt-gltrans.actnum" "Account" ? "character" ? ? ? 14 ? ? no ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"fGetAccDscr() @ cAccDscr" "Account Dscription" "x(30)" "character" ? ? ? 14 ? ? no ? no no "26.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > tt-gltrans.tr-dscr
"tt-gltrans.tr-dscr" "Dscription" ? "character" ? ? ? 14 ? ? no ? no no "26.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > tt-gltrans.jrnl
"tt-gltrans.jrnl" ? ? ? ? ? 14 ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > tt-gltrans.yr
"tt-gltrans.yr" ? ">>>>" "integer" ? ? ? 14 ? ? no ? no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > tt-gltrans.period
"tt-gltrans.period" ? ">>>>" "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > tt-gltrans.tr-amt
"tt-gltrans.tr-amt" ? "->>,>>>,>>9.99" "Decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEFINE VARIABLE phandle  AS HANDLE NO-UNDO.
    DEFINE VARIABLE char-hdl AS cha    NO-UNDO.
        {methods/run_link.i "container-source" "select-page" "(2)"}
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
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO: 
   RUN startSearch.
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

   RUN dept-pan-image-proc.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                fi_journal                                               
                cb_itemType                                                 
                fi_runn  
                fi_acc-no
                fi_period
                fi_year
                
                .     
           IF ll-show-all  THEN do:
               RUN set-defaults.
              ll-show-all = NO.
           END.

            RUN build-inquiry.
            RUN openQuery.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
        DO WITH FRAME {&FRAME-NAME}:
            ll-show-all = YES.
            APPLY "choose" TO btn_go.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_acc-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_acc-no B-table-Win
ON HELP OF fi_acc-no IN FRAME F-Main /* Account */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

    RUN windows/l-est.w (g_company,g_loc,"", OUTPUT char-val).
    IF char-val <> "" THEN DO:
        FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
        IF AVAIL eb THEN 
            fi_acc-no:screen-value = eb.est-no.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_journal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_journal B-table-Win
ON HELP OF fi_journal IN FRAME F-Main /* Journal */
DO:

  DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

  RUN system/openlookup.p (g_company, "vend-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN fi_journal:SCREEN-VALUE = cMainField. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_period B-table-Win
ON HELP OF fi_period IN FRAME F-Main /* Period */
DO:

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_runn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_runn B-table-Win
ON HELP OF fi_runn IN FRAME F-Main /* Run# */
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_year B-table-Win
ON HELP OF fi_year IN FRAME F-Main /* Year */
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
/*{methods/ctrl-a_browser.i}*/
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat browsers-b-gltrans

{methods/browsers/setCellColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item B-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-pan-image-proc B-table-Win 
PROCEDURE dept-pan-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   FIND FIRST notes WHERE notes.rec_key = gltrans.rec_key
       NO-LOCK NO-ERROR.

   IF AVAIL notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'spec-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
   
  
    RUN gl/gltrans-Exp.w .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns B-table-Win 
PROCEDURE getCellColumns :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    columnCount = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
    DO idx = 1 TO columnCount:
        cellColumn[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-excel B-table-Win 
PROCEDURE import-excel :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

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
  
    DO WITH FRAME {&FRAME-NAME}:
        fi_sortBy:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
            TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
        cb_itemType:SCREEN-VALUE = cb_itemType .                   
    END.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first B-table-Win 
PROCEDURE local-get-first :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
    {methods/template/local/setvalue.i}

    RUN dept-pan-image-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last B-table-Win 
PROCEDURE local-get-last :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    {methods/template/local/setvalue.i}
     RUN dept-pan-image-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next B-table-Win 
PROCEDURE local-get-next :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    {methods/template/local/setvalue.i}
    RUN dept-pan-image-proc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev B-table-Win 
PROCEDURE local-get-prev :
/*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    {methods/template/local/setvalue.i}
    RUN dept-pan-image-proc.

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
    RUN build-inquiry.
    

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    RUN setCellColumns.
    /* Code placed here will execute AFTER standard behavior.    */
    RUN openQuery.

    FI_moveCol = "Sort".
    DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

    APPLY 'ENTRY':U TO fi_runn IN FRAME {&FRAME-NAME}.

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
    IF AVAILABLE gltrans THEN APPLY "value-changed" TO BROWSE {&browse-name}.

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
            Browser-Table:COLUMN-MOVABLE   = v-col-move
            Browser-Table:COLUMN-RESIZABLE = v-col-move
            v-col-move                     = NOT v-col-move.
        FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
        DISPLAY FI_moveCol.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipNavType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opNavType AS CHARACTER NO-UNDO.
 
    IF ipNavType NE '' THEN
        CASE ipNavType:
            WHEN 'F' THEN RUN dispatch ('get-first':U).
            WHEN 'L' THEN RUN dispatch ('get-last':U).
            WHEN 'N' THEN RUN dispatch ('get-next':U).
            WHEN 'P' THEN RUN dispatch ('get-prev':U).
            WHEN 'G' THEN RUN lookup-eb.
        END CASE.
    
    IF ROWID(gltrans) EQ lvLastRowID THEN
        opNavType = 'L'.
      
    IF ROWID(gltrans) EQ lvFirstRowID THEN
        opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query2 B-table-Win 
PROCEDURE repo-query2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-Run AS INTEGER NO-UNDO.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.

    RUN set-defaults.

    fi_runn = ip-Run .
    fi_runn:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(ip-Run) .
    RUN build-inquiry.
    {&open-query-{&browse-name}}
    
   /* REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.*/

    RUN dispatch IN this-procedure ("row-changed").
 
    APPLY "value-changed" TO BROWSE {&browse-name}.

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
  {src/adm/template/snd-list.i "gltrans"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fi_journal:SCREEN-VALUE  = ""
            cb_itemType:SCREEN-VALUE = "All"
            fi_runn:SCREEN-VALUE     = ""
            fi_acc-no:SCREEN-VALUE   = "" 
            fi_journal
            cb_itemType
            fi_runn
            fi_acc-no
            fi_period
            fi_year
            cb_itemType
            
            .     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBrowseFocus B-table-Win 
PROCEDURE setBrowseFocus :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
       IF AVAIL gltrans THEN do:
           RUN dept-pan-image-proc.
       END.
  END.
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
  DEFINE BUFFER bf-gltrans FOR gltrans .

  EMPTY TEMP-TABLE tt-gltrans.

  IF cb_itemType EQ "I" THEN DO:

      FOR EACH account WHERE account.company = g_company
                       AND account.inactive EQ YES
                       AND account.actnum BEGINS fi_acc-no NO-LOCK ,
          EACH bf-gltrans  NO-LOCK
          where bf-gltrans.company eq cocode
            AND bf-gltrans.actnum EQ account.actnum
            AND (bf-gltrans.trnum EQ fi_runn OR fi_runn EQ 0 )
            AND (bf-gltrans.yr EQ fi_year OR fi_year EQ 0 )
            AND (bf-gltrans.period EQ fi_period OR fi_period EQ 0 )
            AND {system/brMatches.i  bf-gltrans.jrnl fi_journal}
          by bf-gltrans.trnum DESC:
          CREATE tt-gltrans.
    
          BUFFER-COPY bf-gltrans  TO tt-gltrans .
          ASSIGN tt-gltrans.rdRowid = ROWID(bf-gltrans) .
      END.
  END.
  ELSE  IF cb_itemType EQ "O" THEN DO:

  END.
  ELSE  IF cb_itemType EQ "D" THEN DO:
      for each bf-gltrans  NO-LOCK
          where bf-gltrans.company eq cocode
            AND (bf-gltrans.trnum EQ fi_runn OR fi_runn EQ 0 )
            AND (bf-gltrans.yr EQ fi_year OR fi_year EQ 0 )
            AND bf-gltrans.period NE MONTH(bf-gltrans.tr-date)
            AND {system/brMatches.i  bf-gltrans.jrnl fi_journal}
            AND {system/brMatches.i  bf-gltrans.actnum fi_acc-no}
          by bf-gltrans.trnum DESC:
          CREATE tt-gltrans.
    
          BUFFER-COPY bf-gltrans  TO tt-gltrans .
          ASSIGN tt-gltrans.rdRowid = ROWID(bf-gltrans) .
          
      end.

  END.
  ELSE  IF cb_itemType EQ "P" THEN DO:
      for each bf-gltrans  NO-LOCK
          where bf-gltrans.company eq cocode
            AND (bf-gltrans.trnum EQ fi_runn OR fi_runn EQ 0 )
            AND (bf-gltrans.yr EQ fi_year OR fi_year EQ 0 )
            AND bf-gltrans.period EQ 0
            AND {system/brMatches.i  bf-gltrans.jrnl fi_journal}
            AND {system/brMatches.i  bf-gltrans.actnum fi_acc-no}
          by bf-gltrans.trnum DESC:
          CREATE tt-gltrans.
    
          BUFFER-COPY bf-gltrans  TO tt-gltrans .
          ASSIGN tt-gltrans.rdRowid = ROWID(bf-gltrans) .
          
      end.

  END.
  ELSE do:
      for each bf-gltrans  NO-LOCK
          where bf-gltrans.company eq cocode
            AND (bf-gltrans.trnum EQ fi_runn OR fi_runn EQ 0 )
            AND (bf-gltrans.yr EQ fi_year OR fi_year EQ 0 )
            AND (bf-gltrans.period EQ fi_period OR fi_period EQ 0 )
            AND {system/brMatches.i  bf-gltrans.jrnl fi_journal}
            AND {system/brMatches.i  bf-gltrans.actnum fi_acc-no}
          by bf-gltrans.trnum DESC:
          CREATE tt-gltrans.
    
          BUFFER-COPY bf-gltrans  TO tt-gltrans .
          ASSIGN tt-gltrans.rdRowid = ROWID(bf-gltrans) .
          
      end.
  END.
 
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
  DEF OUTPUT PARAM op-company LIKE gltrans.company NO-UNDO.
  DEF OUTPUT PARAM op-tr-num  LIKE gltrans.trnum  NO-UNDO.

  ASSIGN
   op-company = g_company
   op-tr-num  = IF AVAIL tt-gltrans THEN tt-gltrans.trnum ELSE 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetAccDscr B-table-Win 
FUNCTION fGetAccDscr RETURNS CHARACTER
  ( /*ipiLevel AS INTEGER*/ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    FIND FIRST account  NO-LOCK
        WHERE account.company EQ cocode 
          AND account.actnum EQ  tt-gltrans.actnum NO-ERROR .
    IF AVAIL account THEN
          lc-result = account.dscr .
   
    RETURN lc-result.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

