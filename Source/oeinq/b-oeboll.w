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

&SCOPED-DEFINE cellColumnBrowserName oeinqb-oeboll
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
DEF VAR v-called-setCellColumns AS LOG NO-UNDO.
DEF VAR li-cost AS DEC NO-UNDO.
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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ar-invl
&Scoped-define FIRST-EXTERNAL-TABLE ar-invl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-invl.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-boll

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-boll.bol-no oe-boll.i-no ~
oe-boll.job-no oe-boll.job-no2 oe-boll.loc oe-boll.loc-bin oe-boll.tag ~
oe-boll.cases oe-boll.qty-case oe-boll.partial oe-boll.weight ~
oe-boll.s-code get-cost() @ li-cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-boll WHERE oe-boll.company = ar-invl.company ~
  AND oe-boll.b-no = ar-invl.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-boll WHERE oe-boll.company = ar-invl.company ~
  AND oe-boll.b-no = ar-invl.b-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-boll
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-boll


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS printBOL printSBOL Browser-Table RECT-4 ~
browse-order auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order FI_moveCol auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON printBOL 
     IMAGE-UP FILE "Graphics/32x32/printer_gearwheel.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 9 BY 2.14 TOOLTIP "Print BOL".

DEFINE BUTTON printSBOL 
     IMAGE-UP FILE "Graphics/32x32/document_check_edit.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 9 BY 2.14 TOOLTIP "Print Signed BOL".

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 57 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-boll
    FIELDS(oe-boll.bol-no
      oe-boll.i-no
      oe-boll.job-no
      oe-boll.job-no2
      oe-boll.loc
      oe-boll.loc-bin
      oe-boll.tag
      oe-boll.cases
      oe-boll.qty-case
      oe-boll.partial
      oe-boll.weight
      oe-boll.s-code) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-boll.bol-no COLUMN-LABEL "Bol#" FORMAT ">>>>>>>9":U
      oe-boll.i-no COLUMN-LABEL "Item#" FORMAT "x(15)":U
      oe-boll.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 11
      oe-boll.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 4
      oe-boll.loc FORMAT "x(5)":U
      oe-boll.loc-bin COLUMN-LABEL "Bin Loc" FORMAT "x(8)":U
      oe-boll.tag COLUMN-LABEL "Tag" FORMAT "x(23)":U
      oe-boll.cases COLUMN-LABEL "Units" FORMAT "->>>,>>Z":U
      oe-boll.qty-case COLUMN-LABEL "Qty/Unit" FORMAT "->>>,>>Z":U
      oe-boll.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
      oe-boll.weight COLUMN-LABEL "Weight" FORMAT "->>>,>>9":U
      oe-boll.s-code COLUMN-LABEL "B/S/I/T" FORMAT "x":U
      get-cost() @ li-cost
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 149 BY 16.19
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     printBOL AT ROW 18.62 COL 131
     printSBOL AT ROW 18.62 COL 141
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.43 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     FI_moveCol AT ROW 17.43 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     auto_find AT ROW 17.43 COL 93 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.43 COL 136 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.43 COL 2
     RECT-4 AT ROW 17.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.ar-invl
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
         HEIGHT             = 19.76
         WIDTH              = 149.
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
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       li-cost:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI_moveCol:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.oe-boll WHERE asi.ar-invl <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "asi.oe-boll.company = asi.ar-invl.company
  AND asi.oe-boll.b-no = asi.ar-invl.b-no"
     _FldNameList[1]   > asi.oe-boll.bol-no
"oe-boll.bol-no" "Bol#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.oe-boll.i-no
"oe-boll.i-no" "Item#" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.oe-boll.job-no
"oe-boll.job-no" "Job#" ? "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.oe-boll.job-no2
"oe-boll.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = asi.oe-boll.loc
     _FldNameList[6]   > asi.oe-boll.loc-bin
"oe-boll.loc-bin" "Bin Loc" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.oe-boll.tag
"oe-boll.tag" "Tag" "x(23)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.oe-boll.cases
"oe-boll.cases" "Units" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.oe-boll.qty-case
"oe-boll.qty-case" "Qty/Unit" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.oe-boll.partial
"oe-boll.partial" "Partial" "->>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.oe-boll.weight
"oe-boll.weight" "Weight" "->>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.oe-boll.s-code
"oe-boll.s-code" "B/S/I/T" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"get-cost() @ li-cost" ? ? ? ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME printBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL printBOL B-table-Win
ON CHOOSE OF printBOL IN FRAME F-Main
DO:
  IF AVAILABLE oe-boll THEN DO:
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN
    RUN custom/setUserPrint.p (g_company,'oe-boll_.',
                               'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted',
                               oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +
                               STRING(oe-boll.bol-no) + ',' + STRING(oe-boll.bol-no) +
                               ',,99999999,' + STRING(oe-bolh.printed) + ',' + STRING(oe-bolh.posted)).
    RUN listobjs/oe-boll_.w.
    {methods/run_link.i "CONTAINER-SOURCE" "moveToTop"}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME printSBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL printSBOL B-table-Win
ON CHOOSE OF printSBOL IN FRAME F-Main
DO:
  IF AVAILABLE oe-boll THEN DO:
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN DO:
       find first sys-ctrl where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "BOLSIGN"
                           no-lock no-error.
       if not avail sys-ctrl then do transaction:
          create sys-ctrl.
          ASSIGN sys-ctrl.company = cocode
                 sys-ctrl.name    = "BOLSIGN"
                 sys-ctrl.descrip = "Signed Bill of Lading Path"
                 sys-ctrl.char-fld = "C:\tmp\".
          MESSAGE sys-ctrl.descrip
                  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                  UPDATE sys-ctrl.log-fld.
       end.
       DEF VAR v-bolimage AS cha NO-UNDO.
       v-bolimage =  sys-ctrl.char-fld + STRING(oe-bolh.bol-no) + ".jpg" .
       IF SEARCH(v-bolimage) = ? THEN DO:
          IF SEARCH(sys-ctrl.char-fld + STRING(oe-bolh.bol-no) + ".pdf") = ? THEN DO:
            MESSAGE "Signed receipt is not available. Cannot locate files:" SKIP
                sys-ctrl.char-fld + STRING(oe-bolh.bol-no) + ".jpg" SKIP
                sys-ctrl.char-fld + STRING(oe-bolh.bol-no) + ".pdf"
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
          END.
       END.
       
       RUN custom/setUserPrint.p (g_company,'r-bolpr2.',
                               'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted',
                               oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +
                               STRING(oe-boll.bol-no) + ',' + STRING(oe-boll.bol-no) +
                               ',,99999999,' + STRING(oe-bolh.printed) + ',' + STRING(oe-bolh.posted)).

       RUN oerep/r-bolpr2.w.
       {methods/run_link.i "CONTAINER-SOURCE" "moveToTop"}

       
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
&SCOPED-DEFINE cellColumnDat oeinqb-oeboll
{methods/browsers/setCellColumns.i}

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
  {src/adm/template/row-list.i "ar-invl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-invl"}

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
  IF v-called-setCellColumns = NO THEN
  DO:
     RUN setCellColumns.
     v-called-setCellColumns = YES.
  END.

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
     Browser-Table:COLUMN-MOVABLE = v-col-move
     Browser-Table:COLUMN-RESIZABLE = v-col-move
     v-col-move = NOT v-col-move.
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
  {src/adm/template/snd-list.i "ar-invl"}
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF li-cost:VISIBLE IN BROWSE Browser-Table EQ NO THEN
     RETURN 0.
    
  DEF VAR v-t-cost AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR v-uom AS CHAR NO-UNDO.
  def var v-cost AS DEC DECIMALS 4 extent 4 NO-UNDO.
  def var v-cost-m AS DEC DECIMALS 4 extent 4 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  find first job-hdr WHERE
       job-hdr.company eq oe-boll.company AND
       job-hdr.job-no  eq oe-boll.job-no AND
       job-hdr.job-no2 eq oe-boll.job-no2 AND
       job-hdr.i-no    eq oe-boll.i-no
       no-lock no-error.

  find first itemfg WHERE
       itemfg.company EQ oe-boll.company AND
       itemfg.i-no eq oe-boll.i-no
       NO-LOCK no-error.

    find first fg-bin
        where fg-bin.company eq oe-boll.company
          and fg-bin.i-no    eq oe-boll.i-no
          and fg-bin.tag     eq oe-boll.tag
          and fg-bin.loc     eq oe-boll.loc
          and fg-bin.loc-bin eq oe-boll.loc-bin
          and fg-bin.job-no  eq oe-boll.job-no
          and fg-bin.job-no2 eq oe-boll.job-no2
        no-lock no-error.
  
    if avail fg-bin and fg-bin.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = fg-bin.std-lab-cost
       v-cost-m[2] = fg-bin.std-fix-cost
       v-cost-m[3] = fg-bin.std-var-cost
       v-cost-m[4] = fg-bin.std-mat-cost
       v-uom       = fg-bin.pur-uom.
       
    else
    if avail job-hdr and job-hdr.std-tot-cost ne 0 then
      assign
       v-cost-m[1] = job-hdr.std-lab-cost
       v-cost-m[2] = job-hdr.std-fix-cost
       v-cost-m[3] = job-hdr.std-var-cost
       v-cost-m[4] = job-hdr.std-mat-cost
       v-uom       = "M".
       
    else   
      assign
       v-cost-m[1] = itemfg.std-lab-cost
       v-cost-m[2] = itemfg.std-fix-cost
       v-cost-m[3] = itemfg.std-var-cost
       v-cost-m[4] = itemfg.std-mat-cost.

    if v-uom eq "" then
       v-uom = itemfg.prod-uom.

    do i = 1 to 4:

       if v-uom ne "M" then
          run sys/ref/convcuom3.p(cocode,v-uom, "M", 0, 0, 0, 0,
                                 v-cost-m[i], output v-cost-m[i]).
       
       v-cost[i] = v-cost[i] + (v-cost-m[i] * oe-boll.qty / 1000).
    end.
  
  do i = 1 to 4:
     v-cost[i] = v-cost[i] / (oe-boll.qty / 1000).
    
     if v-cost[i] eq ? then v-cost[i] = 0.
  end.

  v-t-cost = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4].

  if v-t-cost eq ? then
     v-t-cost = 0.

  RETURN v-t-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

