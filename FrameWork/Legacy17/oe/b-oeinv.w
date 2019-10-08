&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  oe\b-oeinv.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\oe\b-oeinv.i
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}

{sys/inc/var.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ls-status AS cha NO-UNDO.

&SCOPED-DEFINE yellowColumnsName inv-head
&SCOPED-DEFINE browse2 oe/j-oeinv.i

DEF VAR li-ord-no LIKE inv-line.ord-no NO-UNDO.
DEF VAR vcText    AS CHAR NO-UNDO INIT 'By:'.

DEF SHARED VARIABLE vfWinOrigW      AS DECIMAL  NO-UNDO.
DEF SHARED VARIABLE vfWinOrigH      AS DECIMAL  NO-UNDO.

/* gdm - 11180901*/
DEF VAR v-sort-name  AS LOG NO-UNDO.
DEF VAR invcopys-cha AS CHAR NO-UNDO.

DEF BUFFER b-cust FOR cust.

{sys/inc/invcopys.i}
IF AVAIL sys-ctrl THEN
   invcopys-cha  = sys-ctrl.char-fld.

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
&Scoped-define INTERNAL-TABLES inv-head

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table inv-head.inv-no ~
inv-head.cust-no inv-head.cust-name inv-head.inv-date inv-head.bol-no ~
f-ordno() @ li-ord-no inv-head.printed inv-head.t-inv-rev ~
getStatus() @ ls-status inv-head.r-no inv-head.company 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH inv-head WHERE ~{&KEY-PHRASE} ~
      AND inv-head.company = cocode and ~
ASI.inv-head.multi-invoice = no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH inv-head WHERE ~{&KEY-PHRASE} ~
      AND inv-head.company = cocode and ~
ASI.inv-head.multi-invoice = no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table inv-head
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table inv-head


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-ordno B-table-Win 
FUNCTION f-ordno RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStatus B-table-Win 
FUNCTION getStatus RETURNS CHARACTER
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
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fi_AutoFindLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Find:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fi_By AS CHARACTER FORMAT "X(256)":U INITIAL "By:" 
      VIEW-AS TEXT 
     SIZE 3.6 BY .62 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 73 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      inv-head SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      inv-head.inv-no COLUMN-LABEL "Invoice #" FORMAT ">>>>>9":U
            WIDTH 16.2 LABEL-BGCOLOR 14
      inv-head.cust-no COLUMN-LABEL "Customer #" FORMAT "x(8)":U
            WIDTH 14.6 LABEL-BGCOLOR 14
      inv-head.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      inv-head.inv-date FORMAT "99/99/9999":U WIDTH 18.6 LABEL-BGCOLOR 14
      inv-head.bol-no COLUMN-LABEL "BOL #" FORMAT ">>>>>>>9":U
            WIDTH 16.2 LABEL-BGCOLOR 14
      f-ordno() @ li-ord-no COLUMN-LABEL "Order#" LABEL-BGCOLOR 14
      inv-head.printed FORMAT "Y/N":U WIDTH 9.6 LABEL-BGCOLOR 14
      inv-head.t-inv-rev COLUMN-LABEL "Invoiced Total" FORMAT "->>,>>>,>>9.99":U
            LABEL-BGCOLOR 14
      getStatus() @ ls-status COLUMN-LABEL "Status" FORMAT "x(8)":U
            WIDTH 13.6 LABEL-BGCOLOR 14
      inv-head.r-no FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      inv-head.company FORMAT "x(3)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 19.33 COL 37 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 19.33 COL 87.4 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 19.38 COL 130.8 HELP
          "CLEAR AUTO FIND Value"
     fi_By AT ROW 19.38 COL 2.6 NO-LABEL
     fi_AutoFindLabel AT ROW 19.57 COL 79 NO-LABEL
     RECT-4 AT ROW 19.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


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
         HEIGHT             = 19.52
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

{Advantzware/WinKit/dataGridProc.i}

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

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2"
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       inv-head.r-no:VISIBLE IN BROWSE Browser-Table = FALSE
       inv-head.company:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR FILL-IN fi_AutoFindLabel IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_By IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.inv-head"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.inv-head.company = cocode and
ASI.inv-head.multi-invoice = no"
     _FldNameList[1]   > ASI.inv-head.inv-no
"inv-head.inv-no" "Invoice #" ? "integer" ? ? ? 14 ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.inv-head.cust-no
"inv-head.cust-no" "Customer #" ? "character" ? ? ? 14 ? ? no ? no no "14.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.inv-head.cust-name
"inv-head.cust-name" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.inv-head.inv-date
"inv-head.inv-date" ? ? "date" ? ? ? 14 ? ? no ? no no "18.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.inv-head.bol-no
"inv-head.bol-no" "BOL #" ? "integer" ? ? ? 14 ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"f-ordno() @ li-ord-no" "Order#" ? ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.inv-head.printed
"inv-head.printed" ? ? "logical" ? ? ? 14 ? ? no ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.inv-head.t-inv-rev
"inv-head.t-inv-rev" "Invoiced Total" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"getStatus() @ ls-status" "Status" "x(8)" ? ? ? ? 14 ? ? no ? no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.inv-head.r-no
"inv-head.r-no" ? ? "integer" ? ? ? 14 ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.inv-head.company
"inv-head.company" ? ? "character" ? ? ? ? ? ? no "" no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:   
  /* gdm - 11180901 */
  IF invcopys-cha NE "" THEN
     RUN set-row-bgcolor.
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

  FIND FIRST b-cust WHERE
       b-cust.company EQ inv-head.company AND
       b-cust.cust-no EQ inv-head.cust-no
       NO-LOCK NO-ERROR.

  IF AVAIL b-cust THEN
     RUN pushpin-image-proc(INPUT b-cust.rec_key).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN oe/rd-invexp.w .

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
  IF AVAIL inv-head THEN APPLY "value-changed" TO BROWSE {&browse-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pushpin-image-proc B-table-Win 
PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.

   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR lv-ord-no AS CHAR NO-UNDO.
   DEF VAR v-ord-no AS INT NO-UNDO.

   FIND FIRST inv-line OF inv-head
        WHERE inv-line.ord-no NE 0
        NO-LOCK NO-ERROR.

   IF NOT AVAIL inv-line THEN
      FIND FIRST inv-misc OF inv-head
           WHERE inv-misc.ord-no NE 0
           NO-LOCK NO-ERROR.

   ASSIGN
      lv-ord-no = IF AVAIL inv-line THEN STRING(inv-line.ord-no)
                  ELSE
                  IF AVAIL inv-misc THEN STRING(inv-misc.ord-no)
                  ELSE
                  STRING(0)

    v-att = CAN-FIND(FIRST asi.attach WHERE
            attach.company = cocode and
            attach.rec_key = ip-rec_key AND
            (attach.est-no eq lv-ord-no OR ATTACH.est-no EQ "")).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachcust-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshBrowse B-table-Win 
PROCEDURE refreshBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE inv-head THEN
  BROWSE {&BROWSE-NAME}:REFRESH().

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

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-row-bgcolor B-table-Win 
PROCEDURE set-row-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-inv-head FOR inv-head.
DEF BUFFER bf-inv-line FOR inv-line.
DEF BUFFER bf-oe-ord   FOR oe-ord.
DEF BUFFER bf-oe-ordl  FOR oe-ordl.

DEF VAR v-totqty LIKE inv-line.qty NO-UNDO.

IF AVAIL inv-head THEN DO:
   ASSIGN li-ord-no = f-ordno().

   IF li-ord-no NE 0 THEN DO:

      FIND FIRST bf-oe-ord NO-LOCK 
        WHERE bf-oe-ord.company EQ inv-head.company 
          AND bf-oe-ord.ord-no  EQ li-ord-no NO-ERROR.

      FOR EACH bf-inv-line OF inv-head NO-LOCK
          BREAK BY bf-inv-line.line
                BY bf-inv-line.i-no:

          IF FIRST-OF(bf-inv-line.i-no) THEN DO:

             ASSIGN v-totqty = 0.
             FOR EACH inv-line FIELDS(ship-qty) NO-LOCK
               WHERE inv-line.company EQ bf-inv-line.company 
                 AND inv-line.inv-no  EQ bf-inv-line.inv-no  
                 AND inv-line.line    EQ bf-inv-line.line
                 AND inv-line.ord-no  EQ bf-inv-line.ord-no
                 AND inv-line.i-no    EQ bf-inv-line.i-no:
                 ASSIGN v-totqty = v-totqty + inv-line.ship-qty.
             END.

             IF v-totqty NE bf-inv-line.qty THEN DO:

                ASSIGN inv-head.inv-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
                       inv-head.cust-no:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 12
                       inv-head.cust-name:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
                       inv-head.inv-date:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12
                       inv-head.bol-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
                       li-ord-no:BGCOLOR IN BROWSE {&BROWSE-NAME}          = 12
                       inv-head.printed:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 12
                       inv-head.t-inv-rev:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
                       ls-status:BGCOLOR IN BROWSE {&BROWSE-NAME}          = 12
                       inv-head.r-no:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 12.

                LEAVE.
             END.
          END.
      END.
   END.
   ASSIGN li-ord-no =  0.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-ordno B-table-Win 
FUNCTION f-ordno RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR lf-ord-no AS INT NO-UNDO.


  RELEASE inv-line.
  RELEASE inv-misc.

  FIND FIRST inv-line OF inv-head NO-LOCK
      WHERE inv-line.ord-no NE 0
      NO-ERROR.
  IF NOT AVAIL inv-line THEN
  FIND FIRST inv-misc OF inv-head NO-LOCK
      WHERE inv-misc.ord-no NE 0
      NO-ERROR.
  lf-ord-no = IF AVAIL inv-line THEN inv-line.ord-no ELSE
              IF AVAIL inv-misc THEN inv-misc.ord-no ELSE 0.

  RETURN lf-ord-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStatus B-table-Win 
FUNCTION getStatus RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.

CASE inv-head.stat:
    WHEN "H" THEN
        cReturn = "On Hold".
    WHEN ""  THEN
        cReturn = "Released".
    WHEN "W" THEN
        cReturn = "Wait/App".
    OTHERWISE
        cReturn = "".
END.

  RETURN cReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

