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
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED} 

ASSIGN
 cocode = g_company
 locode = g_loc.

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEFINE VARIABLE cComboList AS CHARACTER NO-UNDO .
DEFINE VARIABLE hPgmReason AS HANDLE NO-UNDO.

DO TRANSACTION:
  {sys/inc/rmrecpt.i}
END.

DEF VAR cRtnChar AS CHAR NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lAdjustReason-log AS LOGICAL NO-UNDO .
RUN sys/ref/nk1look.p (INPUT cocode, "AdjustReason", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
lAdjustReason-log = LOGICAL(cRtnChar) NO-ERROR.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date rm-rctd.i-no ~
rm-rctd.i-name rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty ~
rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.USER-ID rm-rctd.reject-code[1] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty ~
rm-rctd.cost rm-rctd.reject-code[1] 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "A" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "A" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 RECT-5 browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
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
     SIZE 52 BY 1 NO-UNDO.

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
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.rct-date COLUMN-LABEL "Adjustment!Date" FORMAT "99/99/9999":U
      rm-rctd.i-no FORMAT "x(10)":U
      rm-rctd.i-name FORMAT "x(30)":U
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rctd.tag COLUMN-LABEL "Tag" FORMAT "x(20)":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      rm-rctd.cost COLUMN-LABEL "Unit Cost" FORMAT "->>>,>>9.99<<<<":U
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U WIDTH 15
      rm-rctd.reject-code[1] COLUMN-LABEL "Reason" WIDTH 25
      VIEW-AS COMBO-BOX INNER-LINES 10
  ENABLE
      rm-rctd.rct-date
      rm-rctd.i-no
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.tag
      rm-rctd.qty
      rm-rctd.cost
      rm-rctd.reject-code[1]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 79 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 133 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
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
         HEIGHT             = 18.43
         WIDTH              = 154.4.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""A"""
     _FldNameList[1]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" "Adjustment!Date" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.i-no
"rm-rctd.i-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = asi.rm-rctd.i-name
     _FldNameList[4]   > asi.rm-rctd.loc
"rm-rctd.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.tag
"rm-rctd.tag" "Tag" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.pur-uom
"rm-rctd.pur-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.cost
"rm-rctd.cost" "Unit Cost" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.cost-uom
"rm-rctd.cost-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.user-id
"rm-rctd.user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON HELP OF Browser-Table IN FRAME F-Main
DO:
  def var lv-rowid as ROWID no-undo.
  def var ll-tag# as log no-undo.

  IF NOT avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-lock no-error. 
 
 
  ll-help-run = yes.
  case focus:name :
     when "i-no" then do:
           run windows/l-itemi.w (rm-rctd.company, "", "", "R", focus:screen-value in browse {&browse-name}, output char-val).
           if char-val <> "" AND focus:screen-value in browse {&browse-name} NE entry(1,char-val) THEN DO:
              focus:screen-value in browse {&browse-name} = entry(1,char-val).
              RUN new-i-no.
           end.            
     end.
     when "loc" then do:
           run windows/l-rmibn1.w (1, rm-rctd.company, rm-rctd.i-no:screen-value in browse {&browse-name}, rm-rctd.loc:screen-value in browse {&browse-name}, rm-rctd.loc-bin:screen-value in browse {&browse-name}, rm-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).
           FIND rm-bin WHERE ROWID(rm-bin) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL rm-bin AND (rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.loc     OR
                                rm-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name} NE rm-bin.loc-bin OR
                                rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.tag)
           THEN DO:
              ASSIGN
               rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
               rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
               rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag.
              RUN new-bin.
           END.   
     end.
     when "loc-bin" then do:
           run windows/l-rmibn1.w (1, rm-rctd.company, rm-rctd.i-no:screen-value in browse {&browse-name}, rm-rctd.loc:screen-value in browse {&browse-name}, rm-rctd.loc-bin:screen-value in browse {&browse-name}, rm-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).
           FIND rm-bin WHERE ROWID(rm-bin) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL rm-bin AND (rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.loc     OR
                                rm-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name} NE rm-bin.loc-bin OR
                                rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.tag)
           THEN DO:
              ASSIGN
               rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
               rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
               rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag.
              RUN new-bin.
           END.  
     end.
     when "tag" then do:
           run windows/l-rmibnN.w (1, rm-rctd.company, rm-rctd.i-no:screen-value in browse {&browse-name}, rm-rctd.loc:screen-value in browse {&browse-name}, rm-rctd.loc-bin:screen-value in browse {&browse-name}, rm-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).
           FIND rm-bin WHERE ROWID(rm-bin) EQ lv-rowid NO-LOCK NO-ERROR.
           IF AVAIL rm-bin AND (rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.loc     OR
                                rm-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name} NE rm-bin.loc-bin OR
                                rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.tag)
           THEN DO:
              ASSIGN
               rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
               rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
               rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag.
              RUN new-bin.
           END.
     end.

  end case.

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:  /* display calculated field */
  /* def var ii as int.
   ii = if avail rm-rctd then integer(rm-rctd.po-no) else 0.
   
   if avail rm-rctd then    run get-matrix (true).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /* {src/adm/template/brsleave.i}*/
   {est/brsleave.i}  /* same as src but update will be same as add record*/

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


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  IF LASTKEY NE -1 THEN DO:
    /*
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}).
    */
    IF SELF:MODIFIED THEN RUN new-i-no.

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN valid-loc-bin-tag (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN valid-loc-bin-tag (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag */
DO:
  IF LASTKEY NE -1 THEN DO:
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN valid-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) . 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
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
  DEF VAR lv-pur-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR lv-cst-uom LIKE rm-rctd.cost-uom NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-pur-uom = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     lv-cst-uom = rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   rm-rctd.pur-uom  = lv-pur-uom
   rm-rctd.cost-uom = lv-cst-uom.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li-nxt-r-no AS INT INIT 1 NO-UNDO.
 DEF BUFFER bf-rctd FOR rm-rctd.


  /* Code placed here will execute PRIOR to standard behavior. */
/* 07021301 */
/*  FOR EACH bf-rctd NO-LOCK BY bf-rctd.r-no DESCENDING:             */
/*      li-nxt-r-no = bf-rctd.r-no.                                  */
/*      LEAVE.                                                       */
/*  END.                                                             */
/*  FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.              */
/*  li-nxt-r-no = IF AVAIL rm-rcpth AND rm-rcpth.r-no >= li-nxt-r-no */
/*             THEN rm-rcpth.r-no + 1 ELSE li-nxt-r-no.              */
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign rm-rctd.company = cocode
         rm-rctd.r-no    = li-nxt-r-no
         rm-rctd.rita-code = "A"
         rm-rctd.s-num  = 0
         rm-rctd.rct-date = today
         .
  disp rm-rctd.rct-date with browse {&browse-name}. 
  lv-recid = recid(rm-rctd).  
  
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

  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if valid-handle(hd-post-child) then  hd-post-child:sensitive = yes.
            /* value assigned from local-enable-fields*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
     hd-post-child = hd-post:current-window.    
    /*  
     do while valid-handle(hd-post-child):
        ii = ii + 1.
        hd-post-child = hd-post-child:first-child.  /* frame */
       /* if hd-post-child:type = "field-group" 
           then hd-next = hd-post-child:next-sibling.
       */
       message ii valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.   
     end. 
    */ 
     hd-post-child = hd-post-child:first-child.  /* frame */
     hd-post-child = hd-post-child:first-child. /* field-group */
     hd-post-child = hd-post-child:first-child.  /* field */
/*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
*/
     hd-post-child:sensitive = no.
  end.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply "entry" to rm-rctd.rct-date in browse {&browse-name}.
  return no-apply.
 
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
 
  RUN pAdjReason .
  GET FIRST {&browse-name}.

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

  /* when new record created from last row, get error "No rm-rctd" record ava */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.
  
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-reason NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin-tag (3) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin B-table-Win 
PROCEDURE new-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST rm-bin 
        WHERE rm-bin.company EQ cocode
          AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL rm-bin THEN
      ASSIGN
       rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(rm-bin.qty)
       rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(rm-bin.cost).
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-i-no B-table-Win 
PROCEDURE new-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN DO:
      ASSIGN
       rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}   = item.i-name
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = item.cons-uom
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = item.cons-uom.

      FOR EACH rm-bin 
          WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK
          BY rm-bin.loc BY rm-bin.loc-bin BY rm-bin.tag:
        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = rm-bin.tag.

        RUN new-bin.

        LEAVE.
      END.
    END.
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
  {src/adm/template/snd-list.i "rm-rctd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-reason B-table-Win 
PROCEDURE valid-reason :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:

    IF cComboList NE "" AND lAdjustReason-log AND rm-rctd.reject-code[1]:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""
    THEN DO:
      MESSAGE "Please Enter , Adjustment Reason code..." VIEW-AS ALERT-BOX INFO.
      APPLY "entry" TO rm-rctd.reject-code[1] IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST item 
                    WHERE item.company EQ cocode
                      AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND item.i-code  EQ "R")
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin-tag B-table-Win 
PROCEDURE valid-loc-bin-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST rm-bin 
                    WHERE rm-bin.company  EQ cocode
                      AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND (rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     OR ip-int LT 1)
                      AND (rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} OR ip-int LT 2)
                      AND (rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     OR ip-int LT 3))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO rm-rctd.loc IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF rmrecpt-int EQ 1 THEN DO:
    FIND FIRST loadtag WHERE loadtag.company = g_company
                         AND loadtag.item-type = YES
                         AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:
       MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
       rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''.
       APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
       RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  run get-matrix (true).
  return ext-cost.
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAdjReason B-table-Win 
PROCEDURE pAdjReason :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  cComboList = "".
  DEFINE VARIABLE cComboList AS CHARACTER NO-UNDO .
     
     RUN "fg/ReasonCode.p" PERSISTENT SET hPgmReason.
             RUN pBuildReasonCode IN hPgmReason ("ADJ",OUTPUT cComboList).
    DELETE OBJECT hPgmReason.

  DO WITH FRAME {&FRAME-NAME}:   
      rm-rctd.reject-code[1]:LIST-ITEM-PAIRS IN BROWSE {&browse-name} = cComboList .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
