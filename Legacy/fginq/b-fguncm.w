&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
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
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE tt-report LIKE report
    FIELD required AS INT
    FIELD variance AS INT.

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.

{sa/sa-sls01.i}

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
&Scoped-define INTERNAL-TABLES tt-report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-report.key-02 tt-report.required tt-report.variance   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table   
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-report NO-LOCK
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-report


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_i-no fi_date btn_go btn_print ~
Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_i-no fi_name fi_date v-ono v-onh v-net ~
v-bal v-opo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_print 
     LABEL "&Print" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-bal AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "BalToShip" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-net AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "NetQty" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-onh AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "QtyOnHand" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-ono AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "Ordered" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-opo AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "OpenPOs" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-report.key-02   FORMAT "x(10)"          COLUMN-LABEL "Release Date"
      tt-report.required FORMAT "->,>>>,>>>,>>>" COLUMN-LABEL "Required"
      tt-report.variance FORMAT "->,>>>,>>>,>>>" COLUMN-LABEL "Variance"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 16.43
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_i-no AT ROW 1.48 COL 11 COLON-ALIGNED
     fi_name AT ROW 1.48 COL 34 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABEL
     fi_date AT ROW 1.48 COL 100 COLON-ALIGNED
     btn_go AT ROW 1.48 COL 121
     btn_print AT ROW 1.48 COL 135
     Browser-Table AT ROW 4.33 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     v-ono AT ROW 2.91 COL 10 COLON-ALIGNED
     v-onh AT ROW 2.91 COL 41 COLON-ALIGNED
     v-net AT ROW 2.91 COL 68 COLON-ALIGNED
     v-bal AT ROW 2.91 COL 98 COLON-ALIGNED
     v-opo AT ROW 2.91 COL 127 COLON-ALIGNED
     "Text 2" VIEW-AS TEXT
          SIZE 1 BY .71 AT ROW 1 COL 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser Template
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
         HEIGHT             = 20
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
/* BROWSE-TAB Browser-Table btn_print F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-net IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-ono IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-opo IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartBrowserCues" B-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartBrowser,uib,49266
A SmartBrowser is a procedure object that retrieves and visualizes data using a browse.

CREATING A MASTER

Step 1
If necessary, specify an external table (steps 1a-1c). 

An external table completes join criteria for a query. For example, a query on 'Order OF Customer' requires the external table Customer. The external table is supplied by another procedure object--typically a SmartBrowser or SmartViewer.

Step 1a
In the UIB main window, Choose the Procedure button.

Step 1b
In the Procedure Settings dialog, choose Add.

Step 1c
From the Table Selector dialog, select the external table.

Step 2 
Double-click the browse to invoke the Query Builder.
    
Step 3
Using the Query Builder, specify the tables and fields for the browse.

Step 4 [Optional]
In the Code Section Editor, change the Foreign Keys and/or Sort Options for the browse query. Use the "List..." button to access these sections.
  
Step 5
Save and close the SmartBrowser master.

INSERTING AN INSTANCE

Step 1
Open or create a SmartContainer, such as a SmartWindow.
   
Step 2 
Choose the SmartBrowser master from the Object Palette.

Step 3
Draw the SmartBrowser instance into the SmartContainer.
   
Step 4
Add all necessary SmartLinks between the SmartBrowser and other SmartObjects. 

During assembly, the PROGRESS Advisor suggests links and creates them for you. However, you can also add and remove SmartLinks with the SmartLinks dialog box. To access this dialog box, choose the Procedure button from the UIB main window. Then choose the SmartLinks button from the Procedure Settings dialog box.
*/
/* _UIB-CODE-BLOCK-END */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_i-no
     fi_date.
  END.

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_print B-table-Win
ON CHOOSE OF btn_print IN FRAME F-Main /* Print */
DO:
  {sys/form/r-topw.f}

  DEF VAR lv-i-no LIKE itemfg.i-no   NO-UNDO.
  DEF VAR lv-name LIKE itemfg.i-name NO-UNDO.
  DEF VAR lv-date AS   DATE          NO-UNDO.

  FORM SKIP(1)
       lv-i-no LABEL "Item"             COLON 30
       lv-name NO-LABEL
       lv-date LABEL "From Ship Date"   COLON 30
       v-ono   LABEL "Ordered"          COLON 30
       v-onh   LABEL "Qty On Hand"      COLON 30
       v-net   LABEL "Net"              COLON 30
       v-bal   LABEL "Bal To Ship"      COLON 30
       v-opo   LABEL "Open PO Qty"      COLON 30

      WITH STREAM-IO WIDTH 80 FRAME fg-uncmph SIDE-LABELS NO-UNDERLINE PAGE-TOP
           TITLE "       COMPARISON OF SCHEDULED SHIPMENTS VS. QUANTITY ON HAND       ".


  FIND FIRST tt-report NO-ERROR.

  IF AVAIL tt-report THEN DO WITH FRAME fg-uncmph:
    SESSION:SET-WAIT-STATE ("general").
        
    {sys/inc/print1.i}
    {sys/inc/outprint.i 56}

    DISPLAY fi_i-no @ lv-i-no
            fi_name @ lv-name
            fi_date @ lv-date
            v-ono
            v-onh
            v-net
            v-bal
            v-opo.

    FOR EACH tt-report:
      DISPLAY SPACE(20)
              tt-report.key-02   FORMAT "x(10)"          COLUMN-LABEL "Release Date"
              tt-report.required FORMAT "->,>>>,>>>,>>>" COLUMN-LABEL "Required"
              tt-report.variance FORMAT "->,>>>,>>>,>>>" COLUMN-LABEL "Variance"
          WITH FRAME fg-uncmp NO-BOX STREAM-IO WIDTH 80 DOWN.
      DOWN WITH FRAME fg-uncmp.
    END.

    OUTPUT CLOSE.

    SESSION:SET-WAIT-STATE ("").

    RUN scr-rpt.w (list-name,TRIM(FRAME fg-uncmph:TITLE),11,"P"). /* open file-name, title */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_date B-table-Win
ON LEAVE OF fi_date IN FRAME F-Main /* From Ship Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON ENTRY OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  IF lv-save-char NE {&self-name}:SCREEN-VALUE THEN
    APPLY "value-changed" TO {&self-name}.

  lv-save-char = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  FIND itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    BEGINS fi_i-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN fi_name:SCREEN-VALUE = itemfg.i-name.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN
 SESSION:DATA-ENTRY-RETURN = YES
 fi_date                   = TODAY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile B-table-Win 
PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-rel     LIKE v-ono NO-UNDO.
  DEF VAR v-var     LIKE v-ono NO-UNDO.
  DEF VAR v-hld-qty AS   DEC   NO-UNDO.
  DEF VAR v-loc     LIKE oe-boll.loc NO-UNDO.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fi_i-no
      NO-LOCK.
  fi_name = itemfg.i-name.

  v-onh = 0.
  for each fg-bin
      where fg-bin.company eq cocode
        and fg-bin.i-no    eq itemfg.i-no
      no-lock:
    v-onh = v-onh + fg-bin.qty.
  end.
  v-net = v-onh.

  assign
   v-rel = 0
   v-ono = 0
   v-bal = 0.

  for each oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.i-no    eq itemfg.i-no
      use-index item no-lock,

      first oe-ord of oe-ordl
      where oe-ord.stat     ne "D"
        and oe-ord.stat     ne "C"
        and oe-ord.stat     ne "Z"
      no-lock,

      each oe-rel
      where oe-rel.company  eq cocode
        and oe-rel.ord-no   eq oe-ord.ord-no
        and oe-rel.i-no     eq oe-ordl.i-no
        and oe-rel.line     eq oe-ordl.line
      no-lock

      break by oe-rel.rel-date:

    assign
     v-bal = v-bal + oe-rel.qty
     v-rel = v-rel + oe-rel.qty
     v-ono = v-ono + oe-rel.qty
     v-net = v-net - oe-rel.qty.

    RELEASE oe-bolh.

    IF oe-rel.link-no NE 0 THEN
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-rel.company
          AND oe-rell.ord-no  EQ oe-rel.ord-no
          AND oe-rell.r-no    EQ oe-rel.link-no
          AND oe-rell.i-no    EQ oe-rel.i-no
          AND oe-rell.line    EQ oe-rel.line
        NO-LOCK,
        FIRST oe-relh
        WHERE oe-relh.r-no   EQ oe-rell.r-no
          AND oe-relh.posted EQ YES
        NO-LOCK,
        EACH oe-boll
        WHERE oe-boll.company  EQ oe-rell.company
          AND oe-boll.ord-no   EQ oe-rell.ord-no
          AND oe-boll.line     EQ oe-rell.line
          AND oe-boll.i-no     EQ oe-rell.i-no
          AND oe-boll.r-no     EQ oe-rell.r-no
          AND oe-boll.rel-no   EQ oe-rell.rel-no
          AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
        NO-LOCK,
        FIRST oe-bolh
        WHERE oe-bolh.b-no   EQ oe-boll.b-no
          AND oe-bolh.posted EQ YES
        NO-LOCK:
      LEAVE.
    END.

    if avail oe-bolh then
      assign
       v-bal = v-bal - oe-rell.qty
       v-rel = v-rel - oe-rell.qty
       v-net = v-net + oe-rell.qty.

    if last-of(oe-rel.rel-date)  and
       oe-rel.rel-date ge fi_date and
       v-rel gt 0                then do:
      create tt-report.
      assign
       tt-report.term-id = v-term
       tt-report.key-01   = string(year(oe-rel.rel-date),"9999") +
                            string(month(oe-rel.rel-date),"99")  +
                            string(day(oe-rel.rel-date),"99")
       tt-report.key-02   = string(month(oe-rel.rel-date),"99")  + "/" +
                            string(day(oe-rel.rel-date),"99")    + "/" +
                            string(year(oe-rel.rel-date),"9999")
       tt-report.key-03   = string(v-rel,"-9999999999")
       tt-report.key-04   = string(v-net,"-9999999999")
       tt-report.required = v-rel
       tt-report.variance = v-net

       v-rel = 0.
    end.
  end.

  v-opo = 0.
  for each po-ordl
      where po-ordl.company   eq itemfg.company
        and po-ordl.i-no      eq itemfg.i-no
        and po-ordl.item-type eq no
        and lookup(po-ordl.stat,"O,P,U") gt 0
        and po-ordl.t-rec-qty lt po-ordl.cons-qty
      no-lock,

      first po-ord WHERE
            po-ord.company EQ po-ordl.company AND
            po-ord.po-no   EQ po-ordl.po-no AND
            lookup(po-ord.stat,"N,O,R,U") gt 0
        and po-ord.po-date                le fi_date
      no-lock:

    if po-ordl.cons-uom eq "EA" then
      v-hld-qty = po-ordl.cons-qty.
    else
      run sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                             po-ordl.cons-qty, output v-hld-qty).

    if v-hld-qty - po-ordl.t-rec-qty gt 0 then
      v-opo = v-opo + (v-hld-qty - po-ordl.t-rec-qty).
  end.

  v-net = v-net + v-opo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
    APPLY "entry" TO fi_i-no.
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
  SESSION:SET-WAIT-STATE ("general").
  
  IF fi_i-no NE "" THEN RUN create-tempfile.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  SESSION:SET-WAIT-STATE ("").

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
  {src/adm/template/snd-list.i "tt-report"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
{methods/setfocus.i {&BROWSE-NAME}}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_i-no:SCREEN-VALUE = CAPS(fi_i-no:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST itemfg WHERE itemfg.company EQ cocode
                                   AND itemfg.i-no    EQ fi_i-no:screen-value)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_i-no.
      RETURN ERROR.
    END.
  END.

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
DEF VAR first-cust AS CHAR NO-UNDO.
DEF VAR last-cust AS CHAR NO-UNDO.

/*GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

/*RUN fg/phon-exp.w (first-cust ,last-cust).*/

RUN fginq/fgu-exp.w (fi_i-no, fi_date).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
