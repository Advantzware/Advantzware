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
{sys/inc/varasgn.i}

DEF NEW SHARED TEMP-TABLE tt-selected NO-UNDO
    FIELD tt-rowid AS ROWID
    FIELD tt-import-zero AS LOG.

DEF VAR char-val AS cha NO-UNDO.
DEF VAR ext-cost AS DECIMAL NO-UNDO.
DEF VAR lv-recid AS RECID NO-UNDO.
DEF VAR ls-prev-po AS cha NO-UNDO.
DEF VAR hd-post AS WIDGET-HANDLE NO-UNDO.
DEF VAR hd-post-child AS WIDGET-HANDLE NO-UNDO.
DEF VAR lv-date-ent AS DATE NO-UNDO.
DEF VAR lv-prior-date AS DATE NO-UNDO.
DEF VAR ll-help-run AS LOG NO-UNDO.  /* set on browse help, reset row-entry */

DO TRANSACTION:
  {sys/inc/rmrecpt.i}
END.

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
&Scoped-define INTERNAL-TABLES rm-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty ~
rm-rctd.pur-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
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
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 78 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U
      rm-rctd.rct-date COLUMN-LABEL "Count Date" FORMAT "99/99/9999":U
            WIDTH 14
      rm-rctd.i-no COLUMN-LABEL "RM Item#" FORMAT "x(15)":U WIDTH 27
      rm-rctd.i-name COLUMN-LABEL "RM Item Name" FORMAT "x(30)":U
            WIDTH 35
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U WIDTH 8
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 12
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 25
      rm-rctd.qty COLUMN-LABEL "Qty Counted" FORMAT "->>>>>>9.9<<<<<":U
            WIDTH 17
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(3)":U
  ENABLE
      rm-rctd.rct-date
      rm-rctd.i-no
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.tag
      rm-rctd.qty
      rm-rctd.pur-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 140 BY 9.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.76 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 10.76 COL 96 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 10.81 COL 127 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.76 COL 2
     RECT-4 AT ROW 10.52 COL 1
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
         HEIGHT             = 11.57
         WIDTH              = 141.
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
       rm-rctd.pur-uom:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""C"""
     _FldNameList[1]   > asi.rm-rctd.r-no
"rm-rctd.r-no" "Seq#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" "Count Date" ? "date" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.i-no
"rm-rctd.i-no" "RM Item#" "x(15)" "character" ? ? ? ? ? ? yes ? no no "27" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.i-name
"rm-rctd.i-name" "RM Item Name" ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.loc
"rm-rctd.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.tag
"rm-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty Counted" ? "decimal" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.pur-uom
"rm-rctd.pur-uom" "UOM" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
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
  DEF VAR recid-val AS RECID NO-UNDO.


 IF NOT AVAIL rm-rctd THEN FIND rm-rctd WHERE RECID(rm-rctd) = lv-recid NO-LOCK NO-ERROR. 
 
 DEF VAR ll-tag# AS LOG NO-UNDO.
 ll-help-run = YES.
 CASE FOCUS:NAME :
     WHEN "i-no" THEN DO:
           RUN windows/l-itmre.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT recid-val).
           FIND item WHERE RECID(item) EQ recid-val NO-LOCK NO-ERROR.
           IF AVAIL item AND item.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN DO:
             FOCUS:SCREEN-VALUE = item.i-no.
             APPLY "value-changed" TO FOCUS.
           END.   
     END.

     WHEN "loc" THEN DO:
             RUN windows/l-loc.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN DO :
                ASSIGN rm-rctd.loc:screen-value IN BROWSE {&browse-name} = ENTRY(1,char-val)
                       .

             END.  
     END.
     WHEN "loc-bin" THEN DO:
             RUN windows/l-rmbin.w (cocode,rm-rctd.loc:screen-value IN BROWSE {&browse-name}, rm-rctd.loc-bin:screen-value,OUTPUT char-val).
             IF char-val <> "" THEN DO :
                ASSIGN rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name} = ENTRY(1,char-val)
                       /*rm-rctd.loc:screen-value = entry(2,char-val)
                        rm-rctd.tag:screen-value = entry(4,char-val)*/
                       .

             END.
     END.
     WHEN "tag" THEN DO: 
         RUN rmbin-help.
         APPLY "ENTRY" TO rm-rctd.tag IN BROWSE {&browse-name}.
     END.

   END CASE.

   RETURN NO-APPLY.
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
  
  ll-help-run = NO.
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


&Scoped-define SELF-NAME rm-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.rct-date IN BROWSE Browser-Table /* Count Date */
DO:
 lv-date-ent = DATE(SELF:SCREEN-VALUE IN BROWSE {&browse-name}).
 lv-prior-date = lv-date-ent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* RM Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* RM Item# */
DO:
  DEF VAR li AS INT NO-UNDO.

  FIND item
      WHERE item.company EQ cocode
        AND item.i-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
        /*AND item.i-code  EQ "R"*/ /* task 11241401*/
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN DO:
    RUN display-item (RECID(item)).
    DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}) + 1:
      APPLY "cursor-right" TO {&self-name} IN BROWSE {&browse-name}.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    /*RUN valid-loc-bin-tag (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
  RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    /*RUN valid-loc-bin-tag (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    /*RUN valid-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  RUN new-bin.
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
   
   FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
   IF NOT AVAIL rm-rctd THEN
       FIND FIRST rm-rctd NO-LOCK
        WHERE recid(rm-rctd) EQ lv-recid NO-ERROR .

   IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN 
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
   
   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-pcount B-table-Win 
PROCEDURE crt-pcount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.
 DEF VAR li-nxt-r-no AS INT INIT 0 NO-UNDO.

 DEF BUFFER bf-rctd FOR rm-rctd.


 FOR EACH tt-selected,
     FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid:
     FIND FIRST ITEM WHERE ITEM.company = cocode
                       AND ITEM.i-no = rm-bin.i-no NO-LOCK NO-ERROR.

  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


   CREATE bf-rctd.
   ASSIGN bf-rctd.company = cocode
          bf-rctd.loc = locode
          bf-rctd.r-no    = li-nxt-r-no
          bf-rctd.rita-code = "C"
          bf-rctd.s-num  = 0
          bf-rctd.rct-date = TODAY
          bf-rctd.i-no = rm-bin.i-no
          bf-rctd.i-name = ITEM.i-name
          bf-rctd.loc     = rm-bin.loc
          bf-rctd.loc-bin = rm-bin.loc-bin
          bf-rctd.tag    = rm-bin.tag
          bf-rctd.qty = IF tt-selected.tt-import-zero THEN 0 ELSE rm-bin.qty
          bf-rctd.pur-uom = ITEM.cons-uom
          op-rowid = ROWID(bf-rctd).
  IF lv-date-ent NE ? THEN
      bf-rctd.rct-date = lv-date-ent.
  lv-date-ent = ?.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-item-recid AS RECID.

  FIND ITEM WHERE RECID(ITEM) = ip-item-recid NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
     ASSIGN rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = ITEM.i-no
            rm-rctd.i-name:SCREEN-VALUE = ITEM.i-name
            rm-rctd.pur-uom:SCREEN-VALUE = ITEM.cons-uom
           /* rm-rctd.loc:SCREEN-VALUE = ITEM.loc
            rm-rctd.loc-bin:SCREEN-VALUE = ITEM.loc-bin */
            .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-focus B-table-Win 
PROCEDURE init-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/setfocus.i {&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win
PROCEDURE local-assign-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      rm-rctd.enteredBy = USERID("asi")
      rm-rctd.enteredDT = DATETIME(TODAY, MTIME) 
      .


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rowid AS ROWID NO-UNDO.
   DEF VAR lAddZeroBin AS LOG NO-UNDO.
   /* Code placed here will execute PRIOR to standard behavior. */
   DO WITH FRAME {&FRAME-NAME}:
      lAddZeroBin = NO.
      IF NOT CAN-FIND(FIRST rm-bin WHERE rm-bin.company EQ cocode
                                     AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                                     AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                                     AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                                     AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
         IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} GT "" THEN 
          MESSAGE "A negative Issue to a previously Issued Tag will" SKIP 
                 "Properly Adjust Job Cost and update On Hand Quantity." SKIP
                 "Do you Still want add this physical count quantity? " 
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                 UPDATE lAddZeroBin.
         IF NOT lAddZeroBin THEN DO:
             RUN windows/l-rmibn1.w (2, rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc2, rm-rctd.loc-bin2, rm-rctd.tag2, OUTPUT lv-rowid).
             FIND rm-bin WHERE ROWID(rm-bin) EQ lv-rowid NO-LOCK NO-ERROR.
             
             IF NOT AVAIL rm-bin THEN DO:
               APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
               RETURN ERROR.
             END. /* if not avail rm-bin */
         END.
      END. /* If not can-find */
     
    END.

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   FIND FIRST item WHERE item.company EQ cocode
                     AND item.i-no    EQ rm-rctd.i-no NO-LOCK NO-ERROR.
   IF AVAIL item THEN rm-rctd.pur-uom = item.cons-uom.
      
   FIND FIRST rm-bin WHERE rm-bin.company EQ cocode
                       AND rm-bin.i-no    EQ rm-rctd.i-no
                       AND rm-bin.loc     EQ rm-rctd.loc
                       AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                       AND rm-bin.tag     EQ rm-rctd.tag NO-LOCK NO-ERROR.
  
   IF NOT AVAIL rm-bin THEN DO:
      FIND rm-bin WHERE ROWID(rm-bin) EQ lv-rowid NO-LOCK NO-ERROR.
      IF AVAIL rm-bin THEN DO:
         ASSIGN
            rm-rctd.loc     = rm-bin.loc
            rm-rctd.loc-bin = rm-bin.loc-bin
            rm-rctd.tag     = rm-bin.tag.
         DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
               rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-rctd.loc
               rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-rctd.loc-bin
               rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-rctd.tag.
         END.
      END.
      ELSE IF lAddZeroBin THEN DO:
          /* User entered a valid tag with not in bin and indicated to proceed */
          CREATE rm-bin.
          ASSIGN 
            rm-bin.company = cocode
            rm-bin.i-no    = rm-rctd.i-no
            rm-bin.loc     = rm-rctd.loc
            rm-bin.loc-bin = rm-rctd.loc-bin
            rm-bin.tag     = rm-rctd.tag
            .
      END.
   END.

   IF AVAIL rm-bin THEN rm-rctd.cost = rm-bin.cost.

   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         rm-rctd.i-name = rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}
         rm-rctd.tag    = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li-nxt-r-no AS INT INIT 0 NO-UNDO.

 DEF BUFFER bf-rctd FOR rm-rctd.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN rm-rctd.company = cocode
         rm-rctd.loc = locode
         rm-rctd.r-no    = li-nxt-r-no
         rm-rctd.rita-code = "C"
         .

  IF adm-adding-record THEN DO:
    ASSIGN rm-rctd.s-num  = 0
           rm-rctd.rct-date = TODAY.
    IF lv-prior-date NE ? THEN
        rm-rctd.rct-date = lv-prior-date.
    DISP rm-rctd.rct-date WITH BROWSE {&browse-name}. 
  END.

  lv-recid = RECID(rm-rctd).  

/*
  run tag-method (output lv-tag-meth). 
  /*  if lv-tag-meth and rm-rctd:po-no:screen*/
  run tag-sequence.
*/  
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
  IF NOT AVAIL rm-rctd THEN FIND rm-rctd WHERE RECID(rm-rctd) = lv-recid NO-ERROR.

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
  IF VALID-HANDLE(hd-post-child) THEN  hd-post-child:SENSITIVE = YES.
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
  DEF VAR out-hd-lst AS cha NO-UNDO.
  DEF VAR ii AS INT NO-UNDO.
  DEF VAR hd-next AS WIDGET-HANDLE NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target", OUTPUT out-hd-lst).
  hd-post = WIDGET-HANDLE(out-hd-lst).  /* procedure */
  IF VALID-HANDLE(WIDGET-HANDLE(out-hd-lst)) THEN DO:
     hd-post-child = hd-post:CURRENT-WINDOW.    
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
     hd-post-child = hd-post-child:FIRST-CHILD.  /* frame */
     hd-post-child = hd-post-child:FIRST-CHILD. /* field-group */
     hd-post-child = hd-post-child:FIRST-CHILD.  /* field */
/*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
*/
     hd-post-child:SENSITIVE = NO.
  END.

  IF NOT winReSize THEN
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO rm-rctd.rct-date IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
 
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
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "value-changed" TO browse-order.
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


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /*RUN valid-loc-bin-tag (99) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT winReSize THEN
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

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
    IF AVAIL rm-bin THEN DO:
/*       IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN */
        rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(rm-bin.qty).
    END.
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
 DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:
    RUN clear_auto_find.
    RUN change-order (browse-order:SCREEN-VALUE). 
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.
  RUN dispatch ('row-changed').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rmbin-help B-table-Win 
PROCEDURE rmbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR save-rowid AS ROWID NO-UNDO.
  DEF VAR save-focus AS CHAR NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR ll-error AS LOG NO-UNDO.



  DO WITH FRAME {&FRAME-NAME}:
    EMPTY TEMP-TABLE tt-selected.

    RUN windows/l-rmibn3.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc-bin:screen-value IN BROWSE {&browse-name}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-rowid).
    /*
    FOR FIRST tt-selected WHERE tt-rowid EQ lv-rowid,
        FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid:

      IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.loc     OR
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} NE rm-bin.loc-bin OR
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     NE rm-bin.tag     THEN DO:
        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag.

        RUN new-bin.
      END.

      DELETE tt-selected.
    END.
    */
     FIND FIRST tt-selected NO-LOCK NO-ERROR.
     IF AVAIL tt-selected THEN DO:
        RUN dispatch ('cancel-record').
        RUN crt-pcount (OUTPUT lv-rowid).
        RUN dispatch ('open-query').
        RUN repo-query (lv-rowid).
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
                      /*AND item.i-code  EQ "R"*/ ) THEN DO: /* task 11241401*/
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc B-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ cocode
                      AND loc.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid Warehouse, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin B-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST rm-bin
                    WHERE rm-bin.company EQ cocode 
                      AND rm-bin.i-no    EQ ""
                      AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Bin does not exist for this warehouse, please create it..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
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
      MESSAGE "Bin does not exist for this warehouse..." VIEW-AS ALERT-BOX.
      IF ip-int EQ 3 THEN
        APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 2 THEN
        APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO rm-rctd.loc IN BROWSE {&browse-name}.
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


  RUN get-matrix (TRUE).
  RETURN ext-cost.
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

