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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}
{methods/template/brwCustomDef.i}
{Inventory/ttInventory.i "NEW SHARED"}

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE iWarehouseLength  AS INTEGER   NO-UNDO.

RUN Inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

DEF BUFFER bf-tmp FOR fg-rctd.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.tag rm-rctd.loc ~
rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.i-no rm-rctd.i-name rm-rctd.qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.i-no rm-rctd.qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


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
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 66 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 137 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 139 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 25
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U WIDTH 8
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 12
      rm-rctd.rct-date COLUMN-LABEL "Count Date" FORMAT "99/99/9999":U
            WIDTH 14
      rm-rctd.i-no COLUMN-LABEL "RM Item#" FORMAT "x(10)":U WIDTH 13
      rm-rctd.i-name COLUMN-LABEL "RM Item Name" FORMAT "x(30)":U
            WIDTH 35
      rm-rctd.qty COLUMN-LABEL "Qty Counted" FORMAT "->>>>>>9.9<<":U
            WIDTH 17
  ENABLE
      rm-rctd.tag
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.rct-date
      rm-rctd.i-no
      rm-rctd.qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 137 BY 14.76
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1.24 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 82 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 125 HELP
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
         HEIGHT             = 17.14
         WIDTH              = 139.
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
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
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
     _Where[1]         = "rm-rctd.company = gcompany and
rm-rctd.rita-code = ""C"""
     _FldNameList[1]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" ""
     _FldNameList[2]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" ""
     _FldNameList[3]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" ""
     _FldNameList[4]   > asi.rm-rctd.rct-date
"rct-date" "Count Date" ? "date" ? ? ? ? ? ? yes ? no no "14" yes no no "U" "" ""
     _FldNameList[5]   > asi.rm-rctd.i-no
"i-no" "RM Item#" ? "character" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" ""
     _FldNameList[6]   > asi.rm-rctd.i-name
"i-name" "RM Item Name" ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _FldNameList[7]   > asi.rm-rctd.qty
"qty" "Qty Counted" ? "decimal" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" ""
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
  DEF VAR recid-val AS RECID NO-UNDO.


 IF NOT avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-lock no-error. 
 
 def var ll-tag# as log no-undo.
 ll-help-run = yes.
 case focus:name :
     when "i-no" then do:
           run windows/l-itmre.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, output char-val, output recid-val).
           find item where recid(item) eq recid-val no-lock no-error.
           if avail item and item.i-no ne FOCUS:SCREEN-VALUE then do:
             FOCUS:SCREEN-VALUE = item.i-no.
             apply "value-changed" to focus.
           end.   
     end.

     WHEN "loc"     THEN RUN rmbin-help.   
     WHEN "loc-bin" THEN RUN rmbin-help.
     WHEN "tag"     THEN
     DO:
       /* RUN rmbin-help. */
       run windows/l-ldtag6.w (g_company,YES,focus:screen-value,output char-val,OUTPUT recid-val).
       if char-val <> "" then do :
              rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val).
              /*  ===*/
              FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                                      bf-tmp.rita-code = "C" AND
                                      bf-tmp.tag = rm-rctd.tag:SCREEN-VALUE
                                  AND RECID(bf-tmp) <> RECID(rm-rctd)
                        NO-LOCK NO-ERROR.
              IF AVAIL bf-tmp THEN DO:
                 MESSAGE "This Tag Number Has Already Been Used." skip
                         "Please Enter A Unique Tag Number." 
                         VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
              END.

              FIND FIRST loadtag WHERE loadtag.company = g_company
                         AND loadtag.item-type = YES
                         AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
              IF NOT AVAIL loadtag THEN DO:
                 MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
                 RETURN NO-APPLY.
              END.

              ASSIGN rm-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
                     rm-rctd.i-name:SCREEN-VALUE =  loadtag.i-name         
                     rm-rctd.loc:SCREEN-VALUE = loadtag.loc
                     rm-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
                     rm-rctd.rct-date:SCREEN-VALUE = IF rm-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE rm-rctd.rct-date:SCREEN-VALUE.
              FIND FIRST rm-bin WHERE rm-bin.company EQ cocode 
                       AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}                         
                       AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
                       AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                       NO-LOCK NO-ERROR.
              IF AVAIL rm-bin THEN
              rm-rctd.qty:SCREEN-VALUE = string(rm-bin.qty).

              APPLY "entry" TO rm-rctd.qty.
              RETURN NO-APPLY.
           END.
     END.

   end case.

   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO: 
    &SCOPED-DEFINE exclude-row-display true
    {methods/template/brwRowDisplay.i}    
     /* display calculated field */
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


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company EQ g_company
           AND loadtag.item-type EQ YES
           AND loadtag.tag-no EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
         NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    ASSIGN
      rm-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
      rm-rctd.i-name:SCREEN-VALUE =  loadtag.i-name
      rm-rctd.loc:SCREEN-VALUE = loadtag.loc
      rm-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin.
    RUN valid-loc-bin-tag (3) NO-ERROR.
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
    RUN valid-loc-bin-tag (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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
        AND item.i-code  EQ "R"
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

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
            rm-rctd.i-name:SCREEN-VALUE = ITEM.i-name.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-in-update B-table-Win 
PROCEDURE is-in-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-in-update AS LOGICAL NO-UNDO.

  op-in-update = adm-brs-in-update OR adm-new-record.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN rm-rctd.i-name = rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}
         rm-rctd.tag = rm-rctd.tag:SCREEN-VALUE.

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
/*  FOR EACH bf-rctd NO-LOCK BY bf-rctd.r-no DESCENDING:             */
/*      li-nxt-r-no = bf-rctd.r-no.                                  */
/*      LEAVE.                                                       */
/*  END.                                                             */
/*  FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.              */
/*  li-nxt-r-no = IF AVAIL rm-rcpth AND rm-rcpth.r-no >= li-nxt-r-no */
/*                THEN rm-rcpth.r-no + 1 ELSE li-nxt-r-no.           */
  RUN sys/ref/asiseq.p (INPUT gcompany, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign rm-rctd.company = gcompany
         rm-rctd.loc = gloc
         rm-rctd.r-no = li-nxt-r-no
         rm-rctd.rita-code = "C"
         rm-rctd.s-num = 0
         rm-rctd.rct-date = TODAY.
  find first sys-ctrl where sys-ctrl.company eq gcompany
                          and sys-ctrl.name    eq "RMWHSBIN"
                          no-lock no-error.
    if not avail sys-ctrl then do:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
                 sys-ctrl.name    = "RMWHSBIN"
                 sys-ctrl.descrip = "Default Location for RM Warehouse / Bin for RM Receipts"
                 sys-ctrl.log-fld = YES.
      message "Sys-ctrl record NOT found. " sys-ctrl.descrip
                 update sys-ctrl.char-fld.
    end.
    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
       INPUT  cocode,
       OUTPUT iWarehouseLength
       ).
    
    IF sys-ctrl.char-fld NE 'RMITEM' THEN
    ASSIGN
      rm-rctd.loc = SUBSTR(sys-ctrl.char-fld,1,iWarehouseLength)
      rm-rctd.loc-bin = SUBSTR(sys-ctrl.char-fld,iWarehouseLength + 1).
  disp rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date with browse {&browse-name}. 
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
  /*
  def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
  DEF VAR li AS INT NO-UNDO.
  */

  /* Code placed here will execute PRIOR to standard behavior. */
  /*
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
    assign
     hd-post-child = hd-post:current-window
     hd-post-child = hd-post-child:first-child /* frame */
     hd-post-child = hd-post-child:first-child /* field-group */
     hd-post-child = hd-post-child:first-child /* field */
     hd-post-child:sensitive = no.
  end.
  
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.
  */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply "entry" to rm-rctd.tag in browse {&browse-name}.
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

  /* when new record created from last row, get error "No rm-rctd" record ava */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.
  
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin-tag (99) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
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
  
  /*
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST rm-bin 
        WHERE rm-bin.company EQ cocode
          AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL rm-bin THEN DO:
      IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
        rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(rm-bin.qty).
    END.
  END.
  */

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
    RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc-bin:screen-value in browse {&browse-name}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name},0, OUTPUT lv-rowid).

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
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runWhichPost B-table-Win 
PROCEDURE runWhichPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opRunWhichPost AS CHARACTER NO-UNDO.

  opRunWhichPost = 'rmrep/r-pce&p.w'.

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
                      AND item.i-code  EQ "R") THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
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
    FIND FIRST rm-bin NO-LOCK
         WHERE rm-bin.company  EQ cocode
           AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
           AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
           AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
           AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-ERROR.
    IF ip-int LE 3 AND adm-new-record THEN
    rm-rctd.qty:SCREEN-VALUE = '0'.
    IF NOT AVAILABLE rm-bin THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      IF ip-int EQ 3 THEN
        APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 2 THEN
        APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO rm-rctd.loc IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
    IF ip-int LE 3 AND adm-new-record THEN
    rm-rctd.qty:SCREEN-VALUE = STRING(rm-bin.qty).
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

