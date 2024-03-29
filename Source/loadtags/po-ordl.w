&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  loadtags/po-ordl.w

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
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
def var v-locode like locode no-undo.

def var v-tag-meth as log no-undo.
def var v-num-tags like sys-ctrl.int-fld no-undo.
def var v-tag-seq as int no-undo.

def buffer xrm-rdtl for rm-rdtl.

def var v_cost      like    rm-rdtl.cost no-undo.
def var v-avgcost   as      log no-undo.
def var nufile      as      log no-undo.
def var v-cost-uom  like    job-mat.sc-uom no-undo.
def var v-bwt       like    job-mat.basis-w no-undo.
def var v-len       like    job-mat.len no-undo.
def var v-wid       like    job-mat.wid no-undo.
def var v_con-uom   like    po-ordl.cons-uom no-undo.
def var v-s-num     like    po-ordl.s-num init 1 no-undo.
def var v-exp-dir   as      char initial "" no-undo.
def var v-sheet     as      logical init false no-undo.
def var v-label     as      char format "x(20)" no-undo.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE a AS CHARACTER FORMAT "x(255)" NO-UNDO.

DEFINE TEMP-TABLE ttbl NO-UNDO
  FIELD po-no LIKE po-ordl.po-no
  field po-line like po-ordl.line
  FIELD job-no LIKE po-ordl.job-no
  FIELD job-no2 LIKE po-ordl.job-no2
  FIELD ord-no LIKE po-ordl.ord-no
  FIELD i-no LIKE po-ordl.i-no
  FIELD i-name LIKE ITEM.i-name
  FIELD s-wid LIKE po-ordl.s-wid
  FIELD s-len LIKE po-ordl.s-len
  FIELD qty AS INT 
  FIELD qty-case AS INT
  FIELD case-bundle AS INT
  FIELD loc AS cha
  FIELD bin AS cha
  FIELD no-of-tags AS INTEGER FORMAT '>>9' LABEL '# of Tags'
  FIELD count AS INTEGER FORMAT '>>>>>>9' LABEL 'Count'
  FIELD partial AS INTEGER FORMAT '>>>>>>9' LABEL 'Partial'
  FIELD roll AS LOGICAL FORMAT 'ROLL/ ' LABEL 'Roll'
        INDEX ttbl IS PRIMARY UNIQUE
              po-no
              job-no
              job-no2
              i-no
              s-wid
              s-len.

DEFINE NEW SHARED TEMP-TABLE ttbl-roll NO-UNDO
  FIELD po-no LIKE po-ordl.po-no
  field po-line like po-ordl.line
  FIELD job-no LIKE po-ordl.job-no
  FIELD job-no2 LIKE po-ordl.job-no2
  FIELD ord-no LIKE po-ordl.ord-no
  FIELD i-no LIKE po-ordl.i-no
  FIELD i-name LIKE ITEM.i-name
  FIELD s-wid LIKE po-ordl.s-wid
  FIELD s-len LIKE po-ordl.s-len
  FIELD qty AS INT 
  FIELD qty-case AS INT
  FIELD case-bundle AS INT
  FIELD loc AS cha
  FIELD bin AS cha
  FIELD weight AS integer label 'LBS'
  FIELD lf AS integer label 'LF'
  FIELD count AS INTEGER FORMAT '>>>>>>9' LABEL 'Count'
  FIELD partial AS INTEGER FORMAT '>>>>>>9' LABEL 'Partial'
/*
  FIELD no-of-tags AS INTEGER FORMAT '>>9' LABEL '# of Tags'
  FIELD count AS INTEGER FORMAT '>>>>>>9' LABEL 'Count'
  FIELD partial AS INTEGER FORMAT '>>>>>>9' LABEL 'Partial'
*/
        INDEX ttbl-roll IS PRIMARY 
              po-no
              job-no
              job-no2
              i-no
              s-wid
              s-len.

/*&Scoped-Define SORTBY-PHRASE BY asi.po-ordl.po-no desc*/

&scoped-define fld-name-1 po-ordl.po-no
&scoped-define fld-name-2 po-ordl.i-no
&scoped-define SORTBY-1 BY po-ordl.po-no
&scoped-define SORTBY-2 BY po-ordl.i-no
&scoped-define datatype-1 integer

/*&scoped-define IAMWHAT LOOKUP*/
DEF VAR lv-first-time AS LOG INIT YES NO-UNDO.

ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES po-ordl ttbl ttbl-roll

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 po-ordl.po-no po-ordl.i-no ~
po-ordl.job-no po-ordl.job-no2 po-ordl.s-wid po-ordl.s-len po-ordl.ord-qty ~
po-ordl.pr-qty-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH po-ordl WHERE ~{&KEY-PHRASE} ~
      AND po-ordl.company = g_company  ~
and po-ordl.stat <> "C" and po-ordl.item-type  NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH po-ordl WHERE ~{&KEY-PHRASE} ~
      AND po-ordl.company = g_company  ~
and po-ordl.stat <> "C" and po-ordl.item-type  NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 po-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 po-ordl


/* Definitions for BROWSE ttbl-browse                                   */
&Scoped-define FIELDS-IN-QUERY-ttbl-browse ttbl.po-no ttbl.job-no ttbl.job-no2 ttbl.i-no ttbl.s-wid ttbl.s-len ttbl.no-of-tags ttbl.count ttbl.partial ttbl.roll   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttbl-browse ttbl.no-of-tags  ttbl.count  ttbl.partial   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttbl-browse ttbl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttbl-browse ttbl
&Scoped-define SELF-NAME ttbl-browse
&Scoped-define QUERY-STRING-ttbl-browse FOR EACH ttbl
&Scoped-define OPEN-QUERY-ttbl-browse OPEN QUERY {&SELF-NAME} FOR EACH ttbl.
&Scoped-define TABLES-IN-QUERY-ttbl-browse ttbl
&Scoped-define FIRST-TABLE-IN-QUERY-ttbl-browse ttbl


/* Definitions for BROWSE ttbl-browse-2                                 */
&Scoped-define FIELDS-IN-QUERY-ttbl-browse-2 ttbl-roll.po-no ttbl-roll.job-no ttbl-roll.job-no2 ttbl-roll.i-no ttbl-roll.s-wid ttbl-roll.s-len ttbl-roll.weight ttbl-roll.lf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttbl-browse-2 ttbl-roll.weight  ttbl-roll.lf   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttbl-browse-2 ttbl-roll
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttbl-browse-2 ttbl-roll
&Scoped-define SELF-NAME ttbl-browse-2
&Scoped-define QUERY-STRING-ttbl-browse-2 FOR EACH ttbl-roll
&Scoped-define OPEN-QUERY-ttbl-browse-2 OPEN QUERY {&SELF-NAME} FOR EACH ttbl-roll.
&Scoped-define TABLES-IN-QUERY-ttbl-browse-2 ttbl-roll
&Scoped-define FIRST-TABLE-IN-QUERY-ttbl-browse-2 ttbl-roll


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-4 Btn_Add Btn_Select-PO ~
Btn_Unselect-PO Btn_Select-Tag Btn_Delete Btn_Remove Btn_LoadTags ~
Btn_LoadTags-2 rd-sort lv-search Btn_Clear_Find ttbl-browse 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add 
     LABEL "&Add Line(s) to Loadtag" 
     SIZE 35 BY .95.

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 11 BY 1
     FONT 4.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete Selected Lines from Loadtag" 
     SIZE 35 BY .95.

DEFINE BUTTON Btn_LoadTags 
     LABEL "&Create Tags" 
     SIZE 35 BY 1.91.

DEFINE BUTTON Btn_LoadTags-2 
     LABEL "&Roll Stock Load Tags" 
     SIZE 35 BY 1.91.

DEFINE BUTTON Btn_Remove 
     LABEL "&Remove All Lines from Loadtag" 
     SIZE 35 BY .95.

DEFINE BUTTON Btn_Select-PO 
     LABEL "S&elect All Lines for Loadtag" 
     SIZE 35 BY .95.

DEFINE BUTTON Btn_Select-Tag 
     LABEL "&Select All Lines from Loadtag" 
     SIZE 35 BY .95.

DEFINE BUTTON Btn_Unselect-PO 
     LABEL "&Unselect All Lines for Loadtag" 
     SIZE 35 BY .95.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PO #", 1,
"Item#", 2
     SIZE 44 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      po-ordl SCROLLING.

DEFINE QUERY ttbl-browse FOR 
      ttbl SCROLLING.

DEFINE QUERY ttbl-browse-2 FOR 
      ttbl-roll SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 B-table-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      po-ordl.po-no COLUMN-LABEL "PO #" FORMAT ">>>>>>>9":U
      po-ordl.i-no FORMAT "x(15)":U
      po-ordl.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U
      po-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U WIDTH 15.8
      po-ordl.s-wid FORMAT ">>>>>9.9999<<":U
      po-ordl.s-len FORMAT ">>>>>>>9.9999<<":U
      po-ordl.ord-qty COLUMN-LABEL "Qty" FORMAT "->>>,>>>,>>9.9<<":U
      po-ordl.pr-qty-uom COLUMN-LABEL "Uom" FORMAT "x(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 7.86 EXPANDABLE.

DEFINE BROWSE ttbl-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttbl-browse B-table-Win _FREEFORM
  QUERY ttbl-browse DISPLAY
      ttbl.po-no LABEL 'PO #'
  ttbl.job-no
  ttbl.job-no2
  ttbl.i-no
  ttbl.s-wid
  ttbl.s-len
  ttbl.no-of-tags
  ttbl.count
  ttbl.partial
  ttbl.roll 
ENABLE
  ttbl.no-of-tags HELP 'Enter Number of Tags to Print'
  ttbl.count HELP 'Enter Count for Tags' 
  ttbl.partial HELP 'Enter Partial Tag Count'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 133 BY 10.48
         FONT 2.

DEFINE BROWSE ttbl-browse-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttbl-browse-2 B-table-Win _FREEFORM
  QUERY ttbl-browse-2 DISPLAY
      ttbl-roll.po-no LABEL 'PO #'
  ttbl-roll.job-no
  ttbl-roll.job-no2
  ttbl-roll.i-no
  ttbl-roll.s-wid
  ttbl-roll.s-len
  ttbl-roll.weight
  ttbl-roll.lf
ENABLE
  ttbl-roll.weight HELP 'Enter Weight in Pounds'
  ttbl-roll.lf HELP 'Enter Linear Feet'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 119 BY 10.48
         FONT 2 ROW-HEIGHT-CHARS 4.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-1 AT ROW 1 COL 1
     Btn_Add AT ROW 1.24 COL 99
     Btn_Select-PO AT ROW 2.43 COL 99 HELP
          "Unselect Records"
     Btn_Unselect-PO AT ROW 3.62 COL 99 HELP
          "Unselect Records"
     Btn_Select-Tag AT ROW 4.81 COL 99 HELP
          "Unselect Records"
     Btn_Delete AT ROW 6 COL 99
     Btn_Remove AT ROW 7.19 COL 99
     Btn_LoadTags AT ROW 8.38 COL 99
     Btn_LoadTags-2 AT ROW 8.38 COL 99
     rd-sort AT ROW 9.1 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     lv-search AT ROW 9.1 COL 63 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 9.1 COL 85 HELP
          "CLEAR AUTO FIND Value"
     ttbl-browse-2 AT ROW 10.52 COL 1
     ttbl-browse AT ROW 10.52 COL 1
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 9.1 COL 2
     RECT-4 AT ROW 8.86 COL 1
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
         HEIGHT             = 20
         WIDTH              = 133.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
/*{methods/template/browser.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB BROWSE-1 1 F-Main */
/* BROWSE-TAB ttbl-browse-2 Btn_Clear_Find F-Main */
/* BROWSE-TAB ttbl-browse ttbl-browse-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Btn_LoadTags-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BROWSE ttbl-browse-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       ttbl-browse-2:HIDDEN  IN FRAME F-Main                = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.po-ordl"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "po-ordl.company = g_company 
and ASI.po-ordl.stat <> ""C"" and po-ordl.item-type "
     _FldNameList[1]   > ASI.po-ordl.po-no
"po-ordl.po-no" "PO #" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = ASI.po-ordl.i-no
     _FldNameList[3]   > ASI.po-ordl.job-no
"po-ordl.job-no" "Job #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > ASI.po-ordl.job-no2
"po-ordl.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no "15.8" yes no no "U" "" ""
     _FldNameList[5]   = ASI.po-ordl.s-wid
     _FldNameList[6]   > ASI.po-ordl.s-len
"po-ordl.s-len" ? ">>>>>>>9.9999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > ASI.po-ordl.ord-qty
"po-ordl.ord-qty" "Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.po-ordl.pr-qty-uom
"po-ordl.pr-qty-uom" "Uom" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttbl-browse
/* Query rebuild information for BROWSE ttbl-browse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttbl.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE ttbl-browse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttbl-browse-2
/* Query rebuild information for BROWSE ttbl-browse-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttbl-roll.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE ttbl-browse-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 B-table-Win
ON ANY-PRINTABLE OF BROWSE-1 IN FRAME F-Main
DO:
   if lv-first-time then assign lv-search:screen-value = ""
                                lv-first-time = no.
                                
   lv-search:screen-value = lv-search:screen-value + keylabel(lastkey).
   apply "leave" to lv-search.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 B-table-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO Btn_Add.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add B-table-Win
ON CHOOSE OF Btn_Add IN FRAME F-Main /* Add Line(s) to Loadtag */
DO:
  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS = 0 THEN
  RETURN NO-APPLY.
  DO i = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS:
    ldummy = {&BROWSE-NAME}:FETCH-SELECTED-ROW(i).
    IF CAN-FIND(ttbl WHERE ttbl.po-no = po-ordl.po-no
                       AND ttbl.job-no eq po-ordl.job-no
                       AND ttbl.job-no2 eq po-ordl.job-no2
                       AND ttbl.i-no = po-ordl.i-no
                       AND ttbl.s-wid = po-ordl.s-wid
                       AND ttbl.s-len = po-ordl.s-len) THEN
    NEXT.
    CREATE ttbl.
    ASSIGN
      ttbl.po-no = po-ordl.po-no
      ttbl.po-line = po-ordl.line
      ttbl.job-no = po-ordl.job-no
      ttbl.job-no2 = po-ordl.job-no2
      ttbl.i-no = po-ordl.i-no
      ttbl.s-wid = po-ordl.s-wid
      ttbl.s-len = po-ordl.s-len
      ttbl.no-of-tags = 0
      ttbl.count = 0
      ttbl.partial = 0.
    find first item where item.company eq po-ordl.company
                      and item.i-no    eq po-ordl.i-no
                    no-lock no-error.
    if avail item and item.r-wid eq 0 then
      assign ttbl.roll = no.
    else
      assign ttbl.roll = yes.
  END.
  {&OPEN-QUERY-ttbl-browse}
  APPLY 'CHOOSE' TO Btn_Unselect-PO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear_Find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear_Find B-table-Win
ON CHOOSE OF Btn_Clear_Find IN FRAME F-Main /* Clear Find */
DO:
  assign lv-search:screen-value = "".
         lv-search = "".
    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete B-table-Win
ON CHOOSE OF Btn_Delete IN FRAME F-Main /* Delete Selected Lines from Loadtag */
DO:
  IF ttbl-browse:NUM-SELECTED-ROWS = 0 THEN
  RETURN NO-APPLY.
  DO i = 1 TO ttbl-browse:NUM-SELECTED-ROWS:
    ldummy = ttbl-browse:FETCH-SELECTED-ROW(i).
    DELETE ttbl.
  END.
  ldummy = ttbl-browse:DELETE-SELECTED-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_LoadTags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_LoadTags B-table-Win
ON CHOOSE OF Btn_LoadTags IN FRAME F-Main /* Create Tags */
DO:
  DEF VAR lv-tag-no AS INT NO-UNDO.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "TAG#"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
      sys-ctrl.company = cocode
      sys-ctrl.name    = "TAG#"
      sys-ctrl.descrip = "Assign RM Receipt Tag# Using PO# and Sequence?"
      sys-ctrl.log-fld = no.
      message sys-ctrl.descrip update sys-ctrl.log-fld.
  end.
  assign v-tag-meth = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "RMTAGS"
    NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "RMTAGS"
      sys-ctrl.descrip = "Number of RM Loadtags to Print"
      sys-ctrl.char-fld = ""
      sys-ctrl.int-fld = 1.
    MESSAGE "System control record NOT found.  Please enter Number of Tags"
      UPDATE sys-ctrl.int-fld.
  END.
  assign v-num-tags = sys-ctrl.int-fld.

  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "BARDIR"
    NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "BARDIR"
      sys-ctrl.descrip = ""
      sys-ctrl.char-fld = "".
    MESSAGE "Enter Directory for Bar One Text file and Labels."
      UPDATE sys-ctrl.descrip.
    MESSAGE "Enter Label Name Bar One Labels."
      UPDATE sys-ctrl.char-fld.
  END.
  assign v-label   = sys-ctrl.descrip + "century2.lbl". /*"sheet.lbl"*/
         v-exp-dir = sys-ctrl.descrip + "rmtag.txt" . /*rmload.txt".*/
  
/*****************/
  
/*  output to c:\pnpbar\rmload.txt. */  
  output to value(v-exp-dir).

/*****************/

  assign v-sheet = false.
  FOR EACH ttbl EXCLUSIVE-LOCK:
  
    do i = 1 to (ttbl.no-of-tags + (if ttbl.partial gt 0 then 1 else 0)):

      find first item where item.company = cocode
                        and item.i-no    = ttbl.i-no no-lock no-error.

      if avail item and item.r-wid eq 0 then
      do:
        find first rm-rcpt where rm-rcpt.company eq cocode
                           and rm-rcpt.po-no   eq string(ttbl.po-no)
                           and rm-rcpt.i-no    eq ttbl.i-no
                           and rm-rcpt.job-no  eq ttbl.job-no 
                           and rm-rcpt.job-no2 eq ttbl.job-no2 
                         no-error.
        if not avail rm-rcpt then do:
           {rm/rm-rcpt.a}       
           assign rm-rcpt.po-no   = string(ttbl.po-no)
               rm-rcpt.i-no    = ttbl.i-no
               rm-rcpt.job-no  = ttbl.job-no 
               rm-rcpt.job-no2 = ttbl.job-no2.
           nufile = true.
        end.

        if v_cost = 0 then do:
           find first item where item.company = cocode
                          and item.i-no    = rm-rcpt.i-no no-lock no-error.
           if v-avgcost = true then v_cost = item.avg-cost.
           else v_cost = item.last-cost.
        end.

      create rm-rdtl.
      assign rm-rdtl.r-no       = rm-rcpt.r-no
             rm-rdtl.company    = cocode
             rm-rdtl.loc        = locode
             rm-rdtl.cost       = v_cost
             rm-rdtl.rita-code  = "R"
             rm-rdtl.job-no     = rm-rcpt.job-no
             rm-rdtl.job-no2    = rm-rcpt.job-no2. 

      if avail item then
        assign rm-rdtl.loc-bin = item.loc-bin.

      /* === create new rm table ==============*/
      create asi.rm-rctd.
      assign asi.rm-rctd.r-no = 0
             asi.rm-rctd.company = cocode
             asi.rm-rctd.rita-code = "R"
             asi.rm-rctd.rct-date = today
             asi.rm-rctd.loc = locode
             asi.rm-rctd.po-no = string(ttbl.po-no)
             asi.rm-rctd.po-line = ttbl.po-line
             asi.rm-rctd.i-no = ttbl.i-no
             asi.rm-rctd.job-no = ttbl.job-no
             asi.rm-rctd.job-no2 = ttbl.job-no2
             asi.rm-rctd.loc-bin = if avail item then item.loc-bin else ""
             asi.rm-rctd.s-num = 0
             .
      /* =======================*/


      if rm-rdtl.loc-bin eq "" then do:
        find first sys-ctrl where sys-ctrl.company eq cocode
                              and sys-ctrl.name    eq "AUTOISSU"
                            no-lock no-error.
        if not avail sys-ctrl then do:
          create sys-ctrl.
          assign sys-ctrl.company = cocode
                 sys-ctrl.name    = "AUTOISSU"
                 sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs"
                 sys-ctrl.log-fld = yes.
          message "Sys-ctrl record NOT found. " sys-ctrl.descrip
            update sys-ctrl.char-fld.
        end.
        assign rm-rdtl.loc-bin = sys-ctrl.char-fld.
      end.
      
      if asi.rm-rctd.loc-bin = "" and avail sys-ctrl 
      then asi.rm-rctd.loc-bin = sys-ctrl.char-fld.
      
      find first item where item.company eq cocode
                        and item.i-no    eq rm-rcpt.i-no
                      no-lock no-error.     
      if avail item and item.i-code ne "R" then do:
        find first po-ordl where po-ordl.company eq cocode
                             and po-ordl.i-no    eq rm-rcpt.i-no
                             and po-ordl.po-no   eq integer(rm-rcpt.po-no)
                             and po-ordl.job-no  eq rm-rcpt.job-no
                             and po-ordl.job-no2 eq rm-rcpt.job-no2
                           use-index item-ordno no-lock no-error.
        if avail po-ordl then
           assign v_con-uom = po-ordl.cons-uom
                  asi.rm-rctd.s-num = po-ordl.s-num.
      end.
      else v_con-uom = item.cons-uom.

      assign rm-rcpt.pur-uom  = v_con-uom
             asi.rm-rctd.pur-uom = v_con-uom
             .

      find first po-ordl where po-ordl.company eq rm-rcpt.company
                           and po-ordl.po-no   eq integer(rm-rcpt.po-no)
                           and po-ordl.i-no    eq rm-rcpt.i-no
                           and po-ordl.job-no  eq rm-rcpt.job-no
                           and po-ordl.job-no2 eq rm-rcpt.job-no2
                           and po-ordl.item-type
                         no-lock no-error.

      if avail po-ordl then do:
        assign  v-len = po-ordl.s-len
                v-wid = po-ordl.s-wid
                v-bwt = 0.

/*        {po/pol-dims.i}  */
/* The code below is pol-dims.i minus the do for job-mat statement */

       if (v-len eq 0 or v-wid eq 0 or v-bwt eq 0) then do:
          find first job where job.company eq cocode
                           and job.job-no  eq po-ordl.job-no
                           and job.job-no2 eq po-ordl.job-no2
               no-lock no-error.
          if avail job then do:
             for each job-mat where job-mat.company eq cocode
                                and job-mat.job     eq job.job
                                and job-mat.job-no  eq job.job-no
                                and job-mat.job-no2 eq job.job-no2
                               and job-mat.i-no    eq po-ordl.i-no
                   no-lock
                   by job-mat.frm desc:
                  
                   if job-mat.frm eq po-ordl.s-num then leave.
             end.              
             if avail job-mat then  assign  v-len = if v-len eq 0 then job-mat.len     else v-len
                                            v-wid = if v-wid eq 0 then job-mat.wid     else v-wid
                                            v-bwt = if v-bwt eq 0 then job-mat.basis-w else v-bwt.
          end.
          if v-len eq 0 then v-len = item.s-len.
          if v-wid eq 0 then v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.
          if v-bwt eq 0 then v-bwt = item.basis-w.
       end.    
       if nufile then do:
          assign v-cost-uom = rm-rcpt.pur-uom.
          if po-ordl.pr-uom eq v-cost-uom then
            rm-rdtl.cost = po-ordl.cost.
          else
/*            run r:/asiaddon/development/source/loadtags/convcuom.p   */
            /*run sys/ref/convcuom.p*/
              run rm/convcuom.p
                (po-ordl.pr-uom, v-cost-uom, v-bwt, v-len, v-wid, 0,
                 po-ordl.cost, output rm-rdtl.cost).
          assign asi.rm-rctd.cost-uom = po-ordl.pr-uom
                 asi.rm-rctd.cost = po-ordl.cost.
       end.
      end.  /* if avail po-ordl */

      if v-tag-meth and rm-rcpt.po-no ne "" then do:
        assign v-tag-seq = 0
               v-locode  = "".

        do while true:
          find first xrm-rdtl where xrm-rdtl.company eq cocode
                                and xrm-rdtl.loc     gt v-locode
                              no-lock no-error.

          if avail xrm-rdtl then do:
            v-locode = xrm-rdtl.loc.

            for each xrm-rdtl 
                where xrm-rdtl.company eq cocode
                  and xrm-rdtl.loc     eq v-locode
                  and xrm-rdtl.tag     begins string(int(rm-rcpt.po-no),"999999")
                use-index tag no-lock
                by xrm-rdtl.tag desc:

              if int(substr(xrm-rdtl.tag,7,2)) gt v-tag-seq then
                v-tag-seq = int(substr(xrm-rdtl.tag,7,2)).
              leave.
            end.
          end.
          else   leave.
        end.  /* do while */

        v-locode = "".

        if v-tag-seq eq 0 then do while true:
          find first rm-rdtlh where rm-rdtlh.company eq cocode
                                and rm-rdtlh.loc     gt v-locode
                              no-lock no-error.
          if avail rm-rdtlh then do:
            v-locode = rm-rdtlh.loc.

            for each rm-rdtlh
                where rm-rdtlh.company eq cocode
                  and rm-rdtlh.loc     eq v-locode
                  and rm-rdtlh.tag     begins 
                      (string(int(rm-rcpt.po-no),"9999999") + STRING(rm-rctd.po-line,"999"))
                use-index tag no-lock
                by rm-rdtlh.tag desc:

              if int(substr(rm-rdtlh.tag,7,2)) gt v-tag-seq then
                v-tag-seq = int(substr(rm-rdtlh.tag,7,2)).
              leave.
            end.
          end.
          else 
            leave.
        end.

        assign
          v-tag-seq   = v-tag-seq + 1
          /*rm-rdtl.tag = string(int(rm-rcpt.po-no),"999999") + string(v-tag-seq,"99") */
          asi.rm-rctd.barcode = string(int(rm-rcpt.po-no),"9999999") + 
                            string(asi.rm-rctd.po-line,"999") +
                            string(v-tag-seq,"9999999") 
                           /* string(ttbl.count,"9999") */
          rm-rdtl.tag = rm-rctd.barcode
          asi.rm-rctd.tag = rm-rdtl.tag .
          .
      end. /* if tag-meth and po-no ne "" */

      /* assign rm-rdtl.qty = (ttbl.no-of-tags * ttbl.count) + ttbl.partial. */

      if i eq (ttbl.no-of-tags + (if ttbl.partial gt 0 then 1 else 0)) and
         ttbl.partial gt 0 then
        assign rm-rdtl.qty = ttbl.partial
               asi.rm-rctd.qty = ttbl.partial.
      else
        assign rm-rdtl.qty = ttbl.count
               asi.rm-rctd.qty = ttbl.count.

      end.
      
      /* put "PO#,JOB#,PART#,W+L,QTY,TAG" skip. */  
      /* Create text file to print loadtags */
        /*  PUT unformatted
          "CUSTOMER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
          "SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPZIP,INAME,DUEDATE," +
          "RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
          "TAREWGT,NETWGT,SHEETWGT,UOM,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,TAG#,PARTIAL".
      PUT SKIP.
      */
      PUT "VENDOR,PO#,JOB#,Item#,WL,COUNT,TAG#" SKIP.
      if i le ttbl.no-of-tags then
      do /*x = 1 to (v-num-tags)*/ :
         X = 1.
        if avail item and item.r-wid eq 0 and avail rm-rdtl then     
        do:
          IF X = 1 THEN DO:
             /* generate loadtag file */
             lv-tag-no = v-tag-seq.
             REPEAT:
             
               FIND FIRST loadtag WHERE loadtag.company = cocode
                           AND loadtag.item-type = YES
                           AND loadtag.tag-no = STRING(ttbl.po-no,"9999999") +
                                                STRING(ttbl.po-line,"999") +
                                                string(v-tag-seq,"9999999") 
                                               /* string(ttbl.count,"9999") */
                           NO-LOCK NO-ERROR.
               IF AVAIL loadtag THEN v-tag-seq = v-tag-seq + 1.
               ELSE LEAVE.
             END. /* repeat*/
      
             CREATE loadtag.
             ASSIGN loadtag.company = cocode
             loadtag.tag-no = STRING(ttbl.po-no,"9999999") +
                              STRING(ttbl.po-line,"999") +
                              string(v-tag-seq,"9999999") 
                              /*string(ttbl.count,"9999")*/
             loadtag.item-type = yes /*RMitem*/
             loadtag.po-no = ttbl.po-no
             loadtag.job-no = ttbl.job-no
             loadtag.job-no2 = ttbl.job-no2
             loadtag.ord-no = ttbl.ord-no
             loadtag.i-no = ttbl.i-no
             loadtag.i-name = ttbl.i-name
             loadtag.qty = ttbl.qty
             loadtag.qty-case =   1 /*ttbl.qty-case*/
             loadtag.case-bundle =  1 /*ttbl.case-bundle*/
             loadtag.pallet-count = ttbl.COUNT
             loadtag.loc = ttbl.loc
             loadtag.loc-bin  = ttbl.bin
             loadtag.tot-cases = ttbl.COUNT
             .
          END.
          FIND FIRST po-ord WHERE po-ord.company = cocode AND
                                  po-ord.po-no = ttbl.po-no NO-LOCK NO-ERROR.
          assign v-sheet = true.
          assign a =  "~"" + STRING(po-ord.vend-no) 
                      + "~""    + string(ttbl.po-no)
                      + "~",~"" + (if string(ttbl.job-no) ne "" then
                                      string(ttbl.job-no) + "-" + string(ttbl.job-no2)
                                   else "")
                      + "~",~"" + string(ttbl.i-no)
                      + "~",~"" + string(ttbl.s-wid) + " X " + string(ttbl.s-len)
                      + "~",~"" + trim(string(ttbl.count,"->>>>>>9.9<<<<"))
                      + "~",~"" + /*string(rm-rdtl.tag)*/ /*asi.rm-rctd.barcode*/
                                  loadtag.tag-no  
                      + "~",~"" + /* ==============
                                    string(ttbl.po-no) 
                      + "/"     + string(ttbl.i-no)
                      + "/"     + trim(string(ttbl.count,"->>>>>>9.9<<<<"))
                      + "/"     + string(rm-rdtl.tag)
                                  =============  */
                                  loadtag.tag-no
                      + "~",~"" + "" /*string(po-ordl.company)*/
                      + "~",~"" + "" /*string(rm-bin.loc)*/
                      + "~",~"" + "" /*string(rm-bin.loc-bin)*/
                      + "~"".
          put a format "x(255)" skip.
        end.
        ELSE do:
          /* Create another temp table for Roll Stock */
          if x eq 1 then do:
            create ttbl-roll.
            buffer-copy ttbl 
              except ttbl.no-of-tags 
                     ttbl.count 
                     ttbl.partial
              to ttbl-roll.
          end.
        end.  
      end.  /* do x = */
       
      if ttbl.partial gt 0 and
         i eq (ttbl.no-of-tags + (if ttbl.partial gt 0 then 1 else 0)) then
      do /* x = 1 to v-num-tags: */  :
        X = 1.
        if avail item and item.r-wid eq 0 and avail rm-rdtl then     
        do:
          IF X = 1 THEN DO:
             /* generate loadtag file */
             lv-tag-no = i.
             REPEAT:
                FIND FIRST loadtag WHERE loadtag.company = cocode
                           AND loadtag.item-type = yes
                           AND loadtag.tag-no = STRING(ttbl.po-no,"9999999") +
                                                STRING(ttbl.po-line,"999") +
                                                string(v-tag-seq,"9999999") 
                                            /*    string(ttbl.count,"9999") */
                           NO-LOCK NO-ERROR.
                IF AVAIL loadtag THEN v-tag-seq = v-tag-seq + 1.
                ELSE LEAVE.
             END. /* repeat*/
      
             CREATE loadtag.
             ASSIGN loadtag.company = cocode
             loadtag.tag-no = STRING(ttbl.po-no,"9999999") +
                              STRING(ttbl.po-line,"999") +
                              string(v-tag-seq,"9999999") 
                              /*string(ttbl.count,"9999")  */
             loadtag.item-type = yes /*rmitem*/
             loadtag.po-no = ttbl.po-no
             loadtag.job-no = ttbl.job-no
             loadtag.job-no2 = ttbl.job-no2
             loadtag.ord-no = ttbl.ord-no
             loadtag.i-no = ttbl.i-no
             loadtag.i-name = ttbl.i-name
             loadtag.qty = ttbl.qty
             loadtag.qty-case =   ttbl.qty-case
             loadtag.case-bundle =  ttbl.case-bundle
             loadtag.pallet-count = ttbl.COUNT
             loadtag.loc = ttbl.loc
             loadtag.loc-bin  = ttbl.bin
             .
          END.

          assign v-sheet = true.
          assign a =  "~"" + STRING(po-ord.vend-no) 
                      +  "~""    + string(ttbl.po-no)
                      + "~",~"" + (if string(ttbl.job-no) ne "" then
                                      string(ttbl.job-no) + "-" + string(ttbl.job-no2)
                                   else "")
                      + "~",~"" + string(ttbl.i-no)
                      + "~",~"" + string(ttbl.s-wid) + " X " + string(ttbl.s-len)
                      + "~",~"" + trim(string(ttbl.partial,"->>>>>>9.9<<<<"))
                      + "~",~"" + /*string(rm-rdtl.tag)*/ /*asi.rm-rctd.barcode*/
                                  loadtag.tag-no
                      + "~",~"" + /* ========= barcode new loginc
                                     string(ttbl.po-no) 
                      + "/"     + string(ttbl.i-no)
                      + "/"     + trim(string(ttbl.partial,"->>>>>>9.9<<<<"))
                      + "/"     + string(rm-rdtl.tag)*/ loadtag.tag-no
                      + "~",~"" + "" /*string(po-ordl.company)*/
                      + "~",~"" + "" /*string(rm-bin.loc)*/
                      + "~",~"" + "" /*string(rm-bin.loc-bin)*/
                      + "~"".   
          put a format "x(255)" skip.
        end.
      end.

    end. /* do i = 1 to ttbl.no-of-tags */
    DELETE ttbl.
  END.

  output close.

  v-label = '"' + "c:\barcode anything\label\label.exe " + '" ' + v-label.
  /*if v-sheet then
   /* os-command no-wait value("c:\barone\bin\labels.exe " + v-label). */
      /*OS-COMMAND no-wait  VALUE("c:\barcode anything\label\label.exe " + v-label).*/
       OS-COMMAND VALUE(v-label). */


  find first ttbl-roll no-lock no-error.
  if avail ttbl-roll then
  do:
  /*  message "Roll Stock Items Found. Please Enter Weights and Linear Feet." view-as alert-box.
  */
    hide ttbl-browse no-pause.
    hide Btn_LoadTags no-pause.
    
    enable ttbl-browse-2 with frame {&frame-name}.
    enable Btn_LoadTags-2 with frame {&frame-name}.   
    {&OPEN-QUERY-ttbl-browse-2}
    
  end.

  {&OPEN-QUERY-ttbl-browse}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_LoadTags-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_LoadTags-2 B-table-Win
ON CHOOSE OF Btn_LoadTags-2 IN FRAME F-Main /* Roll Stock Load Tags */
DO:
  DEF VAR lv-tag-no AS INT NO-UNDO.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "TAG#"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
      sys-ctrl.company = cocode
      sys-ctrl.name    = "TAG#"
      sys-ctrl.descrip = "Assign RM Receipt Tag# Using PO# and Sequence?"
      sys-ctrl.log-fld = no.
      message sys-ctrl.descrip update sys-ctrl.log-fld.
  end.
  assign v-tag-meth = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "RMTAGS"
    NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "RMTAGS"
      sys-ctrl.descrip = "Number of RM Loadtags to Print"
      sys-ctrl.char-fld = ""
      sys-ctrl.int-fld = 1.
    MESSAGE "System control record NOT found.  Please enter Number of Tags"
      UPDATE sys-ctrl.int-fld.
  END.
  assign v-num-tags = sys-ctrl.int-fld.

  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "BARDIR"
    NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "BARDIR"
      sys-ctrl.descrip = ""
      sys-ctrl.char-fld = "".
    MESSAGE "Enter Directory for Bar One Text file."
      UPDATE sys-ctrl.descrip.
    MESSAGE "Enter Label Name Bar One Labels."
      UPDATE sys-ctrl.char-fld.
  END.
  assign v-label   = sys-ctrl.descrip + "roll.lbl"
         v-exp-dir = sys-ctrl.descrip + "rmload1.txt".
  
/*****************/
  
/*  output to c:\pnpbar\rmload1.txt. */
  output to value(v-exp-dir). 

/*****************/

  FOR EACH ttbl-roll EXCLUSIVE-LOCK:

/*
    do i = 1 to v-num-tags:
*/  
      find first rm-rcpt where rm-rcpt.company eq cocode
                           and rm-rcpt.po-no   eq string(ttbl-roll.po-no)
                           and rm-rcpt.i-no    eq ttbl-roll.i-no
                           and rm-rcpt.job-no  eq ttbl-roll.job-no 
                           and rm-rcpt.job-no2 eq ttbl-roll.job-no2 
                         no-error.
      if not avail rm-rcpt then do:
        {rm/rm-rcpt.a}
        assign rm-rcpt.po-no   = string(ttbl-roll.po-no)
               rm-rcpt.i-no    = ttbl-roll.i-no
               rm-rcpt.job-no  = ttbl-roll.job-no 
               rm-rcpt.job-no2 = ttbl-roll.job-no2.
         nufile = true.
      end.

      if v_cost = 0 then do:
        find first item where item.company = cocode
                          and item.i-no    = rm-rcpt.i-no no-lock no-error.
        if v-avgcost = true then v_cost = item.avg-cost.
        else v_cost = item.last-cost.
      end.

      create rm-rdtl.
      assign rm-rdtl.r-no       = rm-rcpt.r-no
             rm-rdtl.company    = cocode
             rm-rdtl.loc        = locode
             rm-rdtl.cost       = v_cost
             rm-rdtl.rita-code  = "R"
             rm-rdtl.job-no     = rm-rcpt.job-no
             rm-rdtl.job-no2    = rm-rcpt.job-no2. 

      if avail item then
        assign rm-rdtl.loc-bin = item.loc-bin.

      /* === create new rm table ==============*/
      create asi.rm-rctd.
      assign asi.rm-rctd.r-no = 0
             asi.rm-rctd.company = cocode
             asi.rm-rctd.rita-code = "R"
             asi.rm-rctd.rct-date = today
             asi.rm-rctd.loc = locode
             asi.rm-rctd.po-no = string(ttbl-roll.po-no)
             asi.rm-rctd.po-line = ttbl-roll.po-line
             asi.rm-rctd.i-no = ttbl-roll.i-no
             asi.rm-rctd.job-no = ttbl-roll.job-no
             asi.rm-rctd.job-no2 = ttbl-roll.job-no2
             asi.rm-rctd.loc-bin = if avail item then item.loc-bin else ""
             asi.rm-rctd.s-num = 0
             .
      /* =======================*/

      if rm-rdtl.loc-bin eq "" then do:
        find first sys-ctrl where sys-ctrl.company eq cocode
                              and sys-ctrl.name    eq "AUTOISSU"
                            no-lock no-error.
        if not avail sys-ctrl then do:
          create sys-ctrl.
          assign sys-ctrl.company = cocode
                 sys-ctrl.name    = "AUTOISSU"
                 sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs"
                 sys-ctrl.log-fld = yes.
          message "Sys-ctrl record NOT found. " sys-ctrl.descrip
            update sys-ctrl.char-fld.
        end.
        assign rm-rdtl.loc-bin = sys-ctrl.char-fld.
      end.
   
      if asi.rm-rctd.loc-bin = "" and avail sys-ctrl 
      then asi.rm-rctd.loc-bin = sys-ctrl.char-fld.

      find first item where item.company eq cocode
                        and item.i-no    eq rm-rcpt.i-no
                      no-lock no-error.     
      if avail item and item.i-code ne "R" then do:
        find first po-ordl where po-ordl.company eq cocode
                             and po-ordl.i-no    eq rm-rcpt.i-no
                             and po-ordl.po-no   eq integer(rm-rcpt.po-no)
                             and po-ordl.job-no  eq rm-rcpt.job-no
                             and po-ordl.job-no2 eq rm-rcpt.job-no2
                           use-index item-ordno no-lock no-error.
        if avail po-ordl then
          assign v_con-uom = po-ordl.cons-uom
                 asi.rm-rctd.s-num = po-ordl.s-num
                 .      
      end.
      else v_con-uom = item.cons-uom.

      assign rm-rcpt.pur-uom  = v_con-uom
             asi.rm-rctd.pur-uom = v_con-uom.

      find first po-ordl where po-ordl.company eq rm-rcpt.company
                           and po-ordl.po-no   eq integer(rm-rcpt.po-no)
                           and po-ordl.i-no    eq rm-rcpt.i-no
                           and po-ordl.job-no  eq rm-rcpt.job-no
                           and po-ordl.job-no2 eq rm-rcpt.job-no2
                           and po-ordl.item-type
                         no-lock no-error.

      if avail po-ordl then do:
        assign
          v-len = po-ordl.s-len
          v-wid = po-ordl.s-wid
          v-bwt = 0.

/*        {po/pol-dims.i}  */
/* The code below is pol-dims.i minus the do for job-mat statement */
     
     if (v-len eq 0 or v-wid eq 0 or v-bwt eq 0) then do:
       find first job
           where job.company eq cocode
             and job.job-no  eq po-ordl.job-no
             and job.job-no2 eq po-ordl.job-no2
           no-lock no-error.

       if avail job then do:
         for each job-mat
             where job-mat.company eq cocode
               and job-mat.job     eq job.job
               and job-mat.job-no  eq job.job-no
               and job-mat.job-no2 eq job.job-no2
               and job-mat.i-no    eq po-ordl.i-no
             no-lock
             by job-mat.frm desc:
                  
           if job-mat.frm eq po-ordl.s-num then leave.
         end.
              
         if avail job-mat then
           assign
            v-len = if v-len eq 0 then job-mat.len     else v-len
            v-wid = if v-wid eq 0 then job-mat.wid     else v-wid
            v-bwt = if v-bwt eq 0 then job-mat.basis-w else v-bwt.
       end.

       if v-len eq 0 then v-len = item.s-len.

       if v-wid eq 0 then
         v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

       if v-bwt eq 0 then v-bwt = item.basis-w.
     end.

        if nufile then do:
          assign v-cost-uom = rm-rcpt.pur-uom.

          if po-ordl.pr-uom eq v-cost-uom then
            assign rm-rdtl.cost = po-ordl.cost.
          else
/*            run r:/asiaddon/development/source/loadtags/convcuom.p */
           /* run sys/ref/convcuom.p  */
              run rm/convcuom.p
                (po-ordl.pr-uom, v-cost-uom, v-bwt, v-len, v-wid, 0,
                 po-ordl.cost, output rm-rdtl.cost).
                 
           assign asi.rm-rctd.cost-uom = po-ordl.pr-uom
                  asi.rm-rctd.cost = po-ordl.cost.      
        end.
      end.

      if v-tag-meth and rm-rcpt.po-no ne "" then do:
        assign v-tag-seq = 0
               v-locode  = "".

        do while true:
          find first xrm-rdtl where xrm-rdtl.company eq cocode
                                and xrm-rdtl.loc     gt v-locode
                              no-lock no-error.

          if avail xrm-rdtl then do:
             v-locode = xrm-rdtl.loc.

             for each xrm-rdtl 
                 where xrm-rdtl.company eq cocode
                    and xrm-rdtl.loc     eq v-locode
                    and xrm-rdtl.tag     begins string(int(rm-rcpt.po-no),"999999")
                    use-index tag no-lock
                    by xrm-rdtl.tag desc:

                if int(substr(xrm-rdtl.tag,7,2)) gt v-tag-seq then
                       v-tag-seq = int(substr(xrm-rdtl.tag,7,2)).
                leave.
             end.
          end.
          else      leave.
        end.

        v-locode = "".

        if v-tag-seq eq 0 then do while true:
          find first rm-rdtlh where rm-rdtlh.company eq cocode
                                and rm-rdtlh.loc     gt v-locode
                              no-lock no-error.
          if avail rm-rdtlh then do:
            v-locode = rm-rdtlh.loc.

            for each rm-rdtlh
                where rm-rdtlh.company eq cocode
                  and rm-rdtlh.loc     eq v-locode
                  and rm-rdtlh.tag     begins string(int(rm-rcpt.po-no),"999999")
                use-index tag no-lock
                by rm-rdtlh.tag desc:

              if int(substr(rm-rdtlh.tag,7,2)) gt v-tag-seq then
                v-tag-seq = int(substr(rm-rdtlh.tag,7,2)).
              leave.
            end.
          end.
          else 
            leave.
        end.

        assign
          v-tag-seq   = v-tag-seq + 1
          rm-rdtl.tag = string(int(rm-rcpt.po-no),"999999") + string(v-tag-seq,"99")
          asi.rm-rctd.tag = rm-rdtl.tag
          asi.rm-rctd.barcode = string(int(rm-rcpt.po-no),"9999999") + 
                            string(asi.rm-rctd.po-line,"999") +
                            string(v-tag-seq,"999") +
                            string(ttbl.count,"9999")
          .
          
      end. /* if tag-meth and po-no ne "" */

       /* What QTY for RM receipt ????????  */
      if po-ordl.cons-uom eq "LBS" then
        assign rm-rdtl.qty = ttbl-roll.weight
               asi.rm-rctd.qty = ttbl-roll.weight.
      else if po-ordl.cons-uom eq "LF" then
        assign rm-rdtl.qty = ttbl-roll.lf
               asi.rm-rctd.qty = ttbl-roll.lf.
      else do:     
        /*run sys/ref/convquom.p*/
        run rm/convquom.p 
        ("LF",po-ordl.cons-uom, v-bwt, v-len, v-wid, 0, 
                               ttbl-roll.lf, output rm-rdtl.qty).
        asi.rm-rctd.qty = rm-rdtl.qty.
        
      end.
      /* put "PO#,JOB#,PART#,LF,WEIGHT,TAG,TAGFIELD" skip.  */
      /* Create text file to print loadtags */

      do x = 1 to (v-num-tags):
        assign a =    "~""    + string(ttbl-roll.po-no)
                    + "~",~"" + (if string(ttbl-roll.job-no) ne "" then
                                    string(ttbl-roll.job-no) + "-" + string(ttbl-roll.job-no2)
                                 else "")
                    + "~",~"" + string(ttbl-roll.i-no)
                    + "~",~"" + trim(string(ttbl-roll.lf,"->>>>>>9"))
                    + "~",~"" + trim(string(ttbl-roll.weight,"->>>>>>9"))
                    + "~",~"" + /*string(rm-rdtl.tag)*/ asi.rm-rctd.barcode
                    + "~",~"" + /* ==== barcode login is changed
                               string(ttbl-roll.po-no) 
                    + "/"     + string(ttbl-roll.i-no)
                    + "/"     + trim(string(ttbl-roll.lf,"->>>>>>9")) 
                    + "/"     + string(rm-rdtl.tag)
                               ========== */ asi.rm-rctd.barcode
                    + "~",~"" + "" /* string(po-ordl.company) */
                    + "~",~"" + "" /* string(rm-bin.loc) */
                    + "~",~"" + "" /* string(rm-bin.loc-bin) */
                    + "~"".
        put a format "x(255)" skip.
        IF X = 1 THEN DO:
          /* generate loadtag file */
             lv-tag-no = i.
             REPEAT:
                FIND FIRST loadtag WHERE loadtag.company = cocode
                           AND loadtag.item-type = yes
                           AND loadtag.tag-no = STRING(ttbl-roll.po-no,"9999999") +
                                                STRING(ttbl-roll.po-line,"999") +
                                                string(v-tag-seq,"9999999") 
                                            /*    string(ttbl.count,"9999") */
                           NO-LOCK NO-ERROR.
                IF AVAIL loadtag THEN v-tag-seq = v-tag-seq + 1.
                ELSE LEAVE.
             END. /* repeat*/
      
             CREATE loadtag.
             ASSIGN loadtag.company = cocode
             loadtag.tag-no = STRING(ttbl-roll.po-no,"9999999") +
                              STRING(ttbl-roll.po-line,"999") +
                              string(v-tag-seq,"9999999") 
                              /*string(ttbl.count,"9999")  */
             loadtag.item-type = yes /*rmitem*/
             loadtag.po-no = ttbl-roll.po-no
             loadtag.job-no = ttbl-roll.job-no
             loadtag.job-no2 = ttbl-roll.job-no2
             loadtag.ord-no = ttbl-roll.ord-no
             loadtag.i-no = ttbl-roll.i-no
             loadtag.i-name = ttbl-roll.i-name
             loadtag.qty = ttbl-roll.qty
             loadtag.qty-case =   1 /*ttbl-roll.qty-case*/
             loadtag.case-bundle =  1 /*ttbl-roll.case-bundle*/
             loadtag.pallet-count = ttbl-roll.COUNT
             loadtag.loc = ttbl-roll.loc
             loadtag.loc-bin  = ttbl-roll.bin
             loadtag.tot-cases = ttbl.COUNT
             .
        END.
      end.
       
/*
    end. /* do i = 1 to ttbl-roll.no-of-tags */
*/

    DELETE ttbl-roll.
  END.

  output close.
  
/*  os-command no-wait value("c:\barone\bin\labels.exe " + v-label). */
/*  os-command no-wait value("c:\barone\bin\labels.exe c:\barone\labels\p&p2.lbl"). */

  hide ttbl-browse-2 no-pause.
  hide Btn_LoadTags-2 no-pause.
  view ttbl-browse.
  view Btn_LoadTags.
  {&OPEN-QUERY-ttbl-browse}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove B-table-Win
ON CHOOSE OF Btn_Remove IN FRAME F-Main /* Remove All Lines from Loadtag */
DO:
  FOR EACH ttbl EXCLUSIVE-LOCK:
    DELETE ttbl.
  END.
  {&OPEN-QUERY-ttbl-browse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select-PO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select-PO B-table-Win
ON CHOOSE OF Btn_Select-PO IN FRAME F-Main /* Select All Lines for Loadtag */
DO:
  DO i = 1 TO {&BROWSE-NAME}:NUM-ITERATIONS:
    ldummy = {&BROWSE-NAME}:SELECT-ROW(i).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select-Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select-Tag B-table-Win
ON CHOOSE OF Btn_Select-Tag IN FRAME F-Main /* Select All Lines from Loadtag */
DO:
  DO i = 1 TO ttbl-browse:NUM-ITERATIONS:
    ldummy = ttbl-browse:SELECT-ROW(i).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Unselect-PO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Unselect-PO B-table-Win
ON CHOOSE OF Btn_Unselect-PO IN FRAME F-Main /* Unselect All Lines for Loadtag */
DO:
  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS NE 0 THEN
  ldummy = {&BROWSE-NAME}:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search B-table-Win
ON LEAVE OF lv-search IN FRAME F-Main /* Auto Find */
or return of lv-search
DO:
    assign rd-sort 
           lv-search.

    &scoped-define IAMWHAT Search
   /* &scoped-define where-statement >= (lv-search)*/
    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
    end.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort B-table-Win
ON VALUE-CHANGED OF rd-sort IN FRAME F-Main
DO:
   &scoped-define IAMWHAT LOOKUP   
         
    assign rd-sort.
    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
    end.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttbl-browse
&Scoped-define SELF-NAME ttbl-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttbl-browse B-table-Win
ON DEFAULT-ACTION OF ttbl-browse IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO Btn_Delete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttbl-browse-2
&Scoped-define SELF-NAME ttbl-browse-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttbl-browse-2 B-table-Win
ON DEFAULT-ACTION OF ttbl-browse-2 IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO Btn_Delete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/snd-list.i "ttbl-roll"}
  {src/adm/template/snd-list.i "ttbl"}
  {src/adm/template/snd-list.i "po-ordl"}

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

