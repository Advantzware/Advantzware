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

{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

def var v-locode like locode no-undo.

def var v-tag-meth as log no-undo.
def var v-num-tags like sys-ctrl.int-fld no-undo.
def var v-tag-seq as int no-undo.
def var v-fgfile as log no-undo.
def buffer xfg-rdtl for rm-rdtl.

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
/*  field po-line like po-ordl.line */
  FIELD job-no LIKE job-hdr.job-no
  FIELD job-no2 LIKE job-hdr.job-no2
  FIELD i-no LIKE job-hdr.i-no
  FIELD frm LIKE job-hdr.frm
  FIELD blank-no LIKE job-hdr.blank-no
  field j-no like job-hdr.j-no
  FIELD no-of-tags AS INTEGER FORMAT '>>9' LABEL '# of Tags'
  FIELD count AS INTEGER FORMAT '>>>>>>9' LABEL 'Count'
  FIELD partial AS INTEGER FORMAT '>>>>>>9' LABEL 'Partial'
  FIELD roll AS LOGICAL FORMAT 'ROLL/ ' LABEL 'Roll'
  FIELD loc LIKE ITEMfg.def-loc
  FIELD bin LIKE ITEMfg.def-loc-bin
        INDEX ttbl IS PRIMARY UNIQUE
              job-no
              job-no2
              i-no
              frm
              blank-no.

DEFINE NEW SHARED TEMP-TABLE ttbl-roll NO-UNDO
  field po-no like po-ordl.po-no

  FIELD job-no LIKE job-hdr.job-no
  FIELD job-no2 LIKE job-hdr.job-no2
  FIELD i-no LIKE job-hdr.i-no
  FIELD frm LIKE job-hdr.frm
  FIELD blank-no LIKE job-hdr.blank-no
  field j-no like job-hdr.j-no
  FIELD weight AS integer label 'LBS'
  FIELD lf AS integer label 'LF'
  FIELD no-of-tags AS INTEGER FORMAT '>>9' LABEL '# of Tags'
  FIELD count AS INTEGER FORMAT '>>>>>>9' LABEL 'Count'
  FIELD partial AS INTEGER FORMAT '>>>>>>9' LABEL 'Partial'

        INDEX ttbl-roll IS PRIMARY 
              job-no
              job-no2
              i-no
              frm
              blank-no.

&Scoped-Define SORTBY-PHRASE BY asi.job-hdr.job-no desc

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
&Scoped-define INTERNAL-TABLES job-hdr ttbl ttbl-roll

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table job-hdr.job-no job-hdr.job-no2 ~
job-hdr.i-no job-hdr.est-no job-hdr.ord-no job-hdr.blank-no job-hdr.frm ~
job-hdr.qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH job-hdr WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table job-hdr


/* Definitions for BROWSE ttbl-browse                                   */
&Scoped-define FIELDS-IN-QUERY-ttbl-browse /* ttbl.po-no */ ttbl.job-no ttbl.job-no2 ttbl.i-no /* ttbl.s-wid ttbl.s-len */ ttbl.loc ttbl.bin ttbl.no-of-tags ttbl.count ttbl.partial /*ttbl.roll */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttbl-browse ttbl.no-of-tags  ttbl.count  ttbl.partial   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttbl-browse ttbl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttbl-browse ttbl
&Scoped-define SELF-NAME ttbl-browse
&Scoped-define OPEN-QUERY-ttbl-browse OPEN QUERY {&SELF-NAME} FOR EACH ttbl.
&Scoped-define TABLES-IN-QUERY-ttbl-browse ttbl
&Scoped-define FIRST-TABLE-IN-QUERY-ttbl-browse ttbl


/* Definitions for BROWSE ttbl-browse-2                                 */
&Scoped-define FIELDS-IN-QUERY-ttbl-browse-2 ttbl-roll.po-no ttbl-roll.job-no ttbl-roll.job-no2 ttbl-roll.i-no /*ttbl-roll.s-wid ttbl-roll.s-len ttbl-roll.weight ttbl-roll.lf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttbl-browse-2 ttbl-roll.weight  ttbl-roll.lf */   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttbl-browse-2 ttbl-roll
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttbl-browse-2 ttbl-roll
&Scoped-define SELF-NAME ttbl-browse-2
&Scoped-define OPEN-QUERY-ttbl-browse-2 OPEN QUERY {&SELF-NAME} FOR EACH ttbl-roll.
&Scoped-define TABLES-IN-QUERY-ttbl-browse-2 ttbl-roll
&Scoped-define FIRST-TABLE-IN-QUERY-ttbl-browse-2 ttbl-roll


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table Btn_Add Btn_Select-PO ~
Btn_Unselect-PO Btn_Select-Tag Btn_Delete Btn_Remove Btn_LoadTags ~
Btn_LoadTags-2    ttbl-browse  

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add 
     LABEL "&Add Line(s) to Loadtag" 
     SIZE 35 BY .95.


DEFINE BUTTON Btn_Delete 
     LABEL "&Delete Selected Lines from Loadtag" 
     SIZE 35 BY .95.

DEFINE BUTTON Btn_LoadTags 
     LABEL "&Load Tags" 
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




/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      job-hdr SCROLLING.

DEFINE QUERY ttbl-browse FOR 
      ttbl SCROLLING.

DEFINE QUERY ttbl-browse-2 FOR 
      ttbl-roll SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      job-hdr.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U
      job-hdr.job-no2 COLUMN-LABEL "" FORMAT ">9":U
      job-hdr.i-no FORMAT "x(15)":U
      job-hdr.est-no FORMAT "x(5)":U
      job-hdr.ord-no FORMAT ">>>>>9":U
      job-hdr.blank-no FORMAT ">9":U
      job-hdr.frm FORMAT ">>9":U
      job-hdr.qty FORMAT ">>,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 96 BY 7.86
         FONT 2.

DEFINE BROWSE ttbl-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttbl-browse B-table-Win _FREEFORM
  QUERY ttbl-browse DISPLAY
      /*   ttbl.po-no LABEL 'PO #'  */
  ttbl.job-no
  ttbl.job-no2
  ttbl.i-no
/*  ttbl.s-wid
  ttbl.s-len
*/  
  ttbl.loc
  ttbl.bin
  ttbl.no-of-tags
  ttbl.count
  ttbl.partial
  /*ttbl.roll */
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
  /*ttbl-roll.s-wid
  ttbl-roll.s-len
  ttbl-roll.weight
  ttbl-roll.lf

ENABLE
  ttbl-roll.weight HELP 'Enter Weight in Pounds'
  ttbl-roll.lf HELP 'Enter Linear Feet'
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 119 BY 10.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
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
     ttbl-browse-2 AT ROW 10.52 COL 1
     ttbl-browse AT ROW 10.52 COL 1
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
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table 1 F-Main */
/* BROWSE-TAB ttbl-browse-2  F-Main */
/* BROWSE-TAB ttbl-browse ttbl-browse-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "1".

ASSIGN 
       Btn_LoadTags-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BROWSE ttbl-browse-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       ttbl-browse-2:HIDDEN  IN FRAME F-Main                = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.job-hdr"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > ASI.job-hdr.job-no
"job-hdr.job-no" "Job #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.job-hdr.job-no2
"job-hdr.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = ASI.job-hdr.i-no
     _FldNameList[4]   = ASI.job-hdr.est-no
     _FldNameList[5]   = ASI.job-hdr.ord-no
     _FldNameList[6]   = ASI.job-hdr.blank-no
     _FldNameList[7]   = ASI.job-hdr.frm
     _FldNameList[8]   = ASI.job-hdr.qty
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
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

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO Btn_Add.
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
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
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
    IF CAN-FIND(ttbl WHERE ttbl.job-no eq job-hdr.job-no
                       AND ttbl.job-no2 eq job-hdr.job-no2
                       AND ttbl.i-no = job-hdr.i-no
                       AND ttbl.frm = job-hdr.frm
                       AND ttbl.blank-no = job-hdr.blank-no) THEN
    NEXT.
    CREATE ttbl.
    ASSIGN
      ttbl.job-no = job-hdr.job-no
      ttbl.job-no2 = job-hdr.job-no2
      ttbl.i-no = job-hdr.i-no
      ttbl.frm = job-hdr.frm
      ttbl.blank-no = job-hdr.blank-no
      ttbl.j-no = job-hdr.j-no
      ttbl.no-of-tags = 0
      ttbl.count = 0
      ttbl.partial = 0.
      ttbl.loc = job-hdr.loc.

    find first itemfg where itemfg.company eq job-hdr.company
                      and itemfg.i-no    eq job-hdr.i-no
                    no-lock no-error.
   /* if avail itemfg and itemfg.r-wid eq 0 then
      assign ttbl.roll = no.
    else
      assign ttbl.roll = yes.
   */
     IF AVAIL itemfg THEN ttbl.bin = itemfg.def-loc-bin.
  END.

  {&OPEN-QUERY-ttbl-browse}
  APPLY 'CHOOSE' TO Btn_Unselect-PO.
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
ON CHOOSE OF Btn_LoadTags IN FRAME F-Main /* Load Tags */
DO:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "TAG#"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
      sys-ctrl.company = cocode
      sys-ctrl.name    = "TAG#"
      sys-ctrl.descrip = "Assign FG Receipt Tag# Using PO# and Sequence?"
      sys-ctrl.log-fld = no.
      message sys-ctrl.descrip update sys-ctrl.log-fld.
  end.
  assign v-tag-meth = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "FGTAGS"
    NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "FGTAGS"
      sys-ctrl.descrip = "Number of FG Loadtags to Print"
      sys-ctrl.char-fld = ""
      sys-ctrl.int-fld = 1.
    MESSAGE "System control record NOT found.  Please enter Number of Tags"
      UPDATE sys-ctrl.int-fld.
  END.
  assign v-num-tags = sys-ctrl.int-fld.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "AUTOPOST"
      no-lock no-error.
  if not avail sys-ctrl then do:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name = "AUTOPOST"
            sys-ctrl.descrip = "Autopost to Finished Goods Receipts".
     MESSAGE "System control record NOT found. Would you like to autopost to FG?"
      UPDATE sys-ctrl.log-fld.
  end.

  v-fgfile = avail sys-ctrl and sys-ctrl.char-fld = "FGFile".
  /*v-num-tags = 1. */

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
  assign v-label   = sys-ctrl.descrip + "sheet.lbl"
         v-exp-dir = sys-ctrl.descrip + "fgload.txt".


/*****************/

/*  output to c:\pnpbar\rmload.txt. */
  output to value(v-exp-dir).

/*****************/
  def var fil_id as recid no-undo.
  assign v-sheet = false.
  FOR EACH ttbl EXCLUSIVE-LOCK:

      do i = 1 to (ttbl.no-of-tags + (if ttbl.partial gt 0 then 1 else 0)):


      find first itemfg where itemfg.company = cocode
                        and itemfg.i-no    = ttbl.i-no no-lock no-error.

      if avail itemfg /*and item.r-wid eq 0 */ then
      do:

        find first fg-rcpts where fg-rcpts.company eq cocode
                           and fg-rcpts.po-no   eq string(ttbl.po-no)
                           and fg-rcpts.i-no    eq ttbl.i-no
                           and fg-rcpts.job-no  eq ttbl.job-no 
                           and fg-rcpts.job-no2 eq ttbl.job-no2 
                         no-error.
        if not avail fg-rcpts then do:
           {fg/fg-rcpts.a}       
           assign /*fg-rcpts.po-no   = string(ttbl.po-no)*/
               fg-rcpts.i-no    = ttbl.i-no
               fg-rcpts.job-no  = ttbl.job-no 
               fg-rcpts.job-no2 = ttbl.job-no2.
           nufile = true.
        end.     
/*
        if v_cost = 0 then do:
           find first item where item.company = cocode
                          and item.i-no    = fg-rcpts.i-no no-lock no-error.
           if v-avgcost = true then v_cost = item.avg-cost.
           else v_cost = item.last-cost.
        end.
*/
        create fg-rdtl.
        assign fg-rdtl.r-no       = fg-rcpts.r-no
             fg-rdtl.company    = cocode
             fg-rdtl.loc        = locode
       /*      fg-rdtl.cost       = v_cost */
             fg-rdtl.rita-code  = "R"
             .

        if v-fgfile and fg-rdtl.loc-bin = "" then do:
           find fg-bin where fg-bin.company = cocode and
                           fg-bin.i-no = ttbl.i-no and
                           fg-bin.job-no = ttbl.job-no and
                           ((ttbl.job-no <> " " and fg-bin.job-no2 = ttbl.job-no2)
                             or ttbl.job-no = " ")
                           and fg-bin.qty <= 0
                           no-lock no-error.
           if avail fg-bin then assign fg-rdtl.loc = fg-bin.loc
                                     fg-rdtl.loc-bin = fg-bin.loc-bin.                  
           else if avail itemfg then  
              assign fg-rdtl.loc = itemfg.def-loc
                     fg-rdtl.loc-bin = itemfg.def-loc-bin
                     .
        end.
      /* === create new rm table ==============*/
        create asi.fg-rctd.
        assign asi.fg-rctd.r-no = 0
             asi.fg-rctd.company = cocode
             asi.fg-rctd.rita-code = "R"
             asi.fg-rctd.rct-date = today
             asi.fg-rctd.trans-time = TIME
             asi.fg-rctd.loc = fg-rdtl.loc /*locode*/
             asi.fg-rctd.po-no = string(ttbl.po-no)
             asi.fg-rctd.po-line = ttbl.j-no
             asi.fg-rctd.i-no = ttbl.i-no
             asi.fg-rctd.job-no = ttbl.job-no
             asi.fg-rctd.job-no2 = ttbl.job-no2
             asi.fg-rctd.loc-bin = fg-rdtl.loc-bin
             asi.fg-rctd.s-num = 0
             .
      /* =======================*/

        if fg-rdtl.loc-bin eq "" then do:
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
          assign fg-rdtl.loc-bin = sys-ctrl.char-fld.
        end.

        if asi.fg-rctd.loc-bin = "" and avail sys-ctrl 
        then asi.fg-rctd.loc-bin = sys-ctrl.char-fld.
   /*   if avail item and item.i-code ne "R" then do:
        find first po-ordl where po-ordl.company eq cocode
                             and po-ordl.i-no    eq fg-rcpts.i-no
                             and po-ordl.po-no   eq integer(fg-rcpts.po-no)
                             and po-ordl.job-no  eq fg-rcpts.job-no
                             and po-ordl.job-no2 eq fg-rcpts.job-no2
                           use-index item-ordno no-lock no-error.
        if avail po-ordl then
           assign v_con-uom = po-ordl.cons-uom
                  asi.fg-rctd.s-num = po-ordl.s-num.
      end.
      else 
      */
        v_con-uom = itemfg.cons-uom.

        assign fg-rcpts.pur-uom  = v_con-uom
             asi.fg-rctd.pur-uom = v_con-uom
             .
/* =====================
      find first po-ordl where po-ordl.company eq fg-rcpts.company
                           and po-ordl.po-no   eq integer(fg-rcpts.po-no)
                           and po-ordl.i-no    eq fg-rcpts.i-no
                           and po-ordl.job-no  eq fg-rcpts.job-no
                           and po-ordl.job-no2 eq fg-rcpts.job-no2
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
          assign v-cost-uom = fg-rcpts.pur-uom.
          if po-ordl.pr-uom eq v-cost-uom then
            fg-rdtl.cost = po-ordl.cost.
          else
/*            run r:/asiaddon/development/source/loadtags/convcuom.p   */
            /*run sys/ref/convcuom.p*/
              run rm/convcuom.p
                (po-ordl.pr-uom, v-cost-uom, v-bwt, v-len, v-wid, 0,
                 po-ordl.cost, output fg-rdtl.cost).
          assign asi.fg-rctd.cost-uom = po-ordl.pr-uom
                 asi.fg-rctd.cost = po-ordl.cost.
       end.
      end.  /* if avail po-ordl */
=================*/

        if v-tag-meth and /*fg-rcpts.po-no ne ""*/ asi.fg-rctd.job-no <> "" then do:
           assign v-tag-seq = 0
                  v-locode  = "".

        do while true:
          find first xfg-rdtl where xfg-rdtl.company eq cocode
                                and xfg-rdtl.loc     gt v-locode
                              no-lock no-error.

          if avail xfg-rdtl then do:
            v-locode = xfg-rdtl.loc.

            for each xfg-rdtl 
                where xfg-rdtl.company eq cocode
                  and xfg-rdtl.loc     eq v-locode
                  and xfg-rdtl.tag     begins string(int(fg-rcpts.job-no),"999999")
                use-index tag no-lock
                by xfg-rdtl.tag desc:

              if int(substr(xfg-rdtl.tag,7,2)) gt v-tag-seq then
                v-tag-seq = int(substr(xfg-rdtl.tag,7,2)).
              leave.
            end.  /* for each */
          end. /* avail xfg-rdtl */
          else   leave.
        end.  /* do while */

        v-locode = "".

        if v-tag-seq eq 0 then do while true:
          find first fg-rdtlh where fg-rdtlh.company eq cocode
                                and fg-rdtlh.loc     gt v-locode
                              no-lock no-error.
          if avail fg-rdtlh then do:
            v-locode = fg-rdtlh.loc.

            for each fg-rdtlh
                where fg-rdtlh.company eq cocode
                  and fg-rdtlh.loc     eq v-locode
                  and fg-rdtlh.tag     begins string(int(fg-rcpts.job-no),"999999")
                use-index tag no-lock
                by fg-rdtlh.tag desc:

              if int(substr(fg-rdtlh.tag,7,2)) gt v-tag-seq then
                v-tag-seq = int(substr(fg-rdtlh.tag,7,2)).
              leave.
            end.
          end.
          else 
            leave.
        end.

        assign
          v-tag-seq   = v-tag-seq + 1
          fg-rdtl.tag = string(int(fg-rcpts.po-no),"999999") + string(v-tag-seq,"99")
          asi.fg-rctd.tag = fg-rdtl.tag
          asi.fg-rctd.barcode = "JO" +
                            string(asi.fg-rctd.job-no,"999999") + 
                            string(asi.fg-rctd.job-no2,"999") +
                            string(v-tag-seq,"999") +
                            string(ttbl.count,"9999")
          .
      end. /* if tag-meth and po-no ne "" */

      /* assign fg-rdtl.qty = (ttbl.no-of-tags * ttbl.count) + ttbl.partial. */

      if i eq (ttbl.no-of-tags + (if ttbl.partial gt 0 then 1 else 0)) and
         ttbl.partial gt 0 then
        assign fg-rdtl.qty = ttbl.partial
               asi.fg-rctd.qty = ttbl.partial.
      else
        assign fg-rdtl.qty = ttbl.count
               asi.fg-rctd.qty = ttbl.count.

      end.

      /* put "PO#,JOB#,PART#,W+L,QTY,TAG" skip. */  
      /* Create text file to print loadtags */

      if i le ttbl.no-of-tags then
      do x = 1 to (v-num-tags):
        if avail itemfg /*and item.r-wid eq 0 */ and avail asi.fg-rctd then     
        do:
          assign v-sheet = true.
          assign a =    "~""    + string(ttbl.po-no)
                      + "~",~"" + (if string(ttbl.job-no) ne "" then
                                      string(ttbl.job-no) + "-" + string(ttbl.job-no2)
                                   else "")
                      + "~",~"" + string(ttbl.i-no)
                      + "~",~"" + "0" + "X" + "0"
                                   /*string(ttbl.s-wid) + " X " + string(ttbl.s-len) */
                      + "~",~"" + trim(string(ttbl.count,"->>>>>>9.9<<<<"))
                      + "~",~"" + /*string(fg-rdtl.tag)*/ asi.fg-rctd.barcode
                      + "~",~"" + /*========
                                  string(ttbl.po-no) 
                      + "/"     + string(ttbl.i-no)
                      + "/"     + trim(string(ttbl.count,"->>>>>>9.9<<<<"))
                      + "/"     + string(fg-rdtl.tag)
                                  =========*/ asi.fg-rctd.barcode
                      + "~",~"" + "" /*string(po-ordl.company)*/
                      + "~",~"" + "" /*string(rm-bin.loc)*/
                      + "~",~"" + "" /*string(rm-bin.loc-bin)*/
                      + "~"".
          put a format "x(255)" skip.
        end.
        else
        do:
          if ttbl.i-no = "" then do:
             message "Invalid Item #" view-as alert-box.
             return.
          end.   
          /* Create another temp table for Roll Stock */
          if x eq 1 then 
          do:
            create ttbl-roll.
            buffer-copy ttbl 
              except ttbl.no-of-tags 
                     ttbl.count 
                     ttbl.partial
              to ttbl-roll.
          end.
        end.  /* else  ttbl-roll */  
      end.  /* i le ttbl.no-of-tags */

      if ttbl.partial gt 0 and
         i eq (ttbl.no-of-tags + (if ttbl.partial gt 0 then 1 else 0)) then
      do x = 1 to v-num-tags:
        if avail item and item.r-wid eq 0 and avail fg-rdtl then     
        do:
          assign v-sheet = true.
          assign a =    "~""    + string(ttbl.po-no)
                      + "~",~"" + (if string(ttbl.job-no) ne "" then
                                      string(ttbl.job-no) + "-" + string(ttbl.job-no2)
                                   else "")
                      + "~",~"" + string(ttbl.i-no)
                      + "~",~"" + "0" + "X" + "0"
                                  /*string(ttbl.s-wid) + " X " + string(ttbl.s-len)*/
                      + "~",~"" + trim(string(ttbl.partial,"->>>>>>9.9<<<<"))
                      + "~",~"" + /*string(fg-rdtl.tag)*/ asi.fg-rctd.barcode
                      + "~",~"" + /* =========
                                    string(ttbl.po-no) 
                      + "/"     + string(ttbl.i-no)
                      + "/"     + trim(string(ttbl.partial,"->>>>>>9.9<<<<"))
                      + "/"     + string(fg-rdtl.tag)
                                  */ asi.fg-rctd.barcode
                      + "~",~"" + "" /*string(po-ordl.company)*/
                      + "~",~"" + "" /*string(rm-bin.loc)*/
                      + "~",~"" + "" /*string(rm-bin.loc-bin)*/
                      + "~"".   
          put a format "x(255)" skip.
        end.
      end.  /* partial > 0 */
    end. /* do i = 1 to ttbl.no-of-tags */

    DELETE ttbl.
  END.

  output close.

  if v-sheet then
    os-command no-wait value("c:\barone\bin\labels.exe " + v-label).
/*    os-command no-wait value("c:\barone\bin\labels.exe c:\barone\labels\p&p1.lbl"). */

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
             asi.rm-rctd.po-line = ttbl-roll.j-no
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
          find first xfg-rdtl where xfg-rdtl.company eq cocode
                                and xfg-rdtl.loc     gt v-locode
                              no-lock no-error.

          if avail xfg-rdtl then do:
             v-locode = xfg-rdtl.loc.

             for each xfg-rdtl 
                 where xfg-rdtl.company eq cocode
                    and xfg-rdtl.loc     eq v-locode
                    and xfg-rdtl.tag     begins string(int(rm-rcpt.po-no),"999999")
                    use-index tag no-lock
                    by xfg-rdtl.tag desc:

                if int(substr(xfg-rdtl.tag,7,2)) gt v-tag-seq then
                       v-tag-seq = int(substr(xfg-rdtl.tag,7,2)).
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
          asi.rm-rctd.barcode = "JO" + string(int(asi.rm-rctd.job-no),"999999") + 
                            string(asi.rm-rctd.job-no2,"99") +
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
                    + "~",~"" + /* ==========
                                  string(ttbl-roll.po-no) 
                    + "/"     + string(ttbl-roll.i-no)
                    + "/"     + trim(string(ttbl-roll.lf,"->>>>>>9")) 
                    + "/"     + string(rm-rdtl.tag) 
                                ============= */ asi.rm-rctd.barcode
                    + "~",~"" + "" /* string(po-ordl.company) */
                    + "~",~"" + "" /* string(rm-bin.loc) */
                    + "~",~"" + "" /* string(rm-bin.loc-bin) */
                    + "~"".
        put a format "x(255)" skip.
      end.

/*
    end. /* do i = 1 to ttbl-roll.no-of-tags */
*/

    DELETE ttbl-roll.
  END.

  output close.

  os-command no-wait value("c:\barone\bin\labels.exe " + v-label).
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


&Scoped-define BROWSE-NAME Browser-Table
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
  {src/adm/template/snd-list.i "job-hdr"}

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

