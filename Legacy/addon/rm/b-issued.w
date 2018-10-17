&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  addon\rm\b-issued.w

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

&SCOPED-DEFINE checkNewRecord

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

&SCOPED-DEFINE BRWSDEFS rm-issued

ASSIGN
 cocode = g_company
 locode = g_loc.

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.

def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR lv-po-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-len LIKE lv-po-len NO-UNDO.
DEF VAR v-wid LIKE lv-po-len NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF,DIA"] NO-UNDO.
DEF VAR lv-rmissue AS CHAR NO-UNDO.

DEFINE VARIABLE lvLastRowID AS ROWID NO-UNDO.

DEF BUFFER xitem FOR ITEM.

DEF NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no LIKE job-mat.i-no
    FIELD rec-id AS RECID.

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR lv-job LIKE job.job NO-UNDO.
DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

DEF VAR lv-i-no LIKE po-ordl.i-no NO-UNDO.
DEF VAR lv-line LIKE po-ordl.line NO-UNDO.
DEF VAR v-rmtags-log AS LOG NO-UNDO.
DEF VAR v-autopost-log AS LOG NO-UNDO.
DEF VAR v-get-tandem-rec AS LOG NO-UNDO.
DEF VAR v-scanTagOnly AS LOG NO-UNDO INIT NO.
DEF VAR gv-job-no LIKE job.job-no NO-UNDO INIT "".
DEF VAR gv-job-no2 LIKE job.job-no2 NO-UNDO INIT 0.
DEF VAR gv-item-no LIKE itemfg.i-no NO-UNDO INIT "".

DEF BUFFER br-tmp FOR rm-rctd.  /* for tag validation */
DEF BUFFER xrm-rdtlh FOR rm-rdtlh. /* for tag validation */
DEF BUFFER b-rm-rctd FOR rm-rctd.

DO TRANSACTION:
  {sys/inc/rmrecpt.i}
END.

  FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name eq "RMTAGS"
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
   END.

v-rmtags-log = sys-ctrl.log-fld.

RELEASE sys-ctrl.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name eq "SSRMISSUE"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company = cocode
         sys-ctrl.name    = "SSRMISSUE"
         sys-ctrl.descrip = "Post Sharp Shooter Scanned Issues"
         sys-ctrl.char-fld = ""
         sys-ctrl.int-fld = 0
         sys-ctrl.log-fld = NO.
   END.

v-autopost-log = sys-ctrl.log-fld.

IF sys-ctrl.char-fld = "ScanTagOnly" THEN
    ASSIGN v-scanTagOnly = TRUE.

RELEASE sys-ctrl.

DEF VAR lv-job-no    LIKE rm-rctd.job-no NO-UNDO.
DEF VAR look-recid   AS RECID NO-UNDO.

DEF VAR v-number-rows-selected AS INT NO-UNDO.

{windows/l-jobmt3.i NEW}

    DEF TEMP-TABLE tt-frm NO-UNDO 
        FIELD frm     LIKE job-mat.frm
        FIELD mrp     LIKE job-mat.qty
        FIELD qty     LIKE job-mat.qty
        FIELD qtypct  AS DEC
        INDEX frm frm.      

    DEF TEMP-TABLE tt-tag NO-UNDO 
        FIELD tag-no  LIKE rm-rctd.tag
        FIELD qty LIKE rm-rctd.qty
        FIELD tt-rowid AS ROWID
        INDEX tt-rowid tt-rowid
        INDEX tag-no tag-no.

&SCOPED-DEFINE yellowColumnsName b-issued

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num rm-rctd.b-num ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom ~
calc-ext-cost() @ ext-cost display-dimension('W') @ lv-po-wid ~
display-dimension('L') @ lv-po-len rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.job-no rm-rctd.job-no2 rm-rctd.s-num rm-rctd.b-num rm-rctd.qty ~
rm-rctd.pur-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ 'I' ~
AND rm-rctd.qty GT 0 ~
AND rm-rctd.tag NE '' NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ 'I' ~
AND rm-rctd.qty GT 0 ~
AND rm-rctd.tag NE '' NO-LOCK ~
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-oh B-table-Win 
FUNCTION calc-oh RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD onlyOneForm B-table-Win 
FUNCTION onlyOneForm RETURNS LOGICAL
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
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 74 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      rm-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rctd.rct-date COLUMN-LABEL "Issue Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      rm-rctd.po-no FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.pur-uom COLUMN-LABEL "PUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.cost-uom COLUMN-LABEL "CUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>9.99<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U
  ENABLE
      rm-rctd.tag
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.qty
      rm-rctd.pur-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 79 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 16.71 COL 99 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 131 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 2
     RECT-4 AT ROW 16.48 COL 1
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
         HEIGHT             = 16.91
         WIDTH              = 144.
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
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company EQ cocode
AND rm-rctd.rita-code EQ 'I'
AND rm-rctd.qty GT 0
AND rm-rctd.tag NE ''"
     _FldNameList[1]   > asi.rm-rctd.r-no
"r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.rct-date
"rct-date" "Issue Date" ? "date" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.po-no
"po-no" ? "x(6)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.job-no
"job-no" "Job" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.i-no
"i-no" "Item" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.s-num
"s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.b-num
"b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.pur-uom
"pur-uom" "PUOM" "x(4)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost
"cost" "Cost" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.rm-rctd.cost-uom
"cost-uom" "CUOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>9.99<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > asi.rm-rctd.user-id
"user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   
   MESSAGE "default-action > run update-begin"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO: 
 def var ll-tag# as log no-undo.
 DEF VAR help-recid AS RECID NO-UNDO.
 DEF VAR lv-search AS cha NO-UNDO.

 ll-help-run = yes.
 
 case focus:NAME:
     when "i-no" then do:
          RUN rm/g-joblk.w (OUTPUT lv-search).  /* search job or item */
          IF lv-search = "job" THEN DO:
              RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                      rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
              IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
              
          END.
          ELSE DO:
           /* company,industry,mat-type,i-code,i-no, output, output */
             run windows/l-itmRE.w (rm-rctd.company,"","","R",i-no:SCREEN-VALUE, output char-val,OUTPUT help-recid).
             if char-val <> "" AND ENTRY(1,char-val) NE i-no:SCREEN-VALUE then do :
                i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val).
                RUN new-i-no.
             END.
          END.  
     end.

     when "po-no" then do:
          run windows/l-poords.w (rm-rctd.company, po-no:screen-value, 0, output char-val).
          if char-val <> "" THEN
             assign
                po-no:screen-value in browse {&BROWSE-NAME} = entry(1,char-val)
                lv-i-no                                     = entry(2,char-val)
                lv-line                                     = int(entry(6,char-val)).
     end.

     when "job-no" or when "job-no2" then do:
          run windows/l-jobnoopn.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, output char-val, OUTPUT help-recid).
          if char-val <> "" THEN
             assign rm-rctd.job-no:screen-value = entry(1,char-val)
                    rm-rctd.job-no2:screen-value = entry(2,char-val).
     END.

     WHEN "loc"     OR
     WHEN "loc-bin" /* OR
     WHEN "tag"     */ THEN RUN rmbin-help.
     WHEN "tag" THEN DO:
         run windows/l-ldtag8.w (g_company,yes,tag:screen-value,output char-val,OUTPUT HELP-recid).
         if char-val <> "" then do :
            tag:SCREEN-VALUE = ENTRY(1,char-val).
            /*  ===*/
            FIND FIRST br-tmp WHERE br-tmp.company = g_company AND
                          br-tmp.tag = SELF:SCREEN-VALUE AND
                          br-tmp.rita-code = "I"
                      AND RECID(br-tmp) <> RECID(rm-rctd)
                      NO-LOCK NO-ERROR.
            IF AVAIL br-tmp THEN DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            {addon/loadtags/disptagr.i "RMItem" rm-rctd.tag:SCREEN-VALUE}
            ASSIGN
              rm-rctd.qty:SCREEN-VALUE = ''
              rm-rctd.po-no:SCREEN-VALUE = ''.
            RETURN NO-APPLY.
         END.
     END.

     WHEN "s-num"   OR 
     WHEN "b-num"   THEN RUN s-b-help.
   END CASE.

   help-recid = ?.

   RETURN NO-APPLY.
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
  
  ASSIGN
   ll-help-run = NO
   lv-i-no     = ""
   lv-line     = 0.

  IF AVAIL rm-rctd AND INT(rm-rctd.po-no) NE 0 THEN
    ASSIGN
     lv-i-no = SUBSTR(rm-rctd.BOL,1,30)
     lv-line = INT(SUBSTR(rm-rctd.BOL,31,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
 /*  {src/adm/template/brsleave.i}  */
    if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:  
      return no-apply.
   end.

   {est/brsleave.i}  /* same as src but update will be same as add record*/

   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
    /* ah 02-19-10 */
   /*rm-rctd.qty:READ-ONLY IN BROWSE {&browse-name} = adm-new-record.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  DEFINE VARIABLE lvTag AS CHARACTER NO-UNDO.
  DEF VAR lv-qty AS DEC NO-UNDO.
  DEF VAR lv-new-qty AS DEC NO-UNDO.
  DEF VAR hquery AS HANDLE NO-UNDO.

  DEF BUFFER b-rm-rctd-2 FOR rm-rctd.

  IF LASTKEY NE -1 THEN DO:

    lvTag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    {addon/loadtags/disptagr2.i "RMItem" lvTag}
    ASSIGN
      rm-rctd.po-no:SCREEN-VALUE = ''
      /*rm-rctd.job-no:READ-ONLY = loadtag.job-no NE ''*/.

    RUN valid-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF adm-new-record OR rm-rctd.tag NE rm-rctd.tag:SCREEN-VALUE THEN
    DO:
       FIND FIRST rm-bin NO-LOCK
            WHERE rm-bin.company EQ cocode
              AND rm-bin.i-no    EQ loadtag.i-no
              AND rm-bin.loc     EQ loadtag.loc
              AND rm-bin.loc-bin EQ loadtag.loc-bin
              AND rm-bin.tag     EQ loadtag.tag-no NO-ERROR.

       IF AVAILABLE rm-bin THEN
          ASSIGN
             rm-rctd.qty:SCREEN-VALUE = STRING(rm-bin.qty)
             rm-rctd.cost:SCREEN-VALUE = STRING(rm-bin.cost).
    END.

    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    /* task# 10050601
    IF rm-rctd.job-no:SCREEN-VALUE EQ '' OR NOT adm-new-record THEN
          APPLY 'ENTRY' TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
    ELSE APPLY 'ENTRY' TO rm-rctd.s-num IN BROWSE {&BROWSE-NAME}.
    */

    FOR EACH b-rm-rctd-2 FIELDS(qty) WHERE
        b-rm-rctd-2.company EQ cocode AND
        b-rm-rctd-2.rita-code EQ 'I' AND
        b-rm-rctd-2.qty GT 0 AND
        b-rm-rctd-2.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
        ROWID(b-rm-rctd-2) NE ROWID(rm-rctd)
        NO-LOCK:

        lv-qty = lv-qty + b-rm-rctd-2.qty.
    END.

    IF adm-new-record THEN
    DO:
      find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(SUBSTRING(rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name},1,7))
                       and po-ordl.LINE  = integer(SUBSTRING(rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name},8,3))
                       no-lock no-error.
     
      IF AVAIL po-ordl THEN do:
          ASSIGN
            rm-rctd.s-num:SCREEN-VALUE = string(po-ordl.s-num)
            rm-rctd.b-num:SCREEN-VALUE = string(po-ordl.b-num) .
      END.

       lv-new-qty = DEC(rm-rctd.qty:SCREEN-VALUE) - lv-qty.
      
       IF lv-new-qty LT 0 THEN
          lv-new-qty = 0.
      
       rm-rctd.qty:SCREEN-VALUE = STRING(lv-new-qty).

       IF lv-new-qty = 0 THEN
       DO:
          MESSAGE "Tag Already Issues or Qty on Hand = ZERO."
             VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.


    /* If "scan tag only" is set, then attempt to auto-save and auto-add. */
    IF adm-new-record AND v-scanTagOnly = YES THEN DO:

        /* If job number is blank, and item is same as last item,
           then fill job number from the previous line. */
     
        IF rm-rctd.job-no:SCREEN-VALUE = "" AND rm-rctd.i-no:SCREEN-VALUE = gv-item-no THEN DO:        
            ASSIGN rm-rctd.job-no:SCREEN-VALUE = gv-job-no
                   rm-rctd.job-no2:SCREEN-VALUE = STRING(gv-job-no2, "99").
            rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
                  FILL(" ",6 - LENGTH(TRIM(gv-job-no))) + TRIM(gv-job-no).
            
            IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
               RUN set-s-b-proc.
        END.
        /* If job number is not blank, then save it for the next line. */
        IF rm-rctd.job-no:SCREEN-VALUE <> "" THEN
            ASSIGN gv-job-no = rm-rctd.job-no:SCREEN-VALUE
                   gv-job-no2 = INTEGER(rm-rctd.job-no2:SCREEN-VALUE).

        /* Save item number. */
        IF rm-rctd.i-no:SCREEN-VALUE <> "" THEN
            ASSIGN gv-item-no = rm-rctd.i-no:SCREEN-VALUE.

        /* If a job number exists, then go ahead and auto-save. */
        IF rm-rctd.job-no:SCREEN-VALUE <> "" THEN
            APPLY 'row-leave' TO BROWSE {&BROWSE-NAME}.
        /* Else go to the job number field. */
        ELSE
            APPLY 'ENTRY' TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
    END.
    /* Else if not "scan tag only" is set, then go to the job number field. */
    ELSE
        APPLY 'ENTRY' TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.

    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
   IF adm-new-record OR
      rm-rctd.tag NE rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} THEN
      RUN new-bin.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
  IF adm-new-record OR
     rm-rctd.loc NE rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} THEN
     RUN new-bin.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
   IF adm-new-record OR
      rm-rctd.loc-bin NE rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} THEN
      RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  /*
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
   IF rm-rctd.job-no:READ-ONLY IN BROWSE {&browse-name} THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
   DEF BUFFER b-job FOR job.
   DEF VAR v-single-job AS LOG INIT TRUE NO-UNDO.
   DEF VAR v-job-no-2 AS INT INIT -1 NO-UNDO.
   DEF VAR v-job-no AS CHAR NO-UNDO.

   IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
   DO:
      ASSIGN gv-job-no = trim(rm-rctd.job-no:SCREEN-VALUE) /* stacey */
             gv-job-no2 = INTEGER(rm-rctd.job-no2:SCREEN-VALUE)
             gv-item-no = rm-rctd.i-no:SCREEN-VALUE /* stacey */
             v-job-no = fill(" ",6 - length(trim(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
                 trim(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).
   
      FOR EACH b-job FIELDS(job-no2) WHERE
          b-job.company EQ cocode AND
          b-job.job-no EQ v-job-no
          NO-LOCK:
     
          IF v-job-no-2 EQ -1 THEN
             v-job-no-2 = b-job.job-no2.
          ELSE
             IF v-job-no-2 NE b-job.job-no2 THEN
             DO:
                v-single-job = NO.
                LEAVE.
             END.
      END.
   END.

   IF v-single-job AND v-job-no-2 NE -1 THEN 
     ASSIGN rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-job-no-2).
            gv-job-no2 = v-job-no-2.

   IF LASTKEY NE -1 AND v-single-job THEN
   DO:
      RUN valid-job-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
      RUN valid-job-no2 NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
      RUN valid-i-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
      IF adm-new-record AND rm-rctd.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
         rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
         RUN set-s-b-proc.

      RUN validate-jobmat (YES) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
      
      APPLY 'TAB' TO rm-rctd.s-num IN BROWSE {&BROWSE-NAME}.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
   IF rm-rctd.job-no2:READ-ONLY IN BROWSE {&browse-name} THEN RETURN NO-APPLY.

   IF LASTKEY NE -1 AND rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ '' THEN DO:
         APPLY 'ENTRY':U TO rm-rctd.tag.
         RETURN NO-APPLY.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 THEN DO:
     IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
      ASSIGN gv-job-no2 = INTEGER(rm-rctd.job-no2:SCREEN-VALUE).

     RUN valid-job-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     
     RUN valid-job-no2 NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     
     RUN valid-i-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     
     IF adm-new-record AND rm-rctd.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
        rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
        RUN set-s-b-proc.

     RUN validate-jobmat (YES) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
     IF adm-new-record THEN DO:
        APPLY 'ENTRY' TO rm-rctd.s-num IN BROWSE {&BROWSE-NAME}.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
   RUN new-job-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF adm-new-record AND rm-rctd.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
       rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
       RUN set-s-b-proc.

    RUN validate-jobmat (YES) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
  IF rm-rctd.i-no NE rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} THEN
     RUN new-i-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:    
  IF onlyOneForm() THEN DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.s-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.s-num IN BROWSE Browser-Table /* S */
DO:
   IF LASTKEY NE -1 THEN DO:
      IF INTEGER(SELF:SCREEN-VALUE) EQ 0 THEN DO:
        MESSAGE "Sheet # cannot be 0. Press F1 for help"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
  IF onlyOneForm() THEN DO:
    APPLY 'TAB' TO SELF.     
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
  IF LASTKEY = -1 THEN RETURN.
  RUN validate-jobmat (NO) NO-ERROR.
  if error-status:error then return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
     RUN valid-qty NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

     IF {&self-name}:MODIFIED IN BROWSE {&BROWSE-NAME} THEN DO:
        RUN valid-loc-bin-tag (99) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
           APPLY "entry" TO {&self-name} IN BROWSE {&BROWSE-NAME}.
           RETURN NO-APPLY.
        END.

        IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) LT 0 AND
           DEC(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 AND
           rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
           FOR EACH rm-rcpth FIELDS(company r-no rita-code pur-uom) WHERE
               rm-rcpth.company EQ cocode AND
               rm-rcpth.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
               rm-rcpth.rita-code EQ "I"
               NO-LOCK,
               EACH rm-rdtlh FIELDS(trans-date trans-time cost) WHERE
                    rm-rdtlh.company   EQ rm-rcpth.company AND
                    rm-rdtlh.r-no      EQ rm-rcpth.r-no AND
                    rm-rdtlh.rita-code EQ rm-rcpth.rita-code AND
                    rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
                    rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
                    rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
                    rm-rdtlh.qty       GT 0
                    NO-LOCK
                    BREAK BY rm-rcpth.trans-date DESC
                          BY rm-rdtlh.trans-time DESC:

               ASSIGN
                  rm-rctd.cost:SCREEN-VALUE = STRING(rm-rdtlh.cost)
                  rm-rctd.cost-uom:SCREEN-VALUE = rm-rcpth.pur-uom.
               LEAVE.
           END.
     END.
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* PUOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN get-matrix (NO).

    RUN valid-qty2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
  run get-matrix (false).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* CUOM */
DO:
    IF LASTKEY = -1 THEN RETURN .

    IF index(lv-uom-list,SELF:SCREEN-VALUE) <= 0 THEN DO:
       MESSAGE "Invalid UOM." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    run get-matrix (false).
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{sys/inc/rmissue.i}
lv-rmissue = v-rmissue.
{custom/yellowColumns.i}

/*rm-rctd.qty:READ-ONLY IN BROWSE {&browse-name} = YES.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-item B-table-Win 
PROCEDURE cancel-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL rm-rctd AND rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN
      RUN dispatch IN THIS-PROCEDURE (INPUT 'cancel-record':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-multi-line B-table-Win 
PROCEDURE check-multi-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-job-mat FOR job-mat.
DEF BUFFER b-w-rm-rctd FOR w-rm-rctd.

DEF VAR v-cnt    AS INT          NO-UNDO.
DEF VAR ll-multi AS LOG INIT YES NO-UNDO.

   ASSIGN ll-multi = AVAIL w-rm-rctd.

   IF ll-multi THEN
      FIND FIRST job-mat WHERE
           ROWID(job-mat) EQ w-rm-rctd.job-mat-rowid
           NO-LOCK NO-ERROR.

   ASSIGN ll-multi = AVAIL job-mat AND
                     CAN-FIND(FIRST b-job-mat
                               WHERE b-job-mat.company EQ job-mat.company
                                 AND b-job-mat.job     EQ job-mat.job
                                 AND b-job-mat.job-no  EQ job-mat.job-no
                                 AND b-job-mat.job-no2 EQ job-mat.job-no2
                                 AND b-job-mat.rm-i-no EQ job-mat.rm-i-no
                                 AND ROWID(b-job-mat)  NE ROWID(job-mat)
                              ).

   IF ll-multi THEN 
     ASSIGN rm-rctd.job-no:SCREEN-VALUE IN  BROWSE {&BROWSE-NAME} = w-rm-rctd.job-no
            rm-rctd.job-no2:SCREEN-VALUE = STRING(w-rm-rctd.job-no2)
            rm-rctd.i-no:SCREEN-VALUE    = w-rm-rctd.i-no
            rm-rctd.s-num:SCREEN-VALUE   = "?"
            rm-rctd.b-num:SCREEN-VALUE   = STRING(w-rm-rctd.b-num).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item B-table-Win 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
     CREATE ITEM.
     ASSIGN ITEM.company = cocode
            ITEM.loc = locode
            ITEM.i-no = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            ITEM.i-code = "R".

    /* run rm/item.p */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-jobmat B-table-Win 
PROCEDURE display-jobmat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK NO-ERROR.
    IF AVAIL job-mat THEN DO:
      rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = job-mat.i-no.

      RUN new-i-no.

      FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK.

      ASSIGN
       rm-rctd.s-num:SCREEN-VALUE = string(job-mat.frm)
       rm-rctd.b-num:SCREEN-VALUE = string(job-mat.blank-no).
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Find-Last-Job-Num B-table-Win 
PROCEDURE Find-Last-Job-Num :
/*------------------------------------------------------------------------------
  Purpose:     Get the last record in the browser.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*  IF NUM-RESULTS ("{&browse-name}") > 0 THEN DO:  */
/*   RUN dispatch ('get-last':U).                   */
/*   IF AVAIL rm-rctd THEN                          */
/*   lvLastRowID  = ROWID(rm-rctd).                 */
/*   ASSIGN gv-job-no = rm-rctd.job-no              */
/*          gv-item-no = rm-rctd.i-no.              */
/*  END.                                            */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-mat B-table-Win 
PROCEDURE get-job-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 EMPTY TEMP-TABLE w-rm-rctd.

    IF lv-job-no NE "" OR 
       rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
       RUN windows/l-jobmt3.w (cocode, lv-job-no,
                               INT(rm-rctd.job-no2:SCREEN-VALUE),
                               rm-rctd.i-no:SCREEN-VALUE,
                               OUTPUT char-val,OUTPUT look-recid,
                               OUTPUT v-number-rows-selected).

    FIND FIRST w-rm-rctd NO-ERROR.

    IF NOT AVAIL w-rm-rctd THEN DO:
       APPLY "ENTRY" TO rm-rctd.s-num.
       RETURN NO-APPLY.
    END.

    IF v-number-rows-selected > 1 THEN
       RUN check-multi-line.
    ELSE DO:         
       ASSIGN fil_id = look-recid.
       RUN s-b-help. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix B-table-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-qty-uom AS cha NO-UNDO.
  DEF VAR lv-cost-uom AS cha NO-UNDO.
  def var v-job-up like job-hdr.n-on no-undo.
  def var v-out like ef.n-out init 1 no-undo.
  DEF VAR lv-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR ld-lf-used AS DEC NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO FORMAT ">,>>9.9<<<".
  DEF VAR lv-qty AS DEC NO-UNDO.
  DEF VAR lv-new-qty AS DEC NO-UNDO.
  DEF VAR ll-disp AS LOG INIT NO NO-UNDO.

  DEF BUFFER b-rm-rctd FOR rm-rctd.
  DEF BUFFER b-rm-rctd-2 FOR rm-rctd.


if ip-first-disp  AND avail rm-rctd and rm-rctd.i-no <> "" then do: /* for row-display */
  find item  where item.company eq cocode                           /* no screen-value used */
                     and item.i-no  eq rm-rctd.i-no /*:screen-value in browse {&BROWSE-NAME}*/
                     use-index i-no no-lock no-error.
  if avail item then v-dep = item.s-dep.      
  find first po-ordl where po-ordl.company = rm-rctd.company
                       and po-ordl.po-no = integer(rm-rctd.po-no)
                       and po-ordl.i-no  = rm-rctd.i-no
                       and po-ordl.job-no = rm-rctd.job-no
                       and po-ordl.job-no2 = rm-rctd.job-no2
                       and po-ordl.item-type = yes 
                       and po-ordl.s-num = rm-rctd.s-num
                           no-lock no-error.
  /*if not avail po-ordl then return.  */

  if avail po-ordl then do:
     assign  v-len = po-ordl.s-len
             v-wid = po-ordl.s-wid
             v-bwt = 0
             lv-qty-uom = po-ordl.cons-uom
             lv-cost-uom = po-ordl.cons-uom.
     {rm/pol-dims.i}
  end.
  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no
                         and job.job-no2 eq rm-rctd.job-no2
                no-lock no-error.
        if avail job then do :
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no
                                  and job-mat.frm     eq rm-rctd.s-num
                   no-lock no-error.
             if avail job-mat then do:
               assign 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             end.
        end.
  end.
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
        ASSIGN lv-qty-uom = rm-rctd.pur-uom
               lv-cost-uom = rm-rctd.cost-uom.
  
  
  /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
 /* run rm/convquom.p(rm-rctd.pur-uom,
                    po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input rm-rctd.qty,
                         output lv-out-qty).
  
  /* convert cost pr-uom*/
  run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                    v-bwt, v-len, v-wid, v-dep,
                               rm-rctd.cost, output lv-out-cost).
  */
  run custom/convquom.p(cocode,
                        rm-rctd.pur-uom,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.qty,
                        output lv-out-qty).
  
  /* convert cost pr-uom*/
  run custom/convcuom.p(cocode,
                        rm-rctd.cost-uom,
                        lv-cost-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.cost, output lv-out-cost).
   ASSIGN
    ext-cost = lv-out-qty * lv-out-cost
    ll-disp  = YES.
  
  /*disp ext-cost with browse {&BROWSE-NAME}. it's displayed automatically */
 /* message "after calc:" po-ordl.cons-uom rm-rctd.cost-uom lv-out-cost ext-cost.
  */

end. /* ip-first */
/* ======================================================================= */
else 
if avail rm-rctd and rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> "" then do: /* in update mode - use screen-value */
  assign
   lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
   lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

  find item  where item.company eq cocode
                and item.i-no  eq rm-rctd.i-no:screen-value in browse {&BROWSE-NAME}
                      use-index i-no no-lock no-error.
  if avail item then v-dep = item.s-dep.    
  
  RELEASE po-ordl.

  IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN
  FIND FIRST po-ordl NO-LOCK
      WHERE po-ordl.company EQ cocode
        AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        AND (po-ordl.i-no   EQ lv-i-no OR lv-i-no EQ "")
        AND (po-ordl.line   EQ lv-line OR lv-line EQ 0)
      NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len       = po-ordl.s-len
     v-wid       = po-ordl.s-wid
     v-bwt       = 0
     lv-i-no     = po-ordl.i-no
     lv-line     = po-ordl.line.
    {rm/pol-dims.i}
  END.

  else do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         and job.job-no2 eq integer(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                no-lock no-error.
        if avail job then do :
             v-job-up = 0.
             for each job-hdr FIELDS(n-on)
                 where job-hdr.company eq cocode
                   and job-hdr.job     eq job.job
                   and job-hdr.job-no  eq job.job-no
                   and job-hdr.job-no2 eq job.job-no2
                   and job-hdr.frm     eq int(rm-rctd.s-num:screen-value)
                 no-lock:
               v-job-up = v-job-up + job-hdr.n-on.  
             end.
             
             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                                  and job-mat.frm     eq int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                   no-lock no-error.
             if avail job-mat then do:
               if lv-rmissue eq "Net" then v-out = job-mat.n-up / v-job-up.
               assign 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             end.
        end.
   END.

   if v-len eq 0 then v-len = if avail item then item.s-len else 0.
   if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
   if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.

   ASSIGN lv-qty-uom = item.cons-uom
          lv-cost-uom = ITEM.cons-uom .

  IF lv-uom EQ "DIA" THEN DO:
    ld-lf-used = 0.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rcpth.rita-code EQ "R"
        NO-LOCK,
        EACH rm-rdtlh FIELDS(qty)
        WHERE rm-rdtlh.company   EQ rm-rcpth.company
          AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK:

      ld = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE "LF" THEN
        RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, output ld).

      ld-lf-used = ld-lf-used + ld.
    END.

    FOR EACH b-rm-rctd FIELDS(qty pur-uom)
        WHERE b-rm-rctd.company   EQ cocode
          AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.rita-code EQ "R"
          AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
        NO-LOCK:

      ld = b-rm-rctd.qty.

      IF b-rm-rctd.pur-uom NE "LF" THEN
        RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, output ld).

      ld-lf-used = ld-lf-used + ld.
    END.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rcpth.rita-code EQ "I"
        NO-LOCK,
        EACH rm-rdtlh FIELDS(qty)
        WHERE rm-rdtlh.company   EQ rm-rcpth.company
          AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK:

      ld = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE "LF" THEN
        RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, output ld).

      ld-lf-used = ld-lf-used - ld.
    END.

    FOR EACH b-rm-rctd FIELDS(qty pur-uom)
        WHERE b-rm-rctd.company   EQ cocode
          AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.rita-code EQ "I"
          AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
        NO-LOCK:

      ld = b-rm-rctd.qty.

      IF b-rm-rctd.pur-uom NE "LF" THEN
        RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                          v-bwt, v-len, v-wid, v-dep,
                          ld, output ld).

      ld-lf-used = ld-lf-used - ld.
    END.

    ld = item.ect / 10000.

    IF ld LE 0 THEN DO TRANSACTION:
      MESSAGE "Please enter Core Diameter:" UPDATE ld.
      FIND CURRENT item EXCLUSIVE NO-ERROR.
      IF AVAIL item THEN item.ect = ld * 10000.
      FIND CURRENT item NO-LOCK NO-ERROR.
    END.

    ASSIGN
     lv-out-qty = ld-lf-used -
                  (((lv-out-qty * lv-out-qty) - (ld * ld)) *
                   .0655 / item.cal)
     lv-uom     = "LF".
  END.

  /* convert qty */
  IF lv-uom NE lv-qty-uom THEN
    run rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                      lv-out-qty / v-out, output lv-out-qty).

  /* convert cost */
  IF rm-rctd.cost-uom:screen-value in browse {&BROWSE-NAME} EQ lv-cost-uom THEN
    lv-out-cost = dec(rm-rctd.cost:screen-value in browse {&BROWSE-NAME}).
  ELSE
    run rm/convcuom.p(rm-rctd.cost-uom:screen-value in browse {&BROWSE-NAME},
                      lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                      rm-rctd.cost:screen-value in browse {&BROWSE-NAME},
                      output lv-out-cost).
  ASSIGN
   ext-cost = lv-out-qty * lv-out-cost
   ll-disp  = YES.

  IF adm-new-record THEN
  DO:
     ASSIGN
        ext-cost = lv-out-qty * lv-out-cost
        rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
        rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
        rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
        rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom.
     disp ext-cost with browse {&BROWSE-NAME}.

     FOR EACH b-rm-rctd-2 FIELDS(qty) WHERE
         b-rm-rctd-2.company EQ cocode AND
         b-rm-rctd-2.rita-code EQ 'I' AND
         b-rm-rctd-2.qty GT 0 AND
         b-rm-rctd-2.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
         ROWID(b-rm-rctd-2) NE ROWID(rm-rctd)
         NO-LOCK:
     
         lv-qty = lv-qty + b-rm-rctd-2.qty.
     END.
     
     lv-new-qty = DEC(rm-rctd.qty:SCREEN-VALUE) - lv-qty.
     
     IF lv-new-qty LT 0 THEN
        lv-new-qty = 0.
     
     rm-rctd.qty:SCREEN-VALUE = STRING(lv-new-qty).
     
  END.
end.

IF ll-disp THEN
  ASSIGN
   rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
   rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
   rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
   rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom
   ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)
   NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-qty-matrix B-table-Win 
PROCEDURE get-qty-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ld LIKE job-mat.qty NO-UNDO.
  DEF VAR v-qty LIKE job-mat.qty NO-UNDO. 

  RELEASE job.

  EMPTY TEMP-TABLE tt-frm.

  FIND FIRST job WHERE
       job.company EQ cocode AND
       job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
       job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
       NO-LOCK NO-ERROR.

  IF AVAIL job THEN DO:
     ld = 0.
    
     FOR EACH w-rm-rctd WHERE
         w-rm-rctd.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}:

         FIND FIRST tt-frm WHERE
              tt-frm.frm = w-rm-rctd.s-num
              NO-ERROR.

         IF NOT AVAIL tt-frm THEN
         DO:
            CREATE tt-frm.
            ASSIGN tt-frm.frm = w-rm-rctd.s-num.
            RELEASE tt-frm.
         END.
     END.

     FOR EACH tt-frm,
         EACH job-mat FIELDS(frm qty qty-uom rm-i-no job-no job-no2) WHERE
         job-mat.company EQ job.company AND
         job-mat.job     EQ job.job AND
         job-mat.job-no  EQ job.job-no AND
         job-mat.job-no2 EQ job.job-no2 AND
         job-mat.rm-i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
         job-mat.frm EQ tt-frm.frm
         NO-LOCK:
    
         RUN tandem-rec-uom-conv(INPUT job-mat.rm-i-no,
                                 INPUT job-mat.qty-uom,
                                 INPUT job-mat.qty,
                                 INPUT job-mat.job-no,
                                 INPUT job-mat.job-no2,
                                 INPUT job-mat.frm,
                                 OUTPUT v-qty).

         ASSIGN
            tt-frm.mrp = tt-frm.mrp + v-qty
            ld         = ld + v-qty.
     END.
    
     IF ld NE 0 THEN
        FOR EACH tt-frm:
            tt-frm.qtypct = (tt-frm.mrp / ld).
        END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tandem-rec B-table-Win 
PROCEDURE get-tandem-rec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-cons-uom AS CHAR NO-UNDO.
    
   DEF VAR ld AS DEC NO-UNDO.

   v-get-tandem-rec = YES.

   RUN get-qty-matrix.

   EMPTY TEMP-TABLE tt-tag.

   FOR EACH tt-selected,
       FIRST rm-bin FIELDS(qty tag) WHERE
             ROWID(rm-bin) EQ tt-selected.tt-rowid
             NO-LOCK:

       CREATE tt-tag.
       ASSIGN
          tt-tag.tag = rm-bin.tag
          tt-tag.qty = rm-bin.qty
          tt-tag.tt-rowid = ROWID(rm-bin)
          ld = ld + rm-bin.qty.
   END.

   FOR EACH tt-frm:
       tt-frm.qty = ROUND(ld * tt-frm.qtypct,2).

       IF ip-cons-uom EQ "EA" THEN
       DO:
          {sys/inc/roundup.i tt-frm.qty}
       END.
   END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-std-cost AS DEC NO-UNDO.
  DEF VAR ld-cost-uom AS cha NO-UNDO.
DEF VAR v-rctd-rowid AS ROWID.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN ld-std-cost = dec(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} )
         ld-cost-uom = rm-rctd.cost-uom:SCREEN-VALUE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   rm-rctd.cost     = ld-std-cost
   rm-rctd.cost-uom = ld-cost-uom
   rm-rctd.enteredBy = USERID("asi")
   rm-rctd.enteredDT = DATETIME(TODAY, MTIME) 
   .

  IF INT(rm-rctd.po-no) NE 0 THEN
    rm-rctd.bol = STRING(lv-i-no,"x(30)") + STRING(lv-line,"999").
  v-rctd-rowid = ROWID(rm-rctd).
  IF v-autopost-log THEN DO:
    RUN rm/rmpost.p (INPUT ROWID(rm-rctd)).
    RUN adm-initialize.
    RUN adm-open-query.
  END.

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
  ASSIGN
    rm-rctd.loc = rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    rm-rctd.loc-bin = rm-rctd.loc-bin:SCREEN-VALUE
    rm-rctd.rct-date = DATE(rm-rctd.rct-date:SCREEN-VALUE)
    rm-rctd.po-no = rm-rctd.po-no:SCREEN-VALUE
    rm-rctd.job-no = rm-rctd.job-no:SCREEN-VALUE
    rm-rctd.job-no2 = INT(rm-rctd.job-no2:SCREEN-VALUE)
    rm-rctd.i-no = rm-rctd.i-no:SCREEN-VALUE
    rm-rctd.i-name = rm-rctd.i-name:SCREEN-VALUE
    rm-rctd.qty = DECIMAL(rm-rctd.qty:SCREEN-VALUE)
    rm-rctd.s-num = INT(rm-rctd.s-num:SCREEN-VALUE)
    rm-rctd.b-num = INT(rm-rctd.b-num:SCREEN-VALUE)
    rm-rctd.pur-uom = rm-rctd.pur-uom:SCREEN-VALUE.


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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*rm-rctd.qty:READ-ONLY IN BROWSE {&browse-name} = YES.*/


  ASSIGN gv-job-no = ""
         gv-job-no2 = 0
         gv-item-no = "".

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
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li-nxt-r-no) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign rm-rctd.company = cocode
         rm-rctd.loc = locode
         rm-rctd.r-no = li-nxt-r-no
         rm-rctd.rita-code = "I"
         rm-rctd.s-num  = 1
         rm-rctd.b-num = 0
         rm-rctd.rct-date = TODAY.


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
  FOR EACH tt-selected:
    DELETE tt-selected.
  END.

  /*rm-rctd.qty:READ-ONLY IN BROWSE {&browse-name} = YES.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.

    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.

    {&BROWSE-NAME}:READ-ONLY = NO.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

/*   IF gv-job-no = "" THEN      */
/*       RUN Find-Last-Job-Num.  */


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
  DEF VAR lv-qty AS DEC NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN valid-po-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      FIND po-ordl
          WHERE po-ordl.company EQ rm-rctd.company
            AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN
        ASSIGN
         lv-i-no = po-ordl.i-no
         lv-line = po-ordl.line.

      IF lv-i-no EQ "" OR lv-line EQ 0 THEN DO:
        RUN windows/l-poords.w (rm-rctd.company, rm-rctd.po-no, INT(rm-rctd.po-no), OUTPUT char-val).

        IF char-val NE "" THEN
          ASSIGN
           lv-i-no = ENTRY(2,char-val)
           lv-line = INT(ENTRY(6,char-val)).
      END.

      IF lv-i-no EQ "" OR lv-line EQ 0 THEN DO:
        MESSAGE "Must select PO Line to Issue to..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.po-no IN BROWSE {&BROWSE-NAME}.
        RETURN NO-APPLY.
      END.
    END.
  END.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN .

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
  RUN valid-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN .

  FOR EACH b-rm-rctd FIELDS(qty) WHERE
      b-rm-rctd.company EQ cocode AND
      b-rm-rctd.rita-code EQ 'I' AND
      b-rm-rctd.qty GT 0 AND
      b-rm-rctd.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
      ROWID(b-rm-rctd) NE ROWID(rm-rctd)
      NO-LOCK:

      lv-qty = lv-qty + b-rm-rctd.qty.
  END.
  
  lv-qty = lv-qty + DEC(rm-rctd.qty:SCREEN-VALUE).
  
  IF AVAIL loadtag THEN
  DO:
     FIND FIRST rm-bin WHERE
          rm-bin.company EQ cocode AND
          rm-bin.i-no    EQ loadtag.i-no AND
          rm-bin.loc     EQ loadtag.loc AND
          rm-bin.loc-bin EQ loadtag.loc-bin AND
          rm-bin.tag     EQ loadtag.tag-no
          NO-LOCK NO-ERROR.
     
     IF AVAIL rm-bin AND lv-qty > rm-bin.qty THEN
     DO:
        MESSAGE "Tag Already Issues or Qty on Hand = ZERO."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'CURSOR-LEFT':U TO {&BROWSE-NAME}.
    END.
  END.

  RUN multi-issues (ROWID(rm-rctd)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /*rm-rctd.qty:READ-ONLY IN BROWSE {&browse-name} = YES.*/

  RUN scan-next.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lookup-job-mat B-table-Win 
PROCEDURE lookup-job-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-for-item-only AS LOG NO-UNDO.

  DEF VAR count-mat AS INT NO-UNDO.
  DEF VAR ll-lookup AS LOG NO-UNDO.

  IF ip-for-item-only EQ ? THEN
    ASSIGN
     ip-for-item-only = YES
     ll-lookup        = YES.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.

    IF AVAIL item AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      EMPTY TEMP-TABLE item-chg.
      count-mat = 0.

      FOR EACH job-mat
          WHERE job-mat.company    EQ cocode
            AND job-mat.job        EQ job.job 
            AND job-mat.job-no     EQ INPUT rm-rctd.job-no
            AND job-mat.job-no2    EQ INPUT rm-rctd.job-no2 
            AND (ip-for-item-only OR
                 (job-mat.frm      EQ INT(rm-rctd.s-num:SCREEN-VALUE) AND
                  job-mat.blank-no EQ INT(rm-rctd.b-num:SCREEN-VALUE)))
          USE-INDEX seq-idx NO-LOCK,

          FIRST xitem
          WHERE xitem.company  EQ cocode
            AND xitem.i-no     EQ job-mat.rm-i-no
            AND xitem.mat-type EQ item.mat-type
          NO-LOCK
          
          BREAK BY job-mat.frm
                BY job-mat.blank-no:

        IF FIRST-OF(job-mat.blank-no) OR NOT ll-lookup THEN DO:
          count-mat = count-mat + 1.
          CREATE item-chg.
          ASSIGN
           item-chg.i-no   = xitem.i-no
           item-chg.rec-id = RECID(job-mat)
           fil_id          = RECID(item-chg).
        END.
      END.

      IF ll-lookup THEN fil_id = ?.
      
      IF count-mat NE 1 OR ll-lookup THEN RUN rm/g-itmchg.w.
      
      FIND FIRST item-chg WHERE RECID(item-chg) EQ fil_id NO-LOCK NO-ERROR.
      IF AVAIL item-chg THEN fil_id = item-chg.rec-id.
    END.
  END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE multi-issues B-table-Win 
PROCEDURE multi-issues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-rm-rctd FOR rm-rctd.

  DO WITH FRAME {&frame-name}:
    FOR EACH tt-selected,
        FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid:

      IF NOT CAN-FIND(FIRST b-rm-rctd
                      WHERE b-rm-rctd.company  EQ rm-rctd.company
                        AND b-rm-rctd.rct-date EQ rm-rctd.rct-date
                        AND b-rm-rctd.job-no   EQ rm-rctd.job-no
                        AND b-rm-rctd.job-no2  EQ rm-rctd.job-no2
                        AND b-rm-rctd.i-no     EQ rm-rctd.i-no
                        AND b-rm-rctd.loc      EQ rm-bin.loc
                        AND b-rm-rctd.loc-bin  EQ rm-bin.loc-bin
                        AND b-rm-rctd.tag      EQ rm-bin.tag) THEN DO:

        RUN dispatch ('copy-record').

        {&BROWSE-NAME}:SELECT-FOCUSED-ROW().

        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.tag
         rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = ""
         rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}    = "".

        RUN new-bin.

        RUN get-matrix (TRUE).

        RUN valid-all NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN dispatch ('assign-record').

        RUN dispatch ('open-query').

        RUN dispatch ('end-update').

        REPOSITION {&BROWSE-NAME} TO ROWID ip-rowid.

        {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      END.

      DELETE tt-selected.
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
  DEF VAR lv-qty AS DEC NO-UNDO.
  DEF VAR lv-new-qty AS DEC NO-UNDO.

  DEF BUFFER b-rm-rctd-2 FOR rm-rctd.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST rm-bin 
        WHERE rm-bin.company EQ cocode
          AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.
    IF AVAIL rm-bin THEN DO:
       rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(rm-bin.cost).
       IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
       DO:
          rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(rm-bin.qty).

          IF adm-new-record THEN
          DO:
             FOR EACH b-rm-rctd-2 FIELDS(qty) WHERE
                 b-rm-rctd-2.company EQ cocode AND
                 b-rm-rctd-2.rita-code EQ 'I' AND
                 b-rm-rctd-2.qty GT 0 AND
                 b-rm-rctd-2.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
                 ROWID(b-rm-rctd-2) NE ROWID(rm-rctd)
                 NO-LOCK:
            
                 lv-qty = lv-qty + b-rm-rctd-2.qty.
             END.
            
             lv-new-qty = DEC(rm-rctd.qty:SCREEN-VALUE) - lv-qty.
            
             IF lv-new-qty LT 0 THEN
                lv-new-qty = 0.
            
             rm-rctd.qty:SCREEN-VALUE = STRING(lv-new-qty).
          END.
       END.
    END.
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
  DEF BUFFER b-item FOR item.
  DEF VAR v-oh-hand AS DEC NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    v-oh-hand = calc-oh().
    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND (item.i-code  EQ "R"
             OR (ITEM.i-code = "E" 
                      AND rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} GT "" 
                      AND v-oh-hand GT 0 ))
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      ASSIGN
       rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}   = item.i-name
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = item.cons-uom
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom.

      FOR EACH rm-bin 
          WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          NO-LOCK
          BY rm-bin.qty DESC BY rm-bin.loc BY rm-bin.loc-bin BY rm-bin.tag:
        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.tag.

        RUN new-bin.

        LEAVE.
      END.

      FOR EACH job
          WHERE job.company EQ cocode
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK,
          EACH job-mat
          WHERE job-mat.company EQ job.company
            AND job-mat.job     EQ job.job
            AND job-mat.job-no  EQ job.job-no
            AND job-mat.job-no2 EQ job.job-no2
          NO-LOCK,
          FIRST b-item
          WHERE b-item.company  EQ job-mat.company
            AND b-item.i-no     EQ job-mat.i-no
            AND b-item.mat-type EQ item.mat-type
          NO-LOCK
          BREAK BY job-mat.frm      DESC
                BY job-mat.blank-no DESC:

        IF job-mat.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} OR
           LAST(job-mat.frm)                                                  THEN DO:
          ASSIGN
           rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(job-mat.frm)
           rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(job-mat.blank-no).
          LEAVE.
        END.
      END.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no B-table-Win 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-job-no LIKE rm-rctd.job-no NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    /*
    ASSIGN
     rm-rctd.s-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
     rm-rctd.b-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO.
    */

    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      ASSIGN
       lv-job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
       lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no).

      RELEASE job-hdr.

      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ lv-job-no
            AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          USE-INDEX job-no NO-LOCK NO-ERROR.

      IF AVAIL job THEN
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job.job-no2     EQ job.job-no2
          NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN DO:
        FIND FIRST est
            WHERE est.company EQ job-hdr.company
              AND est.est-no  EQ job-hdr.est-no
            NO-LOCK NO-ERROR.
        IF AVAIL est AND (est.est-type EQ 1 OR est.est-type EQ 5) THEN 
          ASSIGN
           rm-rctd.s-num:SCREEN-VALUE = "1" /*string(job-hdr.frm) */
           rm-rctd.b-num:SCREEN-VALUE = "1" /* string(job-hdr.blank-no) */
           /*
           rm-rctd.s-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
           rm-rctd.b-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
           */ .
      END.
    END.
  END.

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
    RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, rm-rctd.loc-bin:screen-value in browse {&BROWSE-NAME}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, OUTPUT lv-rowid).

    FOR FIRST tt-selected WHERE tt-rowid EQ lv-rowid,
        FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid:

      IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     NE rm-bin.loc     OR
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE rm-bin.loc-bin OR
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     NE rm-bin.tag     THEN DO:
        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.tag.

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

  opRunWhichPost = 'rm/r-rmtpst.p'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE s-b-help B-table-Win 
PROCEDURE s-b-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN lookup-job-mat (?).
    RELEASE job-mat.
    IF fil_id NE ? THEN
    FIND job-mat WHERE RECID(job-mat) EQ fil_id NO-LOCK NO-ERROR.
    IF AVAIL job-mat THEN
      ASSIGN
       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(job-mat.frm) 
       rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(job-mat.blank-no).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scan-next B-table-Win 
PROCEDURE scan-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN local-open-query.

  FIND LAST b-rm-rctd WHERE
       b-rm-rctd.company EQ cocode AND
       b-rm-rctd.rita-code EQ 'I' AND
       b-rm-rctd.qty GT 0 AND
       b-rm-rctd.tag NE ''
       {&SORTBY-PHRASE}
       NO-LOCK NO-ERROR.

  IF AVAILABLE b-rm-rctd THEN
     REPOSITION browser-table TO ROWID ROWID(b-rm-rctd) NO-ERROR.
  
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).

  RUN auto-add IN WIDGET-HANDLE(char-hdl).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-s-b-proc B-table-Win 
PROCEDURE set-s-b-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-item FOR ITEM.

   FOR EACH job
       WHERE job.company EQ cocode
         AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
         AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
       NO-LOCK,
       EACH job-mat
       WHERE job-mat.company EQ job.company
         AND job-mat.job     EQ job.job
         AND job-mat.job-no  EQ job.job-no
         AND job-mat.job-no2 EQ job.job-no2
       NO-LOCK,
       FIRST b-item
       WHERE b-item.company  EQ job-mat.company
         AND b-item.i-no     EQ job-mat.i-no
         AND b-item.mat-type EQ item.mat-type
       NO-LOCK
       BREAK BY job-mat.frm      DESC
             BY job-mat.blank-no DESC:

       IF job-mat.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} OR
          LAST(job-mat.frm)                                                  THEN DO:
         ASSIGN
          rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(job-mat.frm)
          rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(job-mat.blank-no).
         LEAVE.
       END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method B-table-Win 
PROCEDURE tag-method :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-tag# as log no-undo.
 
  
  {rm/tag#.i}
  op-tag# = v-tag#.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-sequence B-table-Win 
PROCEDURE tag-sequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  def var v-tag-seq as int no-undo.
  def var v-locode as cha no-undo.
  def buffer xrm-rctd for rm-rctd.
  
  assign v-tag-seq = 0
         v-locode  = "".

  do while true:
    find first xrm-rctd
        where xrm-rctd.company eq rm-rctd.company
          and xrm-rctd.loc     gt v-locode
        no-lock no-error.

    if avail xrm-rctd then do:
      v-locode = xrm-rctd.loc.

      for each xrm-rctd where xrm-rctd.company eq rm-rctd.company
            and xrm-rctd.loc     eq v-locode
            and xrm-rctd.tag     begins string(int(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME}),"999999")
            use-index tag no-lock
            by xrm-rctd.tag desc:

           if int(substr(xrm-rctd.tag,7,2)) gt v-tag-seq then
           v-tag-seq = int(substr(xrm-rctd.tag,7,2)).
            leave.
      end.
    end.

    else leave.
  end.  /* do while */
/* ======= may not need any more 
  v-locode = "".
  if v-tag-seq eq 0 then do while true:
    find first rm-rctdh where rm-rctdh.company eq rm-rcth.company
          and rm-rctdh.loc     gt v-locode
        no-lock no-error.

    if avail rm-rctdh then do:
      v-locode = rm-rctdh.loc.

      for each rm-rctdh
          where rm-rctdh.company eq cocode
            and rm-rctdh.loc     eq v-locode
            and rm-rctdh.tag     begins string(int(rm-rctd.po-no),"999999")
          use-index tag no-lock
          by rm-rctdh.tag desc:

        if int(substr(rm-rctdh.tag,7,2)) gt v-tag-seq then
          v-tag-seq = int(substr(rm-rctdh.tag,7,2)).
        leave.
      end.
    end.

    else leave.
  end.
============================== */
  assign  v-tag-seq   = v-tag-seq + 1
         /* rm-rctd.tag:screen-value in browse {&BROWSE-NAME}
          = string(int(rm-rctd.po-no:screen-value in browse {&BROWSE-NAME}),"999999") + string(v-tag-seq,"99").
           */
        .        
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tandem-rec-uom-conv B-table-Win 
PROCEDURE tandem-rec-uom-conv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-i-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-pur-uom AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-qty AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-job-no AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-job-no2 AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-s-num AS INT NO-UNDO.

   DEFINE OUTPUT PARAMETER lv-out-qty as dec no-undo.

   def var v-dep like po-ordl.s-len no-undo. 
   def var v-job-up like job-hdr.n-on no-undo.
   def var v-out like ef.n-out init 1 no-undo.
   def var v-bwt like po-ordl.s-len no-undo.

   IF ip-i-no EQ "" THEN
      LEAVE.

   assign
      lv-out-qty = ip-qty.
   
   find FIRST item where
        item.company eq cocode AND
        item.i-no eq ip-i-no
        no-lock no-error.

   IF NOT AVAIL ITEM OR
      ip-pur-uom EQ item.cons-uom THEN
      LEAVE.

   v-dep = item.s-dep.

   find first job where
        job.company eq cocode AND
        job.job-no  eq ip-job-no AND
        job.job-no2 eq ip-job-no2
        no-lock no-error.
   
   if avail job then do:
      v-job-up = 0.
      for each job-hdr FIELDS(n-on)
          where job-hdr.company eq cocode
            and job-hdr.job     eq job.job
            and job-hdr.job-no  eq job.job-no
            and job-hdr.job-no2 eq job.job-no2
            and job-hdr.frm     eq ip-s-num
          no-lock:
          v-job-up = v-job-up + job-hdr.n-on.  
      end.
      
      find first job-mat where
           job-mat.company eq cocode AND
           job-mat.job     eq job.job AND
           job-mat.i-no    eq ip-i-no AND
           job-mat.frm     eq ip-s-num
           no-lock no-error.

      if avail job-mat then do:
         if lv-rmissue eq "Net" then
            v-out = job-mat.n-up / v-job-up.
         assign 
            v-len = job-mat.len
            v-wid = job-mat.wid
            v-bwt = job-mat.basis-w.
      end.
   end.
   
   if v-len eq 0 then v-len = if avail item then item.s-len else 0.
   if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
   if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
   
   run rm/convquom.p(ip-pur-uom, item.cons-uom, v-bwt, v-len, v-wid, v-dep,
                     lv-out-qty / v-out, output lv-out-qty).
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-all B-table-Win 
PROCEDURE valid-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-loc-bin-tag (3) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN validate-jobmat (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-s-num NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

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
  DEF VAR v-msg AS CHAR NO-UNDO.
  DEF VAR v-oh-hand AS DEC NO-UNDO.

  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" THEN
        v-msg = TRIM(rm-rctd.i-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                " may not be spaces".

    IF v-msg EQ "" THEN DO:
       
      FIND FIRST item
          WHERE item.company EQ cocode
            AND TRIM(item.i-no)    EQ TRIM(rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
          NO-LOCK NO-ERROR.
      v-oh-hand = calc-oh().
          
      FIND FIRST item 
          WHERE item.company EQ cocode
            AND TRIM(item.i-no)    EQ TRIM(rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND (item.i-code  EQ "R"
                OR (ITEM.i-code = "E" 
                      AND rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} GT "" 
                      AND v-oh-hand GT 0 ))
          NO-LOCK NO-ERROR.

      IF NOT AVAIL item THEN DO:
        MESSAGE "Item is not on file. Do you want to add it? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF ll-ans THEN DO:
          RUN create-item.
          NEXT.
        END.
        ELSE v-msg = "Invalid entry, try help".
      END.
    END.

    IF v-msg EQ "" THEN
      IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ ""    AND
         INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 AND
         item.mat-type NE "P"                                          THEN
      v-msg = "If PO# and Job# are blank then RM Type must be 'P'aper".

    IF v-msg EQ "" THEN
      IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
         lv-i-no NE "" AND lv-line NE 0                                THEN DO:
        FIND FIRST po-ordl
            WHERE po-ordl.company EQ rm-rctd.company
              AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
              AND po-ordl.i-no    EQ lv-i-no
              AND po-ordl.line    EQ lv-line
              AND po-ordl.s-wid   LE (IF item.r-wid NE 0 THEN item.r-wid
                                                         ELSE item.s-wid)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL po-ordl THEN
          v-msg = "RM width must be greater than PO RM you are issuing to...".
      END.

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.

    LEAVE.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no B-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-job-no LIKE rm-rctd.job-no NO-UNDO.
  DEF VAR lv-po-no LIKE po-ord.po-no NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      ASSIGN
       lv-job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
       rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
           FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no)
       lv-po-no = INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

      IF NOT CAN-FIND(FIRST job
                      WHERE job.company EQ cocode
                        AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                      USE-INDEX job-no) OR lv-po-no NE 0
      THEN DO:
        IF lv-po-no NE 0 THEN
          MESSAGE "You may not enter both " +
                  TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) + " and " +
                  TRIM(rm-rctd.po-no:LABEL IN BROWSE {&BROWSE-NAME}) + "..."
              VIEW-AS ALERT-BOX ERROR.
        ELSE
          MESSAGE "Invalid " +
                  TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                  ", try help..."
              VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 B-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job NO-LOCK
           WHERE job.company EQ cocode
             AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
             AND job.job-no2 EQ int(rm-rctd.job-no2:SCREEN-VALUE)
                      USE-INDEX job-no NO-ERROR.
      IF NOT AVAIL job THEN
      DO:
        MESSAGE "Invalid " +
                TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                ", try help..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.

      RUN jc/chk-stat.p(RECID(job), 1, YES, OUTPUT ll).
      IF NOT ll THEN DO:
        rm-rctd.job-no:READ-ONLY = NO.
        APPLY 'ENTRY':U TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR. 
      END.
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
                      AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                      AND (rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     OR ip-int LT 1)
                      AND (rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} OR ip-int LT 2)
                      AND (rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     OR ip-int LT 3))
    THEN DO:
      RELEASE rm-rdtlh.
      IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) LE 0 OR ip-int NE 99 THEN
        IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" AND ip-int GE 3 THEN
        FOR EACH rm-rdtlh
            WHERE rm-rdtlh.company  EQ cocode
              AND rm-rdtlh.loc      EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rdtlh.tag      EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              AND rm-rdtlh.loc-bin  EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            USE-INDEX tag NO-LOCK,
            FIRST rm-rcpth
            WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
              AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code
              AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            USE-INDEX r-no NO-LOCK:
          LEAVE.
        END.
        ELSE
        FOR EACH rm-rcpth
            WHERE rm-rcpth.company EQ cocode
              AND rm-rcpth.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            USE-INDEX i-no NO-LOCK,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
              AND (rm-rdtlh.loc      EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     OR ip-int LT 1)
              AND (rm-rdtlh.loc-bin  EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} OR ip-int LT 2)
            NO-LOCK:
          LEAVE.
        END.

      IF NOT AVAIL rm-rdtlh THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
        IF ip-int EQ 3 THEN
          APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
        ELSE
        IF ip-int EQ 2 THEN
          APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
        ELSE
          APPLY "entry" TO rm-rctd.loc IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no B-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 THEN DO:
      IF v-msg EQ "" THEN
        IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
          v-msg = "You may not enter both " +
                  TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) + " and " +
                  TRIM(rm-rctd.po-no:LABEL IN BROWSE {&BROWSE-NAME}).

      FIND FIRST po-ordl
          WHERE po-ordl.company   EQ rm-rctd.company
            AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            AND po-ordl.item-type EQ YES
          NO-LOCK NO-ERROR.

      IF v-msg EQ "" THEN
        IF NOT AVAIL po-ordl THEN v-msg = "is invalid, try help".

      IF v-msg EQ "" THEN
        IF NOT CAN-FIND(FIRST po-ord WHERE
           po-ord.company EQ po-ordl.company AND
           po-ord.po-no EQ po-ordl.po-no AND
           po-ord.type EQ "S") THEN
           v-msg = "must be 'S'heets from Roll type PO".
    END.

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(rm-rctd.po-no:LABEL IN BROWSE {&BROWSE-NAME}) +
              " " + v-msg + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.po-no IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty B-table-Win 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN DO:
      MESSAGE "Issued qty may not be 0..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty2 B-table-Win 
PROCEDURE valid-qty2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-rm-rctd FOR rm-rctd.
  DEFINE VARIABLE dTotalI AS DEC     NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST rm-bin NO-LOCK
        WHERE rm-bin.company EQ cocode
          AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-ERROR.
    FOR EACH bf-rm-rctd 
        WHERE bf-rm-rctd.company EQ cocode
          AND bf-rm-rctd.rita-code EQ "I"                    
          AND bf-rm-rctd.i-no      EQ rm-bin.i-no
          AND bf-rm-rctd.loc       EQ rm-bin.loc
          AND bf-rm-rctd.loc-bin   EQ rm-bin.loc-bin     
          AND bf-rm-rctd.tag       EQ rm-bin.tag  
          AND NOT (AVAIL(rm-rctd) AND ROWID(rm-rctd) EQ ROWID(bf-rm-rctd))
        NO-LOCK USE-INDEX rita-code:
     dTotalI = dTotalI + bf-rm-rctd.qty.    
    END. /* for each bf-rm-rctd */

    IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 AND
        rm-bin.qty LT DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "Issue Quantity exceeds Quantity on Hand for this Warehouse/Bin/Tag Location..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END. /* If quantity is over */
    ELSE DO:
      FIND ITEM WHERE ITEM.company EQ rm-bin.company
        AND ITEM.i-no EQ rm-bin.i-no
        NO-LOCK NO-ERROR.

      IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 
          AND rm-bin.qty LT (DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) + dTotalI) 
          AND AVAIL(item) AND item.stocked THEN DO:
        MESSAGE "Issue Quantity exceeds Quantity on Hand + Unposted Issues for this Warehouse/Bin/Tag Location..."          
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END. /* If quantity is over on-hand + issued */ 
    END. /* Else... */

  END. /* Do with frame ... */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num B-table-Win 
PROCEDURE valid-s-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 THEN DO:
      MESSAGE "Sheet # may not be 0..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.s-num IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom B-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
        CAPS(rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    IF INDEX(lv-uom-list,rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) LE 0 THEN DO:
      MESSAGE TRIM(rm-rctd.pur-uom:LABEL IN BROWSE {&BROWSE-NAME}) +
              " must be " + TRIM(lv-uom-list) + "..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-jobmat B-table-Win 
PROCEDURE validate-jobmat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-for-item-only AS LOG NO-UNDO.

    DEF VAR v-job-up AS DEC NO-UNDO.
    DEF VAR v-frm AS INT NO-UNDO.
    DEF VAR v-blk AS INT NO-UNDO.
    DEF VAR v-out AS INT NO-UNDO.
    DEF VAR choice AS LOG NO-UNDO.
    DEF VAR v-cost AS DEC NO-UNDO.
    DEF VAR v-bwt AS DEC NO-UNDO.
    DEF VAR v-len AS DEC NO-UNDO.
    DEF VAR v-wid AS DEC NO-UNDO.
    DEF VAR v-dep AS DEC NO-UNDO.
    DEF VAR v-oh-hand AS DEC NO-UNDO.

    DEF BUFFER xjob-mat FOR job-mat.

    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.
    RELEASE job.
    RELEASE job-mat.

    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
    FIND FIRST job
        WHERE job.company EQ cocode
          AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        NO-LOCK NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat WHERE job-mat.company = job.company
                         AND job-mat.job = job.job
                         AND job-mat.job-no = job.job-no
                         AND job-mat.job-no2 = job.job-no2
                         AND job-mat.i-no = rm-rctd.i-no:SCREEN-VALUE
                         AND (ip-for-item-only OR
                              (job-mat.frm = INT(rm-rctd.s-num:SCREEN-VALUE)))
/*                                AND job-mat.blank-no = INT(rm-rctd.b-num:SCREEN-VALUE))) */
                         USE-INDEX seq-idx NO-LOCK NO-ERROR.
    IF NOT AVAIL job-mat AND rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" 
     THEN DO:
       MESSAGE "Update item on Job file? " VIEW-AS ALERT-BOX QUESTION
                     BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF ll-ans THEN DO:
            FIND FIRST job WHERE job.company = cocode
                             AND job.job-no =  rm-rctd.job-no:SCREEN-VALUE
                             AND job.job-no2 = int(rm-rctd.job-no2:SCREEN-VALUE)
                             NO-LOCK NO-ERROR.
            v-job-up = 0.
            for each job-hdr
                where job-hdr.company eq cocode
                  and job-hdr.job     eq job.job
                  and job-hdr.job-no  eq job.job-no
                  and job-hdr.job-no2 eq job.job-no2
                  and job-hdr.frm     eq input rm-rctd.s-num
                no-lock:
                v-job-up = v-job-up + job-hdr.n-on.
            end.

            RUN lookup-job-mat (ip-for-item-only).

            find job-mat where recid(job-mat) eq fil_id no-error.

            if avail job-mat then do:

               if index("1234BPR",item.mat-type) gt 0 then do on endkey undo, retry:
                  assign
                   v-frm = job-mat.frm
                   v-blk = job-mat.blank-no
                   v-out = job-mat.n-up / v-job-up.
                   run rm/g-iss2.w ( v-frm, v-blk , input-output v-out ).
                /*
                  display v-frm v-blk with frame s-b.
                  update v-out with frame s-b.
                */
               end.
              v-oh-hand = calc-oh().
              if item.i-code eq "R"
                  OR (ITEM.i-code = "E"
                      AND rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} GT ""
                      AND v-oh-hand GT 0 ) then do:
                 if (item.r-wid ne 0 and  item.r-wid lt job-mat.wid) or
                    (item.r-wid eq 0 and (item.s-wid lt job-mat.wid or
                                         item.s-len lt job-mat.len))
                 then do on endkey undo, retry:
                     choice = no.

                     IF ITEM.r-wid <> 0 THEN
                         run rm/g-iss21.w (job-mat.len, job-mat.len, item.r-wid,job-mat.wid, job-mat.frm,
                                        output choice)  .
                     else run rm/g-iss21.w (item.s-len, job-mat.len, item.s-wid,job-mat.wid, job-mat.frm,
                                        output choice)  .
                    if not choice then do: release job-mat.
                       APPLY "entry" TO rm-rctd.job-no.
                       RETURN error.
                    END.
                 END.
              END.
            end. /* avail job-mat */

            find first xitem where xitem.company eq cocode
                    and xitem.i-no    eq job-mat.rm-i-no
                  no-lock no-error.
            if not avail xitem then release job-mat.

            if avail job-mat then do:
                create xjob-mat.
                buffer-copy job-mat to xjob-mat.

                find job-mat where recid(job-mat) eq recid(xjob-mat).

                if job-mat.sc-uom eq job-mat.qty-uom then
                  v-cost = job-mat.std-cost.
                else
                  run sys/ref/convcuom.p(job-mat.sc-uom,
                                         job-mat.qty-uom,
                                         job-mat.basis-w,
                                         job-mat.len,
                                         job-mat.wid,
                                         item.s-dep,
                                         job-mat.std-cost,
                                         output v-cost).

                v-cost = v-cost * job-mat.qty.

                assign
                 rm-rctd.s-num:SCREEN-VALUE   = string(job-mat.frm)
                 rm-rctd.b-num:SCREEN-VALUE   = string(job-mat.blank-no)
                 job-mat.rm-i-no = item.i-no
                 job-mat.i-no    = item.i-no
                 job-mat.sc-uom  = item.cons-uom
                 job-mat.wid     = if item.r-wid ne 0 then
                                     item.r-wid else item.s-wid
                 job-mat.len     = if item.r-wid ne 0 then
                                     job-mat.len else item.s-len
                 job-mat.basis-w = item.basis-w
                 job-mat.qty     = job-mat.qty * IF job-mat.n-up EQ 0 THEN 1 ELSE job-mat.n-up
                 job-mat.n-up    = v-job-up * v-out
                 job-mat.qty     = job-mat.qty / IF job-mat.n-up EQ 0 THEN 1 ELSE job-mat.n-up.

                {sys/inc/roundup.i job-mat.qty}

                v-cost = v-cost / job-mat.qty.

                if job-mat.qty-uom eq job-mat.sc-uom then
                  job-mat.std-cost = v-cost.
                else
                  run sys/ref/convcuom.p(job-mat.qty-uom,
                                         job-mat.sc-uom,
                                         job-mat.basis-w,
                                         job-mat.len,
                                         job-mat.wid,
                                         item.s-dep,
                                         v-cost,
                                         output job-mat.std-cost).

                assign
                 v-bwt = job-mat.basis-w
                 v-len = job-mat.len
                 v-wid = job-mat.wid
                 v-dep = item.s-dep.
            end. /* avail job-mat */

       end.  /* ll-ans = yes */
       ELSE do:
           APPLY "entry" TO rm-rctd.job-no.
           RETURN error.  /* not update item */
       END.
    END. /* not avail job-mat */

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
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-oh B-table-Win 
FUNCTION calc-oh RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lvTag LIKE rm-rctd.tag NO-UNDO.  
lvTag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
  FIND FIRST loadtag NO-LOCK WHERE loadtag.company EQ g_company
                               AND loadtag.item-type EQ YES
                               AND loadtag.tag-no EQ lvTag NO-ERROR.

    IF AVAIL(loadtag) THEN
    DO:
       FIND FIRST rm-bin NO-LOCK
            WHERE rm-bin.company EQ cocode
              AND rm-bin.i-no    EQ loadtag.i-no
              AND rm-bin.loc     EQ loadtag.loc
              AND rm-bin.loc-bin EQ loadtag.loc-bin
              AND rm-bin.tag     EQ loadtag.tag-no 
              AND rm-bin.qty     GT 0 
            NO-ERROR.
       IF AVAIL rm-bin THEN
         RETURN rm-bin.qty.
       ELSE
         RETURN 0.
    END.
    ELSE
     RETURN 0.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS char ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR ld-dim AS DEC NO-UNDO.
  DEF VAR v-wid-num AS DEC NO-UNDO.
  DEF VAR v-len-num AS DEC NO-UNDO.
  DEF BUFFER b-jm FOR job-mat .

  IF AVAIL rm-rctd THEN DO:

     find first po-ordl where po-ordl.company   eq cocode
                          and po-ordl.po-no     eq int(rm-rctd.po-no)
                          and po-ordl.i-no      eq rm-rctd.i-no
                          and po-ordl.job-no    eq rm-rctd.job-no
                          and po-ordl.job-no2   eq rm-rctd.job-no2
                          and po-ordl.item-type eq yes
                          and po-ordl.s-num     eq rm-rctd.s-num
     no-lock no-error.
     if avail po-ordl then
        ASSIGN  v-wid-num = po-ordl.s-wid
                v-len-num = po-ordl.s-len.
     else do:
        if rm-rctd.job-no ne "" then
           find first b-jm where b-jm.company eq cocode
                             and b-jm.rm-i-no eq rm-rctd.i-no
                             and b-jm.job-no  eq rm-rctd.job-no
                             and b-jm.job-no2 eq rm-rctd.job-no2
                             and b-jm.frm     eq rm-rctd.s-num
                             no-lock no-error.
        if avail b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len.
        else do:
           find first ITEM where item.company eq cocode
                             and item.i-no    eq rm-rctd.i-no
                             no-lock no-error.
           if avail item then
              if item.r-wid eq 0 then
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        end.
    end.
      
    IF ip-dim = "W" THEN ld-dim = v-wid-num.
    ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
   
  END.
  
  RETURN ld-dim.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION onlyOneForm B-table-Win 
FUNCTION onlyOneForm RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE firstForm AS INTEGER NO-UNDO.
  DEFINE VARIABLE lastForm AS INTEGER NO-UNDO.

  DEFINE BUFFER bJob FOR job.
  DEFINE BUFFER bJobMat FOR job-mat.

  FIND FIRST bJob NO-LOCK
      WHERE bJob.company EQ cocode
        AND bJob.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        AND bJob.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE) NO-ERROR.
  IF AVAILABLE bJob THEN DO:
    FIND FIRST bJobMat NO-LOCK
         WHERE bJobMat.company EQ bJob.company
           AND bJobMat.job EQ bJob.job
           AND bJobMat.job-no EQ bJob.job-no
           AND bJobMat.job-no2 EQ bJob.job-no2
           AND bJobMat.i-no EQ rm-rctd.i-no:SCREEN-VALUE
         USE-INDEX seq-idx NO-ERROR.
    IF AVAILABLE bJobMat THEN DO:
      firstForm = bJobMat.frm.
      FIND LAST bJobMat NO-LOCK
           WHERE bJobMat.company EQ bJob.company
             AND bJobMat.job EQ bJob.job
             AND bJobMat.job-no EQ bJob.job-no
             AND bJobMat.job-no2 EQ bJob.job-no2
             AND bJobMat.i-no EQ rm-rctd.i-no:SCREEN-VALUE
           USE-INDEX seq-idx NO-ERROR.
      lastForm = bJobMat.frm.
    END. /* avail bjobmat */
  END. /* avail job */
  
  RETURN firstForm EQ lastForm.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

