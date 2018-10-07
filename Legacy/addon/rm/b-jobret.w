&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  addon\rm\b-jobret.w

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
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF,DIA"] NO-UNDO.
DEF VAR lv-rmissue AS CHAR NO-UNDO.

DEF BUFFER xitem FOR ITEM.

DEF NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no LIKE job-mat.i-no
    FIELD rec-id AS RECID.

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR lv-job LIKE job.job NO-UNDO.
DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

DEF VAR lv-i-no LIKE po-ordl.i-no NO-UNDO.
DEF VAR lv-line LIKE po-ordl.line NO-UNDO.
DEF VAR lv-new-record AS LOG NO-UNDO.
DEF VAR ll-casetag AS LOG NO-UNDO.

DEF BUFFER br-tmp FOR rm-rctd.  /* for tag validation */
DEF BUFFER xrm-rdtlh FOR rm-rdtlh. /* for tag validation */

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.r-no rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num rm-rctd.b-num ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom ~
calc-ext-cost() @ ext-cost display-dimension('W') @ lv-po-wid ~
display-dimension('L') @ lv-po-len rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.job-no rm-rctd.job-no2 rm-rctd.s-num ~
rm-rctd.b-num rm-rctd.qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ 'I' ~
AND rm-rctd.tag NE '' ~
AND rm-rctd.qty LT 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company EQ cocode ~
AND rm-rctd.rita-code EQ 'I' ~
AND rm-rctd.tag NE '' ~
AND rm-rctd.qty LT 0 NO-LOCK ~
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
     SIZE 37 BY 1 NO-UNDO.

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
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rctd.rct-date COLUMN-LABEL "Return Date" FORMAT "99/99/9999":U
      rm-rctd.po-no FORMAT "x(6)":U
      rm-rctd.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>9.99<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U
  ENABLE
      rm-rctd.tag
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.qty
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
     auto_find AT ROW 16.71 COL 92 COLON-ALIGNED HELP
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table 1 F-Main */
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
     _Where[1]         = "rm-rctd.company EQ cocode
AND rm-rctd.rita-code EQ 'I'
AND rm-rctd.tag NE ''
AND rm-rctd.qty LT 0"
     _FldNameList[1]   > asi.rm-rctd.r-no
"r-no" "Seq#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.rct-date
"rct-date" "Return Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.po-no
"po-no" ? "x(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.job-no
"job-no" "Job" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.i-no
"i-no" "Item" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.s-num
"s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.b-num
"b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.pur-uom
"pur-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost
"cost" "Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.rm-rctd.cost-uom
"cost-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
 DEF VAR v-i-no AS CHAR NO-UNDO.

 ASSIGN
    ll-help-run = YES
    char-val = "".
 
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
           if char-val <> "" then do:
              assign
               po-no:screen-value in browse {&BROWSE-NAME} = entry(1,char-val)
               lv-i-no                                     = entry(2,char-val)
               lv-line                                     = int(entry(6,char-val)).
           END.
     end.

     when "job-no" or when "job-no2" then do:
           run windows/l-jobno.w (rm-rctd.company,FOCUS:SCREEN-VALUE, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val).
           end.  
     END.
     when "loc" then do:
           run rm/l-loc.w (rm-rctd.company,focus:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&BROWSE-NAME}  = entry(1,char-val).
           end.
     end.
     when "loc-bin" then do:
           run rm/l-locbin.w (rm-rctd.company,rm-rctd.loc:screen-value, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value  = entry(1,char-val).
           end.
     end.
     WHEN "tag" THEN DO:
         RUN rm/rmi-no.w(g_company, OUTPUT v-i-no).

         IF v-i-no NE "" THEN
            run windows/l-ldtag10.w (g_company,tag:screen-value,v-i-no,output char-val).

         if char-val <> "" then do :
            tag:SCREEN-VALUE = ENTRY(1,char-val).
            /*  ===*/
            /*FIND FIRST br-tmp WHERE br-tmp.company = g_company AND
                          br-tmp.tag = SELF:SCREEN-VALUE AND
                          br-tmp.rita-code = "I"
                      AND RECID(br-tmp) <> RECID(rm-rctd)
                      NO-LOCK NO-ERROR.
            IF AVAIL br-tmp THEN DO:
               MESSAGE "This Tag Number Has Already Been Used." skip
                       "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.*/

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
  DEFINE VARIABLE lvTag AS CHARACTER NO-UNDO.
  
  IF LASTKEY NE -1 THEN  DO:
    lvTag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    
    RUN valid-issued-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    lvTag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

    {addon/loadtags/disptagr.i "RMItem" lvTag}
    
    rm-rctd.po-no:SCREEN-VALUE = ''.

    RUN valid-issued-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
    IF adm-new-record THEN
    DO:
       FIND FIRST rm-rdtlh WHERE
            rm-rdtlh.company EQ cocode AND
            rm-rdtlh.loc EQ loadtag.loc AND
            rm-rdtlh.loc-bin EQ loadtag.loc-bin AND
            rm-rdtlh.tag EQ loadtag.tag-no AND
            rm-rdtlh.rita-code EQ "I" AND
            rm-rdtlh.qty GT 0
            NO-LOCK NO-ERROR.
       
       IF AVAIL rm-rdtlh THEN DO:
       
         FOR EACH rm-rcpth OF rm-rdtlh
             NO-LOCK BREAK BY rm-rcpth.trans-date DESC:
              
             ASSIGN
               rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-rcpth.pur-uom
               rm-rctd.cost:SCREEN-VALUE = STRING(rm-rdtlh.cost)
               rm-rctd.cost-uom:SCREEN-VALUE = rm-rcpth.pur-uom
               rm-rctd.job-no:SCREEN-VALUE = STRING(rm-rcpth.job-no)
               rm-rctd.job-no2:SCREEN-VALUE = STRING(rm-rcpth.job-no2)
               rm-rctd.loc:SCREEN-VALUE = rm-rdtlh.loc
               rm-rctd.loc-bin:SCREEN-VALUE = rm-rdtlh.loc-bin
               rm-rctd.s-num:SCREEN-VALUE = STRING(rm-rdtlh.s-num)
               rm-rctd.b-num:SCREEN-VALUE = STRING(rm-rdtlh.b-num).
             LEAVE.
         END.
       
         RUN update-qty-proc.
       END. /* avail rm-rdtlh*/
    END.

       
    
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
    APPLY 'ENTRY' TO rm-rctd.loc IN BROWSE {&BROWSE-NAME}.

    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.tag IN BROWSE Browser-Table /* Tag# */
DO:
  /*RUN new-bin.*/
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
  /*RUN new-bin.*/
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
  /*RUN new-bin.*/
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
  IF LASTKEY NE -1 THEN DO:

    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:   
  RUN new-job-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY NE -1 AND
     rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ '' THEN DO:
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

    RUN valid-job-no2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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

    RUN validate-jobmat (YES) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item */
DO:
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
   IF rm-rctd.s-num NE INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
      RUN update-qty-proc.
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

  IF rm-rctd.b-num NE INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
     RUN update-qty-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  DO WITH FRAME {&FRAME-NAME}:

    RUN valid-return-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  RUN get-matrix (FALSE).
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
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
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

FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CASETAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "CASETAG"
     sys-ctrl.descrip  = "Case Label Format?   Use Case Label as LoadTag?".
  END.
  ll-casetag = sys-ctrl.log-fld.

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

  DEF BUFFER b-rm-rctd FOR rm-rctd.

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
        if v-len eq 0 then v-len = if avail item then item.s-len else 0.
        if v-wid eq 0 then v-wid = if avail item and item.r-wid ne 0 then item.r-wid else if avail item then item.s-wid else 0.
        if v-bwt eq 0 then v-bwt = if avail item then item.basis-w else 0.
        ASSIGN lv-qty-uom = rm-rctd.pur-uom
               lv-cost-uom = rm-rctd.cost-uom.
  end.
  
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

   ext-cost = lv-out-qty * lv-out-cost.
  
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
  
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         and job.job-no2 eq integer(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                no-lock no-error.
        if avail job then do :
             v-job-up = 0.
             for each job-hdr
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
        EACH rm-rdtlh
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

    FOR EACH b-rm-rctd
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
        EACH rm-rdtlh
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

    FOR EACH b-rm-rctd
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

  ext-cost = lv-out-qty * lv-out-cost.
  ASSIGN rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
         rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
         rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
         rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom.
  disp ext-cost with browse {&BROWSE-NAME}.
end.


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
    {&BROWSE-NAME}:READ-ONLY = NO.

    APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.    
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
  DEF VAR lv-tag AS CHAR NO-UNDO.

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
  IF ERROR-STATUS:ERROR THEN RETURN .

  RUN valid-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN .

  lv-tag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN rmrep\rmloadtg3.w(INPUT yes, INPUT lv-tag, INPUT DECIMAL(rm-rctd.qty:SCREEN-VALUE) * -1).
  
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'CURSOR-LEFT':U TO {&BROWSE-NAME}.
    END.
  END.

  /*RUN multi-issues (ROWID(rm-rctd)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

  /*if in update mode and there was another line after line being updated,
    screen-value errors were appearing*/
  IF lv-new-record THEN
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

        /*RUN new-bin.*/

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
  /*DO WITH FRAME {&FRAME-NAME}:
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
        rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(rm-bin.qty).
    END.
  END.*/

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

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND item.i-code  EQ "R"
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

        /*RUN new-bin.*/

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
           rm-rctd.b-num:SCREEN-VALUE = "1". /* string(job-hdr.blank-no) */
            
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

        /*RUN new-bin.*/
      END.

      DELETE tt-selected.
    END.
  END.

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
    RUN lookup-job-mat (?, OUTPUT fil_id).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-qty-proc B-table-Win 
PROCEDURE update-qty-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-out-qty as dec no-undo.
   def var v-len like po-ordl.s-len no-undo.
   def var v-wid like po-ordl.s-len no-undo.
   def var v-dep like po-ordl.s-len no-undo. 
   def var v-bwt like po-ordl.s-len no-undo.
   DEF VAR qty-bal AS DEC DECIMALS 6 NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
   
      FOR EACH rm-rdtlh WHERE
          rm-rdtlh.company EQ cocode AND
          rm-rdtlh.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
          rm-rdtlh.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
          rm-rdtlh.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) AND
          rm-rdtlh.s-num EQ INT(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name}) AND
          rm-rdtlh.b-num EQ INT(rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name}) AND
          rm-rdtlh.rita-code EQ "I" 
          NO-LOCK,
          FIRST rm-rcpth OF rm-rdtlh NO-LOCK:
          
          lv-out-qty = rm-rdtlh.qty.
          
          IF rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE
             rm-rcpth.pur-uom THEN DO:
            
             ASSIGN
               v-bwt = display-dimension('B')
               v-len = display-dimension('L')
               v-wid = display-dimension('W').
            
             find item  where item.company eq cocode                           /* no screen-value used */
                   and item.i-no  eq rm-rctd.i-no:screen-value in browse {&BROWSE-NAME}
                      use-index i-no no-lock no-error.
             if avail item THEN DO:
               v-dep = item.s-dep.
               RELEASE ITEM.
            END.
          
            run custom/convquom.p(cocode,
                          rm-rcpth.pur-uom,
                          rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                          v-bwt,
                          v-len,
                          v-wid,
                          v-dep,
                          rm-rdtlh.qty,
                          output lv-out-qty).
          END.
          
          ASSIGN qty-bal = qty-bal + lv-out-qty.
         END.
       
         rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(qty-bal * -1).
   END.
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

  RUN valid-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN validate-jobmat (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-job-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-return-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
  lv-new-record = adm-new-record.
      
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


  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" THEN
        v-msg = TRIM(rm-rctd.i-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                " may not be spaces".

    IF v-msg EQ "" THEN DO:
      FIND FIRST item 
          WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            /*AND item.i-code  EQ "R"*/
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

    /*IF v-msg EQ "" THEN
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
      END.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-issued-tag B-table-Win 
PROCEDURE valid-issued-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-valid-tag AS LOG NO-UNDO.

   FIND FIRST loadtag NO-LOCK
       WHERE loadtag.company = cocode and
       loadtag.item-type = YES and
       loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-ERROR.

    IF NOT AVAIL loadtag THEN DO:
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company EQ g_company 
            AND loadtag.item-type EQ YES 
            AND loadtag.misc-char[1] EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
             NO-ERROR.

    IF AVAIL loadtag THEN
       rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = loadtag.tag-no.
     
    END.

    IF AVAIL loadtag THEN
      FOR EACH rm-rdtlh FIELDS(r-no) WHERE
           rm-rdtlh.company EQ cocode AND
           rm-rdtlh.loc EQ loadtag.loc AND
           rm-rdtlh.loc-bin EQ loadtag.loc-bin AND
           rm-rdtlh.tag EQ loadtag.tag-no
           NO-LOCK,
      FIRST rm-rcpth WHERE
           rm-rcpth.r-no    EQ rm-rdtlh.r-no AND
           rm-rcpth.i-no    EQ loadtag.i-no
           NO-LOCK:
  
           lv-valid-tag = YES.
           LEAVE.
      END.

  IF NOT lv-valid-tag THEN
  DO:
     MESSAGE "Tag not issued, issue tag to create return."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     RETURN ERROR.
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

      ELSE 
        IF NOT CAN-FIND(FIRST rm-rdtlh WHERE
         rm-rdtlh.company EQ cocode AND
         rm-rdtlh.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
         rm-rdtlh.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
         rm-rdtlh.rita-code EQ "I" AND
         rm-rdtlh.qty GT 0) THEN DO:
           MESSAGE "Invalid " + TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) + " for Tag #." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
           RETURN ERROR.
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
  DO WITH FRAME {&FRAME-NAME}:
    
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
      ELSE 
        IF NOT CAN-FIND(FIRST rm-rdtlh WHERE
         rm-rdtlh.company EQ cocode AND
         rm-rdtlh.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
         rm-rdtlh.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
         rm-rdtlh.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) AND
         rm-rdtlh.rita-code EQ "I" AND
         rm-rdtlh.qty GT 0) THEN DO:
           MESSAGE "Invalid " + TRIM(rm-rctd.job-no2:LABEL IN BROWSE {&BROWSE-NAME}) + " for Tag #." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
           RETURN ERROR.
       END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-tag B-table-Win 
PROCEDURE valid-job-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER rm-rctd-1 FOR rm-rctd.
 DEF VAR dup-var AS LOG NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:

   IF adm-new-record THEN
     dup-var = CAN-FIND(FIRST rm-rctd-1 WHERE
               rm-rctd-1.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
               rm-rctd-1.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
               rm-rctd-1.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) AND
               rm-rctd-1.rita-code EQ 'I').
   ELSE
     dup-var = CAN-FIND(FIRST rm-rctd-1 WHERE
               rm-rctd-1.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
               rm-rctd-1.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
               rm-rctd-1.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) AND
               rm-rctd-1.r-no NE INT(rm-rctd.r-no:SCREEN-VALUE IN BROWSE {&browse-name}) AND
               rm-rctd-1.rita-code EQ 'I').
   
   IF dup-var THEN DO:
     MESSAGE "Duplicate Tag/Job No." VIEW-AS ALERT-BOX ERROR.
     APPLY "ENTRY":U TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
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
                      AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                      AND (rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     OR ip-int LT 1)
                      AND (rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} OR ip-int LT 2)
                      AND (rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     OR ip-int LT 3))
    THEN DO:
      RELEASE rm-rdtlh.
      IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 OR ip-int NE 99 THEN
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

      IF v-msg EQ "" AND
         NOT CAN-FIND(FIRST po-ord WHERE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-return-qty B-table-Win 
PROCEDURE valid-return-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR qty-bal AS DEC DECIMALS 6 NO-UNDO.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  DEF VAR v-qty AS DEC NO-UNDO.

  FOR EACH rm-rdtlh WHERE
    rm-rdtlh.company EQ cocode AND
    rm-rdtlh.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
    rm-rdtlh.job-no EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
    rm-rdtlh.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) AND
    rm-rdtlh.rita-code EQ "I" 
    NO-LOCK,
    FIRST rm-rcpth OF rm-rdtlh NO-LOCK:
    
    lv-out-qty = rm-rdtlh.qty.

    IF rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE
      rm-rcpth.pur-uom THEN DO:
    
      ASSIGN
        v-bwt = display-dimension('B')
        v-len = display-dimension('L')
        v-wid = display-dimension('W').

      find item  where item.company eq cocode                           /* no screen-value used */
            and item.i-no  eq rm-rctd.i-no:screen-value in browse {&BROWSE-NAME}
               use-index i-no no-lock no-error.
      if avail item THEN DO:
        v-dep = item.s-dep.
        RELEASE ITEM.
      END.

      run custom/convquom.p(cocode,
                    rm-rcpth.pur-uom,
                    rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                    v-bwt,
                    v-len,
                    v-wid,
                    v-dep,
                    rm-rdtlh.qty,
                    output lv-out-qty).
    END.

    ASSIGN qty-bal = qty-bal + lv-out-qty.
  END.

  IF ABS(DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name})) GT qty-bal THEN DO:
    MESSAGE "Return Quantity exceeds Issued Quantity of " +
            TRIM(STRING(qty-bal)) + " " + TRIM(rm-rcpth.pur-uom) + "." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
    RETURN ERROR.
  END.
  ELSE IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN
    rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(-1 * DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name})).
  
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

    RELEASE job.
    RELEASE job-mat.

    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
       FIND FIRST job WHERE
            job.company EQ cocode AND
            job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
            job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
            NO-LOCK NO-ERROR.

    IF AVAIL job THEN
       FIND FIRST job-mat WHERE job-mat.company = job.company AND
            job-mat.job = job.job AND
            job-mat.job-no = job.job-no AND
            job-mat.job-no2 = job.job-no2 AND
            job-mat.i-no = rm-rctd.i-no:SCREEN-VALUE AND
            (ip-for-item-only OR
            (job-mat.frm = INT(rm-rctd.s-num:SCREEN-VALUE) AND
            job-mat.blank-no = INT(rm-rctd.b-num:SCREEN-VALUE)))
            USE-INDEX seq-idx NO-LOCK NO-ERROR.

    IF NOT AVAIL job-mat AND rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
       
       MESSAGE "Invalid Job."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "entry" TO rm-rctd.job-no.
       RETURN error.
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
  DEF VAR v-btw AS DEC NO-UNDO.
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
                v-len-num = po-ordl.s-len
                v-btw = 0.
     else do:
        if rm-rctd.job-no ne "" then
           find first b-jm where b-jm.company eq cocode
                             and b-jm.rm-i-no eq rm-rctd.i-no
                             and b-jm.job-no  eq rm-rctd.job-no
                             and b-jm.job-no2 eq rm-rctd.job-no2
                             and b-jm.frm     eq rm-rctd.s-num
                             no-lock no-error.
        if avail b-jm THEN ASSIGN v-wid-num = b-jm.wid
                                  v-len-num = b-jm.len
                                  v-btw     = b-jm.basis-w.
        else do:
           find first ITEM where item.company eq cocode
                             and item.i-no    eq rm-rctd.i-no
                             no-lock no-error.
           if avail item then
              if item.r-wid eq 0 then
                 ASSIGN v-wid-num = item.s-wid
                        v-len-num = item.s-len
                        v-btw     = item.basis-w.
              ELSE ASSIGN v-wid-num = item.r-wid
                          v-len-num = 12.
        end.
    end.
      
    IF ip-dim = "W" THEN ld-dim = v-wid-num.
    ELSE IF ip-dim = "L" THEN ld-dim = v-len-num.
    ELSE IF ip-dim = "B" THEN ld-dim = v-btw.
   
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

