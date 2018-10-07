&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  rm\b-issued.w

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

&SCOPED-DEFINE yellowColumnsName b-issued
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

&SCOPED-DEFINE BRWSDEFS rm-issued

DEF VAR char-val AS cha NO-UNDO.
DEF VAR ext-cost AS DECIMAL NO-UNDO.

DEF VAR ls-prev-po AS cha NO-UNDO.
DEF VAR hd-post AS WIDGET-HANDLE NO-UNDO.
DEF VAR hd-post-child AS WIDGET-HANDLE NO-UNDO.
DEF VAR ll-help-run AS LOG NO-UNDO.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid AS DEC FORMAT ">>>9.9999" NO-UNDO.
DEF VAR lv-po-len AS DEC FORMAT ">>,>>9.9999" NO-UNDO.
DEF VAR v-len LIKE lv-po-len NO-UNDO.
DEF VAR v-wid LIKE lv-po-len NO-UNDO.
DEF VAR lv-rmissue AS CHAR NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF,DIA"] NO-UNDO.
DEF VAR jobreopn-log AS LOG NO-UNDO.
DEF VAR vlc-success AS CHAR INIT "" NO-UNDO.

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
DEF VAR v-get-tandem-rec AS LOG NO-UNDO.
DEF VAR ll-is-copy-record AS LOG NO-UNDO.
DEF VAR lAllowRmAdd AS LOG NO-UNDO.
DEF VAR lcReturn   AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG  NO-UNDO.
DEFINE VARIABLE v-bin   AS CHARACTER     NO-UNDO.
DO TRANSACTION:
  {sys/inc/rmrecpt.i}
END.

RUN sys/ref/nk1look.p (cocode, "RMAllowAdd", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

   lAllowRmAdd = LOGICAL(lcReturn) NO-ERROR.  

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "RMTAGS"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company = cocode
         sys-ctrl.name    = "RMTAGS"
         sys-ctrl.descrip = "Number of RM Loadtags to Print & Create Wip Tags"
         sys-ctrl.char-fld = ""
         sys-ctrl.int-fld = 1
         sys-ctrl.log-fld = FALSE. /* true create wip/false do not */
   END.

ASSIGN v-rmtags-log = sys-ctrl.log-fld.

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
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom ~
calc-ext-cost() @ ext-cost display-dimension('W') @ lv-po-wid ~
display-dimension('L') @ lv-po-len rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = cocode and ~
rm-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 RECT-5 browse-order ~
auto_find Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

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
  ( INPUT ip-dim AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear" 
     SIZE 8 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 78 BY 1 NO-UNDO.

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
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      rm-rctd.rct-date COLUMN-LABEL "Issue Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      rm-rctd.po-no FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U LABEL-BGCOLOR 14
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U LABEL-BGCOLOR 14
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U LABEL-BGCOLOR 14
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U LABEL-BGCOLOR 14
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U LABEL-BGCOLOR 14
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.pur-uom COLUMN-LABEL "PUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>>,>>9.99<<<<":U
            LABEL-BGCOLOR 14
      rm-rctd.cost-uom COLUMN-LABEL "CUOM" FORMAT "x(4)":U WIDTH 7
            LABEL-BGCOLOR 14
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>>,>>9.99<<<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
      rm-rctd.user-id COLUMN-LABEL "UserId" FORMAT "x(8)":U
  ENABLE
      rm-rctd.rct-date
      rm-rctd.po-no
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.tag
      rm-rctd.qty
      rm-rctd.pur-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 85 COLON-ALIGNED NO-LABEL
     auto_find AT ROW 16.71 COL 115 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
     Btn_Clear_Find AT ROW 16.71 COL 138 HELP
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
         WIDTH              = 146.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}
{custom/yellowColumns.i}

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
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""I"""
     _FldNameList[1]   > asi.rm-rctd.r-no
"r-no" "Seq#" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.rct-date
"rct-date" "Issue Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.po-no
"po-no" ? "x(6)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.job-no
"job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.i-no
"i-no" "Item" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.rm-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.rm-rctd.s-num
"s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.b-num
"b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > asi.rm-rctd.pur-uom
"pur-uom" "PUOM" "x(4)" "character" ? ? ? 14 ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > asi.rm-rctd.cost
"cost" "Cost" "->>>,>>>,>>9.99<<<<" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > asi.rm-rctd.cost-uom
"cost-uom" "CUOM" "x(4)" "character" ? ? ? 14 ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>>,>>9.99<<<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > asi.rm-rctd.user-id
"user-id" "UserId" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
 DEF VAR ll-tag# AS LOG NO-UNDO.
 DEF VAR help-recid AS RECID NO-UNDO.
 DEF VAR lv-search AS cha NO-UNDO.

/* gdm - 08070907 */

 ASSIGN 
   lv-job-no = FILL(" ", 6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-name}))) +
               TRIM(rm-rctd.job-no:SCREEN-VALUE)
   char-val               = ""
   v-number-rows-selected = 0
   look-recid = 0.

/* gdm - 08070907 end */

 ll-help-run = YES.
 CASE FOCUS:NAME:
     WHEN "i-no" THEN DO:
            RUN rm/g-joblk.w (OUTPUT lv-search).  /* search job or item */
            IF lv-search = "job" THEN DO:
                RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                        rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
                
            END.
            ELSE DO:
             /* company,industry,mat-type,i-code,i-no, output, output */
               RUN windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT help-recid).
               IF char-val <> "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE THEN DO :
                  FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val).
                  RUN new-i-no.
               END.
            END.  
     END.

     WHEN "po-no" THEN DO:
           RUN windows/l-poords.w (rm-rctd.company, FOCUS:SCREEN-VALUE, 0, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              ASSIGN
               FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val)
               lv-i-no                                     = ENTRY(2,char-val)
               lv-line                                     = int(ENTRY(6,char-val)).
           END.
     END.

     WHEN "job-no" OR WHEN "job-no2" THEN DO:
           RUN windows/l-jobno.w (rm-rctd.company,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
           IF char-val <> "" THEN DO :
              ASSIGN rm-rctd.job-no:screen-value = ENTRY(1,char-val)
                     rm-rctd.job-no2:screen-value = ENTRY(2,char-val).
           END.  
     END.

     WHEN "loc"     OR
     WHEN "loc-bin" OR
     WHEN "tag"     THEN RUN rmbin-help (FOCUS:HANDLE).
     WHEN "s-num"   THEN DO:
      /* gdm - 08070907 */
        RUN get-job-mat.
/*         
      IF lv-job-no NE "" OR 
         rm-rctd.i-no:SCREEN-VALUE NE ""
        THEN
         RUN windows/l-jobmt3.w (cocode, lv-job-no,
                                 INT(rm-rctd.job-no2:SCREEN-VALUE),
                                 rm-rctd.i-no:SCREEN-VALUE,
                                 OUTPUT char-val,OUTPUT look-recid,
                                 OUTPUT v-number-rows-selected).         
         FIND FIRST w-rm-rctd.
         IF NOT AVAIL w-rm-rctd THEN DO:
             MESSAGE 
                 "There was an error on the selection list." SKIP
                 "Please try again. Thank you"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN NO-APPLY.
             APPLY "ENTRY" TO rm-rctd.s-num.
         END.

         IF v-number-rows-selected > 1 
           THEN RUN check-multi-line.
           ELSE DO:         
            ASSIGN fil_id = look-recid.
            RUN s-b-help. 
           END.
*/
     END.
/* gdm - 08070907 end */
     WHEN "b-num"   THEN RUN s-b-help.
   END CASE.

   help-recid = ?.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON RETURN OF Browser-Table IN FRAME F-Main
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
   
   if avail rm-rctd then    run get-matrix (yes).
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
   
    IF KEYFUNCTION(LASTKEY) = "page-up" OR 
      keyfunction(LASTKEY) = "page-down" OR
      keyfunction(LASTKEY) = "cursor-up" OR
      keyfunction(LASTKEY) = "cursor-down" 
   THEN DO:  
      RETURN NO-APPLY.
   END.

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


&Scoped-define SELF-NAME rm-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.rct-date IN BROWSE Browser-Table /* Issue Date */
DO:
  {custom/currentDatePrompt.i SELF:SCREEN-VALUE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.po-no IN BROWSE Browser-Table /* PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.job-no IN BROWSE Browser-Table /* Job# */
DO:
  RUN new-job-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
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


&Scoped-define SELF-NAME rm-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.i-name IN BROWSE Browser-Table /* Name/Desc */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-name IN BROWSE Browser-Table /* Name/Desc */
DO:
  APPLY "entry" TO rm-rctd.s-num IN BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
    IF LASTKEY = -1 THEN RETURN.
    RUN validate-jobmat (NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.


/*
    DEF VAR v-job-up AS DEC NO-UNDO.
    DEF VAR count-mat AS INT NO-UNDO.
    DEF VAR v-frm AS INT NO-UNDO.
    DEF VAR v-blk AS INT NO-UNDO.
    DEF VAR v-out AS INT NO-UNDO.
    DEF VAR choice AS LOG NO-UNDO.
    DEF VAR v-cost AS DEC NO-UNDO.
    DEF VAR v-bwt AS DEC NO-UNDO.
    DEF VAR v-len AS DEC NO-UNDO.
    DEF VAR v-wid AS DEC NO-UNDO.
    DEF VAR v-dep AS DEC NO-UNDO.

    DEF BUFFER xjob-mat FOR job-mat.

    

    
    FIND FIRST job-mat WHERE job-mat.company = cocode
                       /* and job-mat.job = job.job */      
                         AND job-mat.job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         AND job-mat.job-no2 = INT(rm-rctd.job-no2:SCREEN-VALUE)
                         AND job-mat.i-no = rm-rctd.i-no:SCREEN-VALUE
                         AND job-mat.frm = int(rm-rctd.s-num:SCREEN-VALUE) 
                         AND job-mat.blank-no = INT(rm-rctd.b-num)
                         USE-INDEX seq-idx NO-LOCK NO-ERROR.
    IF NOT AVAIL job-mat AND rm-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       MESSAGE "Update item on Job file? " VIEW-AS ALERT-BOX QUESTION
                     BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF ll-ans THEN DO:
            
            v-job-up = 0.
            for each job-hdr
                where job-hdr.company eq cocode
                  /*and job-hdr.job     eq job.job  */
                  and job-hdr.job-no  eq job.job-no
                  and job-hdr.job-no2 eq job.job-no2
                  and job-hdr.frm     eq input rm-rctd.s-num
                no-lock:
                v-job-up = v-job-up + job-hdr.n-on.  
            end.
          
            for each item-chg:
              delete item-chg.
            end.
            count-mat = 0.

            for each job-mat
                where job-mat.company   eq cocode
                  /*and job-mat.job       eq job.job */
                  and job-mat.job-no    eq INPUT rm-rctd.job-no
                  and job-mat.job-no2   eq INPUT rm-rctd.job-no2
                  and job-mat.frm       eq INt(rm-rctd.s-num:SCREEN-VALUE)
                  and (job-mat.blank-no eq int(rm-rctd.b-num:SCREEN-VALUE) /* or
                       v-frst-fld ne "s-num"  */ )
                  use-index seq-idx no-lock,

                first xitem
                where xitem.company  eq cocode
                  and xitem.i-no     eq job-mat.rm-i-no
                  and xitem.mat-type eq item.mat-type
                no-lock:

              count-mat = count-mat + 1.
              create item-chg.
              assign
               item-chg.i-no   = xitem.i-no
               item-chg.rec-id = recid(job-mat)
               fil_id          = recid(item-chg).
              
            end.

            if count-mat ne 1 then run rm/g-itmchg.w  /*po/look/item-chg.p */ .
            /*if keyfunction(v-lastkey) eq "end-error" then fil_id = ?. */

            find first item-chg where recid(item-chg) eq fil_id
                no-lock no-error.

            if avail item-chg then do:
               find job-mat where recid(job-mat) eq item-chg.rec-id no-error.
              
               if avail job-mat then do on endkey undo, retry:
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

              if avail job-mat and item.i-code eq "R" then do:
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
                   /* display item.s-len
                            job-mat.len when item.r-wid ne 0 @ item.s-len
                          job-mat.len
                          item.s-wid
                            item.r-wid  when item.r-wid ne 0 @ item.s-wid
                          job-mat.wid
                          job-mat.frm
                      with frame tsmall.
                   update choice with frame tsmall. 
                   */
                 end.
                 if not choice then do: release job-mat. RETURN NO-APPLY. END.
              end.
            end. /* avai item-chg */
              
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
                 rm-rctd.b-num:SCREEN-VALUE   = string(job-mat.blank-no)
                 job-mat.rm-i-no = item.i-no
                 job-mat.i-no    = item.i-no
                 job-mat.sc-uom  = item.cons-uom
                 job-mat.wid     = if item.r-wid ne 0 then
                                     item.r-wid else item.s-wid
                 job-mat.len     = if item.r-wid ne 0 then
                                     job-mat.len else item.s-len
                 job-mat.basis-w = item.basis-w
                 job-mat.qty     = job-mat.qty * job-mat.n-up
                 job-mat.n-up    = v-job-up * v-out
                 job-mat.qty     = job-mat.qty / job-mat.n-up.
                     
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
       ELSE RETURN NO-APPLY.  /* not update item */
    END. /* not avail job-mat */
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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
    RUN valid-loc-bin (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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
     
    RUN valid-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
     /* Task No- 03151112     */
     /*RUN valid-qty NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

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
  RUN get-matrix (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* CUOM */
DO:
    IF LASTKEY = -1 THEN RETURN .

    IF INDEX(lv-uom-list,SELF:SCREEN-VALUE) <= 0 THEN DO:
       MESSAGE "Invalid UOM." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    RUN get-matrix (NO).
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{sys/inc/rmissue.i}
lv-rmissue = v-rmissue.
jobreopn-log = rmissue-int EQ 1.

FIND FIRST sys-ctrl WHERE
    sys-ctrl.company EQ cocode AND
    sys-ctrl.name    EQ "RMWHSBIN"
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "RMWHSBIN"
        sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
        sys-ctrl.char-fld = "RMITEM".
    FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

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
       rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm)
       rm-rctd.b-num:SCREEN-VALUE = STRING(job-mat.blank-no).
    END.
  END.
  
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
  DEF INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.

  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR lv-qty-uom AS cha NO-UNDO.
  DEF VAR lv-cost-uom AS cha NO-UNDO.
  DEF VAR v-job-up LIKE job-hdr.n-on NO-UNDO.
  DEF VAR v-out LIKE ef.n-out INIT 1 NO-UNDO.
  DEF VAR lv-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR ld-lf-used AS DEC NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO FORMAT ">,>>9.9<<<".
  DEF VAR ll-disp AS LOG INIT NO NO-UNDO.

  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
IF ip-first-disp  AND AVAIL rm-rctd AND rm-rctd.i-no <> "" THEN DO: /* for row-display */
  FIND item  WHERE item.company EQ cocode                           /* no screen-value used */
                     AND item.i-no  EQ rm-rctd.i-no /*:screen-value in browse {&BROWSE-NAME}*/
                     USE-INDEX i-no NO-LOCK NO-ERROR.
  IF AVAIL item THEN v-dep = item.s-dep.

  RELEASE po-ordl.
      
  IF INT(rm-rctd.po-no) NE 0 THEN
  FIND FIRST po-ordl NO-LOCK
      WHERE po-ordl.company   EQ rm-rctd.company
        AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
        AND (po-ordl.i-no     EQ lv-i-no OR lv-i-no EQ "")
        AND (po-ordl.line     EQ lv-line OR lv-line EQ 0)
        AND po-ordl.item-type EQ YES
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

  ELSE DO:
        FIND FIRST job WHERE job.company EQ cocode
                         AND job.job-no  EQ rm-rctd.job-no
                         AND job.job-no2 EQ rm-rctd.job-no2
                NO-LOCK NO-ERROR.
        IF AVAIL job THEN DO :
             FIND FIRST job-mat WHERE job-mat.company EQ cocode
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.i-no    EQ rm-rctd.i-no
                                  AND job-mat.frm     EQ rm-rctd.s-num
                   NO-LOCK NO-ERROR.
             IF AVAIL job-mat THEN DO:
               ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             END.
        END.
  END.
        IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
        IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
        IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
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
        
  RUN custom/convquom.p(cocode,
                        rm-rctd.pur-uom,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.qty,
                        OUTPUT lv-out-qty).
  
  /* convert cost pr-uom*/
  RUN custom/convcuom.p(cocode,
                        rm-rctd.cost-uom,
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.cost, OUTPUT lv-out-cost).

   ASSIGN
    ext-cost = lv-out-qty * lv-out-cost
    ll-disp  = YES.
  
/*    ASSIGN                                                                     */
/*      rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost) */
/*      rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom     */
/*      ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)        */
/*      rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)   */
/*      rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom       */
/*      NO-ERROR.                                                                */

   ASSIGN
   rm-rctd.qty:MODIFIED IN BROWSE {&BROWSE-NAME} = NO
   rm-rctd.pur-uom:MODIFIED IN BROWSE {&BROWSE-NAME} = NO.

  /*disp ext-cost with browse {&BROWSE-NAME}. it's displayed automatically */
 /* message "after calc:" po-ordl.cons-uom rm-rctd.cost-uom lv-out-cost ext-cost.
  */

END. /* ip-first */
/* ======================================================================= */
ELSE 
IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> "" THEN DO: /* in update mode - use screen-value */
  ASSIGN
   lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
   lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

  FIND item  WHERE item.company EQ cocode
                AND item.i-no  EQ rm-rctd.i-no:screen-value IN BROWSE {&BROWSE-NAME}
                      USE-INDEX i-no NO-LOCK NO-ERROR.
  IF AVAIL item THEN v-dep = item.s-dep.    
  
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

  ELSE DO:
        FIND FIRST job WHERE job.company EQ cocode
                         AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         AND job.job-no2 EQ integer(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                NO-LOCK NO-ERROR.
        IF AVAIL job THEN DO :
             v-job-up = 0.
             FOR EACH job-hdr FIELDS(n-on)
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.job     EQ job.job
                   AND job-hdr.job-no  EQ job.job-no
                   AND job-hdr.job-no2 EQ job.job-no2
                   AND job-hdr.frm     EQ int(rm-rctd.s-num:screen-value)
                 NO-LOCK:
               v-job-up = v-job-up + job-hdr.n-on.  
             END.
             
             FIND FIRST job-mat WHERE job-mat.company EQ cocode
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                                  AND job-mat.frm     EQ int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                   NO-LOCK NO-ERROR.
             IF AVAIL job-mat THEN DO:
               IF lv-rmissue EQ "Net" THEN v-out = job-mat.n-up / v-job-up.
               ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             END.
        END.
  END.

  IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
  IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
  IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.

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
                          ld, OUTPUT ld).

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
                          ld, OUTPUT ld).

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
                          ld, OUTPUT ld).

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
                          ld, OUTPUT ld).

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
  END. /* IF lv-uom EQ "DIA" */

  /* convert qty */
  IF lv-uom NE lv-qty-uom THEN
    RUN rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                      lv-out-qty / v-out, OUTPUT lv-out-qty).
  
  /* convert cost */
  IF rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME} EQ lv-cost-uom THEN
    lv-out-cost = dec(rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME}).
  ELSE
    RUN rm/convcuom.p(rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME},
                      lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                      rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME},
                      OUTPUT lv-out-cost).

  ASSIGN
   ext-cost = lv-out-qty * lv-out-cost
   ll-disp  = YES.

  ASSIGN
     rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
     rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
     rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
     rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom
     ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)
     NO-ERROR.
END.


/*IF ll-disp THEN
  ASSIGN
   rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-cost)
   rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-cost-uom
   rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(lv-out-qty)
   rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = lv-qty-uom
   ext-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ext-cost)
   NO-ERROR.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-tandem-rec B-table-Win 
PROCEDURE get-matrix-tandem-rec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
  DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
  DEF VAR lv-out-qty AS DEC NO-UNDO.
  DEF VAR lv-out-cost AS DEC NO-UNDO.
  DEF VAR lv-qty-uom AS cha NO-UNDO.
  DEF VAR lv-cost-uom AS cha NO-UNDO.
  DEF VAR v-job-up LIKE job-hdr.n-on NO-UNDO.
  DEF VAR v-out LIKE ef.n-out INIT 1 NO-UNDO.
  DEF VAR lv-uom LIKE rm-rctd.pur-uom NO-UNDO.
  DEF VAR ld-lf-used AS DEC NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO FORMAT ">,>>9.9<<<".
  DEF VAR ll-disp AS LOG INIT NO NO-UNDO.

  DEF BUFFER b-rm-rctd FOR rm-rctd.
  
  IF AVAIL rm-rctd AND rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> "" THEN DO:
     ASSIGN
      lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
    
     FIND item  WHERE item.company EQ cocode
                   AND item.i-no  EQ rm-rctd.i-no:screen-value IN BROWSE {&BROWSE-NAME}
                         USE-INDEX i-no NO-LOCK NO-ERROR.
     IF AVAIL item THEN v-dep = item.s-dep.    
     
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
    
     ELSE DO:
        FIND FIRST job WHERE job.company EQ cocode
                         AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                         AND job.job-no2 EQ integer(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                NO-LOCK NO-ERROR.
        IF AVAIL job THEN DO :
             v-job-up = 0.
             FOR EACH job-hdr FIELDS(n-on)
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.job     EQ job.job
                   AND job-hdr.job-no  EQ job.job-no
                   AND job-hdr.job-no2 EQ job.job-no2
                   AND job-hdr.frm     EQ int(rm-rctd.s-num:screen-value)
                 NO-LOCK:
               v-job-up = v-job-up + job-hdr.n-on.  
             END.
             
             FIND FIRST job-mat WHERE job-mat.company EQ cocode
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                                  AND job-mat.frm     EQ int(rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
                   NO-LOCK NO-ERROR.
             IF AVAIL job-mat THEN DO:
               IF lv-rmissue EQ "Net" THEN v-out = job-mat.n-up / v-job-up.
               ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
             END.
        END.
     END.
    
     IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
     IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
     IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
    
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
                              ld, OUTPUT ld).

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
                              ld, OUTPUT ld).

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
                              ld, OUTPUT ld).

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
                              ld, OUTPUT ld).

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
     END. /* IF lv-uom EQ "DIA" */
    

     /* convert qty */
     IF lv-uom NE lv-qty-uom THEN
       RUN rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                         lv-out-qty / v-out, OUTPUT lv-out-qty).
     
     /* convert cost */
     IF rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME} EQ lv-cost-uom THEN
       lv-out-cost = dec(rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME}).
     ELSE
       RUN rm/convcuom.p(rm-rctd.cost-uom:screen-value IN BROWSE {&BROWSE-NAME},
                         lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                         rm-rctd.cost:screen-value IN BROWSE {&BROWSE-NAME},
                         OUTPUT lv-out-cost).
    
     ASSIGN
      ext-cost = lv-out-qty * lv-out-cost
      ll-disp  = YES.
END.

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
    rm-rctd.BOL = STRING(lv-i-no,"x(30)") + STRING(lv-line,"999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR li AS INT INIT 0 NO-UNDO.

 DEF BUFFER bf-rctd FOR rm-rctd.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN rm-rctd.company = cocode
         rm-rctd.r-no = li 
         rm-rctd.rita-code = "I".

  IF adm-adding-record THEN DO:
      IF v-bin NE "user entered" THEN
      ASSIGN rm-rctd.loc = SUBSTR(v-bin,1,5).

    ASSIGN
     rm-rctd.s-num  = 1
     rm-rctd.b-num = 0
     rm-rctd.rct-date = TODAY
     rm-rctd.user-id = USERID("nosweat").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   
  /* Code placed here will execute PRIOR to standard behavior. */
  

      /* Dispatch standard ADM method.                             */
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
       ASSIGN ll-is-copy-record = YES .
  
      /* Code placed here will execute AFTER standard behavior.    */

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

  /* Problem was the record was still in browse after a cancel, */
  /* because the record was not deleted here, so make sure it's */
  /* found if there is a blank one                              */
  IF NOT AVAIL rm-rctd THEN
    FIND FIRST rm-rctd WHERE rm-rctd.company = cocode
      AND rm-rctd.i-no EQ ""
      AND rm-rctd.tag EQ ""
      AND rm-rctd.job-no EQ ""
      AND rm-rctd.rita-code EQ "I"
       EXCLUSIVE-LOCK NO-ERROR. 
    IF v-rmtags-log AND AVAIL(rm-rctd) THEN 
       FOR EACH wiptag WHERE wiptag.company = rm-rctd.company 
               AND wiptag.sts = "Issued" 
               AND wiptag.rm-tag-no = rm-rctd.tag EXCLUSIVE-LOCK:
            DELETE wiptag .
       END.
  IF NOT AVAIL rm-rctd THEN
    RETURN.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  EMPTY TEMP-TABLE tt-selected.

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
  
  ASSIGN
   adm-new-record      = NO
   adm-adding-record   = NO
   ll-is-copy-record   = NO.


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
    ASSIGN
     rm-rctd.s-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
     rm-rctd.b-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO.

    APPLY "entry" TO rm-rctd.rct-date IN BROWSE {&BROWSE-NAME}.
    IF {&WINDOW-NAME}:WINDOW-STATE NE 1 THEN    
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
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
  RUN valid-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */    
  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
      APPLY 'cursor-left' TO {&BROWSE-NAME}.
    END.
  END.

  IF v-rmtags-log AND
     rm-rctd.tag NE "" AND
     NOT CAN-FIND(FIRST wiptag WHERE
         wiptag.company EQ cocode AND
         wiptag.rm-tag-no EQ rm-rctd.tag) THEN
     DO:
        MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-wip AS LOG.
        /* btr */
        IF ll-wip THEN
           RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                                 INPUT rm-rctd.job-no,
                                 INPUT rm-rctd.job-no2,
                                 INPUT rm-rctd.i-no,
                                 INPUT rm-rctd.s-num,
                                 INPUT rm-rctd.b-num,
                                 INPUT rm-rctd.qty,
                                 INPUT rm-rctd.pur-uom,
                                 OUTPUT vlc-success).
     END.

     ASSIGN ll-is-copy-record = NO .

  RUN multi-issues (ROWID(rm-rctd)) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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

          FIRST xitem FIELDS(i-no)
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

  DEF VAR v-continue AS LOG INIT YES NO-UNDO.
  
  DO WITH FRAME {&frame-name}:

     IF v-get-tandem-rec THEN
        DO:
           DO WHILE v-continue:
           
              FIND FIRST tt-frm WHERE
                   tt-frm.qty GT 0
                   NO-ERROR.
             
              IF AVAIL tt-frm THEN
              DO:
                 FOR FIRST tt-tag USE-INDEX tag-no,
                     FIRST tt-selected WHERE tt-selected.tt-rowid EQ tt-tag.tt-rowid,
                     FIRST rm-bin FIELDS(loc loc-bin tag cost) WHERE
                           ROWID(rm-bin) EQ tt-tag.tt-rowid
                     NO-LOCK:
             
                     RUN dispatch ('copy-record').
       
                     {&BROWSE-NAME}:SELECT-FOCUSED-ROW().

                     ASSIGN
                        rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
                        rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin
                        rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.tag
                        rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(rm-bin.cost).
              
                     IF tt-tag.qty LE tt-frm.qty THEN
                     DO:
                        ASSIGN
                           rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-tag.qty)
                           rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-frm.frm)
                           tt-frm.qty = tt-frm.qty - tt-tag.qty.
             
                        DELETE tt-tag.
                        DELETE tt-selected.

                        IF tt-frm.qty EQ 0 THEN
                           DELETE tt-frm.
                     END.
                     ELSE /*tt-tag.qty GT tt-frm.qty*/
                     DO:
                        ASSIGN
                           rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-frm.qty)
                           rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-frm.frm)
                           tt-tag.qty = tt-tag.qty - tt-frm.qty.

                        DELETE tt-frm.
                     END.

                     RUN get-matrix-tandem-rec.
       
                     RUN valid-all NO-ERROR.
                     IF ERROR-STATUS:ERROR THEN RETURN ERROR.
                     
                     RUN dispatch ('assign-record').
                     
                     IF v-rmtags-log AND
                        rm-rctd.tag NE "" AND
                        NOT CAN-FIND(FIRST wiptag WHERE
                            wiptag.company EQ cocode AND
                            wiptag.rm-tag-no EQ rm-rctd.tag) THEN
                        DO:
                           MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                                BUTTON YES-NO UPDATE ll-wip AS LOG.
                       
                           /* btr */
                           IF ll-wip THEN
                              RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                                                    INPUT rm-rctd.job-no,
                                                    INPUT rm-rctd.job-no2,
                                                    INPUT rm-rctd.i-no,
                                                    INPUT rm-rctd.s-num,
                                                    INPUT rm-rctd.b-num,
                                                    INPUT rm-rctd.qty,
                                                    INPUT rm-rctd.pur-uom,
                                                    OUTPUT vlc-success).
                        END.
                     
                     RUN dispatch ('open-query').
                     
                     RUN dispatch ('end-update').
                     
                     REPOSITION {&BROWSE-NAME} TO ROWID ip-rowid.
                     
                     {&BROWSE-NAME}:SELECT-FOCUSED-ROW().

                     IF NOT CAN-FIND(FIRST tt-frm) OR
                        NOT CAN-FIND(FIRST tt-tag) THEN
                        v-continue = NO.
                 END.
              END.
           END.
        END.
     ELSE
     DO:
        FOR EACH tt-selected,
            FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-selected.tt-rowid:
       
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
       
            RUN get-matrix (NO).        
       
            RUN valid-all NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN ERROR.
       
            RUN dispatch ('assign-record').
       
            IF v-rmtags-log AND
               rm-rctd.tag NE "" AND
               NOT CAN-FIND(FIRST wiptag WHERE
                   wiptag.company EQ cocode AND
                   wiptag.rm-tag-no EQ rm-rctd.tag) THEN
               DO:
                  MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                       BUTTON YES-NO UPDATE ll-wip-2 AS LOG.
              
                  IF ll-wip-2 AND rm-rctd.spare-char-1 <> "WIP-Issued" THEN
                     RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                                           INPUT rm-rctd.job-no,
                                           INPUT rm-rctd.job-no2,
                                           INPUT rm-rctd.i-no,
                                           INPUT rm-rctd.s-num,
                                           INPUT rm-rctd.b-num,
                                           INPUT rm-rctd.qty,
                                           INPUT rm-rctd.pur-uom,
                                           OUTPUT vlc-success).
               END.
       
            RUN dispatch ('open-query').
       
            RUN dispatch ('end-update').
       
            REPOSITION {&BROWSE-NAME} TO ROWID ip-rowid.
       
            {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
          END.
       
          DELETE tt-selected.
        END.
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
          IF v-bin NE "user entered" THEN
          ASSIGN
           rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
           rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin.
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
    ASSIGN
     rm-rctd.s-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO
     rm-rctd.b-num:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO.

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
           rm-rctd.s-num:SCREEN-VALUE = "1" /*string(job-hdr.frm)      */
           rm-rctd.b-num:SCREEN-VALUE = "1" /* string(job-hdr.blank-no)         */
           rm-rctd.s-num:READ-ONLY = YES
           rm-rctd.b-num:READ-ONLY = YES.
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
  DEF INPUT PARAM ip-focus-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR save-rowid AS ROWID NO-UNDO.
  DEF VAR save-focus AS CHAR NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR ll-error AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
      RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, rm-rctd.loc-bin:screen-value IN BROWSE {&BROWSE-NAME}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, OUTPUT lv-rowid).
    
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" AND
       rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN
       DO:
          FIND FIRST ITEM WHERE
               ITEM.company EQ cocode AND
               ITEM.i-no EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
               NO-LOCK NO-ERROR.

          IF NOT AVAIL ITEM THEN
             LEAVE.

          RUN get-tandem-rec(INPUT ITEM.cons-uom).

          FIND FIRST tt-frm WHERE
               tt-frm.qty GT 0
               NO-ERROR.

          IF AVAIL tt-frm THEN
          DO:
             FOR FIRST tt-tag USE-INDEX tag-no,
                 FIRST tt-selected WHERE tt-selected.tt-rowid EQ tt-tag.tt-rowid,
                 FIRST rm-bin FIELDS(loc loc-bin tag cost ) WHERE
                       ROWID(rm-bin) EQ tt-tag.tt-rowid
                 NO-LOCK:
              IF v-bin NE "user entered" THEN
                 ASSIGN
                    rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
                    rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin.
                 ASSIGN
                    rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.tag
                    rm-rctd.cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(rm-bin.cost).
       
                 IF tt-tag.qty LE tt-frm.qty THEN
                 DO:
                    ASSIGN
                       rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-tag.qty)
                       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-frm.frm)
                       tt-frm.qty = tt-frm.qty - tt-tag.qty.

                    DELETE tt-tag.
                    DELETE tt-selected.

                    IF tt-frm.qty EQ 0 THEN
                       DELETE tt-frm.
                 END.
                 ELSE
                 DO:
                    ASSIGN
                       rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-frm.qty)
                       rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(tt-frm.frm)
                       tt-tag.qty = tt-tag.qty - tt-frm.qty.

                    DELETE tt-frm.
                 END.
             END.
          END.
       END.
    
    ELSE
       FOR FIRST tt-selected WHERE tt-selected.tt-rowid EQ lv-rowid,
           FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-selected.tt-rowid
           NO-LOCK:
       
         IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     NE rm-bin.loc     OR
            rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE rm-bin.loc-bin OR
            rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     NE rm-bin.tag     THEN DO:
           IF v-bin NE "user entered" THEN
             ASSIGN
               rm-rctd.loc:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.loc
               rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = rm-bin.loc-bin.
               rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}     = rm-bin.tag.
       
           RUN new-bin.
         END.
       
         DELETE tt-selected.
       END.
    
    FIND FIRST tt-selected NO-LOCK NO-ERROR.
    /* multiple records selected from Tag F1 lookup*/
    IF AVAIL tt-selected THEN APPLY "row-leave" TO BROWSE {&browse-name}.
    ELSE APPLY "ENTRY" TO ip-focus-hdl.
    
    v-get-tandem-rec = NO.
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

    IF fil_id EQ ? 
      THEN 
       RUN lookup-job-mat (?). /*Mismatched param error, OUTPUT fil_id)  YSK Task# 06060508*/

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
  DEF OUTPUT PARAMETER op-tag# AS LOG NO-UNDO.
 
  
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

   DEFINE OUTPUT PARAMETER lv-out-qty AS DEC NO-UNDO.

   DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
   DEF VAR v-job-up LIKE job-hdr.n-on NO-UNDO.
   DEF VAR v-out LIKE ef.n-out INIT 1 NO-UNDO.
   DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.

   IF ip-i-no EQ "" THEN
      LEAVE.

   ASSIGN
      lv-out-qty = ip-qty.
   
   FIND FIRST item WHERE
        item.company EQ cocode AND
        item.i-no EQ ip-i-no
        NO-LOCK NO-ERROR.

   IF NOT AVAIL ITEM OR
      ip-pur-uom EQ item.cons-uom THEN
      LEAVE.

   v-dep = item.s-dep.

   FIND FIRST job WHERE
        job.company EQ cocode AND
        job.job-no  EQ ip-job-no AND
        job.job-no2 EQ ip-job-no2
        NO-LOCK NO-ERROR.
   
   IF AVAIL job THEN DO:
      v-job-up = 0.
      FOR EACH job-hdr FIELDS(n-on)
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.frm     EQ ip-s-num
          NO-LOCK:
          v-job-up = v-job-up + job-hdr.n-on.  
      END.
      
      FIND FIRST job-mat WHERE
           job-mat.company EQ cocode AND
           job-mat.job     EQ job.job AND
           job-mat.i-no    EQ ip-i-no AND
           job-mat.frm     EQ ip-s-num
           NO-LOCK NO-ERROR.

      IF AVAIL job-mat THEN DO:
         IF lv-rmissue EQ "Net" THEN
            v-out = job-mat.n-up / v-job-up.
         ASSIGN 
            v-len = job-mat.len
            v-wid = job-mat.wid
            v-bwt = job-mat.basis-w.
      END.
   END.
   
   IF v-len EQ 0 THEN v-len = IF AVAIL item THEN item.s-len ELSE 0.
   IF v-wid EQ 0 THEN v-wid = IF AVAIL item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAIL item THEN item.s-wid ELSE 0.
   IF v-bwt EQ 0 THEN v-bwt = IF AVAIL item THEN item.basis-w ELSE 0.
   
   RUN rm/convquom.p(ip-pur-uom, item.cons-uom, v-bwt, v-len, v-wid, v-dep,
                     lv-out-qty / v-out, OUTPUT lv-out-qty).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-added B-table-Win 
PROCEDURE undo-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
  IF adm-new-record THEN RUN dispatch ("cancel-record").

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
   
  RUN valid-loc-bin-tag (99) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
  /* Task No- 03151112     */
  /*RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.*/  /* Task No- 03151112     */

  RUN valid-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-qty2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN validate-jobmat (NO) NO-ERROR.
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

  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
       IF rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" THEN
          v-msg = TRIM(rm-rctd.i-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                  " may not be spaces".

    IF v-msg EQ "" THEN DO:
       IF /*(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "" AND NOT CAN-FIND(FIRST item 
           WHERE item.company EQ cocode
             AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
             AND item.i-code  EQ "R")) OR
           (rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" AND  */
            NOT CAN-FIND(FIRST item 
           WHERE item.company EQ cocode
             AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})/*)*/ THEN DO:
           IF lAllowRmAdd EQ YES THEN DO:
               MESSAGE "Item is not on file. Do you want to add it? "
                   VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
               IF ll-ans THEN DO:
                   RUN create-item.
                   NEXT.
               END.
               ELSE 
                   v-msg = "Invalid entry, try help".
           END.
         ELSE DO:
              v-msg = "Invalid entry, try help".
         END.
       END.
    END.

    IF v-msg EQ "" THEN
      IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ ""    AND
         INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) EQ 0 AND
         AVAIL ITEM AND item.mat-type NE "P"                                          THEN
      v-msg = "If PO# and Job# are blank then RM Type must be 'P'aper".

    IF v-msg EQ "" THEN
      IF INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NE 0 AND
         lv-i-no NE "" AND lv-line NE 0 AND
         NOT CAN-FIND(FIRST po-ordl
            WHERE po-ordl.company EQ rm-rctd.company
              AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
              AND po-ordl.i-no    EQ lv-i-no
              AND po-ordl.line    EQ lv-line
              AND po-ordl.s-wid   LE (IF AVAIL ITEM AND item.r-wid NE 0 THEN item.r-wid
                                                         ELSE item.s-wid)) THEN
          v-msg = "RM width must be greater than PO RM you are issuing to...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.i-no IN BROWSE {&BROWSE-NAME}.
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
  DEF VAR lv-ans AS LOG NO-UNDO.
        
  DO WITH FRAME {&FRAME-NAME}:
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND job.job-no2 EQ int(rm-rctd.job-no2:SCREEN-VALUE)
          USE-INDEX job-no NO-LOCK NO-ERROR.
      IF NOT AVAIL job THEN DO:
        MESSAGE "Invalid " +
                TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                ", try help..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.

      ll = NO.
      /* If not asking to open, and job is closed, just return */
      IF jobreopn-log = NO  THEN 
        RUN jc/chk-stat.p(RECID(job), 1, YES /* with message */, OUTPUT ll).
      ELSE DO:
          /* If job is closed and we are asking, run the open procedure */
          /* and re-check until it's good */
          job-open-check:
          DO WHILE ll EQ NO:
            RUN jc/chk-stat.p(RECID(job), 1, NO /* no message */, OUTPUT ll).
            IF NOT ll THEN DO:
              lv-ans = NO.
              IF jobreopn-log EQ YES  THEN
                 MESSAGE 
                   "Job is CLOSED, would you like to reopen?"
                   VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
              IF lv-ans THEN DO:            
                RUN jc/jc-reopn.p (ROWID(job)).
                RUN jc/chk-stat.p(RECID(job), 1, NO /* no message */, OUTPUT ll).
              END.
              ELSE /* They decided not to open it, so exit loop */
                  LEAVE job-open-check.
            END.
            ELSE
              LEAVE job-open-check.
          END.
      END.

      IF NOT ll THEN DO:    
        APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.

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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST loc
            WHERE loc.company EQ cocode
            AND loc.loc     EQ ip-focus:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid Warehouse, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
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
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST rm-bin
            WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ ""
            AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
            AND rm-bin.loc-bin EQ ip-focus:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid Bin, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
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
       /* Task No- 03151112     */
      /*IF NOT AVAIL rm-rdtlh THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
        IF ip-int EQ 3 THEN
          APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
        ELSE
        IF ip-int EQ 2 THEN
          APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
        ELSE
          APPLY "entry" TO rm-rctd.loc IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.*/  /* Task No- 03151112     */
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
           po-ord.po-no   EQ po-ordl.po-no AND
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
      APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
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
     IF NOT AVAIL rm-bin AND integer(rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) GE 0  THEN DO:
         /* ticket 22650 - Tag does not have to exist for a negative issue */
         MESSAGE "Tag # does not exist in the Bin File..."
             VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
         RETURN ERROR.
     END.

 IF NOT ll-is-copy-record THEN
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
     END.
     ELSE DO:
         FOR EACH bf-rm-rctd 
          WHERE bf-rm-rctd.company EQ cocode
            AND bf-rm-rctd.rita-code EQ "I"                    
            AND bf-rm-rctd.i-no      EQ rm-bin.i-no
            AND bf-rm-rctd.loc       EQ rm-bin.loc
            AND bf-rm-rctd.loc-bin   EQ rm-bin.loc-bin     
            AND bf-rm-rctd.tag       EQ rm-bin.tag  
          NO-LOCK USE-INDEX rita-code:
       dTotalI = dTotalI + bf-rm-rctd.qty.    
     END.

     END.

    
    
    IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 AND
        rm-bin.qty LT DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "Issue Quantity exceeds Quantity on Hand for this Warehouse/Bin/Tag Location..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
    ELSE DO:
      FIND ITEM WHERE ITEM.company EQ rm-bin.company
        AND ITEM.i-no EQ rm-bin.i-no
        NO-LOCK NO-ERROR.

      IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 AND
        rm-bin.qty LT (DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) + dTotalI) 
        AND AVAIL(item) AND item.stocked THEN DO:
        MESSAGE "Issue Quantity exceeds Quantity on Hand + Unposted Issues for this Warehouse/Bin/Tag Location..."          
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
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

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

    lv-uom-list = lv-uom-list + ",DIA".

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

    DEF BUFFER xjob-mat FOR job-mat.

    /* gdm - 08070907 */
    DEF VAR v-reccnt AS INT NO-UNDO.
    
    IF rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} EQ "?"
      THEN ASSIGN ip-for-item-only = YES.
    
    /* gdm - 08070907 end */
    

    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK NO-ERROR.
/*
IF (rm-rctd.b-num:MODIFIED IN BROWSE {&BROWSE-NAME}  OR int(rm-rctd.b-num:SCREEN-VALUE) = 0 )
   OR rm-rctd.s-num:MODIFIED IN BROWSE {&BROWSE-NAME}
   OR rm-rctd.i-no:MODIFIED
THEN DO:
*/
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
                              (job-mat.frm = INT(rm-rctd.s-num:SCREEN-VALUE) AND
                               job-mat.blank-no = INT(rm-rctd.b-num:SCREEN-VALUE)))
                         USE-INDEX seq-idx NO-LOCK NO-ERROR.    
    IF NOT AVAIL job-mat AND rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
        
      /* gdm - */
      ASSIGN 
       lv-job-no = FILL(" ", 6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-name}))) +
                  TRIM(rm-rctd.job-no:SCREEN-VALUE)
       char-val  = ""
       v-number-rows-selected = 0
       look-recid = 0.
                  
      IF rm-rctd.job-no:SCREEN-VALUE NE "" OR 
         rm-rctd.i-no:SCREEN-VALUE NE "" THEN
      DO:
         

         EMPTY TEMP-TABLE w-rm-rctd.

         RUN windows/l-jobmt3.w (cocode, lv-job-no,
                                 INT(rm-rctd.job-no2:SCREEN-VALUE),
                                 rm-rctd.i-no:SCREEN-VALUE,
                                 OUTPUT char-val,OUTPUT look-recid,
                                 OUTPUT v-number-rows-selected).
         
      END.

      ASSIGN char-val = IF char-val BEGINS "," THEN TRIM(SUBSTR(char-val,2))
                                               ELSE TRIM(char-val).                                                    

      IF v-number-rows-selected > 1 AND AVAIL w-rm-rctd
        THEN RUN check-multi-line.
        ELSE 
         IF v-number-rows-selected > 1 THEN
            ASSIGN rm-rctd.s-num:SCREEN-VALUE   = "?"
                   rm-rctd.b-num:SCREEN-VALUE   = "0".
         ELSE DO:         
            ASSIGN fil_id = look-recid.
            RUN s-b-help. 
         END.

      /* gdm - */
    
       MESSAGE "Update item on Job file? " VIEW-AS ALERT-BOX QUESTION
                     BUTTON YES-NO UPDATE ll-ans AS LOG.
       IF ll-ans THEN DO:

            FIND FIRST job WHERE job.company = cocode
                             AND job.job-no =  rm-rctd.job-no:SCREEN-VALUE
                             AND job.job-no2 = int(rm-rctd.job-no2:SCREEN-VALUE)
                             NO-LOCK NO-ERROR.
/*             v-job-up = 0.                                    */
/*             for each job-hdr fields(n-on)                                */
/*                 where job-hdr.company eq cocode              */
/*                   and job-hdr.job     eq job.job             */
/*                   and job-hdr.job-no  eq job.job-no          */
/*                   and job-hdr.job-no2 eq job.job-no2         */
/*                   and job-hdr.frm     eq input rm-rctd.s-num */
/*                 no-lock:                                     */
/*                 v-job-up = v-job-up + job-hdr.n-on.          */
/*             end.                                             */            

          DO v-reccnt = 1 TO NUM-ENTRIES(char-val):

           ASSIGN fil_id = INT(ENTRY(v-reccnt,char-val)).

            IF fil_id = ? THEN RUN lookup-job-mat (ip-for-item-only).
          
            FIND job-mat WHERE RECID(job-mat) EQ fil_id NO-ERROR.

            IF AVAIL job-mat THEN DO:
               v-job-up = 0.
               FOR EACH job-hdr FIELDS(n-on)
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.job     EQ job.job
                   AND job-hdr.job-no  EQ job.job-no
                   AND job-hdr.job-no2 EQ job.job-no2
                   AND job-hdr.frm     EQ job-mat.frm NO-LOCK:
                   v-job-up = v-job-up + job-hdr.n-on.  
                END.

               IF INDEX("1234BPR",item.mat-type) GT 0 THEN DO ON ENDKEY UNDO, RETRY:
                  ASSIGN
                   v-frm = job-mat.frm
                   v-blk = job-mat.blank-no
                   v-out = job-mat.n-up / v-job-up.
                   RUN rm/g-iss2.w ( v-frm, v-blk , INPUT-OUTPUT v-out ). 
                /*
                  display v-frm v-blk with frame s-b.
                  update v-out with frame s-b.
                */
               END.  

              IF item.i-code EQ "R" THEN DO:
                 IF (item.r-wid NE 0 AND  item.r-wid LT job-mat.wid) OR
                    (item.r-wid EQ 0 AND (item.s-wid LT job-mat.wid OR
                                         item.s-len LT job-mat.len))
                 THEN DO ON ENDKEY UNDO, RETRY:
                     choice = NO.

                     IF ITEM.r-wid <> 0 THEN
                         RUN rm/g-iss21.w (job-mat.len, job-mat.len, item.r-wid,job-mat.wid, job-mat.frm,
                                        OUTPUT choice)  .
                     ELSE RUN rm/g-iss21.w (item.s-len, job-mat.len, item.s-wid,job-mat.wid, job-mat.frm,
                                        OUTPUT choice)  .
                   /* display item.s-len
                            job-mat.len when item.r-wid ne 0 @ item.s-len
                          job-mat.len
                          item.s-wid
                            item.r-wid  when item.r-wid ne 0 @ item.s-wid
                          job-mat.wid
                          job-mat.frm
                      with frame tsmall.
                   update choice with frame tsmall. 
                   */
                
                    IF NOT choice THEN DO: RELEASE job-mat.
                       APPLY "entry" TO rm-rctd.i-no.
                       RETURN ERROR.
                    END.
                 END.
              END.
            END. /* avail job-mat */
         
            FIND FIRST xitem WHERE xitem.company EQ cocode
                    AND xitem.i-no    EQ job-mat.rm-i-no
                  NO-LOCK NO-ERROR.
            IF NOT AVAIL xitem THEN RELEASE job-mat.

            IF AVAIL job-mat THEN DO:
                CREATE xjob-mat.
                BUFFER-COPY job-mat TO xjob-mat.
                
                FIND job-mat WHERE RECID(job-mat) EQ recid(xjob-mat).
          
                IF job-mat.sc-uom EQ job-mat.qty-uom THEN
                  v-cost = job-mat.std-cost.
                ELSE
                  RUN sys/ref/convcuom.p(job-mat.sc-uom,
                                         job-mat.qty-uom,
                                         job-mat.basis-w,
                                         job-mat.len,
                                         job-mat.wid,
                                         item.s-dep,
                                         job-mat.std-cost,
                                         OUTPUT v-cost).
                    
                ASSIGN
                 v-cost = v-cost * job-mat.qty
                 rm-rctd.s-num:SCREEN-VALUE   = IF v-number-rows-selected EQ 1 
                                                  THEN STRING(job-mat.frm)
                                                  ELSE rm-rctd.s-num:SCREEN-VALUE
                 rm-rctd.b-num:SCREEN-VALUE   = STRING(job-mat.blank-no)
                 job-mat.j-no    = 1                 
                 job-mat.rm-i-no = item.i-no
                 job-mat.i-no    = item.i-no
                 job-mat.sc-uom  = item.cons-uom                 
                 job-mat.wid     = IF item.r-wid NE 0 THEN
                                     item.r-wid ELSE item.s-wid
                 job-mat.len     = IF item.r-wid NE 0 THEN
                                     job-mat.len ELSE item.s-len
                 job-mat.basis-w = item.basis-w
                 job-mat.qty     = job-mat.qty * IF job-mat.n-up EQ 0 THEN 1 ELSE job-mat.n-up
                 job-mat.n-up    = v-job-up * v-out                 
                 job-mat.qty     = job-mat.qty / IF job-mat.n-up EQ 0 THEN 1 ELSE job-mat.n-up.
                     
                {sys/inc/roundup.i job-mat.qty}
                
                v-cost = v-cost / job-mat.qty.
                
                IF job-mat.qty-uom EQ job-mat.sc-uom THEN
                  job-mat.std-cost = v-cost.
                ELSE  
                  RUN sys/ref/convcuom.p(job-mat.qty-uom,
                                         job-mat.sc-uom,
                                         job-mat.basis-w,
                                         job-mat.len,
                                         job-mat.wid,
                                         item.s-dep,
                                         v-cost,
                                         OUTPUT job-mat.std-cost).                                                                         

                ASSIGN
                 v-bwt = job-mat.basis-w
                 v-len = job-mat.len
                 v-wid = job-mat.wid
                 v-dep = item.s-dep.
            END. /* avail job-mat */
          END. /* DO counter for recids */
       END.  /* ll-ans = yes */
       ELSE DO: 
           APPLY "entry" TO rm-rctd.i-no.
           RETURN ERROR.  /* not update item */
       END.
    END. /* not avail job-mat */
/*
END.
*/
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
  
  RUN get-matrix (YES).

  RETURN ext-cost.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-dimension B-table-Win 
FUNCTION display-dimension RETURNS DECIMAL
  ( INPUT ip-dim AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  from rcptdims.v  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF ip-dim EQ "W" THEN v-wid ELSE v-len.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

