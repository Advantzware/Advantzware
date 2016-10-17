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

&SCOPED-DEFINE yellowColumnsName wiptag-mch
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

&SCOPED-DEFINE BRWSDEFS wiptag-mch

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.

def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid AS DEC FORMAT ">>>9.9999" NO-UNDO.
DEF VAR lv-po-len AS DEC FORMAT ">>,>>9.9999" NO-UNDO.
DEF VAR v-len LIKE lv-po-len NO-UNDO.
DEF VAR v-wid LIKE lv-po-len NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF,DIA"] NO-UNDO.
DEF VAR lv-rmissue AS CHAR NO-UNDO.
DEF VAR v-parent-handle AS HANDLE NO-UNDO.
DEF VAR vlc-success AS CHAR INIT "" NO-UNDO.
DEF VAR v-last-produced-qty LIKE wiptag-mch.produced-qty NO-UNDO.
DEF VAR v-enable-job AS LOG NO-UNDO.
v-parent-handle = CURRENT-WINDOW:PARENT.
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
DEF VAR v-tag-no AS CHAR NO-UNDO.
DEF VAR v-adm-add-record AS LOG NO-UNDO.
DEF VAR v-state AS CHAR NO-UNDO.
DEF VAR v-qty-checked AS LOG NO-UNDO.
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
         sys-ctrl.descrip = "Number of RM Loadtags to Print & Create Wip Tags"
         sys-ctrl.char-fld = ""
         sys-ctrl.int-fld = 1
         sys-ctrl.log-fld = FALSE. /* true create wip/false do not */
   END.

ASSIGN v-rmtags-log = sys-ctrl.log-fld.

RELEASE sys-ctrl.

DEF VAR lv-spare-char-4    LIKE rm-rctd.spare-char-4 NO-UNDO.
DEF VAR look-recid   AS RECID NO-UNDO.

DEF VAR v-number-rows-selected AS INT NO-UNDO.
DEF VAR v-dept AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES wiptag-mch

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table wiptag-mch.tag-no ~
wiptag-mch.m-code wiptag-mch.spare-char-1 v-dept wiptag-mch.spare-char-3 ~
wiptag-mch.spare-char-2 wiptag-mch.spare-char-4 wiptag-mch.spare-int-1 ~
wiptag-mch.produced-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table wiptag-mch.tag-no ~
wiptag-mch.m-code wiptag-mch.spare-char-3 wiptag-mch.spare-char-2 ~
wiptag-mch.spare-char-4 wiptag-mch.spare-int-1 wiptag-mch.produced-qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table wiptag-mch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table wiptag-mch
&Scoped-define QUERY-STRING-Browser-Table FOR EACH wiptag-mch WHERE ~{&KEY-PHRASE} ~
      AND wiptag-mch.spare-int-2 = 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH wiptag-mch WHERE ~{&KEY-PHRASE} ~
      AND wiptag-mch.spare-int-2 = 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table wiptag-mch
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table wiptag-mch


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 RECT-5 browse-order ~
Btn_Clear_Find auto_find 
&Scoped-Define DISPLAYED-OBJECTS browse-order fi_sortby auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDept B-table-Win 
FUNCTION getDept RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDesc B-table-Win 
FUNCTION getDesc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
     SIZE 35 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 73 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 146 BY 17.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      wiptag-mch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      wiptag-mch.tag-no COLUMN-LABEL "Wip Tag" FORMAT "X(20)":U
            WIDTH 26.2
      wiptag-mch.m-code COLUMN-LABEL "Machine Code" FORMAT "x(6)":U
            WIDTH 17.2
      wiptag-mch.spare-char-1 COLUMN-LABEL "Mach Desc" FORMAT "x(10)":U
            WIDTH 13
      v-dept COLUMN-LABEL "Mach Dept" FORMAT "x(6)":U WIDTH 12.4
      wiptag-mch.spare-char-3 COLUMN-LABEL "WIP Whs" FORMAT "x(8)":U
            WIDTH 10.4
      wiptag-mch.spare-char-2 COLUMN-LABEL "WIP Bin" FORMAT "x(8)":U
            WIDTH 10.2
      wiptag-mch.spare-char-4 COLUMN-LABEL "Job" FORMAT "x(8)":U
      wiptag-mch.spare-int-1 COLUMN-LABEL "" FORMAT ">9":U
      wiptag-mch.produced-qty COLUMN-LABEL "Tag Qty Produced" FORMAT "->>>,>>>,>>9.9<<<<<":U
            WIDTH 27.8
  ENABLE
      wiptag-mch.tag-no
      wiptag-mch.m-code
      wiptag-mch.spare-char-3
      wiptag-mch.spare-char-2
      wiptag-mch.spare-char-4
      wiptag-mch.spare-int-1
      wiptag-mch.produced-qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 15.24
         FONT 2 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 16.71 COL 79 COLON-ALIGNED NO-LABEL
     Btn_Clear_Find AT ROW 16.71 COL 92
     auto_find AT ROW 16.71 COL 115 COLON-ALIGNED HELP
          "Enter Auto Find Value" NO-LABEL
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
   Other Settings: PERSISTENT-ONLY
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
         WIDTH              = 146.6.
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       auto_find:VISIBLE IN FRAME F-Main          = FALSE.

ASSIGN 
       browse-order:VISIBLE IN FRAME F-Main          = FALSE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       Btn_Clear_Find:VISIBLE IN FRAME F-Main          = FALSE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:VISIBLE IN FRAME F-Main          = FALSE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.wiptag-mch"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "asi.wiptag-mch.spare-int-2 = 0"
     _FldNameList[1]   > asi.wiptag-mch.tag-no
"wiptag-mch.tag-no" "Wip Tag" ? "character" ? ? ? ? ? ? yes ? no no "26.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.wiptag-mch.m-code
"wiptag-mch.m-code" "Machine Code" ? "character" ? ? ? ? ? ? yes ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.wiptag-mch.spare-char-1
"wiptag-mch.spare-char-1" "Mach Desc" "x(10)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"v-dept" "Mach Dept" "x(6)" ? ? ? ? ? ? ? no ? no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.wiptag-mch.spare-char-3
"wiptag-mch.spare-char-3" "WIP Whs" ? "character" ? ? ? ? ? ? yes ? no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.wiptag-mch.spare-char-2
"wiptag-mch.spare-char-2" "WIP Bin" ? "character" ? ? ? ? ? ? yes ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.wiptag-mch.spare-char-4
"wiptag-mch.spare-char-4" "Job" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.wiptag-mch.spare-int-1
"wiptag-mch.spare-int-1" "" ">9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.wiptag-mch.produced-qty
"wiptag-mch.produced-qty" "Tag Qty Produced" ? "decimal" ? ? ? ? ? ? yes ? no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
   v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept().
   RUN new-state in phandle ('update-begin':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO: 
 DEF var ll-tag# as log no-undo.
 DEF VAR help-recid AS RECID NO-UNDO.
 DEF VAR lv-search AS cha NO-UNDO.



 ASSIGN 

   char-val               = ""
   v-number-rows-selected = 0
   look-recid = 0.


 ll-help-run = yes.
 case focus:NAME:
     when "tag-no" then do:
         run windows/l-wptag1.w (cocode, focus:screen-value, output char-val, OUTPUT help-recid).
         if char-val <> "" then do:
            assign
             focus:screen-value in browse {&BROWSE-NAME} = ENTRY(1,char-val).
            APPLY 'value-changed' TO wiptag-mch.tag-no in browse {&BROWSE-NAME}.
         END.
     end.

     when "m-code" then do:
           run windows/l-mach.w (cocode, g_loc, focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign
               focus:screen-value in browse {&BROWSE-NAME} = ENTRY(1,char-val)
               /* cr:screen-value in browse {&BROWSE-NAME} = ENTRY(2
                                                                         ,char-val).*/.
              FIND FIRST mach WHERE mach.company = cocode 
                                AND mach.m-code = focus:screen-value in browse {&BROWSE-NAME}
                              NO-LOCK NO-ERROR.
              IF AVAIL mach THEN
                  ASSIGN wiptag-mch.spare-char-1:SCREEN-VALUE IN BROWSE {&browse-name} = mach.m-dscr
                         v-dept:SCREEN-VALUE = mach.dept[1].
           END.
     end.

     when "spare-char-2" then do:
           run windows/l-wipbn1.w (cocode,wiptag-mch.spare-char-3:SCREEN-VALUE, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign wiptag-mch.spare-char-2:SCREEN-VALUE in browse {&BROWSE-NAME} = ENTRY(2,char-val)
                     wiptag-mch.spare-char-3:SCREEN-VALUE in browse {&BROWSE-NAME} = ENTRY(1,char-val).
           end.  
     END.

     when "spare-char-3" then do:
           run windows/l-wipbn1.w (cocode,wiptag-mch.spare-char-3:SCREEN-VALUE, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign wiptag-mch.spare-char-2:SCREEN-VALUE in browse {&BROWSE-NAME} = ENTRY(2,char-val)
                     wiptag-mch.spare-char-3:SCREEN-VALUE in browse {&BROWSE-NAME} = ENTRY(1,char-val).
           end.  
     END.

     when "spare-char-4" then do:
           run windows/l-jobno.w (cocode,FOCUS:SCREEN-VALUE, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign wiptag-mch.spare-char-4:SCREEN-VALUE in browse {&BROWSE-NAME} = ENTRY(1,char-val)
                     wiptag-mch.spare-int-1:SCREEN-VALUE in browse {&BROWSE-NAME} = ENTRY(2,char-val).
           end.  
     END.

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
  IF AVAIL(wiptag-mch) THEN
  ASSIGN
    v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept()
    wiptag-mch.spare-char-1:SCREEN-VALUE in browse {&BROWSE-NAME} = getDesc().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
    FIND FIRST wiptag NO-LOCK NO-ERROR.
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i} 
   IF AVAIL(wiptag-mch) THEN
   v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept().

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

  /* {est/brsleave.i} */  /* same as src but update will be same as add record*/

   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR. 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
    FIND FIRST wiptag NO-LOCK NO-ERROR.
  RUN startSearch.
   v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
    FIND FIRST wiptag NO-LOCK NO-ERROR.
  {src/adm/template/brschnge.i} 
  {methods/template/local/setvalue.i}
  IF AVAIL wiptag-mch THEN
  v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.tag-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.tag-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF wiptag-mch.tag-no IN BROWSE Browser-Table /* Wip Tag */
DO:

  RUN add-tag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.tag-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF wiptag-mch.tag-no IN BROWSE Browser-Table /* Wip Tag */
DO:
  RUN validate-tag.
  IF ERROR-STATUS:ERROR THEN DO:
      APPLY "entry" TO wiptag-mch.tag-no IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
  END.
  ELSE DO:
      RUN populate-values.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.m-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF wiptag-mch.m-code IN BROWSE Browser-Table /* Machine Code */
DO:

  IF LASTKEY NE -1 THEN DO WITH FRAME f-main:
    FIND mach WHERE mach.company = cocode 
                AND mach.m-code = wiptag-mch.m-code:screen-value in browse {&BROWSE-NAME}
              NO-LOCK NO-ERROR.
    IF AVAIL mach THEN
    assign
      wiptag-mch.spare-char-1:SCREEN-VALUE in browse {&BROWSE-NAME} = mach.m-dscr
      v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept(). 
   /* ELSE DO: */
        /*
      MESSAGE "Invalid Machine Code"
          VIEW-AS ALERT-BOX ERROR.
      /* APPLY "entry" TO wiptag-mch.m-code IN BROWSE {&browse-name}. */
      RETURN NO-APPLY.
      */

      RUN validate-mcode.
      IF ERROR-STATUS:ERROR THEN DO:
        APPLY "entry" TO wiptag-mch.m-code IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
      END.

    /* END. */
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.spare-char-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.spare-char-3 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF wiptag-mch.spare-char-3 IN BROWSE Browser-Table /* WIP Whs */
DO:
  IF LASTKEY NE -1 THEN DO :
      RUN validate-whse.
        IF ERROR-STATUS:ERROR THEN DO:
        APPLY "entry" TO wiptag-mch.spare-char-3 IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.spare-char-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.spare-char-2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF wiptag-mch.spare-char-2 IN BROWSE Browser-Table /* WIP Bin */
DO:
  DEF VAR v-error AS LOG NO-UNDO.
  IF LASTKEY NE -1 THEN DO WITH FRAME f-main:
    RUN validate-bin (OUTPUT v-error).
    IF ERROR-STATUS:ERROR OR v-error THEN DO:
       APPLY "entry" TO wiptag-mch.spare-char-2 IN BROWSE {&browse-name}.
       RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.spare-char-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.spare-char-4 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF wiptag-mch.spare-char-4 IN BROWSE Browser-Table /* Job */
DO:
  IF LASTKEY NE -1 THEN DO WITH FRAME f-main:
    RUN validate-job.
    IF ERROR-STATUS:ERROR THEN DO:
       APPLY "entry" TO wiptag-mch.spare-char-4 IN BROWSE {&browse-name}.
       RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.spare-int-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.spare-int-1 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF wiptag-mch.spare-int-1 IN BROWSE Browser-Table
DO:

  IF LASTKEY NE -1 THEN DO WITH FRAME f-main:
    RUN validate-job2.
    IF ERROR-STATUS:ERROR THEN DO:
       APPLY "entry" TO wiptag-mch.spare-char-4 IN BROWSE {&browse-name}.
       RETURN NO-APPLY.
    END.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag-mch.produced-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.produced-qty Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF wiptag-mch.produced-qty IN BROWSE Browser-Table /* Tag Qty Produced */
DO:
  v-qty-checked = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.produced-qty Browser-Table _BROWSE-COLUMN B-table-Win
ON TAB OF wiptag-mch.produced-qty IN BROWSE Browser-Table /* Tag Qty Produced */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.produced-qty Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF wiptag-mch.produced-qty IN BROWSE Browser-Table /* Tag Qty Produced */
DO:
  
  DEF VAR v-override AS LOG NO-UNDO.
  IF v-qty-checked = NO THEN DO:
    RUN validate-qty (OUTPUT v-override).
    IF v-override = NO THEN DO:
      APPLY "entry" TO wiptag-mch.produced-qty IN BROWSE {&browse-name}. 
      /*RETURN. */
    END.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ON find OF asi.wiptag-mch
DO:
    v-dept = getDept().
    RETURN.
END.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept().
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-tag B-table-Win 
PROCEDURE add-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-wiptag FOR wiptag.  
  DEF VAR v-last-machine LIKE wiptag-mch.m-code NO-UNDO.
  DEF VAR v-produced-qty LIKE wiptag-mch.produced-qty NO-UNDO.
  DEF VAR v-all-values AS CHAR.

  v-all-values = "".
  v-all-values = 
    wiptag-mch.spare-char-3:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} +
    wiptag-mch.spare-char-2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} +
    wiptag-mch.spare-char-4:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} 
    NO-ERROR.

 IF v-all-values = "" THEN DO WITH FRAME {&FRAME-NAME}:
     RUN populate-values.
 END.                                                                                                                               
 v-adm-add-record = NO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-last-qty B-table-Win 
PROCEDURE get-last-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER v-last-machine LIKE wiptag-mch.m-code NO-UNDO.
DEF OUTPUT PARAMETER v-produced-qty LIKE wiptag-mch.produced-qty NO-UNDO.
DEF BUFFER bf-wiptag FOR wiptag.
DEF BUFFER bf-wiptag-mch FOR wiptag-mch.
  DO WITH FRAME {&FRAME-NAME}:
    FIND LAST bf-wiptag 
        WHERE bf-wiptag.company EQ cocode
          AND bf-wiptag.tag-no  EQ wiptag-mch.tag-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND bf-wiptag.COMPLETE EQ NO
        NO-LOCK NO-ERROR.
    ASSIGN
        v-last-machine = ""
        v-produced-qty = 0.
    IF AVAIL bf-wiptag THEN DO:
      ASSIGN v-last-machine = ""
             v-produced-qty = 0.
      FOR EACH bf-wiptag-mch WHERE bf-wiptag-mch.company = cocode
                            AND bf-wiptag-mch.tag-no = bf-wiptag.tag-no                          
                          NO-LOCK
                          BY bf-wiptag-mch.rec_key.
         
        ASSIGN v-last-machine = bf-wiptag-mch.m-code
               v-produced-qty = bf-wiptag-mch.produced-qty.
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tag-no B-table-Win 
PROCEDURE get-tag-no :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
DEF OUTPUT PARAMETER op-tag-no AS CHAR NO-UNDO.
op-tag-no = v-tag-no.
IF op-tag-no = "" AND avail(wiptag-mch) THEN
  op-tag-no = wiptag-mch.tag-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  APPLY "entry" TO wiptag-mch.tag-no IN BROWSE {&browse-name}.
  v-adm-add-record = YES.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  wiptag-mch.company = cocode.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  EMPTY TEMP-TABLE tt-selected.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bfWipTag FOR wiptag.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 IF AVAIL wiptag-mch THEN DO:
   ASSIGN 
     v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept()  
     wiptag-mch.spare-char-1:SCREEN-VALUE in browse {&BROWSE-NAME} = getDesc().
   FIND FIRST bfWipTag WHERE bfWipTag.company = cocode
                         AND bfWipTag.tag-no = wiptag-mch.tag-no:SCREEN-VALUE in browse {&BROWSE-NAME}
                         AND bfWiptag.COMPLETE EQ NO
                         AND bfWiptag.job-no GT ""
                       NO-LOCK NO-ERROR.

   IF AVAIL bfWipTag AND 
       wiptag-mch.spare-char-4:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = string(bfWiptag.job-no) AND
       wiptag-mch.spare-int-1:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = string(bfWiptag.job-no2) THEN
         v-enable-job = TRUE.
       ELSE
         v-enable-job = FALSE.
    ASSIGN
      wiptag-mch.spare-char-4:READ-ONLY IN BROWSE {&BROWSE-NAME} = v-enable-job
      wiptag-mch.spare-int-1:READ-ONLY IN BROWSE {&BROWSE-NAME} = v-enable-job.
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
  browse-order:VISIBLE IN FRAME f-main = NO.
  auto_find:VISIBLE IN FRAME f-main = NO.
  fi_sortby:VISIBLE IN FRAME f-main = NO.

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
   IF AVAIL wiptag-mch THEN
  ASSIGN 
     v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept()
     wiptag-mch.spare-char-1:SCREEN-VALUE in browse {&BROWSE-NAME} = getDesc().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL wiptag-mch THEN
      ASSIGN 
        v-dept:SCREEN-VALUE in browse {&BROWSE-NAME} = getDept().
        wiptag-mch.spare-char-1:SCREEN-VALUE in browse {&BROWSE-NAME} = getDesc().
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

  RUN validate-all.

  IF ERROR-STATUS:ERROR THEN DO:
      RUN local-display-fields.
      /*APPLY "entry" TO wiptag-mch.m-code IN BROWSE {&browse-name}. */
      RETURN NO-APPLY.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populate-values B-table-Win 
PROCEDURE populate-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-wiptag FOR wiptag.
    DEF VAR v-last-machine AS CHAR NO-UNDO.
    DEF VAR v-produced-qty AS INT NO-UNDO.

    FIND LAST bf-wiptag 
        WHERE bf-wiptag.company EQ cocode
          AND bf-wiptag.tag-no  EQ wiptag-mch.tag-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND bf-wiptag.COMPLETE EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL bf-wiptag THEN DO:
      ASSIGN
        /*wiptag-mch.company = cocode */
        wiptag-mch.spare-char-3:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = bf-wiptag.wip-warehouse
        /* wiptag-mch.spare-int-1:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = string(bf-wiptag.pallet-count * bf-wiptag.case-bundle) */
        wiptag-mch.spare-char-2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = bf-wiptag.loc-bin
        wiptag-mch.spare-char-4:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = string(bf-wiptag.job-no)
        wiptag-mch.spare-int-1:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = string(bf-wiptag.job-no2).
      v-tag-no = wiptag-mch.tag-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NO-ERROR.
      ASSIGN v-last-machine = ""
             v-produced-qty = 0.
      RUN get-last-qty (OUTPUT v-last-machine, OUTPUT v-produced-qty).

      ASSIGN
        /*wiptag-mch.m-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = v-last-machine */
        wiptag-mch.produced-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = string(v-produced-qty, "->>>,>>>,>>9.9<<<<<").

      IF wiptag-mch.spare-char-4:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = string(bf-wiptag.job-no) AND
         wiptag-mch.spare-int-1:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}  = string(bf-wiptag.job-no2) THEN
        v-enable-job = TRUE.
      ELSE
        v-enable-job = FALSE.
    END.
    ELSE
       v-enable-job = FALSE.
    ASSIGN
    wiptag-mch.spare-char-4:READ-ONLY IN BROWSE {&BROWSE-NAME} = v-enable-job
    wiptag-mch.spare-int-1:READ-ONLY IN BROWSE {&BROWSE-NAME} = v-enable-job.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-all B-table-Win 
PROCEDURE post-all :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
FOR EACH wiptag-mch WHERE wiptag-mch.spare-int-2 = 0 EXCLUSIVE-LOCK:
    FIND wiptag WHERE wiptag.tag-no = wiptag-mch.tag-no
                EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL wiptag THEN
      ASSIGN wiptag.wip-warehouse  = wiptag-mch.spare-char-3
             wiptag.wip-rm-bin     = wiptag-mch.spare-char-2
             wiptag.job-no         = wiptag-mch.spare-char-4
             wiptag.job-no2        = wiptag-mch.spare-int-1.
    wiptag-mch.spare-int-2 = 1.
    v-tag-no = wiptag-mch.tag-no.
END.
RUN adm-open-query.
RUN adm-initialize.
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
  {src/adm/template/snd-list.i "wiptag-mch"}

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
  v-state = p-state. 
 FIND FIRST wiptag NO-ERROR.
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i} 
  END CASE.
  
  IF p-state = "update-begin" THEN
    APPLY "entry" TO wiptag-mch.tag-no IN BROWSE {&browse-name}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-all B-table-Win 
PROCEDURE validate-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-error AS LOG NO-UNDO.
DEF VAR v-override AS LOG NO-UNDO.
v-error = NO.
RUN validate-mcode.
IF ERROR-STATUS:ERROR THEN DO:
  APPLY "entry" TO wiptag-mch.m-code IN BROWSE {&browse-name}.
  v-error = YES.
  RETURN ERROR.
END.

RUN validate-bin (OUTPUT v-error).
IF ERROR-STATUS:ERROR OR v-error THEN DO:
  APPLY "entry" TO wiptag-mch.spare-char-2 IN BROWSE {&browse-name}.
  RETURN ERROR.
END.

RUN validate-job.
IF ERROR-STATUS:ERROR THEN DO:
  APPLY "entry" TO wiptag-mch.spare-char-4 IN BROWSE {&browse-name}.
  RETURN ERROR.
END.
RUN validate-job2.
IF ERROR-STATUS:ERROR THEN DO:
   APPLY "entry" TO wiptag-mch.spare-char-4 IN BROWSE {&browse-name}.
  RETURN ERROR.
END.
RUN validate-qty (OUTPUT v-override).
IF v-override = NO THEN DO:
  APPLY "entry" TO wiptag-mch.produced-qty IN BROWSE {&browse-name}. 
  RETURN ERROR.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-bin B-table-Win 
PROCEDURE validate-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAMETER opv-error AS LOG NO-UNDO.
 opv-error = NO.
 DO WITH FRAME f-main:
   FIND FIRST wip-bin WHERE wip-bin.company = cocode
                        AND wip-bin.loc-bin = wiptag-mch.spare-char-2:screen-value in browse {&BROWSE-NAME}
                      NO-LOCK NO-ERROR.
   IF NOT AVAIL wip-bin OR wiptag-mch.spare-char-2:screen-value in browse {&BROWSE-NAME} = "" THEN DO:
      /*APPLY "entry" TO wiptag-mch.spare-char-2 IN BROWSE {&browse-name}. */
      MESSAGE "Invalid WIP Bin"
         VIEW-AS ALERT-BOX ERROR.
      opv-error = YES. /* error status not being set here for some reason */
   END.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-job B-table-Win 
PROCEDURE validate-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME f-main:
    FIND FIRST job WHERE job.company = cocode
                   AND job.job-no = wiptag-mch.spare-char-4:screen-value in browse {&BROWSE-NAME}
                 NO-LOCK NO-ERROR.
    IF NOT AVAIL job THEN DO:
       MESSAGE "Invalid Job #"
       VIEW-AS ALERT-BOX ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-job2 B-table-Win 
PROCEDURE validate-job2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME f-main:
    FIND FIRST job WHERE job.company = cocode
                   AND job.job-no = wiptag-mch.spare-char-4:screen-value in browse {&BROWSE-NAME}
                   AND job.job-no2 = INTEGER(wiptag-mch.spare-int-1:screen-value in browse {&BROWSE-NAME})
                 NO-LOCK NO-ERROR.
    IF NOT AVAIL job AND wiptag-mch.spare-char-4:screen-value in browse {&BROWSE-NAME} > "" THEN DO:
       MESSAGE "Invalid Job #"
       VIEW-AS ALERT-BOX ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-mcode B-table-Win 
PROCEDURE validate-mcode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME f-main:    
    FIND mach WHERE mach.company = cocode 
                AND mach.m-code = wiptag-mch.m-code:screen-value in browse {&BROWSE-NAME}
              NO-LOCK NO-ERROR.
    IF NOT AVAIL mach THEN DO:
      MESSAGE "Invalid Machine Code"
           VIEW-AS ALERT-BOX ERROR. 
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-qty B-table-Win 
PROCEDURE validate-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER opv-override AS LOG NO-UNDO.
  DEF VAR v-last-machine LIKE wiptag-mch.m-code NO-UNDO.
  DEF VAR v-produced-qty LIKE wiptag-mch.produced-qty NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  opv-override = YES.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN v-last-machine = ""
             v-produced-qty = 0.
      RUN get-last-qty (OUTPUT v-last-machine, OUTPUT v-produced-qty).
      ll = NO.
      IF integer(wiptag-mch.produced-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > v-produced-qty THEN DO:
          message "Note: Quantity entered is higher than the last machine quantity, continue?" view-as alert-box question
                    button yes-no update ll.  
          IF NOT ll THEN DO:
              wiptag-mch.produced-qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING( v-produced-qty ).
              opv-override = NO.
              
          END.
          
      END.
      v-qty-checked = YES.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-tag B-table-Win 
PROCEDURE validate-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-wiptag FOR wiptag.
  DO WITH FRAME f-main:    
      FIND LAST bf-wiptag 
          WHERE bf-wiptag.company EQ cocode
            AND bf-wiptag.tag-no  EQ wiptag-mch.tag-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            AND bf-wiptag.COMPLETE EQ NO
          NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-wiptag THEN DO:
      MESSAGE "Invalid tag entered."
           VIEW-AS ALERT-BOX ERROR. 
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-whse B-table-Win 
PROCEDURE validate-whse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME f-main:
     FIND FIRST wip-bin WHERE wip-bin.company = cocode
                          AND wip-bin.loc = wiptag-mch.spare-char-3:screen-value in browse {&BROWSE-NAME}
                       NO-LOCK NO-ERROR.
     IF NOT AVAIL wip-bin OR wiptag-mch.spare-char-3:screen-value in browse {&BROWSE-NAME} = "" THEN DO:
       MESSAGE "Invalid WIP Warehouse"
            VIEW-AS ALERT-BOX ERROR.
   /*   APPLY "entry" TO wiptag-mch.spare-char-3 IN BROWSE {&browse-name}.
        RETURN NO-APPLY. */
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widget-walk B-table-Win 
PROCEDURE widget-walk :
/*--------------------------------------------------------------------
----------
  Purpose:     Walks the widget tree to various purposes
  Parameters:  Starting widget handle, run procedure info
  Notes:       Requires run procedure to have the following
               parameters - (INPUT widget-handle, INPUT character)

    p-start    Start traversal with this widget, usually a frame
    p-run      Name of the internal procedure to run for each widget
    p-in       Handle of program to run the internal procedure in
    p-pass     A parameter the caller can set to pass to the IP
    p-types    Only affect these types of widgets - selection criteria
    p-name     The list of widget names to be affected
----------------------------------------------------------------------
--------*/

  DEFINE INPUT  PARAMETER  p-start   AS WIDGET-HANDLE  NO-UNDO.
  DEFINE INPUT  PARAMETER  p-names   AS CHAR           NO-UNDO.
  DEFINE OUTPUT PARAMETER op-handle AS HANDLE NO-UNDO.

  DEF VAR  t-wh    AS WIDGET-HANDLE  NO-UNDO.
  DEF VAR  t-name  AS CHAR           NO-UNDO.
  DEF VAR  l-loop  AS LOGICAL        NO-UNDO.
  DEF VAR  l-down  AS LOGICAL        NO-UNDO.
  op-handle = ?.
  ASSIGN 
    t-wh   = p-start
    l-loop = (IF VALID-HANDLE(t-wh) THEN TRUE ELSE FALSE)
    l-down = TRUE.

  BLK-MAIN:
  DO WHILE l-loop:
    /***[ Execute current item ]***/

    /* setup the name portion for checking */
    /* may want this to be an internal procedure to deal with prefixes
    */ ASSIGN t-name = t-wh:NAME.
        /***[ Do type and name checking then execute internal procedure
    ]***/

    IF p-names <> "" AND index(t-name, p-names) > 0
    THEN DO: 
        op-handle = t-wh.
        RETURN.
    END.


    /***[ Try to go down first - only on first time ]***/
    IF CAN-QUERY(t-wh,"FIRST-CHILD")
         AND VALID-HANDLE(t-wh:FIRST-CHILD) 
         AND l-down
       THEN ASSIGN t-wh   = t-wh:FIRST-CHILD
                   l-down = TRUE.

      /***[ If down not an option try sideways ]***/
      ELSE IF VALID-HANDLE(t-wh:NEXT-SIBLING) 
             THEN ASSIGN t-wh   = t-wh:NEXT-SIBLING
                         l-down = TRUE.

             /***[ If not down or sideways then go back up ]***/   
             /***[ If up is back to top then signal exit   ]***/ ELSE
             IF t-wh:PARENT = p-start
                    THEN ASSIGN t-wh   = p-start
                                l-loop = FALSE.

                    /***[ Go up but look sideways ]***/
                    ELSE ASSIGN t-wh   = t-wh:PARENT
                                l-down = FALSE.

  END.  /***[ End of DO WHILE ]***/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDept B-table-Win 
FUNCTION getDept RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDept AS CHAR NO-UNDO.
FIND FIRST mach WHERE mach.company = cocode 
                  AND mach.m-code = wiptag-mch.m-code:screen-value in browse {&BROWSE-NAME}
                NO-LOCK NO-ERROR.
IF AVAIL mach THEN
  cDept = mach.dept[1].

  RETURN cDept.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDesc B-table-Win 
FUNCTION getDesc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDesc AS CHAR NO-UNDO.
FIND FIRST mach WHERE mach.company = cocode 
                  AND mach.m-code = wiptag-mch.m-code
                NO-LOCK NO-ERROR.

IF AVAIL mach THEN
  cDesc = mach.m-Dscr.

  RETURN cDesc.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

