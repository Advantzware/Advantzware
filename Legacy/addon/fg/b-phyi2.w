&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: addon\fg\b-phyi2.w

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

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.
def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF BUFFER bf-tmp FOR fg-rctd.  /* for tag validation */
DEF BUFFER xfg-rdtlh FOR fg-rdtlh. /* for tag validation */


DEF VAR v-msgreturn AS INT NO-UNDO.
DEF VAR lv-prev-job2 AS cha NO-UNDO.
DEF VAR lv-job-no AS CHAR NO-UNDO.
DEF VAR lv-job-no2 AS CHAR NO-UNDO.
DEF VAR v-out AS INT NO-UNDO.

DEF VAR lv-overrun-checked AS LOG NO-UNDO.
DEF VAR lv-closed-checked AS LOG NO-UNDO.
DEF VAR lv-new-job-ran AS LOG NO-UNDO.
DEF VAR char-hdl AS cha NO-UNDO.
DEFINE VARIABLE hBrowse  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hColumn  AS HANDLE  NO-UNDO.
DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/ssfgscan.i}
{sys/inc/ssfgretc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table fg-rctd.tag fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.cases fg-rctd.qty-case fg-rctd.cases-unit ~
fg-rctd.partial fg-rctd.t-qty fg-rctd.inv-no fg-rctd.job-no fg-rctd.job-no2 ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.std-cost fg-rctd.cost-uom ~
fg-rctd.ext-cost fg-rctd.created-by fg-rctd.updated-by fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table fg-rctd.tag fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.t-qty fg-rctd.inv-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table fg-rctd
&Scoped-define QUERY-STRING-br_table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company ~
and fg-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company ~
and fg-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-br_table fg-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-cost B-table-Win 
FUNCTION calc-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      fg-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(13)":U WIDTH 7
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT ">>>,>>9":U WIDTH 9
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9":U
      fg-rctd.cases-unit COLUMN-LABEL "Units!Skid" FORMAT ">>9":U
            WIDTH 9
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
            WIDTH 9
      fg-rctd.t-qty COLUMN-LABEL "Qty" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 16
      fg-rctd.inv-no COLUMN-LABEL "Counted Qty" FORMAT ">>>,>>>,>>9":U
            WIDTH 16
      fg-rctd.job-no FORMAT "x(6)":U
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.i-no FORMAT "x(15)":U
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.std-cost COLUMN-LABEL "Cost" FORMAT ">>>,>>9.99<<":U
      fg-rctd.cost-uom FORMAT "x(3)":U
      fg-rctd.ext-cost COLUMN-LABEL "Ext. Cost" FORMAT "->>>,>>9.99<<":U
      fg-rctd.created-by COLUMN-LABEL "User!Created" FORMAT "x(8)":U
      fg-rctd.updated-by COLUMN-LABEL "User!Updated" FORMAT "x(8)":U
      fg-rctd.rct-date COLUMN-LABEL "Count!Date" FORMAT "99/99/9999":U
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Count!Time"
  ENABLE
      fg-rctd.tag
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.t-qty
      fg-rctd.inv-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 7.38
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
         HEIGHT             = 7.52
         WIDTH              = 129.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.fg-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.fg-rctd.company = g_company
and fg-rctd.rita-code = ""I"""
     _FldNameList[1]   > ASI.fg-rctd.tag
"fg-rctd.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.loc
"fg-rctd.loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" ? "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.cases-unit
"fg-rctd.cases-unit" "Units!Skid" ? "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" ? "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.t-qty
"fg-rctd.t-qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.inv-no
"fg-rctd.inv-no" "Counted Qty" ">>>,>>>,>>9" "integer" ? ? ? ? ? ? yes "" no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.fg-rctd.job-no
     _FldNameList[11]   = ASI.fg-rctd.job-no2
     _FldNameList[12]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" ? "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rctd.std-cost
"fg-rctd.std-cost" "Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   = ASI.fg-rctd.cost-uom
     _FldNameList[16]   > ASI.fg-rctd.ext-cost
"fg-rctd.ext-cost" "Ext. Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.fg-rctd.created-by
"fg-rctd.created-by" "User!Created" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.updated-by
"fg-rctd.updated-by" "User!Updated" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.rct-date
"fg-rctd.rct-date" "Count!Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Count!Time" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME F-Main
DO:
    DEF VAR rec-val AS RECID NO-UNDO.


   IF NOT AVAIL fg-rctd THEN FIND fg-rctd WHERE RECID(fg-rctd) EQ lv-recid NO-LOCK NO-ERROR. 

   ll-help-run = YES.

   CASE FOCUS:NAME:
     WHEN "i-no" THEN DO:
       RUN windows/l-itemfg.w (fg-rctd.company, "", FOCUS:SCREEN-VALUE, OUTPUT char-val).
       IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
         FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
         APPLY "value-changed" TO FOCUS.
       END.         
     END.

     WHEN "job-no" THEN DO:
       RUN fgbin-help.
     END.

     WHEN "job-no2" THEN DO:
       RUN fgbin-help.
     END.

     when "loc" then do:
        run windows/l-loc.w (g_company,focus:screen-value, output char-val).
        if char-val <> "" then 
           assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val).
        return no-apply.   
     end.
     when "loc-bin" then do:
        run windows/l-fgbin.w (g_company,fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} , fg-rctd.loc-bin:screen-value,output char-val).
        if char-val <> "" THEN
           assign focus:screen-value  = entry(1,char-val).
        return no-apply.   
     end.
    WHEN "tag" THEN DO:
     /* RUN fgbin-help. */
          run windows/l-ldtag4.w (g_company,no,focus:screen-value,output char-val,OUTPUT rec-val).
          if char-val <> "" then do :
             FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
             /*  ===*/
             IF CAN-FIND(FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} AND
                RECID(bf-tmp) <> RECID(fg-rctd)) THEN DO:
                MESSAGE "This Tag Number Has Already Been Used." skip
                        "Please Enter A Unique Tag Number." 
                        VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.
             ELSE DO:
                IF CAN-FIND(first xfg-rdtlh where xfg-rdtlh.company   eq g_company
                     and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                     and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                     and xfg-rdtlh.qty       gt 0
                     and xfg-rdtlh.rita-code ne "S"
                     use-index tag) THEN DO:
                   MESSAGE "This Tag Number Has Already Been Used." skip
                           "Please Enter A Unique Tag Number." 
                           VIEW-AS ALERT-BOX ERROR.
                   RETURN NO-APPLY.
                END.
             END.
           /*  {addon/loadtags/disptagf.i "FGItem" FOCUS:SCREEN-VALUE} */
           FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
           IF NOT AVAIL loadtag THEN DO:
              MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
              RETURN NO-APPLY.
           END.

           ASSIGN
           fg-rctd.job-no:SCREEN-VALUE = loadtag.job-no 
           fg-rctd.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
           fg-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
           fg-rctd.i-name:SCREEN-VALUE =  loadtag.i-name 
           fg-rctd.t-qty:SCREEN-VALUE = STRING(loadtag.pallet-count) /*qty*/
           fg-rctd.qty-case:SCREEN-VALUE = string(loadtag.qty-case)
           fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle)
           fg-rctd.cases-unit:SCREEN-VALUE = string(loadtag.case-bundle)
           fg-rctd.partial:SCREEN-VALUE = STRING(loadtag.partial)
           fg-rctd.loc:SCREEN-VALUE = loadtag.loc
           fg-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
           fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE
           lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}
           lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
           lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.
           
           RUN get-fg-bin-cost.      
           APPLY "entry" TO fg-rctd.loc.
           RETURN NO-APPLY.
          END.
    END.

   END CASE.

   RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
  ll-help-run = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i}*/
     {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE br_table /* Tag# */
DO:
  /*  regular physical count
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (5) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  */

   IF LASTKEY = -1 /*OR fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = ""  */
      OR LASTKEY = 27
   THEN RETURN.

 RUN valid-tag# NO-ERROR.
 IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   IF CAN-FIND(FIRST bf-tmp WHERE
      bf-tmp.company = g_company AND
      bf-tmp.tag = SELF:SCREEN-VALUE
      AND RECID(bf-tmp) <> RECID(fg-rctd)) THEN DO:
       /*
      MESSAGE "This Tag Number Has Already Been Used." skip
              "Please Enter A Unique Tag Number." 
          VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
      */
      RUN custom/d-msg.w ("Error","This Tag Number Has Already Been Used.","Please Enter A Unique Tag Number.","",1,"OK", OUTPUT v-msgreturn).
      RETURN NO-APPLY.

   END.
   ELSE DO:
       IF CAN-FIND(first xfg-rdtlh
              where xfg-rdtlh.company   eq g_company
                and xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                and xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE
                and xfg-rdtlh.qty       gt 0
                and xfg-rdtlh.rita-code ne "S" use-index tag) THEN DO:
             /* MESSAGE "This Tag Number Has Already Been Used." skip
                      "Please Enter A Unique Tag Number." 
                      VIEW-AS ALERT-BOX ERROR.
              RETURN NO-APPLY.
             */
              RUN custom/d-msg.w ("Error","This Tag Number Has Already Been Used.","Please Enter A Unique Tag Number.","",1,"OK", OUTPUT v-msgreturn).
              RETURN NO-APPLY.
          END.
   END.
  /* {addon/loadtags/disptagf.i "FGItem" SELF:SCREEN-VALUE}*/
    FIND FIRST loadtag WHERE
         loadtag.company = g_company AND
         loadtag.ITEM-type = NO AND
         loadtag.tag-no = SELF:SCREEN-VALUE
         NO-LOCK NO-ERROR.

   IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   ASSIGN
      fg-rctd.job-no:SCREEN-VALUE = loadtag.job-no 
      fg-rctd.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
      fg-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
      fg-rctd.i-name:SCREEN-VALUE =  loadtag.i-name 
      lv-prev-job2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}
      lv-job-no = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
      lv-job-no2 = fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}.

   IF fg-rctd.loc:SCREEN-VALUE EQ "" THEN
      fg-rctd.loc:SCREEN-VALUE =  loadtag.loc.

   IF fg-rctd.loc-bin:SCREEN-VALUE EQ "" THEN
      fg-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin.

   IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN
      fg-rctd.rct-date:SCREEN-VALUE = STRING(TODAY).

   IF INT(fg-rctd.qty-case:SCREEN-VALUE) EQ 0 THEN
      fg-rctd.qty-case:SCREEN-VALUE = string(loadtag.qty-case).

   IF INT(fg-rctd.cases:SCREEN-VALUE) EQ 0 THEN
      fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle).

   IF INT(fg-rctd.cases-unit:SCREEN-VALUE) EQ 0 THEN
      fg-rctd.cases-unit:SCREEN-VALUE = string(loadtag.case-bundle).

   IF INT(fg-rctd.partial:SCREEN-VALUE) EQ 0 THEN
      fg-rctd.partial:SCREEN-VALUE = STRING(loadtag.partial).

   IF DEC(fg-rctd.t-qty:SCREEN-VALUE) EQ 0 THEN
      /*fg-rctd.t-qty:SCREEN-VALUE = STRING(loadtag.pallet-count).*/
      fg-rctd.t-qty:SCREEN-VALUE = STRING((loadtag.case-bundle * loadtag.qty-case) + loadtag.partial).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.loc IN BROWSE br_table /* Whse */
DO:
  IF NOT ssfgscan THEN do:
     APPLY 'tab' TO SELF .
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE br_table /* Whse */
DO:
   /*IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.*/

   IF LASTKEY = -1 THEN RETURN.

   IF SELF:MODIFIED THEN DO:
       IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
       END.

       IF NOT CAN-FIND(FIRST loc WHERE
          loc.company = g_company AND
          loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}) THEN DO:
          RUN custom/d-msg.w ("Error","","Invalid Warehouse. Try Help...","",1,"OK", OUTPUT v-msgreturn).
          RETURN NO-APPLY.
       END.

       IF NOT CAN-FIND(FIRST fg-bin WHERE
          fg-bin.company = g_company AND
          fg-bin.i-no = "" AND
          fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} AND
          fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          USE-INDEX co-ino) THEN DO:
    /*      MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.loc .                               */
          RUN custom/d-msg.w ("Error","","Invalid Bin#. Try Help...","",1,"OK", OUTPUT v-msgreturn).
          RETURN NO-APPLY.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.loc-bin IN BROWSE br_table /* Bin */
DO:
  IF NOT ssfgscan THEN do:
     APPLY 'tab' TO SELF .
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE br_table /* Bin */
DO:
   IF LASTKEY NE -1 THEN DO:
    /*RUN valid-job-loc-bin-tag (4) NO-ERROR. */
      RUN valid-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no IN BROWSE br_table /* Job # */
DO:
   IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.job-no2 IN BROWSE br_table
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE br_table /* Item No */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no br_table _BROWSE-COLUMN B-table-Win
ON value-changed OF fg-rctd.i-no IN BROWSE br_table /* Item No */
DO:
  DEF VAR li AS INT NO-UNDO.

  FIND FIRST itemfg
      {sys/look/itemfgrlW.i}
        AND itemfg.i-no EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN DO:
    RUN get-def-values.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help B-table-Win 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    
    RUN windows/l-fgibn2.w (g_company, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).
    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.job-no  OR
                         INT(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.tag)
    THEN DO:
      FIND FIRST itemfg WHERE itemfg.company = g_company AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
      ASSIGN
       fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.i-no
       fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
       fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = string(fg-bin.qty).

      RUN new-bin.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-def-values B-table-Win 
PROCEDURE get-def-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      no-lock no-error.
  fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name.
      
  IF adm-new-record THEN DO:
    find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.
    assign
     /*fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.case-count)*/
     fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = ""
     fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = "".
       
     do:
      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.loc     eq itemfg.def-loc
            and fg-bin.loc-bin eq itemfg.def-loc-bin
            and fg-bin.i-no    eq ""
          no-lock no-error.
      if avail fg-bin then 
        assign
         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = itemfg.def-loc
         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.def-loc-bin.
    end. /*else FGFILE*/

    /*if bin and warehouse are blank, goto cust "X" shipto file*/
    if fg-rctd.loc eq "" and fg-rctd.loc-bin eq "" then do:
      find first cust
          where cust.company eq cocode
            and cust.active  eq "X"
          no-lock no-error.
                                
      if avail cust then do:
        find first shipto
            where shipto.company eq cocode
              and shipto.cust-no eq cust.cust-no  
            no-lock no-error.
           
        if avail shipto then do:
          find first fg-bin
              where fg-bin.company eq cocode
                and fg-bin.loc     eq shipto.loc
                and fg-bin.loc-bin eq shipto.loc-bin
                and fg-bin.i-no    eq ""
              no-lock no-error.
          if avail fg-bin then
            assign
             fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = shipto.loc
             fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = shipto.loc-bin.
        end.                                  
      end.
    end.    
    
  END.

  if fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  eq "" then do:
     find first fg-bin where
          fg-bin.company eq cocode
                and fg-bin.i-no    eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                and fg-bin.job-no  eq fg-rctd.job-no:SCREEN-VALUE
                and ((fg-rctd.job-no:SCREEN-VALUE ne " " and
                    fg-bin.job-no2 eq int(fg-rctd.job-no2:SCREEN-VALUE) ) or
                    (fg-rctd.job-no:SCREEN-VALUE eq " "))
                and fg-bin.qty     le 0              
                no-lock no-error.
     if avail fg-bin then 
        ASSIGN fg-rctd.loc:SCREEN-VALUE     = fg-bin.loc
               fg-rctd.loc-bin:SCREEN-VALUE = fg-bin.loc-bin.
    ELSE if avail itemfg THEN
        ASSIGN fg-rctd.loc:SCREEN-VALUE     = itemfg.def-loc
               fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin               
               /*fg-rctd.qty-case:SCREEN-VALUE = /*IF fg-rctd.po-no:SCREEN-VALUE = "" and
                                                  fg-rctd.job-no:SCREEN-VALUE = "" 
                                                THEN   STRING(itemfg.case-count)
                                                ELSE fg-rctd.qty-case:SCREEN-VALUE
                                                */
                                                STRING(itemfg.case-count)*/
               .
  end.
  ELSE if avail itemfg AND fg-rctd.loc:SCREEN-VALUE = "" THEN
    ASSIGN fg-rctd.loc:SCREEN-VALUE     = itemfg.def-loc
           fg-rctd.loc-bin:SCREEN-VALUE = itemfg.def-loc-bin
  
           /*fg-rctd.qty-case:SCREEN-VALUE = /*IF fg-rctd.po-no:SCREEN-VALUE = "" and
                                              fg-rctd.job-no:SCREEN-VALUE = "" 
                                           THEN   STRING(itemfg.case-count)
                                           ELSE fg-rctd.qty-case:SCREEN-VALUE
                                           */
                                           STRING(itemfg.case-count)*/
            .

  RUN new-bin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-fg-bin-cost B-table-Win 
PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bf-reftable FOR reftable.

    IF fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    ELSE DO:
        FIND FIRST loadtag WHERE loadtag.company = cocode
                             AND loadtag.item-type = NO
                             AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAIL loadtag THEN
           FIND FIRST fg-bin WHERE
                fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
                  AND fg-bin.job-no  EQ loadtag.job-no
                  AND fg-bin.job-no2 EQ loadtag.job-no2
                  AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE 
                  AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE 
                  AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE 
                  NO-LOCK NO-ERROR.
                  
    END.
    IF AVAIL fg-bin THEN
      ASSIGN
       fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.std-tot-cost)
       fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.pur-uom).

  FIND FIRST job-hdr WHERE
       job-hdr.company = g_company
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   /*IF AVAIL job-hdr THEN 
      ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost) . */

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST bf-reftable
          WHERE bf-reftable.reftable EQ "jc/jc-calc.p"
            AND bf-reftable.company  EQ job.company
            AND bf-reftable.loc      EQ ""
            AND bf-reftable.code     EQ STRING(job.job,"999999999")
            AND bf-reftable.code2    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = string(job-hdr.std-tot-cost).
    ELSE 
    IF AVAIL bf-reftable AND bf-reftable.val[5] GT 0 THEN
      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(bf-reftable.val[5]).
    else do:
     /* find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            and po-ordl.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost).
      END.
     
      else */
      if avail itemfg then
        assign
         fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.prod-uom
         fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.total-std-cost).
    end.
   
   IF NOT AVAIL job-hdr OR dec(fg-rctd.std-cost:SCREEN-VALUE) = 0 THEN DO:
      IF fg-rctd.job-no:SCREEN-VALUE <> ""  THEN
         FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.i-no = fg-rctd.i-no:SCREEN-VALUE
                           AND oe-ordl.job-no = fg-rctd.job-no:SCREEN-VALUE
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      ELSE IF AVAIL loadtag THEN
         FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.i-no = fg-rctd.i-no:SCREEN-VALUE
                           AND oe-ordl.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.

      IF AVAIL oe-ordl THEN
           ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(oe-ordl.cost)
                  fg-rctd.cost-uom:SCREEN-VALUE = "M".
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
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.


  if not avail fg-rctd then return.  /* no records */

DO WITH FRAME {&FRAME-NAME}:
find FIRST itemfg where itemfg.company eq cocode
              and itemfg.i-no  eq fg-rctd.i-no:screen-value in browse {&browse-name}
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0.

if ip-first-disp  and avail fg-rctd and fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} <> "" then do: /* for row-display */  
  find first po-ordl where po-ordl.company = cocode
                       and po-ordl.po-no = int(fg-rctd.po-no)
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = fg-rctd.job-no
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.

  ASSIGN
   lv-out-qty  = fg-rctd.t-qty
   lv-out-cost = fg-rctd.std-cost.

  /* convert cost pr-uom*/
  IF fg-rctd.cost-uom NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).
end. /* avail fg-rctd */
/* ======================================================================= */
else
if avail fg-rctd and fg-rctd.i-no:SCREEN-VALUE <> "" then do: /* in update mode - use screen-value */
  /*find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name}) 
                       and po-ordl.i-no  = fg-rctd.i-no:screen-value
                       and po-ordl.job-no = (fg-rctd.job-no:screen-value)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       and po-ordl.item-type = no
                       no-lock no-error.
  
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = po-ordl.t-rec-qty + int(fg-rctd.t-qty:SCREEN-VALUE).

    IF po-ordl.pr-qty-uom <> "EA" THEN
       run sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, output v-rec-qty).
    if v-rec-qty gt (po-ordl.ord-qty * 
                    (1 + (po-ordl.over-pct / 100)))
       AND NOT lv-overrun-checked
    then do:
       message "The PO Qty + overrun has been exceeded. "
                  VIEW-AS ALERT-BOX WARNING .
       lv-overrun-checked = YES.
       /*APPLY "entry" TO fg-rctd.cases.
       RETURN ERROR.  */
    end.
  END.
  IF fg-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
       find first job-hdr where job-hdr.company = fg-rctd.company                       
                       and job-hdr.i-no  = fg-rctd.i-no:screen-value
                       and job-hdr.job-no = (fg-rctd.job-no:screen-value)
                       and job-hdr.job-no2 = integer(fg-rctd.job-no2:screen-value)
                       no-lock no-error.
       IF AVAIL job-hdr THEN DO: 
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-rec-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-rec-qty = (job-hdr.qty * (1 + (IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                               IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0 / 100))).
      
          END.
          IF v-rec-qty <  int(fg-rctd.t-qty:SCREEN-VALUE) AND NOT lv-overrun-checked THEN DO:
             MESSAGE "Receipt quantity exceeds job quantity." VIEW-AS ALERT-BOX Warning.
             /*RETURN ERROR.*/
             lv-overrun-checked = YES.
          END.
          
       END.
  END.
  */

  ASSIGN
   lv-out-qty  = DEC(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})
   lv-out-cost = DEC(fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}). 

  IF fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-out-cost).
END.

IF lv-cost-uom NE "EA" THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).

ASSIGN
 fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = lv-cost-uom
 fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-out-cost)
 fg-rctd.ext-cost:SCREEN-VALUE IN BROWSE {&browse-name} =
       STRING((lv-out-qty * lv-out-cost) /*+
           dec(fg-rctd.frt-cost:screen-value in browse {&browse-name}))*/ ).

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
DEF BUFFER bf-reftable FOR reftable.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


    IF fg-rctd.created-by = "" THEN DO:
         ASSIGN fg-rctd.created-by = USERID("nosweat").
    END.
    ASSIGN
        fg-rctd.updated-by = USERID("nosweat")
        fg-rctd.upd-date = TODAY 
        fg-rctd.upd-time = TIME 
        fg-rctd.enteredBy = USERID("asi")
        fg-rctd.enteredDT = DATETIME(TODAY, MTIME) 
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 ASSIGN BROWSE {&browse-name}
     fg-rctd.i-no fg-rctd.i-name fg-rctd.rct-date
     fg-rctd.job-no
     fg-rctd.job-no2
     fg-rctd.qty-case fg-rctd.CASEs fg-rctd.cases-unit fg-rctd.partial
     fg-rctd.std-cost fg-rctd.cost-uom
     fg-rctd.ext-cost.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.

  /* Code placed here will execute PRIOR to standard behavior. */

  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  assign fg-rctd.company = g_company
         fg-rctd.r-no    = lv-rno
         fg-rctd.rita-code = "I"
         fg-rctd.s-num  = 0
         fg-rctd.rct-date = today
         fg-rctd.trans-time = TIME
         lv-recid = recid(fg-rctd).

  disp fg-rctd.rct-date with browse {&browse-name}. 
    
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
         {custom/askdelss.i}
  END.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*apply "entry" to fg-rctd.tag in browse {&browse-name}.
  return no-apply. */

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
  IF ssfgretc-log EQ NO THEN
  DO:
     hBrowse = BROWSE br_table:HANDLE.   
     DO iCounter = hBrowse:NUM-COLUMNS TO 1 BY -1:
        hColumn = hBrowse:GET-BROWSE-COLUMN(iCounter).

        IF hColumn:NAME = "inv-no" THEN
        DO:
           hColumn:READ-ONLY = TRUE.
           LEAVE.
        END.
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

  /* Code placed here will execute PRIOR to standard behavior. */
 /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  RUN valid-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN get-matrix (NO).
  /*
  RUN valid-job-loc-bin-tag (5) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-overrun-checked = NO.
  RUN repo-query (ROWID(fg-rctd)).

  ASSIGN lv-new-job-ran = NO
         lv-prev-job2 = "".
  RUN scan-next.

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
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST fg-bin 
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
          AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       /*fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)*/
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       /*ld-cost                                                = fg-bin.std-tot-cost*/.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-record B-table-Win 
PROCEDURE post-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-done AS INT NO-UNDO.


  /*RUN custom/d-msg.w ("Warning","","Are you ready to post FG Physical Count?","",2,"YES,NO", OUTPUT v-done).
  IF v-done >= 2 THEN RETURN.*/
  
  /*RUN addon/fg/fgpstall.w (?,"C"). */

  RUN fg/fgpstall.w (?, "SETUP").
  
  RUN dispatch ('open-query').

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

  RUN dispatch ('open-query').

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  
  RUN dispatch ('row-changed').

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
  {src/adm/template/snd-list.i "fg-rctd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bin B-table-Win 
PROCEDURE valid-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF NOT CAN-FIND(FIRST fg-bin WHERE
      fg-bin.company = g_company AND
      fg-bin.i-no = "" AND
      fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} AND
      fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE) THEN
      DO:
         RUN custom/d-msg.w ("Error","","Invalid Bin#. Try Help...","",1,"OK", OUTPUT v-msgreturn).
         /*MESSAGE "Invalid Bin. Try help." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO fg-rctd.loc-bin.                          */
         RETURN ERROR.
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
    IF NOT CAN-FIND(FIRST itemfg
       {sys/look/itemfgrlW.i} AND
       itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
    DO:
       MESSAGE "Invalid Item#, try help..." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO fg-rctd.i-no IN BROWSE {&browse-name}.
       RETURN ERROR.
    END.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-loc-bin-tag B-table-Win 
PROCEDURE valid-job-loc-bin-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-int AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
        TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST fg-bin 
                    WHERE fg-bin.company  EQ cocode
                      AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND (fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       OR ip-int LT 1)
                      AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR ip-int LT 2)
                      AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 3)
                      AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR ip-int LT 4)
                      AND (fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR ip-int LT 5))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
      IF ip-int EQ 1 THEN
        APPLY "entry" TO fg-rctd.job-no IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 2 THEN
        APPLY "entry" TO fg-rctd.job-no2 IN BROWSE {&browse-name}.
      ELSE
      IF ip-int EQ 3 THEN
        APPLY "entry" TO fg-rctd.loc IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag# B-table-Win 
PROCEDURE valid-tag# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF (NOT CAN-FIND(FIRST loadtag WHERE
   loadtag.company = g_company AND
   loadtag.item-type = NO AND
   loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name})) OR
   fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
    /*MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.*/
   RUN custom/d-msg.w ("Error","","","Invalid Tag#. Try help or Scan valid tag#...",1,"OK", OUTPUT v-out).
     
   APPLY "entry" TO fg-rctd.tag IN BROWSE {&browse-name}.
   RETURN ERROR.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-record B-table-Win 
PROCEDURE validate-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST itemfg WHERE
       itemfg.company = fg-rctd.company AND
       itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
       NO-LOCK NO-ERROR.

  IF NOT AVAIL itemfg THEN DO:
     IF fg-rctd.i-no:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid Item. Try help. " VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.i-no .
        RETURN ERROR.
     END.
     ELSE DO:
        MESSAGE "F/G Item is not on file.  Would you like to add it? "
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans THEN DO:
            APPLY "entry" TO fg-rctd.i-no .
            RETURN ERROR.           
        END.
        ELSE DO:
            RUN fg/d-crtitm.w (fg-rctd.i-no:SCREEN-VALUE).
            FIND first itemfg {sys/look/itemfgrlW.i}
                       and itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                       no-lock no-error.
            IF AVAIL itemfg THEN ASSIGN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
        END.
     END.
  END.
  
  IF NOT CAN-FIND(FIRST loc WHERE
     loc.company = g_company AND
     loc.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
     DO:
        MESSAGE "Invalid Warehouse. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fg-rctd.loc.
        RETURN ERROR.
     END.
  
  IF NOT CAN-FIND(FIRST fg-bin WHERE
     fg-bin.company = g_company AND
     fg-bin.i-no = "" AND
     fg-bin.loc = fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} AND
     fg-bin.loc-bin = fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
     USE-INDEX co-ino) THEN DO:
     MESSAGE "Invalid Bin#. Try Help. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO fg-rctd.loc-bin.
     RETURN ERROR.
  END.

  /* ===== tag validation =====*/
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN
  DO:
    IF CAN-FIND(FIRST bf-tmp WHERE
       bf-tmp.company = g_company AND
       bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE AND
       RECID(bf-tmp) <> RECID(fg-rctd)) THEN
       DO:
          MESSAGE "This Tag Number Has Already Been Used." skip
                  "Please Enter A Unique Tag Number." 
              VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fg-rctd.tag.
          RETURN ERROR.
       END.
    ELSE DO:
        IF CAN-FIND(first xfg-rdtlh WHERE
           xfg-rdtlh.company   eq g_company AND
           xfg-rdtlh.loc       eq fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} AND
           xfg-rdtlh.tag       eq fg-rctd.tag:SCREEN-VALUE AND
           xfg-rdtlh.qty       gt 0 AND
           xfg-rdtlh.rita-code ne "S"
           use-index tag) THEN
           DO:
              MESSAGE "This Tag Number Has Already Been Used." skip
                      "Please Enter A Unique Tag Number." 
                      VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO fg-rctd.tag.
              RETURN error.
           END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-cost B-table-Win 
FUNCTION calc-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  run get-matrix (true).
  return ext-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

