&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: addon/fg/b-trnss.w

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
DEFINE VARIABLE iLineCnt AS INTEGER     NO-UNDO.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
/*{methods/defines/hndldefs.i}*/

DEF TEMP-TABLE tt-line-cnt NO-UNDO
  FIELD line-rowid AS ROWID 
  FIELD line-number AS INT .

DO TRANSACTION:
   {sys/inc/sstransf.i}
END.

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
&Scoped-define FIELDS-IN-QUERY-br_table iLineCnt fg-rctd.tag fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.partial fg-rctd.loc2 fg-rctd.loc-bin2 fg-rctd.tag2 fg-rctd.job-no ~
fg-rctd.job-no2 fg-rctd.i-no fg-rctd.i-name fg-rctd.rct-date ~
STRING(fg-rctd.trans-time,'HH:MM') @ trans-time fg-rctd.created-by ~
fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table fg-rctd.tag fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.partial fg-rctd.loc2 fg-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table fg-rctd
&Scoped-define QUERY-STRING-br_table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "T" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH fg-rctd WHERE ~{&KEY-PHRASE} ~
      AND fg-rctd.company = g_company and ~
fg-rctd.rita-code = "T" NO-LOCK ~
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calc-ext-cost B-table-Win 
FUNCTION calc-ext-cost RETURNS DECIMAL
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
      iLineCnt COLUMN-LABEL "#" FORMAT "99":U
      fg-rctd.tag COLUMN-LABEL "From!Tag#" FORMAT "x(23)":U
      fg-rctd.loc COLUMN-LABEL "From!Whse" FORMAT "x(5)":U WIDTH 7
      fg-rctd.loc-bin COLUMN-LABEL "From!Bin" FORMAT "x(8)":U
      fg-rctd.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 12
      fg-rctd.cases COLUMN-LABEL "Units" FORMAT ">>>,>>9":U WIDTH 9
      fg-rctd.qty-case COLUMN-LABEL "Unit!Count" FORMAT ">>>,>>9":U
            WIDTH 9
      fg-rctd.partial COLUMN-LABEL "Partial" FORMAT ">>>,>>9":U
            WIDTH 10
      fg-rctd.loc2 COLUMN-LABEL "To!Whse" FORMAT "x(5)":U WIDTH 7
      fg-rctd.loc-bin2 COLUMN-LABEL "To!Bin" FORMAT "x(8)":U
      fg-rctd.tag2 COLUMN-LABEL "To!Tag#" FORMAT "x(23)":U
      fg-rctd.job-no FORMAT "x(6)":U
      fg-rctd.job-no2 FORMAT "99":U
      fg-rctd.i-no COLUMN-LABEL "Item #" FORMAT "x(15)":U
      fg-rctd.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      fg-rctd.rct-date COLUMN-LABEL "Transfer!Date" FORMAT "99/99/9999":U
      STRING(fg-rctd.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Transfer!Time"
      fg-rctd.created-by COLUMN-LABEL "User!Created" FORMAT "x(8)":U
      fg-rctd.updated-by COLUMN-LABEL "User!Updated" FORMAT "x(8)":U
  ENABLE
      fg-rctd.tag
      fg-rctd.loc
      fg-rctd.loc-bin
      fg-rctd.cust-no
      fg-rctd.cases
      fg-rctd.qty-case
      fg-rctd.partial
      fg-rctd.loc2
      fg-rctd.loc-bin2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 126 BY 7.62
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
         HEIGHT             = 7.67
         WIDTH              = 126.8.
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
     _Where[1]         = "ASI.fg-rctd.company = g_company and
fg-rctd.rita-code = ""T"""
     _FldNameList[1]   > "_<CALC>"
"iLineCnt" "#" "99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-rctd.tag
"fg-rctd.tag" "From!Tag#" "x(23)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rctd.loc
"fg-rctd.loc" "From!Whse" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rctd.loc-bin
"fg-rctd.loc-bin" "From!Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-rctd.cust-no
"fg-rctd.cust-no" "Customer#" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rctd.cases
"fg-rctd.cases" "Units" ? "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-rctd.qty-case
"fg-rctd.qty-case" "Unit!Count" ? "integer" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rctd.partial
"fg-rctd.partial" "Partial" ? "integer" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rctd.loc2
"fg-rctd.loc2" "To!Whse" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rctd.loc-bin2
"fg-rctd.loc-bin2" "To!Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.fg-rctd.tag2
"fg-rctd.tag2" "To!Tag#" "x(23)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = ASI.fg-rctd.job-no
     _FldNameList[13]   = ASI.fg-rctd.job-no2
     _FldNameList[14]   > ASI.fg-rctd.i-no
"fg-rctd.i-no" "Item #" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rctd.i-name
"fg-rctd.i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rctd.rct-date
"fg-rctd.rct-date" "Transfer!Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"STRING(fg-rctd.trans-time,'HH:MM') @ trans-time" "Transfer!Time" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rctd.created-by
"fg-rctd.created-by" "User!Created" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rctd.updated-by
"fg-rctd.updated-by" "User!Updated" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.

    DEF BUFFER bf-tmp FOR fg-rctd .


  IF NOT avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid no-lock no-error. 

  def var ll-tag# as log no-undo.
  ll-help-run = yes.

  lw-focus = FOCUS.

  case lw-focus:name :
      when "i-no" then do:
            run windows/l-itemfg.w (fg-rctd.company, "", lw-focus:SCREEN-VALUE, output char-val).
            if char-val <> "" then do :
               assign lw-focus:screen-value = entry(1,char-val)
                      fg-rctd.i-name:screen-value in browse {&browse-name} = entry(2,char-val)
                      .
            end.
      end.

      WHEN "job-no" THEN DO:
        RUN fgbin-help.
      END.

      WHEN "job-no2" THEN DO:
        RUN fgbin-help.
      END.

      WHEN "loc" THEN DO:
        RUN fgbin-help.
      END.

      WHEN "loc-bin" THEN DO:
        RUN fgbin-help.
      END.

      WHEN "cust-no" THEN DO:
        RUN fgbin-help.
      END.

      WHEN "tag" THEN DO:
        /*RUN fgbin-help.*/
          /*run windows/l-ldtag3.w (g_company,no,lw-focus:screen-value,output char-val,OUTPUT rec-val).*/
          run windows/l-fgtg2.w (g_company,lw-focus:screen-value,output char-val).
          if char-val <> "" then do :
             fg-rctd.tag:SCREEN-VALUE = ENTRY(1,char-val).
             /*  ===*/
            FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND
                                     bf-tmp.rita-code = "T" AND
                                     bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE
                                 AND RECID(bf-tmp) <> RECID(fg-rctd)
                       NO-LOCK NO-ERROR.

             IF AVAIL bf-tmp THEN DO:
                MESSAGE "This Tag Number Has Already Been Used." skip
                        "Please Enter A Unique Tag Number." 
                        VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.
             /*FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
             IF NOT AVAIL loadtag THEN DO:
                MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
             END.

             ASSIGN fg-rctd.job-no:SCREEN-VALUE = loadtag.job-no 
                    fg-rctd.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
                    /*   fg-rctd.ord-no:SCREEN-VALUE = STRING(loadtag.ord-no) */
                    fg-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
                    fg-rctd.i-name:SCREEN-VALUE =  loadtag.i-name              
                    /*fg-rctd.t-qty:SCREEN-VALUE = STRING(loadtag.pallet-count) /*qty*/*/
                    fg-rctd.qty-case:SCREEN-VALUE = string(loadtag.qty-case)
                    fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.tot-cases)
                    /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle)*/
                    /*fg-rctd.cases-unit:SCREEN-VALUE = string(loadtag.case-bundle)*/
                    fg-rctd.loc:SCREEN-VALUE = loadtag.loc
                    fg-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
                    fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE.*/

             ASSIGN
              fg-rctd.i-no:SCREEN-VALUE    = ENTRY(6,char-val) 
              fg-rctd.job-no:SCREEN-VALUE  = ENTRY(4,char-val) 
              fg-rctd.job-no2:SCREEN-VALUE = ENTRY(5,char-val)
              fg-rctd.loc:SCREEN-VALUE     = ENTRY(2,char-val)
              fg-rctd.loc-bin:SCREEN-VALUE = ENTRY(3,char-val)
              fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE EQ "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE.

             RUN new-bin.

             FIND FIRST itemfg
                 WHERE itemfg.company EQ g_company
                   AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
                 NO-LOCK NO-ERROR.
             IF AVAIL itemfg THEN fg-rctd.i-name:SCREEN-VALUE = itemfg.i-name.
 
             APPLY "leave" TO fg-rctd.tag.
          END.
      END.
      WHEN "tag2" THEN DO:
        RUN fgbin2-help.
      END.
      when "loc2" then do:
            run windows/l-loc.w (fg-rctd.company,lw-focus:screen-value, output char-val).
            if char-val <> "" then do :
               assign fg-rctd.loc2:screen-value in  browse {&browse-name}  = entry(1,char-val).             
               APPLY "tab" TO fg-rctd.loc2.
            end.   
      end.
      when "loc-bin2" then do:
            run windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc2:screen-value,lw-focus:screen-value, output char-val).
            if char-val <> "" then do :
               assign fg-rctd.loc-bin2:screen-value  = entry(1,char-val).
               APPLY "tab" TO fg-rctd.loc-bin2.
            end.   
      end.
    end case.

    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
  DEF VAR iHoldCnt AS INT.

  IF AVAIL(fg-rctd) AND ROWID(fg-rctd) NE ? THEN DO:
    FIND FIRST tt-line-cnt WHERE tt-line-cnt.line-rowid = ROWID(fg-rctd)
        NO-LOCK NO-ERROR.
    IF AVAIL tt-line-cnt THEN DO:
      iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:       
         iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-line-cnt.line-number).
      END.      
    END.
    ELSE DO:      
        iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:    
          iLineCnt = iLineCnt + 1.        
          iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(iLineCnt, "99").
          CREATE tt-line-cnt.
          ASSIGN tt-line-cnt.line-rowid = ROWID(fg-rctd)
                 tt-line-cnt.line-number = iLineCnt.
        END. /* if no error */
    END. /* if avail tt-line-cnt */
  END. /* if has a rowid */

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
   /*{src/adm/template/brsleave.i} */
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
ON ENTRY OF fg-rctd.tag IN BROWSE br_table /* From!Tag# */
DO:
   IF INTEGER(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) EQ iLineCnt 
    AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""
    AND adm-new-record THEN
     iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(iLineCnt + 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.tag IN BROWSE br_table /* From!Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN leave-tag.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc IN BROWSE br_table /* From!Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
    END.

    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin IN BROWSE br_table /* From!Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.cases IN BROWSE br_table /* Units */
DO:
   IF sstransf-char = "Pallet" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.cases IN BROWSE br_table /* Units */
DO:
  DEF VAR lv-new-tag-number-chosen AS LOG NO-UNDO.
  DEF BUFFER b-loadtag FOR loadtag.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  FIND FIRST b-loadtag WHERE
       b-loadtag.company = g_company AND
       b-loadtag.item-type = NO AND
       b-loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
       NO-LOCK NO-ERROR.

  IF AVAIL b-loadtag THEN
  DO:
     MESSAGE "Units not matching units in Tag will need to create a new tag #"
             SKIP(1)
             "Click OK to continue and create a new tag number on save or cancel"
             VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
             TITLE "" UPDATE choice AS LOGICAL.
     IF choice THEN
        lv-new-tag-number-chosen = TRUE.
     ELSE
        lv-new-tag-number-chosen = FALSE.
     IF NOT lv-new-tag-number-chosen THEN DO:
         /* may be a partial qty transfer 10081201 */
         IF INT(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) < b-loadtag.tot-cases THEN
         DO:
            MESSAGE "Units is Less Than Loadtag O/H Cases."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
         END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.qty-case IN BROWSE br_table /* Unit!Count */
DO:
   APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rctd.partial IN BROWSE br_table /* Partial */
DO:
  IF sstransf-char = "Pallet" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.partial IN BROWSE br_table /* Partial */
DO:
   IF LASTKEY NE -1 THEN DO:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc2 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc2 IN BROWSE br_table /* To!Whse */
DO:
  DEF VAR lv-scanned-loc AS cha NO-UNDO.
  
  IF LASTKEY NE -1 THEN DO:
    lv-scanned-loc = focus:SCREEN-VALUE.
    IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = FOCUS:SCREEN-VALUE.
          ASSIGN fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 fg-rctd.loc-bin2:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
       END.
    RUN valid-loc2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF length(lv-scanned-loc) > 5 THEN do:
       RUN valid-loc-bin2 NO-ERROR.    
       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
       APPLY "tab" TO fg-rctd.loc-bin2 IN BROWSE {&browse-name} .       
       RETURN NO-APPLY.
    END.
    ELSE RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin2 br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.loc-bin2 IN BROWSE br_table /* To!Bin */
DO:
   IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin2 NO-ERROR.
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
    RUN valid-job-loc-bin-tag NO-ERROR.
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
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rctd.i-no IN BROWSE br_table /* Item # */
DO:
   IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

    
ASSIGN
 cocode = g_company
 locode = g_loc.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-entry B-table-Win 
PROCEDURE cancel-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 IF AVAIL fg-rctd AND fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
    RUN dispatch ('cancel-record').
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

    IF AVAIL fg-bin AND (fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       NE fg-bin.job-no  OR
                         INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) NE fg-bin.job-no2 OR
                         fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}      NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          NE fg-bin.tag     OR
                         fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}      NE fg-bin.cust-no)
    THEN DO:
      FIND FIRST itemfg WHERE itemfg.company = g_company AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
      ASSIGN
       fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}    = fg-bin.i-no
       fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}  = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}  = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.cust-no.

      RUN new-bin.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin2-help B-table-Win 
PROCEDURE fgbin2-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-rowid AS ROWID NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-fgibn1.w (g_company, fg-rctd.i-no:screen-value in browse {&browse-name}, fg-rctd.job-no:screen-value in browse {&browse-name}, INT(fg-rctd.job-no2:screen-value in browse {&browse-name}), fg-rctd.loc:screen-value in browse {&browse-name}, fg-rctd.loc-bin:screen-value in browse {&browse-name}, fg-rctd.tag:screen-value in browse {&browse-name}, output lv-rowid).

    FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND (fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.loc     OR
                         fg-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE fg-bin.loc-bin OR
                         fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE fg-bin.tag)
    THEN DO:
      ASSIGN
       fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.loc
       fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.loc-bin
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.tag.

      
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-tag B-table-Win 
PROCEDURE leave-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    RUN valid-job-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF sstransf-char = "Pallet" THEN
       APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
    ELSE IF sstransf-char = "Case" THEN
         APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
    ELSE DO:
       FIND itemfg WHERE itemfg.company = g_company
                  AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF AVAIL itemfg AND itemfg.ship-meth THEN APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
       ELSE APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
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
  DEF VAR lv-tag2 LIKE fg-rctd.tag2 NO-UNDO.
  DEF VAR lcJobNo LIKE fg-rctd.job-no NO-UNDO.
  DEF VAR liJobNo2 LIKE fg-rctd.job-no2 NO-UNDO.
  DEF VAR v-tag-no AS INT NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.

  DEF BUFFER bf-tag FOR loadtag.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    lv-tag2 = fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}.
    lcJobNo = fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}.
    liJobNo2 = INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   fg-rctd.t-qty = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial
   fg-rctd.tag2  = lv-tag2
   fg-rctd.job-no = lcJobNo
   fg-rctd.job-no2 = liJobNo2.

  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    ASSIGN
     fg-rctd.pur-uom  = itemfg.prod-uom
     fg-rctd.cost-uom = itemfg.prod-uom
     fg-rctd.std-cost = itemfg.std-tot-cost.

  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.cust-no EQ fg-rctd.cust-no
      NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
    ASSIGN
     fg-rctd.pur-uom  = fg-bin.pur-uom
     fg-rctd.cost-uom = fg-bin.pur-uom
     fg-rctd.std-cost = fg-bin.std-tot-cost
     fg-rctd.units-pallet = fg-bin.units-pallet
     fg-rctd.cases-unit   = fg-bin.cases-unit.

  ld = fg-rctd.std-cost.

  IF fg-rctd.pur-uom NE "EA" THEN
    RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                           ld, OUTPUT ld).

  fg-rctd.ext-cost = fg-rctd.t-qty * ld.

  /* Old 04/27/07 JLF
  FIND FIRST loadtag WHERE loadtag.company = g_company
                     AND loadtag.ITEM-type = NO
                     AND loadtag.tag-no = fg-rctd.tag NO-LOCK NO-ERROR.
  FIND FIRST fg-bin 
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ fg-rctd.i-no
       /* AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin */
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.qty > 0
      NO-LOCK NO-ERROR.
  IF adm-new-record and
     fg-rctd.cases <> /*loadtag.tot-cases*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
  THEN DO:  /* Partial transfer */
     v-tag-no = 1.
     REPEAT:
     FIND first bf-tag WHERE bf-tag.company   EQ g_company
                           AND bf-tag.item-type EQ NO
                           AND bf-tag.tag-no    EQ STRING(CAPS(fg-rctd.i-no),"x(15)") +
                                                STRING(v-tag-no,"99999") 
                           NO-LOCK NO-ERROR.

         IF NOT AVAIL bf-tag THEN LEAVE.
         v-tag-no = v-tag-no + 1.
     END. /* repeat */
     IF v-tag-no = 0 THEN v-tag-no = 1.
  
     CREATE bf-tag.
     BUFFER-COPY loadtag EXCEPT loadtag.tag-no TO bf-tag.
     ASSIGN bf-tag.tag-no       = /*string(ip-tag-no,"99999") + STRING(w-ord.i-no,"x(15)") */
                          STRING(CAPS(fg-rctd.i-no),"x(15)") + STRING(v-tag-no,"99999") 
            /*loadtag.po-no      = w-ord.po-no*/
           bf-tag.job-no       = fg-rctd.job-no
           bf-tag.job-no2      = fg-rctd.job-no2
           /*loadtag.ord-no       = fg-rctd.ord-no*/
           bf-tag.i-no         = CAPS(fg-rctd.i-no)
           bf-tag.i-name       = fg-rctd.i-name
           bf-tag.qty          = fg-rctd.t-qty
           bf-tag.qty-case     = fg-rctd.qty-case
           bf-tag.tot-case     = fg-rctd.cases
           /*bf-tag.case-bundle  = fg-rctd.cases*/
           bf-tag.pallet-count = fg-rctd.cases * fg-rctd.qty-case
           bf-tag.partial      = fg-rctd.partial /*fg-rctd.total-unit MOD fg-rctd.pcs*/
           bf-tag.sts = "Transfered"  /* task 10190414 */
           bf-tag.tag-date = TODAY
           bf-tag.tag-time = TIME
           bf-tag.loc = fg-rctd.loc2
           bf-tag.loc-bin = fg-rctd.loc-bin2.
           .
           fg-rctd.tag2 = bf-tag.tag-no.

  END.  /* partial */
  Old 04/27/07 JLF */

  /* New 04/27/07 JLF */
  IF AVAIL fg-bin                AND
     fg-bin.qty GT fg-rctd.t-qty AND
     fg-bin.tag NE ""            AND
     fg-bin.tag EQ fg-rctd.tag2  THEN 
    RUN fg/mkloadtg.p (ROWID(fg-rctd), 0, INPUT-OUTPUT fg-rctd.tag2).
  /* New 04/27/07 JLF */

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
  /*IF adm-new-record THEN*/

     ASSIGN BROWSE {&browse-name}
         fg-rctd.i-no
         fg-rctd.tag2
         fg-rctd.rct-date.
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
  DEF BUFFER bf-rctd FOR fg-rctd.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  FOR EACH bf-rctd WHERE bf-rctd.company EQ g_company NO-LOCK
      USE-INDEX fg-rctd
      BY bf-rctd.r-no DESC:
    lv-rno = bf-rctd.r-no.
    LEAVE.
  END.  

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign fg-rctd.company = g_company
         fg-rctd.r-no    = lv-rno + 1
         fg-rctd.rita-code = "T"
         fg-rctd.s-num  = 0
         fg-rctd.rct-date = today
         fg-rctd.trans-time = TIME
         fg-rctd.units-pallet = 1
         fg-rctd.cases-unit   = 1.

  disp fg-rctd.rct-date with browse {&browse-name}. 
  lv-recid = recid(fg-rctd).  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-fg-rctd FOR fg-rctd.
  DEF BUFFER bf-tt-line-cnt FOR tt-line-cnt.
  DEF VAR iCnt AS INT.
  /* Code placed here will execute PRIOR to standard behavior. */
/* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail fg-rctd then find fg-rctd where recid(fg-rctd) = lv-recid NO-LOCK no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Renumber temp-table records after a delete */
  iCnt = 0.
  FOR EACH tt-line-cnt 
    BY tt-line-cnt.line-number:

    FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ tt-line-cnt.line-rowid
       NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-fg-rctd THEN DO:
       DELETE tt-line-cnt.
    END.
    ELSE DO:
      FIND bf-tt-line-cnt WHERE ROWID(bf-tt-line-cnt) EQ ROWID(tt-line-cnt)
        NO-ERROR.
      iCnt = iCnt + 1.      
      bf-tt-line-cnt.line-number = iCnt.
    END.

  END. /* For each */
  iLineCnt = iCnt.
  run dispatch ('open-query').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR iHoldCnt AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL(fg-rctd) AND ROWID(fg-rctd) NE ? THEN DO:

    FIND FIRST tt-line-cnt WHERE tt-line-cnt.line-rowid = ROWID(fg-rctd)
        NO-LOCK NO-ERROR.
    IF AVAIL tt-line-cnt THEN DO:
      iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:       
         iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-line-cnt.line-number).
      END.
      
    END.
    ELSE DO:
      
      iHoldCnt = integer(iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name}) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:    
        iLineCnt = iLineCnt + 1.        
        iLineCnt:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(iLineCnt, "99").
        CREATE tt-line-cnt.
        ASSIGN tt-line-cnt.line-rowid = ROWID(fg-rctd)
               tt-line-cnt.line-number = iLineCnt.
      END.
    END.  
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
 /*def var out-hd-lst as cha no-undo.
  def var ii as int no-undo.
  def var hd-next as widget-handle no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure,"record-target", output out-hd-lst).
  hd-post = widget-handle(out-hd-lst).  /* procedure */
  if valid-handle(widget-handle(out-hd-lst)) then do:
     hd-post-child = hd-post:current-window.    
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
     hd-post-child = hd-post-child:first-child.  /* frame */
     hd-post-child = hd-post-child:first-child. /* field-group */
     hd-post-child = hd-post-child:first-child.  /* field */
/*   message valid-handle(hd-post-child) hd-post-child:name hd-post-child:type.
*/
     hd-post-child:sensitive = no.
  end.
*/
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 /* apply "entry" to fg-rctd.tag in browse {&browse-name}.
  return no-apply. */
  IF NOT adm-new-record THEN DO:
     FIND itemfg WHERE itemfg.company = g_company
                  AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
     IF AVAIL itemfg AND itemfg.ship-meth THEN APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
     ELSE APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.

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
  DEF VAR v-dumb AS LOG NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* when new record created from last row, get error "No fg-rctd" record ava */
  IF fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN RETURN.

  IF NOT AVAIL fg-rctd THEN
  FIND fg-rctd WHERE RECID(fg-rctd) EQ lv-recid NO-LOCK NO-ERROR.
  
  IF int(fg-rctd.cases:SCREEN-VALUE) <= 0 THEN DO:
     MESSAGE "Unit Count must be greater than 0." VIEW-AS ALERT-BOX.
     APPLY "entry" TO fg-rctd.tag.
     RETURN .
  END.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-loc-bin-tag NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN scan-next.
  v-dumb = NO NO-ERROR.

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
  DEF VAR li-cases AS INT NO-UNDO.
  DEF VAR li-partial AS INT NO-UNDO.


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
          AND fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       li-cases   = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
       li-partial = fg-bin.qty - (li-cases * fg-bin.case-count)
       fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(li-cases)
       fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.case-count)
       fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(li-partial)
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no).
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


  /*RUN custom/d-msg.w ("Warning","","Are you ready to post FG Transfer?","",2,"YES,NO", OUTPUT v-done).
  IF v-done >= 2 THEN RETURN.*/
  
  RUN fg/fgpstall.w (?, "T"). 
  
  RUN dispatch ('open-query').

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
  DEF VAR char-hdl AS cha NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
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
  DEF VAR lv-fields AS CHAR INIT "job-no,job-no2,loc,loc-bin,tag,cust-no" NO-UNDO.
  DEF VAR li-field# AS INT NO-UNDO.
  DEF VAR li-fieldc AS CHAR NO-UNDO.

  DEF BUFFER bf-tmp FOR fg-rctd.


  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-tmp WHERE bf-tmp.company = g_company AND

                             bf-tmp.rita-code = "T" AND
                             bf-tmp.tag = fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                         AND RECID(bf-tmp) <> RECID(fg-rctd) NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
        MESSAGE "This Tag Number Has Already Been Used." skip
                "Please Enter A Unique Tag Number." 
                       VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fg-rctd.tag .
        RETURN ERROR.
    END.
    FIND FIRST loadtag WHERE loadtag.company = g_company
               AND loadtag.ITEM-type = NO
               AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:
       MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
       APPLY 'entry' TO fg-rctd.tag .
       RETURN error.
    END.

    FIND FIRST fg-bin WHERE fg-bin.company = g_company
                        AND fg-bin.i-no = loadtag.i-no
                        AND fg-bin.tag = loadtag.tag-no
                        /*AND fg-bin.job-no = loadtag.job-no
                        AND fg-bin.job-no2 = loadtag.job-no2*/
                        AND fg-bin.qty > 0
                        NO-LOCK NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
       MESSAGE "Invalid inventory for the tag. " VIEW-AS ALERT-BOX ERROR.
       APPLY 'entry' TO fg-rctd.tag .
       RETURN ERROR.
    END.
    ASSIGN
     li-fieldc = TRIM(fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
     li-fieldc = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
     fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = li-fieldc

     li-field# = LOOKUP(FOCUS:NAME IN BROWSE {&browse-name},lv-fields).

    IF li-field# EQ 0 THEN li-field# = 9999.
    IF AVAIL fg-bin AND li-field# >= 1 AND li-field# <= 6 THEN DO:
      FIND FIRST itemfg WHERE itemfg.company = g_company
                          AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.
      ASSIGN fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-bin.job-no /*loadtag.job-no */
             fg-rctd.job-no2:SCREEN-VALUE = string(fg-bin.job-no2) /*loadtag.job-no2*/
             fg-rctd.i-no:SCREEN-VALUE = fg-bin.i-no /*loadtag.i-no */
             fg-rctd.i-name:SCREEN-VALUE =  itemfg.i-name         
             fg-rctd.qty-case:SCREEN-VALUE = IF AVAIL fg-bin THEN string(fg-bin.case-count)
                                            ELSE string(loadtag.qty-case)
             fg-rctd.cases:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
                                                 ELSE STRING(loadtag.tot-cases)
                    /*fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle) 
                    fg-rctd.cases-unit:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.CASEs-unit)
                                                      ELSE string(loadtag.case-bundle)*/
             fg-rctd.loc:SCREEN-VALUE = fg-bin.loc /*loadtag.loc*/
             fg-rctd.loc-bin:SCREEN-VALUE = fg-bin.loc-bin /*loadtag.loc-bin*/
             fg-rctd.cust-no:SCREEN-VALUE = fg-bin.cust-no
             fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE  
             fg-rctd.partial:SCREEN-VALUE = IF AVAIL fg-bin THEN STRING(fg-bin.partial-count)
                                           ELSE string(loadtag.partial)       
             .
    END.

    RELEASE fg-bin.
    
    /* To cover previous Transfer Posting Bugs
       if loadtag.tot-cases = 0 then get Unit Count from Fg-bin */

    IF AVAIL loadtag AND INT(fg-rctd.cases:SCREEN-VALUE) = 0 AND li-field# = 5 THEN
       FIND FIRST fg-bin WHERE fg-bin.company = cocode
                           AND fg-bin.tag = loadtag.tag-no
                           AND fg-bin.loc = loadtag.loc
                           AND fg-bin.loc-bin = loadtag.loc-bin
                           AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN  ASSIGN fg-rctd.cases:SCREEN-VALUE = 
         string(ROUND((fg-bin.qty - fg-bin.partial-count) / loadtag.qty-case,0)).

    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company  EQ cocode
          AND fg-bin.tag      EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}       OR
               li-field#      LT 2)
          AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               li-field#      LT 3)
          AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 4)
          AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               li-field#      LT 5)
          AND (fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               li-field#      LT 6)
          AND (fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}         OR
               fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "")
          AND fg-bin.qty      NE 0
        USE-INDEX tag NO-ERROR.

    /*IF li-field# EQ 5 AND AVAIL fg-bin THEN
      fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(fg-bin.partial-count).*/

    IF AVAIL fg-bin AND
       fg-bin.qty GE (DEC(fg-rctd.cases:SCREEN-VALUE IN BROWSE {&browse-name}) *
                      DEC(fg-rctd.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})) +
                     DEC(fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      ASSIGN
       fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}     = fg-bin.i-no
       fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}   = fg-bin.job-no
       fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(fg-bin.job-no2)
       fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.tag)
       fg-rctd.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.cust-no)
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = 
          IF adm-new-record THEN CAPS(fg-bin.tag) ELSE fg-rctd.tag2:SCREEN-VALUE.
      IF li-field# LE 6 THEN RUN new-bin.
    END.

    ELSE DO:
      IF AVAIL fg-bin THEN DO:
        MESSAGE "Insufficient qty in bin..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.cases IN BROWSE {&browse-name}.
      END.

      ELSE DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
        IF li-field# LE 6 THEN
          APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO fg-rctd.loc-bin IN BROWSE {&browse-name}.
      END.

      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin2 B-table-Win 
PROCEDURE valid-loc-bin2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" THEN
      IF fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN
      IF fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      EQ
         fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     AND
         fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  EQ
         fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} THEN
        lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ ""
            AND fg-bin.loc     EQ fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name}
        USE-INDEX co-ino NO-LOCK NO-ERROR.

      IF NOT AVAIL fg-bin THEN lv-msg = "Invalid entry, try help...".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.loc-bin2 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc2 B-table-Win 
PROCEDURE valid-loc2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ "" THEN
      IF fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST loc
          WHERE loc.company EQ cocode
            AND loc.loc     EQ fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loc THEN lv-msg = "Invalid entry, try help".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO fg-rctd.loc2 IN BROWSE {&browse-name}.
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
(fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag2 B-table-Win 
PROCEDURE valid-tag2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    
    FIND FIRST fg-bin
        WHERE fg-bin.company  EQ cocode
          AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
       /*   AND fg-bin.job-no   EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               li-field#      LT 2)
          AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               li-field#      LT 3)
          AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               li-field#      LT 4)
               */
          AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}       /*   OR
               li-field#      LT 5)
          AND fg-bin.qty      NE 0 */
       NO-LOCK NO-ERROR.

     IF AVAIL fg-bin THEN
     ASSIGN
       fg-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}      = CAPS(fg-bin.loc)
       fg-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name}  = CAPS(fg-bin.loc-bin)
       fg-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = CAPS(fg-bin.tag).

    ELSE DO:
        MESSAGE "Invalid Tag#, try help..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO fg-rctd.tag2 IN BROWSE {&browse-name}.

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
run get-matrix (true).
  return ext-cost.
  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

