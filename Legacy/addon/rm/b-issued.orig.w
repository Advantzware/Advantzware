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

{sys/inc/var.i NEW SHARED}

def var char-val as cha no-undo.
def var ext-cost as decimal no-undo.

def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */

DEF VAR lv-po-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR lv-po-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-avgcost AS LOG NO-UNDO.
DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
DEF VAR lv-rmissue AS CHAR NO-UNDO.

DEF BUFFER xitem FOR ITEM.
DEF NEW SHARED WORKFILE item-chg FIELD i-no LIKE job-mat.i-no
                                 FIELD rec-id AS RECID.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR lv-job-no LIKE job.job NO-UNDO.
DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.tag rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.rct-date rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num ~
rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty rm-rctd.pur-uom ~
rm-rctd.cost rm-rctd.cost-uom calc-ext-cost() @ ext-cost ~
display-dimension('W') @ lv-po-wid display-dimension('L') @ lv-po-len 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.job-no rm-rctd.job-no2 rm-rctd.rct-date rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty ~
rm-rctd.pur-uom 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "I" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table browse-order auto_find ~
Btn_Clear_Find RECT-4 RECT-5 
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

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
      rm-rctd.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U
      rm-rctd.job-no FORMAT "x(6)":U
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.rct-date FORMAT "99/99/9999":U
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<":U
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      calc-ext-cost() @ ext-cost COLUMN-LABEL "Ext.Amount" FORMAT "->>>,>>9.99<<":U
            COLUMN-BGCOLOR 14
      display-dimension('W') @ lv-po-wid COLUMN-LABEL "Width"
      display-dimension('L') @ lv-po-len COLUMN-LABEL "Length"
  ENABLE
      rm-rctd.tag
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.rct-date
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.loc
      rm-rctd.loc-bin
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
     auto_find AT ROW 16.71 COL 79 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 133 HELP
          "CLEAR AUTO FIND Value"
     RECT-4 AT ROW 16.48 COL 2
     RECT-5 AT ROW 1 COL 1
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 16.71 COL 3
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
         HEIGHT             = 18.48
         WIDTH              = 154.4.
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
     _Where[1]         = "rm-rctd.company = gcompany and
rm-rctd.rita-code = ""I"""
     _FldNameList[1]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > asi.rm-rctd.job-no
"job-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > asi.rm-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > asi.rm-rctd.rct-date
"rct-date" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > asi.rm-rctd.i-no
"i-no" "Item" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > asi.rm-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > asi.rm-rctd.s-num
"s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > asi.rm-rctd.b-num
"b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > asi.rm-rctd.pur-uom
"pur-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[13]   > asi.rm-rctd.cost
"cost" "Cost" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > asi.rm-rctd.cost-uom
"cost-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[15]   > "_<CALC>"
"calc-ext-cost() @ ext-cost" "Ext.Amount" "->>>,>>9.99<<" ? 14 ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > "_<CALC>"
"display-dimension('W') @ lv-po-wid" "Width" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[17]   > "_<CALC>"
"display-dimension('L') @ lv-po-len" "Length" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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


 ll-help-run = yes.
 
 case focus:NAME:
     when "i-no" then do:
            RUN rm/g-joblk.w (OUTPUT lv-search).  /* search job or item */
            IF lv-search = "job" THEN DO:
                RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name},
                                        rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
                
            END.
            ELSE DO:
             /* company,industry,mat-type,i-code,i-no, output, output */
               run windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, output char-val,OUTPUT help-recid).
               if char-val <> "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE then do :
                  FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
                  RUN new-i-no.
               END.
            END.
            help-recid = ?.   
     end.

     when "job-no" or when "job-no2" then do:
           run windows/l-jobno.w (rm-rctd.company,FOCUS:SCREEN-VALUE, output char-val, OUTPUT help-recid).
           if char-val <> "" then do :
              assign /*focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     */ 
                     rm-rctd.job-no:screen-value = entry(1,char-val)
                     rm-rctd.job-no2:screen-value = entry(2,char-val)
                     .
             
           end.
           help-recid = ?.  
     END.

     WHEN "loc"     THEN RUN rmbin-help.   
     WHEN "loc-bin" THEN RUN rmbin-help.
     WHEN "tag"     THEN RUN rmbin-help.

   END CASE.

   return no-apply. 

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
  IF LASTKEY NE -1 THEN DO:
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


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job # */
DO:
    IF LASTKEY = -1 OR rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN RETURN.

    rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        FILL(" ",6 - LENGTH(TRIM(SELF:SCREEN-VALUE))) + TRIM(SELF:SCREEN-VALUE).

    FIND FIRST job WHERE job.company = cocode
                     AND job.job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     AND job.job-no2 = int(rm-rctd.job-no2:SCREEN-VALUE)
                     USE-INDEX job-no NO-LOCK NO-ERROR.
    IF NOT AVAIL job THEN DO:
       MESSAGE "Invalid Job#. Try Help. "
               rm-rctd.job-no:SCREEN-VALUE int(rm-rctd.job-no2:SCREEN-VALUE)
                VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    lv-job-no = job.job.
    FIND FIRST job-hdr WHERE job-hdr.company = cocode
                         AND job-hdr.job = job.job
                         AND job-hdr.job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     AND job.job-no2 = int(rm-rctd.job-no2:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN DO:
       FIND est WHERE est.company = cocode
                  AND est.est-no = job-hdr.est-no NO-LOCK NO-ERROR.
       IF AVAIL est THEN DO: 
           IF (est.est-type = 1 OR est.est-type = 5) THEN 
              ASSIGN rm-rctd.s-num:SCREEN-VALUE = "1" /*string(job-hdr.frm)      */
                     rm-rctd.b-num:SCREEN-VALUE = "1" /* string(job-hdr.blank-no)         */
                     rm-rctd.s-num:READ-ONLY = YES
                     rm-rctd.b-num:READ-ONLY = YES.


       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no2 IN BROWSE Browser-Table
DO:
   IF LASTKEY = -1 OR rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN RETURN.

    FIND FIRST job WHERE job.company = cocode
                     AND job.job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                     AND job.job-no2 = int(rm-rctd.job-no2:SCREEN-VALUE)
                     USE-INDEX job-no NO-LOCK NO-ERROR.
    IF NOT AVAIL job THEN DO:
       MESSAGE "Invalid Job#. Try Help. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
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
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
    IF LASTKEY = -1 THEN RETURN.
    RUN validate-jobmat NO-ERROR.
    if error-status:error then return no-apply.


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

    

    
    FIND FIRST job-mat WHERE job-mat.company = gcompany
                       /* and job-mat.job = job.job */      
                         AND job-mat.job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
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
                where job-hdr.company eq gcompany
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
                where job-mat.company   eq gcompany
                  /*and job-mat.job       eq job.job */
                  and job-mat.job-no    eq INPUT rm-rctd.job-no
                  and job-mat.job-no2   eq INPUT rm-rctd.job-no2
                  and job-mat.frm       eq INt(rm-rctd.s-num:SCREEN-VALUE)
                  and (job-mat.blank-no eq int(rm-rctd.b-num:SCREEN-VALUE) /* or
                       v-frst-fld ne "s-num"  */ )
                  use-index seq-idx no-lock,

                first xitem
                where xitem.company  eq gcompany
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
    RUN valid-loc-bin-tag (1) NO-ERROR.
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
    RUN valid-loc-bin-tag (2) NO-ERROR.
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


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
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
{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

{sys/inc/rmissue.i}
lv-rmissue = v-rmissue.

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
            ITEM.i-no = rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
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

  FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.i-code  EQ "R" 
        NO-LOCK NO-ERROR.
  IF AVAIL item THEN 
      ASSIGN
       rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}   = item.i-name
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = item.cons-uom
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = item.cons-uom.


  FIND job-mat WHERE RECID(job-mat) = ip-recid NO-LOCK NO-ERROR.
  IF AVAIL job-mat THEN 
     ASSIGN rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-mat.i-no
            rm-rctd.s-num:SCREEN-VALUE = string(job-mat.frm)
            rm-rctd.b-num:SCREEN-VALUE = string(job-mat.blank-no)
            .
  
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
  

 

if ip-first-disp  and avail rm-rctd and rm-rctd.i-no <> "" then do: /* for row-display */
  find item  where item.company eq cocode                           /* no screen-value used */
                     and item.i-no  eq rm-rctd.i-no /*:screen-value in browse {&browse-name}*/
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
                        rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                        lv-qty-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.qty,
                        output lv-out-qty).
  
  /* convert cost pr-uom*/
  run custom/convcuom.p(cocode,
                        rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name},
                        lv-cost-uom,
                        v-bwt,
                        v-len,
                        v-wid,
                        v-dep,
                        rm-rctd.cost, output lv-out-cost).

   ext-cost = lv-out-qty * lv-out-cost.
  
  /*disp ext-cost with browse {&browse-name}. it's displayed automatically */
 /* message "after calc:" po-ordl.cons-uom rm-rctd.cost-uom lv-out-cost ext-cost.
  */

end. /* ip-first */
/* ======================================================================= */
else if avail rm-rctd and rm-rctd.i-no:SCREEN-VALUE <> "" then do: /* in update mode - use screen-value */
  find item  where item.company eq cocode
                and item.i-no  eq rm-rctd.i-no:screen-value in browse {&browse-name}
                      use-index i-no no-lock no-error.
  if avail item then v-dep = item.s-dep.    
  
 /*
  if not avail po-ordl then return.
 */   
   do:
        find first job where job.company eq cocode
                         and job.job-no  eq rm-rctd.job-no:screen-value
                         and job.job-no2 eq integer(rm-rctd.job-no2:screen-value)
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
                                  and job-mat.i-no    eq rm-rctd.i-no:screen-value
                                  and job-mat.frm     eq int(rm-rctd.s-num:screen-value)
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
  end.
  
  /* convert qty */
  IF rm-rctd.pur-uom:screen-value in browse {&browse-name} EQ lv-qty-uom THEN
    lv-out-qty = dec(rm-rctd.qty:screen-value in browse {&browse-name}).
  ELSE
    run rm/convquom.p(rm-rctd.pur-uom:screen-value in browse {&browse-name},
                      lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                      dec(rm-rctd.qty:screen-value in browse {&browse-name}) / v-out,
                      output lv-out-qty).
  
  /* convert cost */
  IF rm-rctd.cost-uom:screen-value in browse {&browse-name} EQ lv-cost-uom THEN
    lv-out-cost = dec(rm-rctd.cost:screen-value in browse {&browse-name}).
  ELSE
    run rm/convcuom.p(rm-rctd.cost-uom:screen-value in browse {&browse-name},
                      lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                      rm-rctd.cost:screen-value in browse {&browse-name},
                      output lv-out-cost).

  ext-cost = lv-out-qty * lv-out-cost.
  ASSIGN rm-rctd.cost:SCREEN-VALUE = STRING(lv-out-cost)
         rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
         rm-rctd.qty:SCREEN-VALUE = STRING(lv-out-qty)
         rm-rctd.pur-uom:SCREEN-VALUE = lv-qty-uom.
  disp ext-cost with browse {&browse-name}.
/*
  MESSAGE "matrix: " lv-qty-uom "," lv-cost-uom skip
              lv-out-qty lv-out-cost VIEW-AS ALERT-BOX. 

*/
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
  ASSIGN ld-std-cost = dec(rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name} )
         ld-cost-uom = rm-rctd.cost-uom:SCREEN-VALUE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN rm-rctd.cost = ld-std-cost
         rm-rctd.cost-uom = ld-cost-uom.

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
 FOR EACH bf-rctd NO-LOCK BY bf-rctd.r-no DESCENDING:
     li-nxt-r-no = bf-rctd.r-no.
     LEAVE.
 END.
 FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
 IF AVAIL rm-rcpth AND rm-rcpth.r-no GT li-nxt-r-no THEN li-nxt-r-no = rm-rcpth.r-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign rm-rctd.company = cocode
         rm-rctd.loc = locode
         rm-rctd.r-no    = li-nxt-r-no + 1
         rm-rctd.rita-code = "I"
         rm-rctd.s-num  = 1
         rm-rctd.b-num = 0
         rm-rctd.rct-date = TODAY.
  
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

    APPLY "entry" TO rm-rctd.tag IN BROWSE {&browse-name}.

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

  RUN multi-issues (ROWID(rm-rctd)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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

        {&browse-name}:SELECT-FOCUSED-ROW().

        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag
         rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}     = ""
         rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name}    = "".

        RUN new-bin.

        RUN get-matrix (TRUE).

        RUN valid-all NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN dispatch ('assign-record').

        RUN dispatch ('open-query').

        RUN dispatch ('end-update').

        REPOSITION {&browse-name} TO ROWID ip-rowid.

        {&browse-name}:SELECT-FOCUSED-ROW().
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

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST rm-bin 
        WHERE rm-bin.company EQ cocode
          AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL rm-bin THEN DO:
      rm-rctd.cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(rm-bin.cost).
      IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
        rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(rm-bin.qty).
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

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.i-code  EQ "R"
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN DO:
      ASSIGN
       rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}   = item.i-name
       rm-rctd.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}  = item.cons-uom
       rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = item.cons-uom.

      FOR EACH rm-bin 
          WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK
          BY rm-bin.qty DESC BY rm-bin.loc BY rm-bin.loc-bin BY rm-bin.tag:
        ASSIGN
         rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag.

        RUN new-bin.

        LEAVE.
      END.

      FIND FIRST job-mat
          WHERE job-mat.company EQ cocode
            AND job-mat.job-no  EQ rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job-mat.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
            AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE  IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF AVAIL job-mat THEN
        ASSIGN
         rm-rctd.s-num:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-mat.frm)
         rm-rctd.b-num:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-mat.blank-no).
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
    RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc-bin:screen-value in browse {&browse-name}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-rowid).

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
            and xrm-rctd.tag     begins string(int(rm-rctd.po-no:screen-value in browse {&browse-name}),"999999")
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
         /* rm-rctd.tag:screen-value in browse {&browse-name}
          = string(int(rm-rctd.po-no:screen-value in browse {&browse-name}),"999999") + string(v-tag-seq,"99").
           */
        .        
*/
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

  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN validate-jobmat NO-ERROR.
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

    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND item.i-code  EQ "R"
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

    ELSE
    IF rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
       item.mat-type NE "P"                                       THEN
      v-msg =  "If Job# is blank then RM Type must be 'P'aper".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO rm-rctd.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    LEAVE.
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
    IF DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "Issued qty may not be 0..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.qty IN BROWSE {&browse-name}.
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


    FIND FIRST item 
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
/*
IF (rm-rctd.b-num:MODIFIED IN BROWSE {&browse-name}  OR int(rm-rctd.b-num:SCREEN-VALUE) = 0 )
   OR rm-rctd.s-num:MODIFIED IN BROWSE {&browse-name}
   OR rm-rctd.i-no:MODIFIED
THEN DO:
*/
    FIND FIRST job-mat WHERE job-mat.company = cocode
                       /* and job-mat.job = job.job */      
                         AND job-mat.job-no = rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
                         AND job-mat.job-no2 = INT(rm-rctd.job-no2:SCREEN-VALUE)
                         AND job-mat.i-no = rm-rctd.i-no:SCREEN-VALUE
                         AND job-mat.frm = int(rm-rctd.s-num:SCREEN-VALUE) 
                         AND job-mat.blank-no = INT(rm-rctd.b-num:SCREEN-VALUE)
                         USE-INDEX seq-idx NO-LOCK NO-ERROR.
    IF NOT AVAIL job-mat AND rm-rctd.job-no:SCREEN-VALUE <> "" THEN DO:
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
          
            for each item-chg:
              delete item-chg.
            end.
            count-mat = 0.

            for each job-mat
                where job-mat.company   eq cocode
                  and job-mat.job       eq job.job 
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
                
                    if not choice then do: release job-mat.
                       APPLY "entry" TO rm-rctd.i-no.
                       RETURN error.
                    END.
                 END.
    
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
       ELSE do: 
           APPLY "entry" TO rm-rctd.i-no.
           RETURN error.  /* not update item */
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
  
  run get-matrix (true).
  return ext-cost.
  
  
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

  IF AVAIL rm-rctd AND rm-rctd.po-no <> "" THEN DO:

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

