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
DEF NEW SHARED WORKFILE item-chg FIELD i-no LIKE job-mat.i-no
                                 FIELD rec-id AS RECID.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR lv-job LIKE job.job NO-UNDO.
DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

DEF VAR lv-i-no LIKE po-ordl.i-no NO-UNDO.
DEF VAR lv-line LIKE po-ordl.line NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table rm-rctd.tag rm-rctd.loc ~
rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.s-num rm-rctd.b-num rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.s-num rm-rctd.b-num rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom 
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
&Scoped-Define ENABLED-OBJECTS RECT-5 Browser-Table ~


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */




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
      rm-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
      rm-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U
      rm-rctd.rct-date COLUMN-LABEL "Issue Date" FORMAT "99/99/9999":U
      rm-rctd.po-no FORMAT "x(6)":U
      rm-rctd.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U
      rm-rctd.job-no2 FORMAT "99":U
      rm-rctd.s-num COLUMN-LABEL "S" FORMAT ">9":U WIDTH 2.4
      rm-rctd.b-num COLUMN-LABEL "B" FORMAT ">9":U WIDTH 2.4
      rm-rctd.i-no COLUMN-LABEL "Item" FORMAT "x(10)":U
      rm-rctd.i-name COLUMN-LABEL "Name/Desc" FORMAT "x(30)":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<":U
      rm-rctd.pur-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
      rm-rctd.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<<<":U
      rm-rctd.cost-uom COLUMN-LABEL "UOM" FORMAT "x(4)":U WIDTH 7
  ENABLE
      rm-rctd.tag
      rm-rctd.loc
      rm-rctd.loc-bin
      rm-rctd.rct-date
      rm-rctd.po-no
      rm-rctd.job-no
      rm-rctd.job-no2
      rm-rctd.s-num
      rm-rctd.b-num
      rm-rctd.i-no
      rm-rctd.i-name
      rm-rctd.qty
      rm-rctd.pur-uom
      rm-rctd.cost
      rm-rctd.cost-uom
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.24
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
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

ASSIGN 
       rm-rctd.loc:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.loc-bin:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.po-no:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.i-no:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.i-name:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.pur-uom:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.cost:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE
       rm-rctd.cost-uom:COLUMN-READ-ONLY IN BROWSE Browser-Table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.rm-rctd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "rm-rctd.company = cocode and
rm-rctd.rita-code = ""I"""
     _FldNameList[1]   > asi.rm-rctd.tag
"tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > asi.rm-rctd.loc
"loc" "Whse" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[3]   > asi.rm-rctd.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[4]   > asi.rm-rctd.rct-date
"rct-date" "Issue Date" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > asi.rm-rctd.po-no
"po-no" ? "x(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[6]   > asi.rm-rctd.job-no
"job-no" "Job" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > asi.rm-rctd.job-no2
"job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > asi.rm-rctd.s-num
"s-num" "S" ? "integer" ? ? ? ? ? ? yes ? no no "2.4" yes no no "U" "" ""
     _FldNameList[9]   > asi.rm-rctd.b-num
"b-num" "B" ? "integer" ? ? ? ? ? ? yes ? no no "2.4" yes no no "U" "" ""
     _FldNameList[10]   > asi.rm-rctd.i-no
"i-no" "Item" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[11]   > asi.rm-rctd.i-name
"i-name" "Name/Desc" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[12]   > asi.rm-rctd.qty
"qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > asi.rm-rctd.pur-uom
"pur-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no yes "U" "" ""
     _FldNameList[14]   > asi.rm-rctd.cost
"cost" "Cost" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[15]   > asi.rm-rctd.cost-uom
"cost-uom" "UOM" "x(4)" "character" ? ? ? ? ? ? yes ? no no "7" yes no yes "U" "" ""
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
                RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                        rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).

            END.
            ELSE DO:
             /* company,industry,mat-type,i-code,i-no, output, output */
               run windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, output char-val,OUTPUT help-recid).
               if char-val <> "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE then do :
                  FOCUS:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ENTRY(1,char-val).
                  RUN new-i-no.
               END.
            END.  
     end.

     when "po-no" then do:
           run windows/l-poords.w (rm-rctd.company, focus:screen-value, 0, output char-val).
           if char-val <> "" then do:
              assign
               focus:screen-value in browse {&BROWSE-NAME} = entry(1,char-val)
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

     WHEN "loc"     OR
     WHEN "loc-bin" /* OR
     WHEN "tag"     */ THEN RUN rmbin-help.
     WHEN "tag" THEN DO:
         run windows/l-ldtag.w (g_company,yes,focus:screen-value,output char-val,OUTPUT HELP-recid).
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

   /*
   if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:  
      return no-apply.
   end.
   */
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
    FIND FIRST br-tmp NO-LOCK
         WHERE br-tmp.company EQ g_company
           AND br-tmp.rita-code EQ 'I'
           AND br-tmp.tag EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
           AND RECID(br-tmp) NE RECID(rm-rctd) NO-ERROR.
    IF AVAIL br-tmp THEN DO:
      MESSAGE 'This Tag Number Has Already Been Used.' skip
              'Please Enter A Unique Tag Number.' VIEW-AS ALERT-BOX ERROR.
      APPLY 'entry' TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
      RETURN NO-APPLY.
    END.
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
      rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = loadtag.i-no 
      rm-rctd.i-name:SCREEN-VALUE =  loadtag.i-name
      rm-rctd.loc:SCREEN-VALUE = loadtag.loc
      rm-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
      rm-rctd.job-no:SCREEN-VALUE = loadtag.job-no
      rm-rctd.job-no2:SCREEN-VALUE = STRING(loadtag.job-no2)
      rm-rctd.po-no:SCREEN-VALUE = STRING(loadtag.po-no).
    RUN valid-loc-bin-tag (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    FIND FIRST rm-bin NO-LOCK
         WHERE rm-bin.company EQ cocode
           AND rm-bin.i-no    EQ loadtag.i-no
           AND rm-bin.loc     EQ loadtag.loc
           AND rm-bin.loc-bin EQ loadtag.loc-bin
           AND rm-bin.tag     EQ loadtag.tag-no NO-ERROR.
    ASSIGN
      rm-rctd.qty:SCREEN-VALUE = STRING(rm-bin.qty)
      rm-rctd.cost:SCREEN-VALUE = STRING(rm-bin.cost).
    FIND FIRST item NO-LOCK
         WHERE item.company EQ cocode
           AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE
           AND item.i-code  EQ "R" NO-ERROR.
    IF AVAIL item THEN
    rm-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = item.cons-uom.
    APPLY 'ENTRY' TO rm-rctd.rct-date IN BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.rct-date IN BROWSE Browser-Table /* Issue Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY 'ENTRY' TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.job-no IN BROWSE Browser-Table /* Job */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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


&Scoped-define SELF-NAME rm-rctd.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.b-num IN BROWSE Browser-Table /* B */
DO:
  IF LASTKEY = -1 THEN RETURN.
  RUN validate-jobmat (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  APPLY 'ENTRY' TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-qty NO-ERROR.
    /*
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-loc-bin-tag (99) NO-ERROR.
    */
    IF ERROR-STATUS:ERROR THEN DO:
      APPLY "entry" TO {&self-name} IN BROWSE {&BROWSE-NAME}.
      RETURN NO-APPLY.
    END.
    APPLY 'ROW-LEAVE' TO BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.pur-uom IN BROWSE Browser-Table /* UOM */
DO:
  /*
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  RUN get-matrix (FALSE).
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost IN BROWSE Browser-Table /* Cost */
DO:
  /* run get-matrix (false). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.cost-uom IN BROWSE Browser-Table /* UOM */
DO:
  /*
    IF LASTKEY = -1 THEN RETURN .

    IF index(lv-uom-list,SELF:SCREEN-VALUE) <= 0 THEN DO:
       MESSAGE "Invalid UOM." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    run get-matrix (false).
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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
                   and job-hdr.frm     eq rm-rctd.s-num
                 no-lock:
               v-job-up = v-job-up + job-hdr.n-on.  
             end.

             find first job-mat where job-mat.company eq cocode
                                  and job-mat.job     eq job.job
                                  and job-mat.i-no    eq rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                                  and job-mat.frm     eq rm-rctd.s-num
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
  /*disp ext-cost with browse {&BROWSE-NAME}.*/
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
   rm-rctd.cost-uom = ld-cost-uom.

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
         rm-rctd.r-no = li-nxt-r-no + 1
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
  /*
  DO WITH FRAME {&FRAME-NAME}:
    {&BROWSE-NAME}:READ-ONLY = NO.
    APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
  END.
  */
  apply "entry" to rm-rctd.tag in browse {&browse-name}.
  return no-apply.

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
  /*
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

        IF char-val EQ "" THEN
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
  */

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
      FOR EACH item-chg:
        DELETE item-chg.
      END.
      count-mat = 0.

      FOR EACH job-mat
          WHERE job-mat.company    EQ cocode
            AND job-mat.job        EQ job.job 
            AND job-mat.job-no     EQ INPUT rm-rctd.job-no
            AND job-mat.job-no2    EQ INPUT rm-rctd.job-no2 
            AND (ip-for-item-only OR
                 (job-mat.frm      EQ rm-rctd.s-num AND
                  job-mat.blank-no EQ rm-rctd.b-num))
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-all B-table-Win 
PROCEDURE valid-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-loc-bin-tag (99) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  */
  RUN valid-qty NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  /*
  RUN valid-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN validate-jobmat (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  */
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
                      USE-INDEX job-no) /* OR lv-po-no NE 0 */
      THEN DO:
        /*
        IF lv-po-no NE 0 THEN
          MESSAGE "You may not enter both " +
                  TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) + " and " +
                  TRIM(rm-rctd.po-no:LABEL IN BROWSE {&BROWSE-NAME}) + "..."
              VIEW-AS ALERT-BOX ERROR.
        ELSE
        */
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
             AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE)
           USE-INDEX job-no NO-ERROR.
      IF NOT AVAILABLE job THEN
      DO:
        MESSAGE "Invalid " +
                TRIM(rm-rctd.job-no:LABEL IN BROWSE {&BROWSE-NAME}) +
                ", try help..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
      RUN jc/chk-stat.p(RECID(job), 1, OUTPUT ll).
      IF NOT ll THEN DO:
        APPLY "entry" TO rm-rctd.job-no IN BROWSE {&BROWSE-NAME}.
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
      /*
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
      */
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
        IF ip-int EQ 3 THEN
          APPLY "entry" TO rm-rctd.tag IN BROWSE {&BROWSE-NAME}.
        ELSE
        IF ip-int EQ 2 THEN
          APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&BROWSE-NAME}.
        ELSE
          APPLY "entry" TO rm-rctd.loc IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      /*
      END.
      */
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
  DEFINE VARIABLE receiptQty AS DECIMAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    receiptQty = DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
    IF receiptQty EQ 0 THEN
    DO:
      MESSAGE "Issued qty may not be 0..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
      RETURN ERROR.
    END.
    ELSE
    IF receiptQty GT 0 THEN
    DO:
      FIND FIRST rm-bin NO-LOCK
           WHERE rm-bin.company EQ cocode
             AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
             AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE
             AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE
             AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE NO-ERROR.
      IF NOT AVAILABLE rm-bin THEN
      DO:
        MESSAGE "Invalid warehouse & bin ..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
      ELSE
      IF receiptQty GT rm-bin.qty THEN
      DO:
        MESSAGE "Issued qty may not exceed QOH" rm-bin.qty "..." VIEW-AS ALERT-BOX.
        APPLY "entry" TO rm-rctd.qty IN BROWSE {&BROWSE-NAME}.
        RETURN ERROR.
      END.
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
                              (job-mat.frm = INT(rm-rctd.s-num:SCREEN-VALUE) AND
                               job-mat.blank-no = INT(rm-rctd.b-num:SCREEN-VALUE)))
                         USE-INDEX seq-idx NO-LOCK NO-ERROR.
    IF NOT AVAIL job-mat AND rm-rctd.job-no:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" THEN DO:
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
               end.  

              if item.i-code eq "R" then do:
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
                       APPLY "entry" TO rm-rctd.i-no.
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
           APPLY "entry" TO rm-rctd.i-no.
           RETURN error.  /* not update item */
       END.
    END. /* not avail job-mat */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

