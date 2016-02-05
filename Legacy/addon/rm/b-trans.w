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
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

def var ext-cost as decimal no-undo.
def var lv-recid as recid no-undo.

def var ls-prev-po as cha no-undo.
def var hd-post as widget-handle no-undo.
def var hd-post-child as widget-handle no-undo.
def var ll-help-run as log no-undo.  /* set on browse help, reset row-entry */
DEF VAR gvr-rm-row AS ROWID NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

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
rm-rctd.loc rm-rctd.loc-bin rm-rctd.rct-date rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.qty rm-rctd.loc2 rm-rctd.loc-bin2 rm-rctd.tag2 rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table rm-rctd.tag ~
rm-rctd.rct-date rm-rctd.qty rm-rctd.loc2 rm-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table rm-rctd
&Scoped-define QUERY-STRING-Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "T" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH rm-rctd WHERE ~{&KEY-PHRASE} ~
      AND rm-rctd.company = gcompany and ~
rm-rctd.rita-code = "T" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table rm-rctd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 Browser-Table browse-order ~
auto_find Btn_Clear_Find 
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
      rm-rctd.r-no COLUMN-LABEL "Seq#" FORMAT ">>>>>>>9":U
      rm-rctd.tag COLUMN-LABEL "From!Tag" FORMAT "x(20)":U WIDTH 28
      rm-rctd.loc COLUMN-LABEL "From!Whs" FORMAT "x(5)":U WIDTH 8
      rm-rctd.loc-bin COLUMN-LABEL "From!Bin" FORMAT "x(8)":U WIDTH 12
      rm-rctd.rct-date COLUMN-LABEL "Transfer!Date" FORMAT "99/99/9999":U
            WIDTH 15
      rm-rctd.i-no FORMAT "x(10)":U WIDTH 15
      rm-rctd.i-name FORMAT "x(30)":U
      rm-rctd.qty COLUMN-LABEL "Qty" FORMAT "->>>>>>9.9<<<<<":U
      rm-rctd.loc2 COLUMN-LABEL "To !Whs" FORMAT "x(13)":U
      rm-rctd.loc-bin2 COLUMN-LABEL "To!Bin" FORMAT "x(8)":U WIDTH 12
      rm-rctd.tag2 COLUMN-LABEL "To !Tag" FORMAT "x(20)":U WIDTH 28
      rm-rctd.user-id COLUMN-LABEL "User ID" FORMAT "x(8)":U WIDTH 15
  ENABLE
      rm-rctd.tag
      rm-rctd.rct-date
      rm-rctd.qty
      rm-rctd.loc2
      rm-rctd.loc-bin2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 2 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 16.71 COL 7 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 16.71 COL 94 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 16.71 COL 133 HELP
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table RECT-5 F-Main */
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
rm-rctd.rita-code = ""T"""
     _FldNameList[1]   > asi.rm-rctd.r-no
"rm-rctd.r-no" "Seq#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.rm-rctd.tag
"rm-rctd.tag" "From!Tag" "x(20)" "character" ? ? ? ? ? ? yes ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.rm-rctd.loc
"rm-rctd.loc" "From!Whs" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.rm-rctd.loc-bin
"rm-rctd.loc-bin" "From!Bin" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.rm-rctd.rct-date
"rm-rctd.rct-date" "Transfer!Date" ? "date" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.rm-rctd.i-no
"rm-rctd.i-no" ? ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = asi.rm-rctd.i-name
     _FldNameList[8]   > asi.rm-rctd.qty
"rm-rctd.qty" "Qty" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.rm-rctd.loc2
"rm-rctd.loc2" "To !Whs" "x(13)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.rm-rctd.loc-bin2
"rm-rctd.loc-bin2" "To!Bin" ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > asi.rm-rctd.tag2
"rm-rctd.tag2" "To !Tag" "x(20)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > asi.rm-rctd.user-id
"rm-rctd.user-id" "User ID" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
 def var char-val as cha no-undo.
 def var recid-val as recid no-undo.
 def var rowid-val as rowid no-undo.
 DEF BUFFER bf-tmp FOR rm-rctd.

 IF NOT avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-lock no-error. 
 
 ll-help-run = yes.

 case focus:name :
     when "i-no" then do:
           run windows/l-itmre.w (rm-rctd.company, "", "", "R", focus:screen-value in browse {&browse-name}, output char-val, output recid-val).
           if char-val <> "" then do :
              focus:screen-value in browse {&browse-name} = entry(1,char-val).
              apply "value-changed" to focus in browse {&browse-name}.
           end.   
     end.

     WHEN "loc"     THEN DO:
           RUN rmbin-help. 
           APPLY "ENTRY" TO rm-rctd.loc IN BROWSE {&browse-name}.  
     END.
     WHEN "loc-bin" THEN DO:
           RUN rmbin-help. 
           APPLY "ENTRY" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.  
     END.
     WHEN "tag"     THEN DO:
           /*
           RUN rmbin-help. 
           APPLY "ENTRY" TO rm-rctd.tag IN BROWSE {&browse-name}.  
           */
       run windows/l-ldtag6.w (gcompany,YES,focus:screen-value,output char-val,OUTPUT recid-val).
       if char-val <> "" then do :
          FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          /*  ===*/
          FIND FIRST bf-tmp WHERE bf-tmp.company = gcompany AND
                                  bf-tmp.rita-code = "T" AND
                                  bf-tmp.tag = FOCUS:SCREEN-VALUE
                              AND RECID(bf-tmp) <> RECID(rm-rctd)
                    NO-LOCK NO-ERROR.
          IF AVAIL bf-tmp THEN DO:
             MESSAGE "This Tag Number Has Already Been Used." skip
                     "Please Enter A Unique Tag Number." 
                     VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.

          FIND FIRST loadtag WHERE loadtag.company = g_company
                     AND loadtag.item-type = YES
                     AND loadtag.tag-no = focus:SCREEN-VALUE NO-LOCK NO-ERROR.
          IF NOT AVAIL loadtag THEN DO:
             MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.

          ASSIGN
            rm-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
            rm-rctd.i-name:SCREEN-VALUE =  loadtag.i-name
            rm-rctd.loc:SCREEN-VALUE = loadtag.loc
            rm-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
            rm-rctd.qty:SCREEN-VALUE = STRING(loadtag.qty)
            rm-rctd.rct-date:SCREEN-VALUE = IF rm-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY)
                                            ELSE rm-rctd.rct-date:SCREEN-VALUE.
          RUN leave-tag.
       END.
     END.
     WHEN "tag2" THEN DO:
       RUN rmbin2-help.
     END.
     when "loc2" then do:
           run rm/l-loc.w (rm-rctd.company, focus:screen-value in browse {&browse-name}, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val).             
           end.
     end.
     when "loc-bin2" then do:
           run rm/l-locbin.w (rm-rctd.company, rm-rctd.loc2:screen-value in browse {&browse-name}, output char-val).
           if char-val <> "" then do :
              assign focus:screen-value in  browse {&browse-name}  = entry(1,char-val).             
           end.
     end.

   end case.

   return no-apply.
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
  /* {src/adm/template/brsleave.i}*/
   {est/brsleave.i}  /* same as src but update will be same as add record*/

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
ON CHOOSE OF rm-rctd.tag IN BROWSE Browser-Table /* From!Tag */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN leave-tag NO-ERROR.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.tag IN BROWSE Browser-Table /* From!Tag */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN leave-tag NO-ERROR.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.tag IN BROWSE Browser-Table /* From!Tag */
DO:
  RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc IN BROWSE Browser-Table /* From!Whs */
DO:
  IF LASTKEY NE -1 THEN DO:

     IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 rm-rctd.loc-bin:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
     END.

     RUN valid-loc-bin-tag NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.loc IN BROWSE Browser-Table /* From!Whs */
DO:
  RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin IN BROWSE Browser-Table /* From!Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.loc-bin IN BROWSE Browser-Table /* From!Bin */
DO:
  RUN new-bin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN BROWSE Browser-Table /* Item No */
DO:
  FIND item
      WHERE item.company EQ cocode
        AND item.i-no    EQ {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN
    ASSIGN
     {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name}   = item.i-no
     rm-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = item.i-name.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF rm-rctd.i-name IN BROWSE Browser-Table /* Name */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.qty IN BROWSE Browser-Table /* Qty */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc2 IN BROWSE Browser-Table /* To !Whs */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF LENGTH(SELF:SCREEN-VALUE) > 5 THEN DO:
          DEF VAR v-locbin AS cha NO-UNDO.
          v-locbin = SELF:SCREEN-VALUE.
          ASSIGN rm-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} = SUBSTRING(v-locbin,1,5)
                 rm-rctd.loc-bin2:SCREEN-VALUE = SUBSTRING(v-locbin,6,8).
    END.
    RUN valid-loc2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF rm-rctd.loc-bin2 IN BROWSE Browser-Table /* To!Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-tag B-table-Win 
PROCEDURE leave-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DEF VAR vtag-no LIKE loadtag.tag-no.
    vtag-no = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    FIND FIRST loadtag NO-LOCK WHERE loadtag.company EQ g_company
                                 AND loadtag.item-type EQ YES
                                 AND loadtag.tag-no EQ vtag-no NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:

      FIND FIRST loadtag WHERE
           loadtag.company EQ g_company AND
           loadtag.item-type EQ YES AND
           loadtag.misc-char[1] EQ vtag-no
         NO-LOCK NO-ERROR.

      IF AVAIL loadtag THEN
         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = loadtag.tag-no.
    END.
    RUN valid-loc-bin-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    APPLY "entry" TO rm-rctd.loc2 IN BROWSE {&browse-name}.
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

  DEF VAR lv-tag2 LIKE rm-rctd.tag2 NO-UNDO.
  DEFINE VARIABLE lv-i-no AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-i-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-loc AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-loc-bin AS CHARACTER NO-UNDO.
  DEF VAR lv-tag LIKE rm-rctd.tag NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      lv-tag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
      lv-tag2 = rm-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}
      lv-i-no = rm-rctd.i-no:SCREEN-VALUE
      lv-i-name = rm-rctd.i-name:SCREEN-VALUE
      lv-loc = rm-rctd.loc:SCREEN-VALUE
      lv-loc-bin = rm-rctd.loc-bin:SCREEN-VALUE.     
      

  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    rm-rctd.tag2 = lv-tag2
    rm-rctd.i-no = lv-i-no
    rm-rctd.i-name = lv-i-name
    rm-rctd.loc = lv-loc
    rm-rctd.loc-bin = lv-loc-bin.
      
  FIND FIRST ITEM WHERE
    ITEM.company = cocode AND
    ITEM.i-no = lv-i-no
    NO-LOCK NO-ERROR.

  IF AVAIL ITEM THEN DO:
    rm-rctd.pur-uom = ITEM.cons-uom.
    RELEASE ITEM.
  END.  

  FIND FIRST rm-rdtlh WHERE
    rm-rdtlh.company = cocode AND
    rm-rdtlh.tag = lv-tag AND
    rm-rdtlh.rita-code = "R"
    USE-INDEX tag
    NO-LOCK NO-ERROR.

  IF AVAIL rm-rdtlh THEN DO:

    FIND FIRST rm-rcpth OF rm-rdtlh NO-LOCK NO-ERROR.

    IF AVAILABLE rm-rcpth THEN DO:

      ASSIGN rm-rctd.po-no = rm-rcpth.po-no
             rm-rctd.job-no = rm-rcpth.job-no
             rm-rctd.job-no2 = rm-rcpth.job-no2.
      RELEASE rm-rcpth.
    END.
            
    RELEASE rm-rdtlh.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-rno LIKE rm-rctd.r-no NO-UNDO.
 DEF BUFFER b-rm-rctd FOR rm-rctd.

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rno = 0.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN sys/ref/asiseq.p (INPUT gcompany, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


  assign rm-rctd.company   = gcompany
         rm-rctd.r-no      = lv-rno
         rm-rctd.rita-code = "T"
         rm-rctd.s-num     = 0
         rm-rctd.rct-date  = today
         rm-rctd.user-id  = USERID("nosweat")
         rm-rctd.upd-date = TODAY
         rm-rctd.upd-time = TIME.

  disp rm-rctd.rct-date with browse {&browse-name}. 
  lv-recid = recid(rm-rctd).
 
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

  /*  progress bug - no rfqitem record available 
      if add is canceled when new line is appended to last line */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* when new record created from last row, get error "No rm-rctd" record ava */
  if not avail rm-rctd then find rm-rctd where recid(rm-rctd) = lv-recid no-error.
  
  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin-tag NO-ERROR.

  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin2 NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN repo-query (ROWID(rm-rctd)).
  gvr-rm-row = ?.
  IF AVAIL(rm-rctd) THEN
     gvr-rm-row = ROWID(rm-rctd).
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
    FIND FIRST rm-bin 
        WHERE rm-bin.company EQ cocode
          AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
          AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL rm-bin THEN DO:
      IF rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
        rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(rm-bin.qty).
    END.
  END.

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
    RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-rowid).

    FOR EACH tt-selected WHERE tt-rowid EQ lv-rowid,
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

      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rmbin2-help B-table-Win 
PROCEDURE rmbin2-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-rowid AS ROWID NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}, rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT lv-rowid).

    FIND rm-bin WHERE ROWID(rm-bin) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL rm-bin AND (rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}         NE rm-bin.loc     OR
                         rm-rctd.loc-bin:SCREEN-VALUE IN browse {&browse-name}     NE rm-bin.loc-bin OR
                         rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}         NE rm-bin.tag)
    THEN DO:
      ASSIGN
       rm-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.loc
       rm-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} = rm-bin.loc-bin
       rm-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name}     = rm-bin.tag.

      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scan-next B-table-Win 
PROCEDURE scan-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bfRm-rctd FOR rm-rctd.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
   IF gvr-rm-row NE ? THEN DO:
       FIND bfRm-rctd WHERE ROWID(bfRm-rctd) EQ gvr-rm-row NO-LOCK NO-ERROR.
       IF AVAIL bfRm-rctd THEN
         RUN repo-query (ROWID(bfRm-rctd)).
   END.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL item THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
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
  DEF VAR lv-fields AS CHAR INIT "loc,loc-bin,tag" NO-UNDO.
  DEF VAR li-field# AS INT NO-UNDO.
  DEF BUFFER bf-tmp FOR rm-rctd.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-tmp WHERE bf-tmp.company = gcompany AND
                            bf-tmp.rita-code = "T" AND
                            bf-tmp.tag = rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}
                        AND RECID(bf-tmp) <> RECID(rm-rctd) NO-LOCK NO-ERROR.
    IF AVAIL bf-tmp THEN DO:
       MESSAGE "This Tag Number Has Already Been Used." skip
               "Please Enter A Unique Tag Number." 
                      VIEW-AS ALERT-BOX ERROR.
       APPLY 'entry' TO rm-rctd.tag.
       RETURN ERROR.
    END.

    FIND FIRST loadtag WHERE loadtag.company = g_company
                   AND loadtag.ITEM-type = YES
                   AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAIL loadtag THEN DO:
           MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
           APPLY 'entry' TO rm-rctd.tag.
           RETURN ERROR.
    END.
    ASSIGN rm-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
           rm-rctd.i-name:SCREEN-VALUE = loadtag.i-name              
           rm-rctd.loc:SCREEN-VALUE = loadtag.loc
           rm-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
           rm-rctd.qty:SCREEN-VALUE = STRING(loadtag.qty)
           rm-rctd.rct-date:SCREEN-VALUE = IF rm-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE rm-rctd.rct-date:SCREEN-VALUE  .
    RUN new-bin.
    li-field# = LOOKUP(FOCUS:NAME IN BROWSE {&browse-name},lv-fields).

    IF li-field# EQ 0 THEN li-field# = 9999.

    FIND FIRST rm-bin
        WHERE rm-bin.company  EQ cocode
          AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND (rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               (li-field#     LT 1 AND
                rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""))
          AND (rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}      OR
               (li-field#     LT 2 AND
                rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""))
          AND (rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name}          OR
               (li-field#     LT 3 AND
                rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} EQ ""))
        USE-INDEX loc-bin NO-LOCK NO-ERROR.

    IF AVAIL rm-bin AND
       (rm-bin.qty GE DEC(rm-rctd.qty:SCREEN-VALUE IN BROWSE {&browse-name}) OR
        li-field# LT 3) THEN
      ASSIGN
       rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(rm-bin.loc)
       rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(rm-bin.loc-bin)
       rm-rctd.tag:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(rm-bin.tag)
       rm-rctd.tag2:SCREEN-VALUE IN BROWSE {&browse-name} = CAPS(rm-bin.tag).

    ELSE DO:
      IF AVAIL rm-bin THEN DO:
        MESSAGE "Insufficient qty in bin..." VIEW-AS ALERT-BOX.
        IF li-field# LE 3 THEN
          APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO rm-rctd.qty IN BROWSE {&browse-name}.
      END.

      ELSE DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
        IF li-field# LE 3 THEN
          APPLY "entry" TO FOCUS IN BROWSE {&browse-name}.
        ELSE
          APPLY "entry" TO rm-rctd.loc-bin IN BROWSE {&browse-name}.
      END.

      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin-tag-old B-table-Win 
PROCEDURE valid-loc-bin-tag-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF /* rm-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE "" AND */
       NOT CAN-FIND(FIRST rm-bin 
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
      IF rm-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Bin may not be spaces".

    IF lv-msg EQ "" THEN
      IF rm-rctd.loc:SCREEN-VALUE IN BROWSE {&browse-name}      EQ
         rm-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}     AND
         rm-rctd.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}  EQ
         rm-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name} THEN
        lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST rm-bin
          WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ ""
            AND rm-bin.loc     EQ rm-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin2:SCREEN-VALUE IN BROWSE {&browse-name}
        USE-INDEX loc-bin NO-LOCK NO-ERROR.

      IF NOT AVAIL rm-bin THEN lv-msg = "Invalid entry, try help...".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.loc-bin2 IN BROWSE {&browse-name}.
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
      IF rm-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
        lv-msg = "To Warehouse may not be spaces".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST loc
          WHERE loc.company EQ cocode
            AND loc.loc     EQ rm-rctd.loc2:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loc THEN lv-msg = "Invalid entry, try help".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO rm-rctd.loc2 IN BROWSE {&browse-name}.
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
  /* 
  RETURN 0.00.   /* Function return value. */
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

