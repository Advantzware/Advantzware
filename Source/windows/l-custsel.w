&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\l-custsel.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo.
DEF OUTPUT PARAMETER op-mode AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
&scoped-define SORTBY-1 BY cust.cust-no
&scoped-define SORTBY-2 BY cust.name {&sortby-1}
&SCOPED-DEFINE SORTBY-3 BY sman.sman
&SCOPED-DEFINE SORTBY-4 BY sman.sname {&sortby-3}
&scoped-define fld-name-1 cust.cust-no
&scoped-define fld-name-2 cust.name
&SCOPED-DEFINE fld-name-3 sman.sman
&SCOPED-DEFINE fld-name-4 sman.sname
&global-define IAMWHAT LOOKUP

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}
{sys/inc/selcust.i}

DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cust sman

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 cust.cust-no cust.name sman.sman ~
sman.sname cust.addr[1] cust.addr[2] cust.city cust.state cust.zip 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH cust WHERE ~{&KEY-PHRASE} ~
      AND cust.company = ip-company NO-LOCK, ~
      FIRST sman OF cust OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH cust WHERE ~{&KEY-PHRASE} ~
      AND cust.company = ip-company NO-LOCK, ~
      FIRST sman OF cust OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 cust sman
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 cust
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 sman


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 rd-sort tg-select-all bt-selcust ~
bt-clear lv-search bt-ok bt-cancel RECT-1 
&Scoped-Define DISPLAYED-OBJECTS rd-sort tg-select-all lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 11 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE BUTTON bt-selcust 
     LABEL "Select Customers for Creation" 
     SIZE 31 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 96 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cust #", 1,
"Cust Name", 2,
"SalesRep ID", 3,
"SalesRep Name", 4
     SIZE 66 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 1.43.

DEFINE VARIABLE tg-select-all AS LOGICAL INITIAL no 
     LABEL "Select All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      cust, 
      sman SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      cust.cust-no FORMAT "x(8)":U COLUMN-FONT 0
      cust.name FORMAT "x(30)":U COLUMN-FONT 0
      sman.sman COLUMN-LABEL "SalesRep" FORMAT "x(3)":U WIDTH 11.6
      sman.sname COLUMN-LABEL "SalesRep Name" FORMAT "x(20)":U
            WIDTH 30
      cust.addr[1] FORMAT "x(30)":U COLUMN-FONT 0
      cust.addr[2] FORMAT "x(30)":U COLUMN-FONT 0
      cust.city FORMAT "x(15)":U COLUMN-FONT 0
      cust.state FORMAT "x(2)":U COLUMN-FONT 0
      cust.zip FORMAT "x(10)":U COLUMN-FONT 0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 140 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.71 COL 14 NO-LABEL
     tg-select-all AT ROW 12.81 COL 80.6 WIDGET-ID 4
     bt-selcust AT ROW 12.62 COL 108.8 WIDGET-ID 2
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 119
     bt-cancel AT ROW 14.1 COL 130
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.81 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(1.39) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Customers".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.cust,ASI.sman OF ASI.cust"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[1]         = "ASI.cust.company = ip-company"
     _FldNameList[1]   > ASI.cust.cust-no
"cust.cust-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.cust.name
"cust.name" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.sman.sman
"sman.sman" "SalesRep" ? "character" ? ? ? ? ? ? no ? no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.sman.sname
"sman.sname" "SalesRep Name" ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.cust.addr[1]
"cust.addr[1]" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.cust.addr[2]
"cust.addr[2]" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.cust.city
"cust.city" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.cust.state
"cust.state" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.cust.zip
"cust.zip" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select Customers */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   if lv-first-time then assign lv-search:screen-value = ""
                                lv-first-time = no.
                                
   lv-search:screen-value = lv-search:screen-value + keylabel(lastkey).
   apply "leave" to lv-search.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    assign lv-search:screen-value = "".
           lv-search = "".
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}

        WHEN 3 THEN
           OPEN QUERY BROWSE-1 FOR EACH cust WHERE
                cust.company = ip-company NO-LOCK,
                FIRST sman OF cust OUTER-JOIN NO-LOCK BY sman.sman
                BY cust.cust-no.
        WHEN 4 THEN
           OPEN QUERY BROWSE-1 FOR EACH cust WHERE
                cust.company = ip-company NO-LOCK,
                FIRST sman OF cust OUTER-JOIN NO-LOCK BY sman.sname
                BY cust.cust-no.
    end.
        apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   DEF VAR li AS INT NO-UNDO.
   DEF VAR v-count AS INT NO-UNDO.
   ASSIGN 
     op-char-val = cust.cust-no:screen-value in browse {&browse-name}
     op-mode = "SELECT".
   DO WITH FRAME {&frame-name}:
     IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN
         DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
           {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
           IF AVAIL cust THEN
           DO:
              CREATE tt-cust.
        
              ASSIGN
                tt-cust.company = ip-company
                tt-cust.cust-no = cust.cust-no.
        
              RELEASE tt-cust.
              v-count = v-count + 1 .
           END.
         END.
   END. /* frame*/

   IF v-count > 1 THEN DO:
       op-mode = "ADD".
   END.

 apply "window-close" to frame {&frame-name}. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-selcust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-selcust Dialog-Frame
ON CHOOSE OF bt-selcust IN FRAME Dialog-Frame /* Select Customers for Creation */
DO:
   DEF VAR li AS INT NO-UNDO.
   DEF BUFFER b-tt-cust FOR tt-cust.

   DO WITH FRAME {&frame-name}:
    
      ASSIGN tg-select-all.

      IF tg-select-all THEN
         op-mode = "ADD ALL".
      ELSE
      DO:
         op-mode = "ADD".

         IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN
         DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
           {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
           IF AVAIL cust THEN
           DO:
              CREATE tt-cust.
        
              ASSIGN
                tt-cust.company = ip-company
                tt-cust.cust-no = cust.cust-no.
        
              RELEASE tt-cust.
           END.
         END.
      END.
   END.

   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
or return of lv-search
DO:
    assign rd-sort 
           lv-search.
    &scoped-define IAMWHAT Search
    &scoped-define fld-name-1 cust.cust-no
    &scoped-define fld-name-2 cust.name
    &scoped-define where-statement begins lv-search
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}

        WHEN 3 THEN DO:
           OPEN QUERY BROWSE-1 FOR EACH cust WHERE
                cust.company = ip-company NO-LOCK,
                FIRST sman OF cust WHERE sman.sman BEGINS lv-search
                NO-LOCK BY sman.sman BY cust.cust-no.

           IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
           DO:
              MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
                 VIEW-AS ALERT-BOX.
              APPLY "ENTRY" TO {&BROWSE-NAME}.
           end.
        END.

        WHEN 4 THEN DO:
           OPEN QUERY BROWSE-1 FOR EACH cust WHERE
                cust.company = ip-company NO-LOCK,
                FIRST sman OF cust WHERE sman.sname BEGINS lv-search
                NO-LOCK BY sman.sname BY cust.cust-no.

           IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
           DO:
              MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
                 VIEW-AS ALERT-BOX.
              APPLY "ENTRY" TO {&BROWSE-NAME}.
           end.
        END.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    lv-search = "".
    lv-search:SCREEN-VALUE = "".
    ASSIGN rd-sort.
    RUN new-rd-sort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-select-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-select-all Dialog-Frame
ON VALUE-CHANGED OF tg-select-all IN FRAME Dialog-Frame /* Select All Customers */
DO:
   DO WITH FRAME {&FRAME-NAME}:

     ASSIGN tg-select-all.

     IF tg-select-all THEN
        {&browse-name}:SELECT-ALL() NO-ERROR.
     ELSE
        {&browse-name}:DESELECT-ROWS() NO-ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = "".
    ASSIGN rd-sort NO-ERROR.

    RUN enable_UI.

    RUN new-rd-sort.

    {custom/lookpos3.i &lookup-file = "cust" &lookup-field = "cust-no"}
  END.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY rd-sort tg-select-all lv-search 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 rd-sort tg-select-all bt-selcust bt-clear lv-search bt-ok 
         bt-cancel RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rd-sort Dialog-Frame 
PROCEDURE new-rd-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* redefined for lookup */
  &scoped-define IAMWHAT LOOKUP   
         
  DO WITH FRAME {&FRAME-NAME}: 
    assign rd-sort.
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        
        WHEN 3 THEN
           OPEN QUERY BROWSE-1 FOR EACH cust WHERE
                cust.company = ip-company NO-LOCK,
                FIRST sman OF cust OUTER-JOIN NO-LOCK BY sman.sman
                BY cust.cust-no.
        WHEN 4 THEN
           OPEN QUERY BROWSE-1 FOR EACH cust WHERE
                cust.company = ip-company NO-LOCK,
                FIRST sman OF cust OUTER-JOIN NO-LOCK BY sman.sname
                BY cust.cust-no.
    end. 
  END.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

