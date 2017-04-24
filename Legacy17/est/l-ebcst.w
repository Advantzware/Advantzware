&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input parameter ip-company like eb.company no-undo.
def input parameter ip-loc like eb.loc no-undo.
def input parameter ip-cur-val as cha no-undo.
def input parameter ip-sort as INT no-undo.
def input parameter ip-set as LOGICAL no-undo.
def input parameter ip-len as DECIMAL no-undo.
def input parameter ip-wid as DECIMAL no-undo.
def input parameter ip-dep as DECIMAL no-undo.
def output parameter op-recid-val as recid no-undo. /* string i-code + i-name */

def var lv-type-dscr as cha no-undo.
DEF VAR ld-cons-qut AS CHAR NO-UNDO.
DEF VAR ld-qut-date AS CHAR NO-UNDO.

&scoped-define SORTBY-1 BY eb.est-no
&scoped-define SORTBY-2 BY eb.cust-no
&scoped-define SORTBY-3 BY eb.stock-no
&scoped-define SORTBY-4 BY eb.part-no
&scoped-define SORTBY-5 BY eb.part-dscr1
&scoped-define SORTBY-6 BY eb.die-no
&scoped-define SORTBY-7 BY eb.cad-no
&scoped-define SORTBY-8 BY eb.len
&scoped-define SORTBY-9 BY eb.wid
&scoped-define SORTBY-10 BY eb.dep
&scoped-define datatype-8 DEC
&scoped-define datatype-9 DEC
&scoped-define datatype-10 DEC
&scoped-define fld-name-1 eb.est-no
&scoped-define fld-name-2 eb.cust-no
&scoped-define fld-name-3 eb.stock-no
&scoped-define fld-name-4 eb.part-no
&scoped-define fld-name-5 eb.part-dscr1
&scoped-define fld-name-6 eb.die-no
&scoped-define fld-name-7 eb.cad-no
&scoped-define fld-name-8 eb.len
&scoped-define fld-name-9 eb.wid
&scoped-define fld-name-10 eb.dep
&scoped-define IAMWHAT LOOKUP

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1  cons-qut() @ ld-cons-qut qut-date() @ ld-qut-date eb.est-no eb.cust-no eb.part-dscr1 ~
eb.part-no eb.stock-no eb.die-no eb.cad-no eb.len eb.wid eb.dep
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company and ~
eb.loc = ip-loc AND (eb.len = ip-len OR ip-len = 0) AND (eb.wid = ip-wid OR ip-wid = 0) AND (eb.dep = ip-dep OR ip-dep = 0) AND ((eb.form-no = 0 AND ip-set) OR NOT ip-set ) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company and ~
eb.loc = ip-loc AND (eb.len = ip-len OR ip-len = 0) AND (eb.wid = ip-wid OR ip-wid = 0) AND (eb.dep = ip-dep OR ip-dep = 0) AND ((eb.form-no = 0 AND ip-set) OR NOT ip-set ) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 eb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 eb


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 rd-sort bt-clear lv-search bt-ok ~
bt-cancel RECT-1 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cons-qty B-table-Win 
FUNCTION cons-qut RETURNS CHAR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD qut-date B-table-Win 
FUNCTION qut-date RETURNS CHAR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate", 1,
"Customer", 2,
"FG Item #", 3,
"Cust Part", 4,
"Item Name", 5,
"Die #", 6,
"CAD #", 7,
"Length", 8,
"Width", 9,
"Depth", 10
     SIZE 120 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 135 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY  
      cons-qut() @ ld-cons-qut COLUMN-LABEL "Quote" FORMAT "X(10)":U WIDTH 14 
      qut-date() @ ld-qut-date COLUMN-LABEL "Quote Date" FORMAT "X(10)":U WIDTH 18 
      eb.est-no FORMAT "x(8)":U WIDTH 14 COLUMN-FONT 0
      eb.cust-no FORMAT "x(8)":U COLUMN-FONT 0
      eb.part-dscr1 COLUMN-LABEL "Item Name" FORMAT "x(30)":U COLUMN-FONT 0
      eb.part-no FORMAT "x(15)":U COLUMN-FONT 0
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U COLUMN-FONT 0
      eb.die-no FORMAT "x(15)":U COLUMN-FONT 0
      eb.cad-no COLUMN-LABEL "CAD #" FORMAT "x(15)":U COLUMN-FONT 0
      eb.len FORMAT ">9.99999":U COLUMN-FONT 0 
      eb.wid FORMAT ">9.99999":U COLUMN-FONT 0
      eb.dep FORMAT ">9.99999":U COLUMN-FONT 0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 138 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 69
     bt-cancel AT ROW 14.1 COL 81
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     SPACE(127.19) SKIP(1.79)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "eb.company = ip-company and
eb.loc = ip-loc"
     _FldNameList[1]   > ASI.eb.est-no
"est-no" ? "x(8)" "character" ? ? 0 ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[2]   > ASI.eb.cust-no
"cust-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.eb.part-dscr1
"part-dscr1" "Item Name" ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > ASI.eb.part-no
"part-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.eb.stock-no
"stock-no" "FG Item#" ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.eb.die-no
"die-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > ASI.eb.cad-no
"cad-no" "CAD #" ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.eb.len
"len" ? ? "decimal" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-recid-val = (recid(eb))                 .
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    assign lv-search:screen-value = "".
          ASSIGN lv-search = ""
                 ip-len    = 0
                 ip-wid    = 0
                 ip-dep    = 0 .
    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
    end.
        APPLY "tab" TO lv-search.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-recid-val = (recid(eb)).
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
    &scoped-define where-statement begins lv-search 
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
        {srtord2.i 5}
        {srtord2.i 6}
        {srtord2.i 7}
        {srtord2.i 8}
        {srtord2.i 9}
        {srtord2.i 10}
    end.      
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON LEAVE OF rd-sort IN FRAME Dialog-Frame
DO:
    assign rd-sort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    /* redefined for lookup */
    &scoped-define IAMWHAT LOOKUP   
         
    assign rd-sort.
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
        {srtord2.i 5}
        {srtord2.i 6}
        {srtord2.i 7}
        {srtord2.i 8}
        {srtord2.i 9}
        {srtord2.i 10}
    end.    

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

  &scoped-define IAMWHAT LOOKUP
  
  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-search = ip-cur-val
     rd-sort   = ip-sort.
    DISPLAY lv-search rd-sort.
    APPLY "tab" TO lv-search.
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
  DISPLAY rd-sort lv-search 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 rd-sort bt-clear lv-search bt-ok bt-cancel RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cons-qut B-table-Win 
FUNCTION cons-qut RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
DEF BUFFER b-item FOR ITEM.
DEF VAR vEst AS CHAR NO-UNDO.
ASSIGN
    vEst = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no).

DEF VAR v-qut AS CHAR NO-UNDO.

FIND FIRST quotehd NO-LOCK
      WHERE quotehd.company EQ eb.company
        AND quotehd.est-no    EQ vEst
      NO-ERROR.


IF AVAIL quotehd THEN v-qut = string(quotehd.q-no) .
   
RETURN v-qut.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION qut-date B-table-Win 
FUNCTION qut-date RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
DEF BUFFER b-item FOR ITEM.
DEF VAR vEst AS CHAR NO-UNDO.
ASSIGN
    vEst = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no).

DEF VAR v-qut AS CHAR NO-UNDO.

FIND FIRST quotehd NO-LOCK
      WHERE quotehd.company EQ eb.company
        AND quotehd.est-no    EQ vEst
      NO-ERROR.


IF AVAIL quotehd THEN v-qut = string(quotehd.quo-date) .
   
RETURN v-qut.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
