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
def input parameter ip-company like itemfg.company no-undo.
def input param ip-cust-no like oe-ordl.cust-no no-undo.
def input param ip-ship-id like oe-relh.ship-id no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-rec-id as recid no-undo.     /* recid output */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

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

{sys/inc/var.i new shared}

cocode = ip-company.

&scoped-define ASCDSC-1 DESC
&scoped-define SORTBY-1 BY oe-ordl.ord-no {&ASCDSC-1} BY oe-ordl.i-no
&scoped-define SORTBY-2 BY oe-ordl.i-no {&sortby-1}
&scoped-define SORTBY-3 BY oe-ordl.part-no {&sortby-1}
&scoped-define fld-name-1 oe-ordl.ord-no
&scoped-define fld-name-2 oe-ordl.i-no
&scoped-define fld-name-3 oe-ordl.part-no
&scoped-define datatype-1 int
/*&scoped-define datatype-2 date */

&scoped-define IAMWHAT LOOKUP

def var lv-ord-ok as cha init "R,I,S,P,A,N,U" no-undo.

def var lv-first-time as log init yes no-undo.
DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR li-prod AS INT NO-UNDO.
DEF VAR li-bal AS INT NO-UNDO.

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
&Scoped-define INTERNAL-TABLES oe-ordl oe-ord

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 oe-ordl.ord-no oe-ordl.est-no ~
oe-ordl.job-no oe-ordl.job-no2 oe-ordl.i-no oe-ordl.part-no oe-ordl.qty ~
oe-ordl.inv-qty oe-ordl.ship-qty get-prod (li-bal) @ li-prod ~
get-bal () @ li-bal get-ord-no () @ lv-ord-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH oe-ordl WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.company eq ip-company and ~
ASI.oe-ordl.opened eq yes and ~
ASI.oe-ordl.cust-no eq ip-cust-no ~
use-index opened NO-LOCK, ~
      FIRST oe-ord OF oe-ordl NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH oe-ordl WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.company eq ip-company and ~
ASI.oe-ordl.opened eq yes and ~
ASI.oe-ordl.cust-no eq ip-cust-no ~
use-index opened NO-LOCK, ~
      FIRST oe-ord OF oe-ordl NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 oe-ordl oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 oe-ord


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 rd-sort bt-clear lv-search bt-ok ~
bt-ok-2 bt-cancel RECT-1 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-bal Dialog-Frame 
FUNCTION get-bal RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ord-no Dialog-Frame 
FUNCTION get-ord-no RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod Dialog-Frame 
FUNCTION get-prod RETURNS INTEGER
  ( OUTPUT op-bal AS INT )  FORWARD.

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
     LABEL "&Order" 
     SIZE 10 BY 1.14.

DEFINE BUTTON bt-ok-2 
     LABEL "&Item" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 96 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order#", 1,
"FG Item#", 2,
"Cust Part#", 3
     SIZE 55 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      oe-ordl, 
      oe-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      oe-ordl.ord-no FORMAT ">>>>>9":U WIDTH 8
      oe-ordl.est-no COLUMN-LABEL "Estimate#" FORMAT "x(8)":U WIDTH 14
      oe-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 8
      oe-ordl.job-no2 COLUMN-LABEL "" FORMAT "99":U
      oe-ordl.i-no FORMAT "x(15)":U WIDTH 18
      oe-ordl.part-no FORMAT "x(15)":U WIDTH 18
      oe-ordl.qty COLUMN-LABEL "Order!Quantity" FORMAT "->>,>>>,>>9.9<<":U
      oe-ordl.inv-qty COLUMN-LABEL "Invoice!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 15
      oe-ordl.ship-qty COLUMN-LABEL "Shipped!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 15
      get-prod (li-bal) @ li-prod COLUMN-LABEL "Qty!Produced" FORMAT "->>,>>>,>>>":U
            WIDTH 15
      get-bal () @ li-bal COLUMN-LABEL "Balance!On Hand" FORMAT "->>,>>>,>>>":U
            WIDTH 15
      get-ord-no () @ lv-ord-no COLUMN-LABEL "Order#" FORMAT ">>>>>>>>":U
            WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 157 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 122
     bt-ok-2 AT ROW 14.1 COL 134
     bt-cancel AT ROW 14.1 COL 146
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     SPACE(146.19) SKIP(2.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order Items for Customer".


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
     _TblList          = "ASI.oe-ordl,ASI.oe-ord OF ASI.oe-ordl"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "ASI.oe-ordl.company eq ip-company and
ASI.oe-ordl.opened eq yes and
ASI.oe-ordl.cust-no eq ip-cust-no
use-index opened"
     _FldNameList[1]   > ASI.oe-ordl.ord-no
"oe-ordl.ord-no" ? ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[2]   > ASI.oe-ordl.est-no
"oe-ordl.est-no" "Estimate#" "x(8)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[3]   > ASI.oe-ordl.job-no
"oe-ordl.job-no" "Job#" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[4]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "" "99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.oe-ordl.i-no
"oe-ordl.i-no" ? ? "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[6]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" ? ? "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[7]   > ASI.oe-ordl.qty
"oe-ordl.qty" "Order!Quantity" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > ASI.oe-ordl.inv-qty
"oe-ordl.inv-qty" "Invoice!Quantity" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[9]   > ASI.oe-ordl.ship-qty
"oe-ordl.ship-qty" "Shipped!Quantity" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[10]   > "_<CALC>"
"get-prod (li-bal) @ li-prod" "Qty!Produced" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[11]   > "_<CALC>"
"get-bal () @ li-bal" "Balance!On Hand" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[12]   > "_<CALC>"
"get-ord-no () @ lv-ord-no" "Order#" ">>>>>>>>" ? ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Items for Customer */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   ASSIGN
    op-char-val = "item"
    op-rec-id   = recid(oe-ordl).

   apply "window-close" to frame {&frame-name}. 
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
        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3}
    end.
    APPLY "entry" TO BROWSE {&browse-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* Order */
DO:
   ASSIGN
    op-char-val = "order".
    op-rec-id   = RECID(oe-ordl).

   apply "window-close" to frame {&frame-name}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok-2 Dialog-Frame
ON CHOOSE OF bt-ok-2 IN FRAME Dialog-Frame /* Item */
DO:
   ASSIGN
    op-char-val = "item"
    op-rec-id   = recid(oe-ordl).

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
        {srtord.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
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

  /*&scoped-define key-phrase {&fld-name-1} >= (ip-cur-val)*/
  &scoped-define sortby-phrase {&sortby-1}
  
  FIND FIRST cust
      WHERE cust.company EQ ip-company
        AND cust.cust-no EQ ip-cust-no
      NO-LOCK NO-ERROR.
  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) +
                      ": " + TRIM(ip-cust-no) +
                      (IF AVAIL cust THEN " (" + TRIM(cust.NAME) + ")" ELSE "").

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = "".
  END.
  
  RUN enable_UI.

  RUN new-rd-sort.

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
  ENABLE BROWSE-1 rd-sort bt-clear lv-search bt-ok bt-ok-2 bt-cancel RECT-1 
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
        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3}
    end.    
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  APPLY 'entry' TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-bal Dialog-Frame 
FUNCTION get-bal RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  /*li = li-bal - (IF AVAIL oe-ordl THEN oe-ordl.ship-qty ELSE 0).*/

  IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
  FOR EACH fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.job-no  EQ oe-ordl.job-no
        AND fg-bin.job-no2 EQ oe-ordl.job-no2
        AND fg-bin.i-no    EQ oe-ordl.i-no
      NO-LOCK:
    li = li + fg-bin.qty.
  END.

  RETURN li.    /* Function return value. */ 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ord-no Dialog-Frame 
FUNCTION get-ord-no RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF AVAIL oe-ordl THEN oe-ordl.ord-no ELSE 0.
                                          /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod Dialog-Frame 
FUNCTION get-prod RETURNS INTEGER
  ( OUTPUT op-bal AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
  FOR EACH fg-act
      WHERE fg-act.company EQ cocode
        AND fg-act.job-no  EQ oe-ordl.job-no
        AND fg-act.job-no2 EQ oe-ordl.job-no2
        AND fg-act.i-no    EQ oe-ordl.i-no
      NO-LOCK:
    li = li + fg-act.qty.
  END.
 
  op-bal = li.

  RETURN li.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

