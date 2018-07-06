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
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cust-no like cust-part.cust-no no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo.
def output parameter op-row-val as rowid no-undo.

DEF TEMP-TABLE tt-cust-part    /* task 12031307 */
    FIELD cust-no AS CHAR 
    FIELD part-no AS CHAR
    FIELD i-no AS CHAR
    FIELD i-name AS CHAR
    FIELD style AS CHAR
    FIELD l-score AS DECIMAL
    FIELD w-score AS DECIMAL
    FIELD d-score AS DECIMAL 
    FIELD r-id  AS ROWID .

def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
DEFINE VARIABLE custX AS CHARACTER NO-UNDO.

&scoped-define SORTBY-1 BY tt-cust-part.cust-no BY tt-cust-part.part-no
&scoped-define SORTBY-2 BY tt-cust-part.part-no {&sortby-1}
&scoped-define SORTBY-3 BY tt-cust-part.i-no {&sortby-1}
&scoped-define SORTBY-4 BY tt-cust-part.i-name {&sortby-1}
&scoped-define fld-name-1 tt-cust-part.cust-no
&scoped-define fld-name-2 tt-cust-part.part-no
&scoped-define fld-name-3 tt-cust-part.i-no
&scoped-define fld-name-4 tt-cust-part.i-name
&SCOPED-DEFINE useMatches
&scoped-define IAMWHAT LOOKUP

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cust-part

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-cust-part.cust-no tt-cust-part.part-no tt-cust-part.i-no tt-cust-part.i-name tt-cust-part.style tt-cust-part.l-score tt-cust-part.w-score tt-cust-part.d-score
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-cust-part WHERE ~{&KEY-PHRASE} ~
NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-cust-part WHERE ~{&KEY-PHRASE} ~
NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-cust-part
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-cust-part


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort bt-clear lv-search ~
bt-ok bt-cancel 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 10 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 84 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", 1,
"Customer Part#", 2,
"FG Item#", 3,
"Name", 4
     SIZE 118 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 129 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-cust-part SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-cust-part.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 12
      tt-cust-part.part-no COLUMN-LABEL "Cust Part #" FORMAT "x(15)":U WIDTH 23
      tt-cust-part.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 23
      tt-cust-part.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      tt-cust-part.style COLUMN-LABEL "Style" FORMAT "x(6)":U WIDTH 8
      tt-cust-part.l-score COLUMN-LABEL "Length" FORMAT ">>>>>9.99<<<<":U
            WIDTH 11
      tt-cust-part.w-score COLUMN-LABEL "Width" FORMAT ">>>>>9.99<<<<":U
            WIDTH 11
      tt-cust-part.d-score COLUMN-LABEL "Depth" FORMAT ">>>>>>9.99<<<<":U
            WIDTH 11
    /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 129 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 11 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 108
     bt-cancel AT ROW 14.1 COL 120
     "Sort By:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 12.67 COL 2
     RECT-1 AT ROW 12.43 COL 1
     SPACE(0.59) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "FG Items (Customer Part)".


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
     _TblList          = "ASI.cust-part,ASI.itemfg WHERE ASI.cust-part ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "ASI.cust-part.company EQ ip-company AND
(ASI.cust-part.cust-no EQ ip-cust-no OR
ASI.cust-part.cust-no EQ custX)"
     _JoinCode[2]      = "ASI.itemfg.company = ASI.cust-part.company
  AND ASI.itemfg.i-no = ASI.cust-part.i-no"
     _FldNameList[1]   > ASI.cust-part.cust-no
"cust-part.cust-no" "Customer#" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > ASI.cust-part.part-no
"cust-part.part-no" ? "x(15)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _FldNameList[3]   > ASI.cust-part.i-no
"cust-part.i-no" "FG Item#" ? "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _FldNameList[4]   = ASI.itemfg.i-name
     _FldNameList[5]   > ASI.itemfg.style
"itemfg.style" "Style" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[6]   > ASI.itemfg.l-score[50]
"itemfg.l-score[50]" "Length" ? "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" ""
     _FldNameList[7]   > ASI.itemfg.w-score[50]
"itemfg.w-score[50]" "Width" ? "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" ""
     _FldNameList[8]   > ASI.itemfg.d-score[50]
"itemfg.d-score[50]" "Depth" ? "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* FG Items (Customer Part) */
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
   assign
    op-row-val  = tt-cust-part.r-id
    op-char-val = tt-cust-part.part-no.

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
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    end.
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   assign
    op-row-val  = tt-cust-part.r-id
    op-char-val = tt-cust-part.part-no.

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
    end.      
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    /* /* redefined for lookup */
    &scoped-define IAMWHAT LOOKUP   
         
    assign rd-sort.
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    end.    
    apply "entry" to {&browse-name}.*/
    
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

     FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.active EQ 'X' NO-ERROR.
    custX = IF AVAILABLE cust THEN cust.cust-no ELSE ''.
    
    RUN create-tt-cust-part.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = "".
    IF rd-sort:SCREEN-VALUE EQ ? THEN rd-sort:SCREEN-VALUE = '1'.
    ASSIGN rd-sort.
  END.
  RUN enable_UI.
  RUN new-rd-sort.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-cust-part Dialog-Frame 
PROCEDURE create-tt-cust-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH cust-part WHERE cust-part.company EQ ip-company AND 
            (ASI.cust-part.cust-no EQ ip-cust-no OR 
             ASI.cust-part.cust-no EQ custX)  NO-LOCK, 
      FIRST itemfg WHERE itemfg.company = cust-part.company 
  AND itemfg.i-no = cust-part.i-no
  AND NOT itemfg.stat EQ "I"  
  NO-LOCK:

            CREATE tt-cust-part.
            ASSIGN
                tt-cust-part.cust-no    =  cust-part.cust-no
                tt-cust-part.part-no    =  cust-part.part-no
                tt-cust-part.i-no       =  cust-part.i-no
                tt-cust-part.i-name     =  itemfg.i-name
                tt-cust-part.style      =  itemfg.style
                tt-cust-part.l-score    =  itemfg.l-score[50]
                tt-cust-part.w-score    =  itemfg.w-score[50]
                tt-cust-part.d-score    =  itemfg.d-score[50] 
                tt-cust-part.r-id       = ROWID(itemfg).
     
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
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
        {srtord2.i 3}
        {srtord2.i 4}
    end. 
  END.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

