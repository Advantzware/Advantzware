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
  
  Mod: Ticket - 103137 (Format Change for Order No. and Job No. 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE yellowColumnsName fgbin4
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input parameter ip-company like fg-bin.company no-undo.
def input parameter ip-i-no like fg-bin.i-no no-undo.
def input parameter ip-job-no like fg-bin.job-no no-undo.
def input parameter ip-job-no2 like fg-bin.job-no2 no-undo.
def input parameter ip-loc like fg-bin.loc no-undo.
def input parameter ip-loc-bin like fg-bin.loc-bin no-undo.
def input parameter ip-tag like fg-bin.tag no-undo.
def output parameter op-rowid-val AS ROWID no-undo.
{sys/inc/var.i}

DEF BUFFER b-fg-bin FOR fg-bin.

def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
&scoped-define SORTBY-1 BY fg-bin.job-no BY fg-bin.job-no2 BY fg-bin.loc BY fg-bin.loc-bin BY fg-bin.tag 
&scoped-define SORTBY-2 BY fg-bin.loc {&sortby-1}
&scoped-define SORTBY-3 BY fg-bin.loc-bin {&sortby-1}
&scoped-define SORTBY-4 BY fg-bin.tag {&sortby-1}
&scoped-define fld-name-1 fg-bin.job-no
&scoped-define fld-name-2 fg-bin.loc
&scoped-define fld-name-3 fg-bin.loc-bin
&scoped-define fld-name-4 fg-bin.tag
&scoped-define IAMWHAT LOOKUP


DEF VAR v-import-zero AS LOG NO-UNDO.

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
&Scoped-define INTERNAL-TABLES fg-bin

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 fg-bin.job-no fg-bin.job-no2 ~
fg-bin.loc fg-bin.loc-bin fg-bin.tag fg-bin.cust-no fg-bin.qty ~
TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) ~
fg-bin.cases-unit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH fg-bin WHERE ~{&KEY-PHRASE} ~
      AND fg-bin.company = ip-company ~
AND (ASI.fg-bin.i-no = ip-i-no ) ~
and fg-bin.qty <> 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH fg-bin WHERE ~{&KEY-PHRASE} ~
      AND fg-bin.company = ip-company ~
AND (ASI.fg-bin.i-no = ip-i-no ) ~
and fg-bin.qty <> 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 fg-bin
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 fg-bin


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
     SIZE 11 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job#", 1,
"Whs", 2,
"Bin", 3,
"Tag#", 4
     SIZE 50 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      fg-bin SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      fg-bin.job-no COLUMN-LABEL "Job#" FORMAT "x(9)":U WIDTH 15
            LABEL-BGCOLOR 14
      fg-bin.job-no2 COLUMN-LABEL "" FORMAT "999":U WIDTH 6
      fg-bin.loc COLUMN-LABEL "Whs" FORMAT "x(5)":U WIDTH 8 LABEL-BGCOLOR 14
      fg-bin.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      fg-bin.tag COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 30
            LABEL-BGCOLOR 14
      fg-bin.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      fg-bin.qty FORMAT "->,>>>,>>>,>>9":U WIDTH 18 LABEL-BGCOLOR 14
      TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) COLUMN-LABEL "Units"
      fg-bin.cases-unit COLUMN-LABEL "Units/Pallet" FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 126 BY 10.95
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     fi_sortby AT ROW 12.67 COL 66 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 86
     bt-cancel AT ROW 14.1 COL 98
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(18.19) SKIP(4.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bin Locations".


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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.fg-bin"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.fg-bin.company = ip-company
AND (ASI.fg-bin.i-no = ip-i-no )
and fg-bin.qty <> 0"
     _FldNameList[1]   > ASI.fg-bin.job-no
"job-no" "Job#" ? "character" ? ? ? 14 ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.fg-bin.job-no2
"job-no2" "" "999" "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-bin.loc
"loc" "Whs" ? "character" ? ? ? 14 ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-bin.loc-bin
"loc-bin" "Bin" ? "character" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.fg-bin.tag
"tag" "Tag#" "x(20)" "character" ? ? ? 14 ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-bin.cust-no
"cust-no" "Customer#" ? "character" ? ? ? 14 ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.fg-bin.qty
"qty" ? "->,>>>,>>>,>>9" "decimal" ? ? ? 14 ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)" "Units" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-bin.cases-unit
"cases-unit" "Units/Pallet" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bin Locations */
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
   op-rowid-val = ROWID(fg-bin).
   apply "window-close" to frame {&frame-name}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON START-SEARCH OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   RUN startSearch.
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
   op-rowid-val = ROWID(fg-bin). 
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
    &scoped-define fld-name-1 fg-bin.job-no
    &scoped-define fld-name-2 fg-bin.loc
    &scoped-define where-statement begins lv-search
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


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    &scoped-define IAMWHAT LOOKUP
    assign rd-sort.
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
    
  FRAME dialog-frame:TITLE = TRIM(FRAME dialog-frame:TITLE) +
                             " for FGItem: " + TRIM(ip-i-no).

  &SCOPED-DEFINE sortby-phrase {&sortby-1}
  {custom/yellowColumns.i}
  
  RUN enable_UI.
    /* Ticket# : 92946
     Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
    fi_sortby:HIDDEN  = TRUE.
    fi_sortby:VISIBLE = FALSE.
/*
  DO WITH frame {&frame-name}:
    {&browse-name}:SET-REPOSITIONED-ROW(INT({&browse-name}:DOWN / 2),"always").
          
    FOR EACH b-fg-bin
        WHERE b-fg-bin.company EQ ip-company
          AND b-fg-bin.i-no    EQ ip-i-no
          AND STRING(b-fg-bin.job-no,"x(20)")       +
              STRING(b-fg-bin.job-no2,"9999999999") +
              STRING(b-fg-bin.loc,"x(20)")          +
              STRING(b-fg-bin.loc-bin,"x(20)")      +
              STRING(b-fg-bin.tag,"x(20)")          GE
                         STRING(ip-job-no,"x(20)")       +
                         STRING(ip-job-no2,"9999999999") +
                         STRING(ip-loc,"x(20)")          +
                         STRING(ip-loc-bin,"x(20)")      +
                         STRING(ip-tag,"x(20)")
        NO-LOCK
        BY b-fg-bin.job-no BY b-fg-bin.job-no2 BY b-fg-bin.loc BY b-fg-bin.loc-bin BY b-fg-bin.tag:
      LEAVE.  
    END.

    IF AVAIL b-fg-bin THEN
      REPOSITION {&browse-name} TO ROWID ROWID(b-fg-bin) NO-ERROR.  
  END.
  */


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
  ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

