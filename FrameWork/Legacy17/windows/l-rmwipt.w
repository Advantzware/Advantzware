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
def input parameter ip-company like wiptag.company no-undo.
def input parameter ip-i-no like wiptag.rm-i-no no-undo.
def input parameter ip-loc like rm-bin.loc no-undo.
def input parameter ip-loc-bin like wiptag.rm-bin no-undo.
def input parameter ip-tag like wiptag.rm-tag-no no-undo.
def input parameter ip-job like wiptag.job-no no-undo.
def output parameter op-rowid-val AS ROWID no-undo.

/* Local Variable Definitions ---                                       */
DEF SHARED TEMP-TABLE tt-selected FIELD tt-rowid AS ROWID.

def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
&scoped-define SORTBY-1 BY wiptag.rm-whs BY wiptag.rm-bin BY wiptag.rm-tag-no
&scoped-define SORTBY-2 BY wiptag.rm-bin
&scoped-define SORTBY-3 BY wiptag.rm-tag-no
&scoped-define fld-name-1 wiptag.rm-whs
&scoped-define fld-name-2 wiptag.rm-bin
&scoped-define fld-name-3 wiptag.rm-tag-no
&scoped-define IAMWHAT LOOKUP

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
&Scoped-define INTERNAL-TABLES wiptag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 wiptag.rm-whs wiptag.rm-bin ~
wiptag.rm-tag-no /*rm-bin.qty */
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH wiptag WHERE ~{&KEY-PHRASE} ~
      AND wiptag.company = ip-company ~
      AND wiptag.sts          EQ "Printed" ~
      AND wiptag.rm-i-no = ip-i-no AND wiptag.job-no = ip-job NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH wiptag WHERE ~{&KEY-PHRASE} ~
      AND wiptag.company = ip-company ~
      AND wiptag.sts          EQ "Printed" ~
      AND wiptag.rm-i-no = ip-i-no AND wiptag.job-no = ip-job NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 wiptag
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 wiptag


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 Btn_Select Btn_Deselect ~
rd-sort bt-clear lv-search bt-ok bt-cancel 
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

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 13 BY 1.14.

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 13 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Warehouse", 1,
"Bin", 2,
"Tag", 3
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      wiptag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      wiptag.rm-whs FORMAT "x(5)":U
      wiptag.rm-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 11.4
      wiptag.rm-tag-no COLUMN-LABEL "Tag#" FORMAT "x(20)":U WIDTH 27.2
      /*rm-bin.qty FORMAT "->>>,>>9.9<<":U WIDTH 13.2*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 92 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     Btn_Select AT ROW 12.43 COL 66
     Btn_Deselect AT ROW 12.43 COL 79
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 69
     bt-cancel AT ROW 14.1 COL 81
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(31.39) SKIP(1.51)
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
/* BROWSE-TAB BROWSE-1 TEXT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.rm-bin"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.rm-bin.company = ip-company
AND ASI.rm-bin.i-no = ip-i-no"
     _FldNameList[1]   = ASI.rm-bin.loc
     _FldNameList[2]   > ASI.rm-bin.loc-bin
"rm-bin.loc-bin" "Bin" ? "character" ? ? ? ? ? ? no ? no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.rm-bin.tag
"rm-bin.tag" "Tag#" "x(20)" "character" ? ? ? ? ? ? no ? no no "27.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.rm-bin.qty
"rm-bin.qty" ? "->>>,>>9.9<<" "decimal" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON CURSOR-DOWN OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  {&browse-name}:SELECT-NEXT-ROW ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON CURSOR-UP OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  {&browse-name}:SELECT-PREV-ROW ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN set-output. 

  APPLY "window-close" TO FRAME {&FRAME-NAME}.
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
    end.
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
  RUN set-output.   

  APPLY "window-close" TO FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect Dialog-Frame
ON CHOOSE OF Btn_Deselect IN FRAME Dialog-Frame /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select Dialog-Frame
ON CHOOSE OF Btn_Select IN FRAME Dialog-Frame /* Select All */
DO:
  {&browse-name}:SELECT-ALL ().
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
    &scoped-define IAMWHAT SEARCH
    &scoped-define where-statement begins lv-search
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
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
    end.    
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEF BUFFER b-wiptag FOR wiptag.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FOR EACH tt-selected:
    DELETE tt-selected.
  END.
    
  FRAME dialog-frame:TITLE = TRIM(FRAME dialog-frame:TITLE) +
                             " for RM Item: " + TRIM(ip-i-no).

  &SCOPED-DEFINE sortby-phrase {&sortby-1}

  RUN enable_UI.

  DO WITH frame {&frame-name}:
    {&browse-name}:SET-REPOSITIONED-ROW(INT({&browse-name}:DOWN / 2),"always").
          
    FOR EACH b-wiptag
        WHERE b-wiptag.company EQ ip-company
          AND b-wiptag.rm-i-no    EQ ip-i-no
          AND b-wiptag.job-no     EQ ip-job
          AND b-wiptag.sts          EQ "Printed"
          AND STRING(b-wiptag.rm-whs,"x(20)")     +
              STRING(b-wiptag.rm-bin,"x(20)") +
              STRING(b-wiptag.rm-tag-no,"x(20)")     GE STRING(ip-loc,"x(20)")     +
                                                  STRING(ip-loc-bin,"x(20)") +
                                                  STRING(ip-tag,"x(20)")
        NO-LOCK
        BY b-wiptag.rm-whs BY b-wiptag.rm-bin BY b-wiptag.rm-tag-no:
      LEAVE.  
    END.

    IF AVAIL b-wiptag THEN DO:
      REPOSITION {&browse-name} TO ROWID ROWID(b-wiptag) NO-ERROR.
      {&browse-name}:SELECT-FOCUSED-ROW().
    END.
      

  END.

  APPLY "entry" TO BROWSE {&browse-name}.
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
  ENABLE BROWSE-1 RECT-1 Btn_Select Btn_Deselect rd-sort bt-clear lv-search 
         bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-output Dialog-Frame 
PROCEDURE set-output :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
 DEFINE BUFFER b-wiptag FOR wiptag .

  DO WITH FRAME {&FRAME-NAME}:
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW(li) NO-ERROR.
      IF AVAIL wiptag THEN DO:
        
        FIND FIRST rm-bin WHERE rm-bin.company EQ wiptag.company 
                               AND  rm-bin.tag     EQ wiptag.rm-tag-no NO-LOCK NO-ERROR.
        IF AVAIL rm-bin THEN DO:
           CREATE tt-selected.
           tt-rowid = ROWID(rm-bin).
           IF NOT ll THEN
              ASSIGN
              op-rowid-val = ROWID(rm-bin)
              ll           = YES.
        END.

        /*FIND CURRENT wiptag EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN
           wiptag.sts = "Issued" .
        FIND CURRENT wiptag NO-LOCK NO-ERROR.*/
        FOR EACH b-wiptag WHERE b-wiptag.company EQ wiptag.company
           AND b-wiptag.rm-tag-no = b-wiptag.rm-tag-no EXCLUSIVE-LOCK:
           ASSIGN
              b-wiptag.sts = "Issued" .
        END.
        
      END.
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

