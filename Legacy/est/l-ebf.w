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
def input parameter ip-est-type like eb.est-type no-undo.
def input parameter ip-rowid as rowid no-undo.
def input parameter ip-sort as int no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-rowid-val as ROWID no-undo. /* string i-code + i-name */

def var lv-type-dscr as cha no-undo.

&scoped-define SORTBY-1 BY eb.est-no BY eb.part-no
&scoped-define SORTBY-2 BY eb.cust-no 
&scoped-define SORTBY-3 BY eb.part-no 
&scoped-define SORTBY-4 BY eb.part-dscr1 
&scoped-define SORTBY-5 BY eb.stock-no 
&scoped-define FLD-NAME-1 eb.est-no
&scoped-define FLD-NAME-2 eb.cust-no
&scoped-define FLD-NAME-3 eb.part-no
&scoped-define FLD-NAME-4 eb.part-dscr1
&scoped-define FLD-NAME-5 eb.stock-no

&scoped-define IAMWHAT LOOKUP

DEF VAR lv-first-time AS LOG INIT YES NO-UNDO.

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
&Scoped-define INTERNAL-TABLES eb ef est

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 eb.est-no eb.cust-no eb.part-no ~
eb.part-dscr1 eb.stock-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company  ~
and eb.loc = ip-loc ~
 AND ((eb.est-type ge 1 and eb.est-type le 4 and ip-est-type ge 1 and ip-est-type le 4) or ~
      (eb.est-type gt 4 and ip-est-type gt 4)) ~
  AND rowid(eb) ne ip-rowid NO-LOCK, ~
      EACH ef OF eb NO-LOCK, ~
      EACH est OF eb NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company  ~
and eb.loc = ip-loc ~
 AND ((eb.est-type ge 1 and eb.est-type le 4 and ip-est-type ge 1 and ip-est-type le 4) or ~
      (eb.est-type gt 4 and ip-est-type gt 4)) ~
  AND rowid(eb) ne ip-rowid NO-LOCK, ~
      EACH ef OF eb NO-LOCK, ~
      EACH est OF eb NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 eb ef est
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 eb
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 ef
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 est


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
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate#", 1,
"Cust#", 2,
"Customer Part#", 3,
"Description", 4,
"FG Item#", 5
     SIZE 88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      eb, 
      ef, 
      est SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      eb.est-no FORMAT "x(8)":U WIDTH 14
      eb.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U
      eb.part-no FORMAT "x(15)":U WIDTH 17
      eb.part-dscr1 COLUMN-LABEL "Description" FORMAT "x(30)":U
            WIDTH 35
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 17
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 11.19
         BGCOLOR 8 FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 12 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 79
     bt-cancel AT ROW 14.1 COL 91
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 12.67 COL 3
     SPACE(89.59) SKIP(1.70)
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
     _TblList          = "ASI.eb,ASI.ef OF ASI.eb,ASI.est OF ASI.eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.eb.company = ip-company 
and asi.eb.loc = ip-loc
 AND ((eb.est-type ge 1 and eb.est-type le 4 and ip-est-type ge 1 and ip-est-type le 4) or
      (eb.est-type gt 4 and ip-est-type gt 4))
  AND rowid(eb) ne ip-rowid"
     _FldNameList[1]   > ASI.eb.est-no
"eb.est-no" ? "x(8)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[2]   > ASI.eb.cust-no
"eb.cust-no" "Cust#" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.eb.part-no
"eb.part-no" ? ? "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" ""
     _FldNameList[4]   > ASI.eb.part-dscr1
"eb.part-dscr1" "Description" ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _FldNameList[5]   > ASI.eb.stock-no
"eb.stock-no" "FG Item#" ? "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" ""
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
ON ANY-KEY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  IF KEYLABEL(LASTKEY) EQ "backspace"    AND
     LENGTH(lv-search:SCREEN-VALUE) GT 0 THEN DO:
    lv-search:SCREEN-VALUE = SUBSTR(lv-search:SCREEN-VALUE,1,LENGTH(lv-search:SCREEN-VALUE) - 1).
    APPLY "leave" TO lv-search.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   op-rowid-val = (ROWID(eb))                 .
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
        {srtord.i 4}
        {srtord.i 5}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-rowid-val = (ROWID(eb)).
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
        {srtord.i 2}
        {srtord.i 3}
        {srtord.i 4}
        {srtord.i 5}
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
        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3}
        {srtord.i 4}
        {srtord.i 5}
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

  /*&scoped-define key-phrase1 asi.eb.company EQ ip-company AND asi.eb.loc = ip-loc AND CAN-FIND(FIRST est OF eb) AND CAN-FIND(FIRST ef OF eb) AND ((asi.eb.est-type GE 1 AND asi.eb.est-type LE 4 AND ip-est-type GE 1 AND ip-est-type LE 4) OR (asi.eb.est-type GT 4 AND ip-est-type GT 4)) AND ROWID(eb) NE ip-rowid */
  &scoped-define key-phrase1 asi.eb.company EQ ip-company AND asi.eb.loc = ip-loc 
  
    /*AND ((asi.eb.est-type GE 1 AND asi.eb.est-type LE 4 AND ip-est-type GE 1 AND ip-est-type LE 4) OR (asi.eb.est-type GT 4 AND ip-est-type GT 4)) AND ROWID(eb) NE ip-rowid   */
  
  rd-sort = ip-sort.

  DO WITH FRAME {&FRAME-NAME}:
    CASE rd-sort:
      WHEN 5 THEN DO:
        {est/l-ebf.i 5 &USE-INDEX="use-index stock"}
      END.

      WHEN 4 THEN DO:
        {est/l-ebf.i 4 &USE-INDEX="use-index pdscr" }
      END.
   
      WHEN 3 THEN DO:
        {est/l-ebf.i 3 &USE-INDEX="use-index part"}
      END.

      WHEN 2 THEN DO:
        {est/l-ebf.i 2 &USE-INDEX="use-index cust"}
      END.

      OTHERWISE DO:
        {est/l-ebf.i 1 &USE-INDEX="use-index estcomp"}
      END.
    END CASE.
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

