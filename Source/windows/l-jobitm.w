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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input parameter ip-company like itemfg.company no-undo.
DEF INPUT PARAM ip-job-no LIKE job-hdr.job-no NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS cha NO-UNDO.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-rec-val as recid no-undo.
{sys/inc/var.i}

def var lv-type-dscr as cha no-undo.

&scoped-define SORTBY-1 BY job-hdr.frm BY job-hdr.blank-no
&scoped-define SORTBY-2 BY job-hdr.blank-no BY job-hdr.frm
&scoped-define SORTBY-3 BY job-hdr.i-no BY job-hdr.frm BY job-hdr.blank-no
&scoped-define SORTBY-4 BY job-hdr.est-no BY job-hdr.frm BY job-hdr.blank-no
&scoped-define SORTBY-5 BY job-hdr.ord-no BY job-hdr.frm BY job-hdr.blank-no
&scoped-define SORTBY-6 BY job-hdr.cust-no BY job-hdr.frm BY job-hdr.blank-no
&scoped-define fld-name-1 job-hdr.frm
&scoped-define fld-name-2 job-hdr.blank-no
&scoped-define fld-name-3 job-hdr.i-no
&scoped-define fld-name-4 job-hdr.est-no
&scoped-define fld-name-5 job-hdr.ord-no
&scoped-define fld-name-6 job-hdr.cust-no
&SCOPED-DEFINE datatype-1 INT
&SCOPED-DEFINE datatype-2 INT
&SCOPED-DEFINE datatype-5 INT

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
&Scoped-define INTERNAL-TABLES job-hdr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 job-hdr.frm job-hdr.blank-no ~
job-hdr.i-no job-hdr.est-no job-hdr.ord-no job-hdr.cust-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH job-hdr WHERE ~{&KEY-PHRASE} ~
      AND job-hdr.company = ip-company and ~
TRIM(job-hdr.job-no) = TRIM(ip-job-no) and ~
job-hdr.job-no2 = int(ip-job-no2) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH job-hdr WHERE ~{&KEY-PHRASE} ~
      AND job-hdr.company = ip-company and ~
TRIM(job-hdr.job-no) = TRIM(ip-job-no) and ~
job-hdr.job-no2 = int(ip-job-no2) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 job-hdr


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
          "Form#", 1,
"Blank#", 2,
"Item#", 3,
"Est#", 4,
"Order#", 5,
"Customer", 6
     SIZE 77 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      job-hdr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      job-hdr.frm COLUMN-LABEL "Form#" FORMAT ">>9":U
      job-hdr.blank-no COLUMN-LABEL "Blank#" FORMAT ">>9":U
      job-hdr.i-no FORMAT "x(15)":U COLUMN-FONT 0
      job-hdr.est-no FORMAT "x(8)":U WIDTH 14 COLUMN-FONT 0
      job-hdr.ord-no FORMAT ">>>>>>>9":U COLUMN-FONT 0
      job-hdr.cust-no FORMAT "x(8)":U COLUMN-FONT 0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 11.19
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
     SPACE(81.39) SKIP(1.84)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "FG Items for".


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
     _TblList          = "ASI.job-hdr"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.job-hdr.company = ip-company and
TRIM(job-hdr.job-no) = TRIM(ip-job-no) and
job-hdr.job-no2 = int(ip-job-no2)"
     _FldNameList[1]   > ASI.job-hdr.frm
"frm" "Form#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.job-hdr.blank-no
"blank-no" "Blank#" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.job-hdr.i-no
"i-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > ASI.job-hdr.est-no
"est-no" ? "x(8)" "character" ? ? 0 ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[5]   > ASI.job-hdr.ord-no
"ord-no" ? ? "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.job-hdr.cust-no
"cust-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* FG Items for */
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
   op-char-val = job-hdr.i-no:screen-value in browse {&browse-name}
                 .
   op-rec-val = recid(job-hdr).              
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
        {srtord2.i 5}
        {srtord2.i 6}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
    op-char-val = 
                 job-hdr.i-no:screen-value in browse {&browse-name}
                 .
   op-rec-val = recid(job-hdr).              
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
    end.      
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
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
    end.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEF BUFFER b-job-hdr FOR job-hdr.
DEF VAR lv-frm AS CHAR NO-UNDO.
DEF VAR lv-blk AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  FRAME dialog-frame:TITLE = TRIM(FRAME dialog-frame:TITLE) + " Job: " +
                             TRIM(ip-job-no) + STRING(INT(ip-job-no2),"999").

  RUN enable_UI.

  DO li = 1 TO NUM-ENTRIES(ip-cur-val):
    IF li EQ 2 THEN lv-frm = ENTRY(2,ip-cur-val).
    ELSE
    IF li EQ 3 THEN lv-blk = ENTRY(3,ip-cur-val).
  END.

  DO WITH FRAME {&frame-name}:
    {&browse-name}:SET-REPOSITIONED-ROW(INT({&browse-name}:DOWN / 2),"always").
          
    FOR EACH b-job-hdr NO-LOCK
        WHERE b-job-hdr.company EQ ip-company
          AND TRIM(b-job-hdr.job-no)  EQ TRIM(ip-job-no)
          AND b-job-hdr.job-no2 EQ INT(ip-job-no2)
          AND STRING(b-job-hdr.frm,"9999999999")      +
              STRING(b-job-hdr.blank-no,"9999999999") +
              STRING(b-job-hdr.i-no,"x(20)") GE STRING(INT(lv-frm),"9999999999") +
                                                STRING(INT(lv-blk),"9999999999") +
                                                STRING(ENTRY(1,ip-cur-val),"x(20)")
       BY b-job-hdr.frm BY b-job-hdr.blank-no BY b-job-hdr.i-no:
      LEAVE.  
    END.

    IF AVAIL b-job-hdr THEN
      REPOSITION {&browse-name} TO ROWID ROWID(b-job-hdr) NO-ERROR.  
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

