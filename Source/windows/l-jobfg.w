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
def input parameter ip-i-no as cha no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-rec-val as recid no-undo.
def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
DEF VAR lv-unit AS INT NO-UNDO.

&scoped-define SORTBY-1 BY job-hdr.job-no DESC BY job-hdr.job-no2 DESC
&scoped-define SORTBY-2 BY job-hdr.est-no {&SORTBY-1}
&scoped-define SORTBY-3 BY job-hdr.ord-no {&SORTBY-1}
&scoped-define SORTBY-4 BY job-hdr.cust-no {&SORTBY-1}
&scoped-define fld-name-1  trim(job-hdr.job-no)
&scoped-define fld-name-2  trim(job-hdr.est-no)
&scoped-define fld-name-3  job-hdr.ord-no
&scoped-define fld-name-4  job-hdr.cust-no
&SCOPED-DEFINE datatype-3 INT

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
&Scoped-define INTERNAL-TABLES job-hdr fg-bin

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 job-hdr.job-no job-hdr.job-no2 ~
job-hdr.est-no job-hdr.ord-no job-hdr.cust-no fg-bin.loc fg-bin.loc-bin ~
fg-bin.qty disp-unit() @ lv-unit fg-bin.case-count fg-bin.cases-unit ~
fg-bin.tag job-hdr.i-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH job-hdr WHERE ~{&KEY-PHRASE} ~
      AND job-hdr.company = ip-company ~
AND (ASI.job-hdr.i-no = ip-i-no or ip-i-no = "") NO-LOCK, ~
      EACH fg-bin WHERE TRUE /* Join to job-hdr incomplete */ ~
      AND fg-bin.company = job-hdr.company ~
and (fg-bin.job-no = job-hdr.job-no /*or fg-bin.job-no = ""*/) ~
and (fg-bin.job-no2 = job-hdr.job-no2 /*or fg-bin.job-no2 = 0*/) ~
AND fg-bin.i-no = job-hdr.i-no ~
and fg-bin.qty <> 0 ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH job-hdr WHERE ~{&KEY-PHRASE} ~
      AND job-hdr.company = ip-company ~
AND (ASI.job-hdr.i-no = ip-i-no or ip-i-no = "") NO-LOCK, ~
      EACH fg-bin WHERE TRUE /* Join to job-hdr incomplete */ ~
      AND fg-bin.company = job-hdr.company ~
and (fg-bin.job-no = job-hdr.job-no /*or fg-bin.job-no = ""*/) ~
and (fg-bin.job-no2 = job-hdr.job-no2 /*or fg-bin.job-no2 = 0*/) ~
AND fg-bin.i-no = job-hdr.i-no ~
and fg-bin.qty <> 0 ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 job-hdr fg-bin
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 job-hdr
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 fg-bin


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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disp-unit Dialog-Frame 
FUNCTION disp-unit RETURNS INTEGER
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
     SIZE 13 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job#", 1,
"Est#", 2,
"Order#", 3,
"Customer", 4
     SIZE 70 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      job-hdr, 
      fg-bin SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      job-hdr.job-no COLUMN-LABEL "   Job#" FORMAT "x(6)":U WIDTH 9
      job-hdr.job-no2 COLUMN-LABEL "" FORMAT ">9":U WIDTH 3
      job-hdr.est-no FORMAT "x(8)":U WIDTH 14
      job-hdr.ord-no FORMAT ">>>>>>>>":U WIDTH 10
      job-hdr.cust-no FORMAT "x(8)":U WIDTH 12
      fg-bin.loc FORMAT "x(5)":U
      fg-bin.loc-bin FORMAT "x(8)":U
      fg-bin.qty FORMAT "->>>,>>9.9<<<<<":U
      disp-unit() @ lv-unit COLUMN-LABEL "Units"
      fg-bin.case-count COLUMN-LABEL "Unit Count" FORMAT ">>>,>>9":U
      fg-bin.cases-unit COLUMN-LABEL "Units/!Pallet" FORMAT ">>>,>>9":U
      fg-bin.tag COLUMN-LABEL "Tag" FORMAT "x(23)":U
      job-hdr.i-no FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 126 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 85
     bt-cancel AT ROW 14.1 COL 98
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     SPACE(115.19) SKIP(1.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Information".


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
     _TblList          = "ASI.job-hdr,ASI.fg-bin WHERE ASI.job-hdr ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.job-hdr.company = ip-company
AND (ASI.job-hdr.i-no = ip-i-no or ip-i-no = """")"
     _Where[2]         = "ASI.fg-bin.company = job-hdr.company
and (fg-bin.job-no = job-hdr.job-no /*or fg-bin.job-no = """"*/)
and (fg-bin.job-no2 = job-hdr.job-no2 /*or fg-bin.job-no2 = 0*/)
AND ASI.fg-bin.i-no = job-hdr.i-no
and fg-bin.qty <> 0
"
     _FldNameList[1]   > ASI.job-hdr.job-no
"job-hdr.job-no" "   Job#" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > ASI.job-hdr.job-no2
"job-hdr.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[3]   > ASI.job-hdr.est-no
"job-hdr.est-no" ? "x(8)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[4]   > ASI.job-hdr.ord-no
"job-hdr.ord-no" ? ">>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[5]   > ASI.job-hdr.cust-no
"job-hdr.cust-no" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[6]   = ASI.fg-bin.loc
     _FldNameList[7]   = ASI.fg-bin.loc-bin
     _FldNameList[8]   = ASI.fg-bin.qty
     _FldNameList[9]   > "_<CALC>"
"disp-unit() @ lv-unit" "Units" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > ASI.fg-bin.case-count
"fg-bin.case-count" "Unit Count" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > ASI.fg-bin.cases-unit
"fg-bin.cases-unit" "Units/!Pallet" ">>>,>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > ASI.fg-bin.tag
"fg-bin.tag" "Tag" "x(23)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[13]   = ASI.job-hdr.i-no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Job Information */
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
   op-char-val = job-hdr.job-no:screen-value in browse {&browse-name} + "," +
                  job-hdr.job-no2:screen-value in browse {&browse-name} + "," +
                  fg-bin.loc + "," + 
                  fg-bin.loc-bin + "," + 
                  string(fg-bin.case-count) + "," +
                  string(fg-bin.cases-unit) + "," +
                  job-hdr.i-no + "," +
                  fg-bin.tag + "," +
                  string(fg-bin.partial-count) + "," +
                  lv-unit:SCREEN-VALUE 
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

    IF rd-sort EQ 1 THEN DO:
      OPEN QUERY {&browse-name}
          FOR EACH ASI.job-hdr
              WHERE {&key-phrase}
                AND job-hdr.company EQ ip-company
                AND TRIM(job-hdr.job-no) BEGINS lv-search
              NO-LOCK,
         EACH ASI.fg-bin WHERE ASI.fg-bin.company = job-hdr.company
             and fg-bin.job-no = job-hdr.job-no
             and fg-bin.job-no2 = job-hdr.job-no2
             AND ASI.fg-bin.i-no = job-hdr.i-no NO-LOCK 
              {&sortby-1}.

      IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
        APPLY "ENTRY" TO {&BROWSE-NAME}.
      END. 
    END.

    ELSE
    CASE rd-sort:
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
    op-char-val = job-hdr.job-no:screen-value in browse {&browse-name} + "," +
                  job-hdr.job-no2:screen-value in browse {&browse-name} + "," +
                  fg-bin.loc + "," + 
                  fg-bin.loc-bin + "," + 
                  string(fg-bin.case-count) + "," +
                  string(fg-bin.cases-unit) + "," +
                  job-hdr.i-no + "," +
                  fg-bin.tag + "," +
                  string(fg-bin.partial-count) + "," +
                  lv-unit:SCREEN-VALUE 
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
    
    CASE rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
    END.   
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

    CASE rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
        {srtord2.i 4}
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

  RUN enable_UI.

  &scoped-define key-phrase ASI.job-hdr.company = ip-company AND (ASI.job-hdr.i-no = ip-i-no OR ip-i-no = "")
  {custom/lookposd.i &lookup-file = "job-hdr" &lookup-field = "job-no" }
  APPLY "value-changed" TO rd-sort IN FRAME {&FRAME-NAME}.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disp-unit Dialog-Frame 
FUNCTION disp-unit RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 IF AVAIL fg-bin
     THEN RETURN int(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)).
ELSE RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

