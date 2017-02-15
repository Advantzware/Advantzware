&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*----------------------------------------------------------------------*/
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
def var lv-type-dscr as cha no-undo.

{pc/pcprdd4u.i}

&scoped-define SORTBY-1 BY tt-job-hdr.frm BY tt-job-hdr.blank-no
&scoped-define SORTBY-2 BY tt-job-hdr.blank-no BY tt-job-hdr.frm
&scoped-define SORTBY-3 BY tt-job-hdr.i-no BY tt-job-hdr.frm BY tt-job-hdr.blank-no
&scoped-define SORTBY-4 BY tt-job-hdr.est-no BY tt-job-hdr.frm BY tt-job-hdr.blank-no
&scoped-define SORTBY-5 BY tt-job-hdr.ord-no BY tt-job-hdr.frm BY tt-job-hdr.blank-no
&scoped-define SORTBY-6 BY tt-job-hdr.cust-no BY tt-job-hdr.frm BY tt-job-hdr.blank-no
&scoped-define fld-name-1 tt-job-hdr.frm
&scoped-define fld-name-2 tt-job-hdr.blank-no
&scoped-define fld-name-3 tt-job-hdr.i-no
&scoped-define fld-name-4 tt-job-hdr.est-no
&scoped-define fld-name-5 tt-job-hdr.ord-no
&scoped-define fld-name-6 tt-job-hdr.cust-no
&SCOPED-DEFINE datatype-1 STRING
&SCOPED-DEFINE datatype-2 STRING
&SCOPED-DEFINE datatype-5 STRING

&scoped-define IAMWHAT LOOKUP
&SCOPED-DEFINE useMatches

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
&Scoped-define INTERNAL-TABLES tt-job-hdr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-job-hdr.frm tt-job-hdr.blank-no tt-job-hdr.i-no tt-job-hdr.est-no tt-job-hdr.ord-no tt-job-hdr.cust-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-job-hdr ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-job-hdr ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-job-hdr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-job-hdr


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
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Form#", 1,
"Blank#", 2,
"Item#", 3,
"Est#", 4,
"Order#", 5,
"Customer", 6
     SIZE 82 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-job-hdr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-job-hdr.frm COLUMN-LABEL "Form#" FORMAT ">>9":U
      tt-job-hdr.blank-no COLUMN-LABEL "Blank#" FORMAT ">>9":U
      tt-job-hdr.i-no FORMAT "x(23)":U COLUMN-FONT 0
      tt-job-hdr.est-no FORMAT "x(8)":U COLUMN-FONT 0
      tt-job-hdr.ord-no FORMAT ">>>>>9":U COLUMN-FONT 0
      tt-job-hdr.cust-no FORMAT "x(8)":U COLUMN-FONT 0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 74
     bt-cancel AT ROW 14.1 COL 86
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     SPACE(85.39) SKIP(1.84)
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
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-job-hdr ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.job-hdr.company = ip-company and
job-hdr.job-no = ip-job-no and
job-hdr.job-no2 = int(ip-job-no2)"
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
   op-char-val = tt-job-hdr.i-no:screen-value in browse {&browse-name}.
   op-rec-val = recid(tt-job-hdr).              
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
   op-char-val = tt-job-hdr.i-no:screen-value in browse {&browse-name}.
   op-rec-val = recid(tt-job-hdr).              
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
                             TRIM(ip-job-no) + "-" + STRING(INT(ip-job-no2),"99").

  RUN build-table.

  RUN enable_UI.

  DO li = 1 TO NUM-ENTRIES(ip-cur-val):
    IF li EQ 2 THEN lv-frm = ENTRY(2,ip-cur-val).
    ELSE
    IF li EQ 3 THEN lv-blk = ENTRY(3,ip-cur-val).
  END.

  DO WITH FRAME {&frame-name}:
    {&browse-name}:SET-REPOSITIONED-ROW(INT({&browse-name}:DOWN / 2),"always").
          
    FOR EACH tt-job-hdr
        WHERE tt-job-hdr.company EQ ip-company
          AND tt-job-hdr.job-no  EQ ip-job-no
          AND tt-job-hdr.job-no2 EQ INT(ip-job-no2)
          AND STRING(tt-job-hdr.frm,"9999999999")      +
              STRING(tt-job-hdr.blank-no,"9999999999") +
              STRING(tt-job-hdr.i-no,"x(20)") GE STRING(INT(lv-frm),"9999999999") +
                                                STRING(INT(lv-blk),"9999999999") +
                                                STRING(ENTRY(1,ip-cur-val),"x(20)")
        NO-LOCK
        BY tt-job-hdr.frm BY tt-job-hdr.blank-no BY tt-job-hdr.i-no:
      LEAVE.  
    END.

    IF AVAIL tt-job-hdr THEN
      REPOSITION {&browse-name} TO ROWID ROWID(tt-job-hdr) NO-ERROR.  
  END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li AS INT NO-UNDO.


FOR EACH tt-job-hdr:
  DELETE tt-job-hdr.
END.

FOR EACH job
    WHERE job.company EQ ip-company
      AND job.job-no  EQ ip-job-no
      AND job.job-no2 EQ INT(ip-job-no2)
    NO-LOCK:

  li = 0.

  FOR EACH job-hdr
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
      NO-LOCK
      BREAK BY job-hdr.job:

    CREATE tt-job-hdr.
    BUFFER-COPY job-hdr TO tt-job-hdr.

    IF FIRST(job-hdr.job)                    AND
       CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ job-hdr.company
                  AND itemfg.i-no    EQ job-hdr.i-no
                  AND itemfg.isaset  EQ YES
                  AND itemfg.alloc   NE YES) THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-hdr.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999")
        NO-LOCK
        BREAK BY reftable.reftable:

      IF FIRST(reftable.reftable) THEN
        ASSIGN
         tt-job-hdr.frm      = 0
         tt-job-hdr.blank-no = 0.

      CREATE tt-job-hdr.
      BUFFER-COPY job-hdr TO tt-job-hdr
      ASSIGN
       li                      = li + 1
       tt-job-hdr.j-no         = job-hdr.j-no + li
       tt-job-hdr.frm          = reftable.val[12]
       tt-job-hdr.blank-no     = reftable.val[13]
       tt-job-hdr.i-no         = reftable.code2
       tt-job-hdr.std-tot-cost = reftable.val[1]
       tt-job-hdr.std-tot-cost = reftable.val[2]
       tt-job-hdr.std-tot-cost = reftable.val[3]
       tt-job-hdr.std-tot-cost = reftable.val[4]
       tt-job-hdr.std-tot-cost = reftable.val[5].
    END.
  END.
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
  ENABLE BROWSE-1 rd-sort bt-clear lv-search bt-ok bt-cancel RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

