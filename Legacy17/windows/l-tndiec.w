&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\l-tndiec.w

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  def var ip-company like itemfg.company init "001" no-undo.
  def var ip-loc like est.loc init "MAIN" no-undo.
  def var ip-cust-no like eb.cust-no no-undo.
  def var ip-est-type like eb.est-type no-undo.
  def var ip-cur-val as cha no-undo.
  def var op-char-val as cha no-undo. /* string i-code + i-name */
&else 
  def input parameter ip-company like itemfg.company no-undo.
  def input parameter ip-loc like est.loc no-undo.
  def input parameter ip-cust-no like eb.cust-no no-undo.
  def input parameter ip-est-type like eb.est-type no-undo.
  def input parameter ip-cur-val as cha no-undo.
  def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
&endif
def var lv-first-time as log init yes no-undo.
def var lv-type-dscr as cha no-undo.
&scoped-define fld-name-1 eb.die-no
&scoped-define fld-name-2 eb.est-no
&scoped-define fld-name-3 eb.part-no
&scoped-define SORTBY-1 BY eb.die-no
&scoped-define SORTBY-2 BY eb.est-no DESC {&sortby-1}
&scoped-define SORTBY-3 BY eb.part-no BY eb.est-no DESC {&sortby-1}

&global-define IAMWHAT LOOKUP

{sa/sa-sls01.i}

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
&Scoped-define INTERNAL-TABLES eb est report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 eb.die-no eb.est-no eb.cust-no ~
eb.part-dscr1 eb.part-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company  ~
and eb.loc = ip-loc and ~
eb.cust-no = ip-cust-no and ~
TRIM(eb.master-est-no) = "" and ~
(eb.est-type = (ip-est-type - 3) or ~
 eb.est-type = ip-est-type) and ~
eb.die-no gt "" NO-LOCK, ~
      FIRST est OF eb NO-LOCK, ~
      FIRST report WHERE TRUE /* Join to est incomplete */ ~
      AND report.term-id eq v-term and ~
report.key-01 eq est.est-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company  ~
and eb.loc = ip-loc and ~
eb.cust-no = ip-cust-no and ~
TRIM(eb.master-est-no) = "" and ~
(eb.est-type = (ip-est-type - 3) or ~
 eb.est-type = ip-est-type) and ~
eb.die-no gt "" NO-LOCK, ~
      FIRST est OF eb NO-LOCK, ~
      FIRST report WHERE TRUE /* Join to est incomplete */ ~
      AND report.term-id eq v-term and ~
report.key-01 eq est.est-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 eb est report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 eb
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 est
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 report


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
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Die#", 1,
"Estimate#", 2,
"Part#", 3
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 103 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      eb, 
      est, 
      report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      eb.die-no FORMAT "x(15)":U WIDTH 22
      eb.est-no FORMAT "x(8)":U WIDTH 14
      eb.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
      eb.part-dscr1 FORMAT "x(30)":U
      eb.part-no FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 11.43
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 82
     bt-cancel AT ROW 14.1 COL 92
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 12.67 COL 4
     SPACE(92.19) SKIP(1.75)
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
     _TblList          = "ASI.eb,ASI.est OF ASI.eb,ASI.report WHERE ASI.est ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST, FIRST"
     _Where[1]         = "ASI.eb.company = ip-company 
and eb.loc = ip-loc and
eb.cust-no = ip-cust-no and
TRIM(eb.master-est-no) = """" and
(eb.est-type = (ip-est-type - 3) or
 eb.est-type = ip-est-type) and
eb.die-no gt """""
     _Where[3]         = "report.term-id eq v-term and
report.key-01 eq est.est-no"
     _FldNameList[1]   > ASI.eb.die-no
"eb.die-no" ? ? "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" ""
     _FldNameList[2]   > ASI.eb.est-no
"eb.est-no" ? "x(8)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[3]   > ASI.eb.cust-no
"eb.cust-no" "Customer#" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = ASI.eb.part-dscr1
     _FldNameList[5]   = ASI.eb.part-no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Estimate Information */
DO:
  RUN clear-report.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Information */
DO:
  RUN clear-report.
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
   op-char-val = string(recid(eb)).
   
   apply "window-close" to frame {&frame-name}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel Dialog-Frame
ON CHOOSE OF bt-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  RUN clear-report.
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
   op-char-val = string(recid(eb)).
   
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
    /*
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
    end.      
    */
    /* srtord2.i    resort include  same as srtord.i but use begins instead of >= and error */        


CASE rd-sort:

WHEN 1 THEN DO:
    &IF "{&IAMWHAT}" = "LOOKUP" &THEN
         &scoped-define sortby-phrase {&sortby-1}
    &ElseIF  "{&IAMWHAT}" = "Search" &THEN
      &scoped-define where-statement begins {&datatype-1}(lv-search)
      &scoped-define key-phrase {&datatype-1}({&fld-name-1}) {&Where-statement}
    &endif

      {&open-query-{&browse-name}}

      IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
         DO:
            MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
            VIEW-AS ALERT-BOX.
        /*    lv-search:screen-value = "".  */
             APPLY "ENTRY" TO {&BROWSE-NAME}.
        end.    
END.

WHEN 2 THEN DO:
 
  OPEN QUERY {&browse-name} FOR EACH ASI.eb WHERE TRUE 
                            AND ASI.eb.company = ip-company 
                            AND eb.loc = ip-loc
                            AND eb.cust-no = ip-cust-no
                            AND (eb.est-type = ip-est-type - 3 OR
                                eb.est-type = ip-est-type)
                            AND trim(eb.est-no) BEGINS lv-search  NO-LOCK,
                            FIRST est OF eb NO-LOCK,
                            FIRST report WHERE report.term-id EQ v-term
                                           AND report.key-01 EQ est.est-no.

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
     DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME}.
     end.    
END.

WHEN 3 THEN DO:
    &IF "{&IAMWHAT}" = "LOOKUP" &THEN
         &scoped-define sortby-phrase {&sortby-3}
    &ElseIF  "{&IAMWHAT}" = "Search" &THEN
      &scoped-define where-statement begins {&datatype-3}(lv-search)
      &scoped-define key-phrase {&datatype-3}({&fld-name-3}) {&Where-statement}
    &endif

      {&open-query-{&browse-name}}

      IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
         DO:
            MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
            VIEW-AS ALERT-BOX.
        /*    lv-search:screen-value = "".  */
             APPLY "ENTRY" TO {&BROWSE-NAME}.
        end.    
END.

END.  /* case */
  apply "entry" to {&browse-name}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    &scoped-define IAMWHAT lookup
    assign rd-sort.
  
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
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
  
  /* &scoped-define key-phrase {&fld-name-2} >= ip-cur-val
   lv-search:screen-value in frame {&frame-name} = ip-cur-val.
   */
   &scoped-define sortby-phrase {&sortby-1}
   &scoped-define IAMWHAT LOOKUP

   RUN build-report.

   RUN enable_UI.
   /*{custom/lookposL.i &lookup-file = "eb" &lookup-field = "est-no"}  */

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report Dialog-Frame 
PROCEDURE build-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-tandem AS LOG NO-UNDO.


  RUN clear-report.
  
  FOR EACH eb
      WHERE {&KEY-PHRASE}
        AND eb.company   EQ ip-company
        AND eb.loc       EQ ip-loc
        AND eb.cust-no   BEGINS ip-cust-no
        AND TRIM(eb.master-est-no) EQ ""
        AND (eb.est-type EQ (ip-est-type - 3) OR eb.est-type EQ ip-est-type)
        AND eb.die-no    GT ""
      USE-INDEX cust NO-LOCK,
      FIRST est OF eb WHERE est.e-num EQ 0 NO-LOCK
      BREAK BY eb.est-no:

    IF LAST-OF(eb.est-no) THEN DO:
      IF eb.est-type EQ (ip-est-type - 3) THEN ll-tandem = YES.
      ELSE RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

      IF ll-tandem THEN DO:
        CREATE report.
        ASSIGN
         report.term-id = v-term
         report.key-01  = est.est-no.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-report Dialog-Frame 
PROCEDURE clear-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH report WHERE report.term-id EQ v-term:
    DELETE report.
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

