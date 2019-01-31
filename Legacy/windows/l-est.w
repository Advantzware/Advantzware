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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  def var ip-company like itemfg.company init "001" no-undo.
  def var ip-loc like est.loc init "MAIN" no-undo.
  def var ip-cur-val as cha no-undo.
  def var op-char-val as cha no-undo. /* string i-code + i-name */
&else 
  def input parameter ip-company like itemfg.company no-undo.
  def input parameter ip-loc like est.loc no-undo.
  def input parameter ip-cur-val as cha no-undo.
  def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
&endif
def var lv-first-time as log init yes no-undo.
def var lv-type-dscr as cha no-undo.

DEFINE TEMP-TABLE tt-eb LIKE eb
     FIELD cust-name AS CHARACTER 
     FIELD rROWID AS ROWID  .

&scoped-define fld-name-1 tt-eb.est-no
&scoped-define fld-name-2 tt-eb.cust-no
&scoped-define fld-name-3 tt-eb.cust-name
&scoped-define fld-name-4 tt-eb.part-no

&scoped-define SORTBY-1 BY tt-eb.est-no DESC BY tt-eb.form-no BY tt-eb.blank-no
&scoped-define SORTBY-2 BY tt-eb.cust-no {&SORTBY-1}
&scoped-define SORTBY-3 BY tt-eb.cust-name {&SORTBY-1}
&scoped-define SORTBY-4 BY tt-eb.part-no {&SORTBY-1}

&global-define IAMWHAT LOOKUP

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

&SCOPED-DEFINE yellowColumnsName l-est

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
&Scoped-define INTERNAL-TABLES tt-eb 

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-eb.est-no tt-eb.cust-no tt-eb.cust-name ~
tt-eb.part-dscr1 tt-eb.part-no tt-eb.style tt-eb.len tt-eb.wid tt-eb.dep 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-eb WHERE ~{&KEY-PHRASE}  ~
           AND tt-eb.company EQ ip-company  NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH tt-eb WHERE ~{&KEY-PHRASE}  ~
           AND tt-eb.company EQ ip-company  NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-eb 
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-eb


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

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 78 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate#", 1,
"Customer#", 2,
"Customer Name", 3,
"Part#", 4
     SIZE 105 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 121 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-eb.est-no FORMAT "x(8)":U WIDTH 14 COLUMN-FONT 2 LABEL-BGCOLOR 14
      tt-eb.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U COLUMN-FONT 2
            LABEL-BGCOLOR 14
      tt-eb.cust-NAME COLUMN-LABEL "Cust Name" FORMAT "x(30)":U COLUMN-FONT 2
            LABEL-BGCOLOR 14
      tt-eb.part-dscr1 FORMAT "x(30)":U COLUMN-FONT 2 LABEL-BGCOLOR 14
      tt-eb.part-no FORMAT "x(15)":U COLUMN-FONT 2 LABEL-BGCOLOR 14
      tt-eb.style FORMAT "x(6)":U LABEL-BGCOLOR 14
      tt-eb.len FORMAT ">>>9.99999":U LABEL-BGCOLOR 14
      tt-eb.wid FORMAT ">>>9.99999":U LABEL-BGCOLOR 14
      tt-eb.dep FORMAT ">>>9.99999":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 121 BY 11.43
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     fi_sortby AT ROW 12.67 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 101
     bt-cancel AT ROW 14.1 COL 111
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(0.59) SKIP(1.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
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
       fi_sortby:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "tt-eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > tt-eb.est-no
"est-no" ? "x(8)" "character" ? ? 2 14 ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > tt-eb.cust-no
"cust-no" "Customer#" ? "character" ? ? 2 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > tt-eb.cust-name
"cust-name" "Cust Name" "x(30)" ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > tt-eb.part-dscr1
"part-dscr1" ? ? "character" ? ? 2 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > tt-eb.part-no
"part-no" ? ? "character" ? ? 2 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > tt-eb.style
"style" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > tt-eb.len
"len" ? ">>>9.99999" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > tt-eb.wid
"wid" ? ">>>9.99999" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > tt-eb.dep
"dep" ? ">>>9.99999" "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    FIND FIRST eb NO-LOCK
        WHERE eb.company EQ ip-company
          AND ROWID(eb) EQ tt-eb.rROWID NO-ERROR .
    IF AVAIL eb THEN 
        op-char-val = string(recid(eb)).
    ELSE  op-char-val = "" .
   
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
           RUN pBuildSearch .
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
    FIND FIRST eb NO-LOCK
        WHERE eb.company EQ ip-company
          AND ROWID(eb) EQ tt-eb.rROWID NO-ERROR .
    IF AVAIL eb THEN 
        op-char-val = string(recid(eb)).
    ELSE  op-char-val = "" .
   
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
    RUN pBuildSearch .       
  
    RUN case-rd-sort.
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
    RUN pBuildSearch .
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
   &scoped-define key-phrase tt-eb.loc = ip-loc
   
   /*&scoped-define key-phrase {&fld-name-2} >= ip-cur-val
   lv-search:screen-value in frame {&frame-name} = ip-cur-val.
   
   &scoped-define sortby-phrase {&sortby-1}
   &scoped-define IAMWHAT LOOKUP
   RUN enable_UI.
   
   {custom/lookpos.i &where-phrase = "eb.company = ip-company and eb.loc = ip-loc and "
       &lookup-file = "eb" &lookup-field = "est-no"} */
  
  &SCOPED-DEFINE sortby-phrase {&sortby-1}
  {custom/yellowColumns.i}


  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = ip-cur-val.
    
    ASSIGN rd-sort 
           lv-search NO-ERROR.
    RUN pBuildSearch .
    
    RUN enable_UI.

    RUN new-rd-sort.
  END.
   
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE case-rd-sort Dialog-Frame 
PROCEDURE case-rd-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
  CASE rd-sort:
    {srtord2.i 1}
    {srtord2.i 2}
    {srtord2.i 3}
    {srtord2.i 4}
  END.  /* case */

  APPLY "entry" TO {&browse-name}.
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
         
  RUN case-rd-sort.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildSearch Dialog-Frame 
PROCEDURE pBuildSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO .
    DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCust AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCustPart AS CHARACTER NO-UNDO .

 DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rd-sort.   
    IF rd-sort EQ 1 THEN
        ASSIGN cEstNo = lv-search .
    ELSE IF rd-sort EQ 2 THEN
        ASSIGN cCust = lv-search .
    ELSE IF rd-sort EQ 3 THEN
        ASSIGN cCustNo = lv-search .
    ELSE IF rd-sort EQ 4 THEN
        ASSIGN cCustPart = lv-search .
  
    EMPTY TEMP-TABLE tt-eb .
  
    
    IF rd-sort EQ 1 THEN DO:
        IF lv-search NE "" THEN
            lv-search = FILL(" ",8 - LENGTH(TRIM(lv-search))) + TRIM(lv-search) .
        FOR EACH eb WHERE eb.company EQ ip-company
               AND eb.loc EQ ip-loc 
               AND (eb.est-no BEGINS lv-search OR lv-search EQ "")
             USE-INDEX est-no NO-LOCK BY eb.est-no DESC :
            
            FIND FIRST cust OF eb NO-LOCK
                 WHERE cust.company EQ ip-company  NO-ERROR.
            CREATE tt-eb .
            BUFFER-COPY eb TO tt-eb .
            tt-eb.rROWID = ROWID(eb) .
            ASSIGN tt-eb.cust-name = IF AVAIL cust THEN cust.NAME ELSE "" .
            ASSIGN 
                iCount = iCount + 1 .
            IF iCount GE 1000 THEN LEAVE .
        END.
    END.
    ELSE IF rd-sort EQ 2 THEN DO: 
        FOR EACH eb WHERE eb.company EQ ip-company
               AND eb.loc EQ ip-loc 
               AND eb.cust-no BEGINS lv-search USE-INDEX cust
             NO-LOCK BY eb.cust-no :
            
            FIND FIRST cust OF eb NO-LOCK
                 WHERE cust.company EQ ip-company  NO-ERROR.
            CREATE tt-eb .
            BUFFER-COPY eb TO tt-eb .
            tt-eb.rROWID = ROWID(eb) .
            ASSIGN tt-eb.cust-name = IF AVAIL cust THEN cust.NAME ELSE "" .
            ASSIGN 
                iCount = iCount + 1 .
            IF iCount GE 1000 THEN LEAVE .
        END.
    END.
    ELSE IF rd-sort EQ 3 THEN DO:
        MAIN :
        FOR EACH eb WHERE eb.company EQ ip-company
               AND eb.loc EQ ip-loc
               AND eb.cust-no NE "" USE-INDEX cust  NO-LOCK, 
            
             FIRST cust OF eb NO-LOCK
                 WHERE cust.NAME BEGINS lv-search  :
           
            CREATE tt-eb .
            BUFFER-COPY eb TO tt-eb .
            tt-eb.rROWID = ROWID(eb) .
            ASSIGN tt-eb.cust-name =  cust.NAME  .
            ASSIGN 
                iCount = iCount + 1 .
            IF iCount GE 200 THEN LEAVE .
        END.
    END.
    ELSE IF rd-sort EQ 4 THEN DO: 
        FOR EACH eb WHERE eb.company EQ ip-company
               AND eb.loc EQ ip-loc 
               AND eb.part-no BEGINS lv-search USE-INDEX part
             NO-LOCK BY eb.part-no :
            
            FIND FIRST cust OF eb NO-LOCK
                 WHERE cust.company EQ ip-company  NO-ERROR.
            CREATE tt-eb .
            BUFFER-COPY eb TO tt-eb .
            tt-eb.rROWID = ROWID(eb) .
            ASSIGN tt-eb.cust-name = IF AVAIL cust THEN cust.NAME ELSE "" .
            ASSIGN 
                iCount = iCount + 1 .
            IF iCount GE 1000 THEN LEAVE .
        END.
    END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

