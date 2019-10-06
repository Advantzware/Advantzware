&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&SCOPED-DEFINE WINDOW-NAME CURRENT-WINDOW
&SCOPED-DEFINE FRAME-NAME Dialog-Frame
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
DEFINE INPUT PARAMETER ip-company LIKE itemfg.company NO-UNDO.
DEFINE INPUT PARAMETER ip-cur-val AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-cur-val2 AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-char-val AS CHARACTER NO-UNDO. /* string i-code + i-name */

/* Local Variable Definitions ---                                       */
def var lv-type-dscr AS CHARACTER NO-UNDO.
def var lv-first-time AS LOGICAL INITIAL YES NO-UNDO.
&SCOPED-DEFINE SORTBY-1 BY cust.cust-no
&SCOPED-DEFINE SORTBY-2 BY cust.name {&sortby-1}
&SCOPED-DEFINE fld-name-1 cust.cust-no
&SCOPED-DEFINE fld-name-2 cust.name
&global-define IAMWHAT LOOKUP
&SCOPED-DEFINE useMatches

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.
DEF VAR lActive AS LOGICAL NO-UNDO.
DEF VAR v-check-page AS LOGICAL INITIAL NO NO-UNDO .
DEFINE VARIABLE ou-log like sys-ctrl.log-fld INIT NO NO-UNDO.
DEFINE VARIABLE ou-cust-int AS INTEGER NO-UNDO .
DEF VAR v-file-name AS CHARACTER NO-UNDO .

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

ASSIGN cocode = ip-company .

IF  PROGRAM-NAME(2) MATCHES "*/b-itemfg.w*"  OR PROGRAM-NAME(2) MATCHES "*/b-estq.w*" OR
    PROGRAM-NAME(2) MATCHES "*/b-estq1.w*" OR PROGRAM-NAME(2) MATCHES "*/b-qthd.w*" OR
    PROGRAM-NAME(2) MATCHES "*/b-jobinq.w*" OR PROGRAM-NAME(2) MATCHES "*/b-ordinq.w*" OR
    PROGRAM-NAME(2) MATCHES "*/b-estitm.w*" OR PROGRAM-NAME(2) MATCHES "*/b-relinq.w*" OR
    PROGRAM-NAME(2) MATCHES "*oerep/r-relprt.w*" OR PROGRAM-NAME(2) MATCHES "*/oe-relh_.w*" THEN
    v-check-page = YES .

DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    /*{sys/inc/custlistform.i ""IF1"" }*/
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE DIALOG-BOX
&SCOPED-DEFINE DB-AWARE NO 

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME Dialog-Frame
&SCOPED-DEFINE BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&SCOPED-DEFINE INTERNAL-TABLES cust sman

/* Define KEY-PHRASE in case it is used by any query. */
&SCOPED-DEFINE KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&SCOPED-DEFINE FIELDS-IN-QUERY-BROWSE-1 cust.cust-no cust.name cust.city ~
cust.state cust.zip cust.type cust.sman sman.territory 
&SCOPED-DEFINE ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&SCOPED-DEFINE QUERY-STRING-BROWSE-1 FOR EACH cust WHERE ~{&KEY-PHRASE} ~
      AND cust.company EQ ip-company ~
      AND ((v-check-page AND ( LOOKUP(cust.cust-no,custcount) NE 0 OR custcount = "")) OR NOT v-check-page) NO-LOCK, ~
      FIRST sman OF cust OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&SCOPED-DEFINE OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH cust WHERE ~{&KEY-PHRASE} ~
      AND cust.company = ip-company ~
      AND ((v-check-page AND ( lookup(cust.cust-no,custcount) <> 0 OR custcount = "")) OR NOT v-check-page) NO-LOCK, ~
      FIRST sman OF cust OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&SCOPED-DEFINE TABLES-IN-QUERY-BROWSE-1 cust sman
&SCOPED-DEFINE FIRST-TABLE-IN-QUERY-BROWSE-1 cust
&SCOPED-DEFINE SECOND-TABLE-IN-QUERY-BROWSE-1 sman


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&SCOPED-DEFINE OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort bt-clear lv-search ~
bt-ok bt-cancel 
&SCOPED-DEFINE DISPLAYED-OBJECTS rd-sort lv-search 

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
     SIZE 96 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cust #", 1,
"Name", 2
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      cust, 
      sman SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      cust.cust-no FORMAT "x(8)":U COLUMN-FONT 0
      cust.name FORMAT "x(30)":U COLUMN-FONT 0
      cust.city FORMAT "x(15)":U COLUMN-FONT 0
      cust.state FORMAT "x(2)":U COLUMN-FONT 0
      cust.zip FORMAT "x(10)":U COLUMN-FONT 0
      cust.type FORMAT "x(8)":U
      cust.sman COLUMN-LABEL "SalesRep" FORMAT "x(3)":U
      sman.territory FORMAT "x(3)":U WIDTH 22.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 140 BY 11.52
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COLUMN 1
     rd-sort AT ROW 12.67 COLUMN 14 NO-LABEL
     bt-clear AT ROW 14.1 COLUMN 2
     lv-search AT ROW 14.1 COLUMN 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COLUMN 119
     bt-cancel AT ROW 14.1 COLUMN 130
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COLUMN 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(1.39) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Customer Information".


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
/* BROWSE-TAB BROWSE-1 TEXT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.cust,ASI.sman OF ASI.cust"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[1]         = "ASI.cust.company = ip-company"
     _FldNameList[1]   > ASI.cust.cust-no
"cust.cust-no" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.cust.name
"cust.name" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.cust.city
"cust.city" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.cust.state
"cust.state" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.cust.zip
"cust.zip" ? ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = ASI.cust.type
     _FldNameList[7]   > ASI.cust.sman
"cust.sman" "SalesRep" ? "character" ? ? ? ? ? ? no "Enter initials or code salesrep that handles this customer" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.sman.territory
"sman.territory" ? ? "character" ? ? ? ? ? ? no ? no no "22.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&SCOPED-DEFINE SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Customer Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE BROWSE-NAME BROWSE-1
&SCOPED-DEFINE SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   IF lv-first-time THEN ASSIGN lv-search:SCREEN-VALUE = ""
                                lv-first-time = NO.
                                
   lv-search:SCREEN-VALUE = lv-search:SCREEN-VALUE + KEYLABEL(LASTKEY).
   APPLY "leave" TO lv-search.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-char-val = cust.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} + "," +
                 cust.name:SCREEN-VALUE IN BROWSE {&browse-name}
                 .
   APPLY "window-close" TO FRAME {&FRAME-NAME}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    ASSIGN lv-search:SCREEN-VALUE = "".
           lv-search = "".
    CASE rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
    END.
        APPLY "entry" TO {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = cust.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} + "," +
                 cust.name:SCREEN-VALUE IN BROWSE {&browse-name}
                 .
   APPLY "window-close" TO FRAME {&FRAME-NAME}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
OR RETURN OF lv-search
DO:
    ASSIGN rd-sort 
           lv-search.
    &SCOPED-DEFINE IAMWHAT SEARCH
    &SCOPED-DEFINE fld-name-1 cust.cust-no
    &SCOPED-DEFINE fld-name-2 cust.name
    &SCOPED-DEFINE where-statement begins lv-search
    CASE rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}  
/*
         WHEN 1 THEN DO:
              &SCOPED-DEFINE key-phrase {&fld-name-1} {&Where-statement}
              {&open-query-{&browse-name}}
         END.
         WHEN 2 THEN DO:
              &SCOPED-DEFINE key-phrase {&fld-name-2} {&Where-statement}
              {&open-query-{&browse-name}}
         END.
 */        
    END.   
        
 /*
    IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
    DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
        lv-search:SCREEN-VALUE = "".
    end.    
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME rd-sort
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

IF  PROGRAM-NAME(2) MATCHES "*/b-itemfg.w*" THEN DO:
    v-file-name  = "IF1" .
    RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'IF1',
                            INPUT YES,
                            OUTPUT lActive).
END.
 IF PROGRAM-NAME(2) MATCHES "*ce/b-estq.w*" OR 
      PROGRAM-NAME(2) MATCHES "*ce/b-estq1.w*" THEN DO:
     v-file-name  = "EF" .
     RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EF',
                            INPUT YES,
                            OUTPUT lActive).
 END.
 IF PROGRAM-NAME(2) MATCHES "*cec/b-estq.w*" OR PROGRAM-NAME(2) MATCHES "*cec/b-estitm.w*"  THEN DO:
     v-file-name  = "EC" .
     RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EC',
                            INPUT YES,
                            OUTPUT lActive).
 END.
 IF PROGRAM-NAME(2) MATCHES "*/b-qthd.w*"  THEN DO:
     v-file-name  = "EQ" .
     RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'EQ',
                            INPUT YES,
                            OUTPUT lActive).
 END.
 IF PROGRAM-NAME(2) MATCHES "*oeinq/b-ordinq.w*"  THEN DO:
     v-file-name  = "OQ1" .
     RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OQ1',
                            INPUT YES,
                            OUTPUT lActive).
 END.
 IF PROGRAM-NAME(2) MATCHES "*oe/b-ordinq.w*"  THEN DO:
     v-file-name  = "OU1" .
     RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OU1',
                            INPUT YES,
                            OUTPUT lActive).
 END.
 IF PROGRAM-NAME(2) MATCHES "*oe/b-relinq.w*" OR PROGRAM-NAME(2) MATCHES "*oerep/r-relprt.w*" 
      OR PROGRAM-NAME(2) MATCHES "*/oe-relh_.w*" THEN DO:
     v-file-name  = "OT1" .
     RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OT1',
                            INPUT YES,
                            OUTPUT lActive).
 END.

 IF PROGRAM-NAME(2) MATCHES "*/b-jobinq.w*"  THEN DO:
     IF ip-cur-val2 = "JQ1" THEN do:
         v-file-name  = "JQ1" .
         RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'JQ1',
                            INPUT YES,
                            OUTPUT lActive).
     END.
     ELSE do:
         v-file-name  = "JU1" .
         RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'JU1',
                            INPUT YES,
                            OUTPUT lActive).
     END.
 END.
/*{sys/inc/custlistform.i "v-file-name" }*/
  RUN sys/inc/custlistform.p (INPUT v-file-name , INPUT cocode , OUTPUT ou-log , OUTPUT ou-cust-int) .
{sys/inc/chblankcust.i "v-file-name"}
    IF ou-cust-int = 0 THEN
        custcount = "".

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = "".
    ASSIGN rd-sort NO-ERROR.

    RUN enable_UI.

    RUN new-rd-sort.
    &SCOPED-DEFINE key-phrase ((v-check-page AND ( LOOKUP(cust.cust-no,custcount) NE 0 OR custcount = "")) OR NOT v-check-page)
    {custom/lookpos3.i &lookup-file = "cust" &lookup-field = "cust-no"}
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
  &SCOPED-DEFINE IAMWHAT LOOKUP   
         
  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN rd-sort.
    CASE rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
    END. 
  END.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


