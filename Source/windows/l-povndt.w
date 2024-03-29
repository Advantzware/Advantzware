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
DEF INPUT PARAM ip-company LIKE po-ord.company NO-UNDO.
DEF INPUT PARAM ip-vend-no LIKE po-ord.vend-no NO-UNDO.
DEF INPUT PARAM ip-du-date LIKE po-ord.due-date NO-UNDO.
DEF PARAM BUFFER io-po-ord FOR po-ord.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

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

{sys/inc/var.i new shared}

cocode = ip-company.

&scoped-define ascdsc-1 DESC
&scoped-define ascdsc-3 DESC
&scoped-define sortby-1 BY po-ord.po-no {&ascdsc-1}
&scoped-define sortby-2 BY po-ord.vend-no {&sortby-1}
&scoped-define sortby-3 BY po-ord.po-date {&ascdsc-1} {&sortby-1} 
&scoped-define fld-name-1 po-ord.po-no
&scoped-define fld-name-2 po-ord.vend-no 
&scoped-define fld-name-3 po-ord.po-date 
&scoped-define datatype-1 INT
&scoped-define datatype-3 DATE

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
&Scoped-define INTERNAL-TABLES po-ord po-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 po-ord.po-no po-ord.vend-no ~
po-ord.po-date po-ord.due-date po-ord.ship-id po-ordl.i-no po-ordl.i-name ~
po-ordl.job-no po-ordl.job-no2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH po-ord WHERE ~{&KEY-PHRASE} ~
      AND po-ord.company  = ip-company AND ~
ASI.po-ord.opened   = yes        AND ~
ASI.po-ord.vend-no  = ip-vend-no AND ~
ASI.po-ord.due-date = ip-du-date NO-LOCK, ~
      FIRST po-ordl WHERE po-ordl.company = po-ord.company AND ~
ASI.po-ordl.po-no   = po-ord.po-no ~
      AND po-ordl.deleted = no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH po-ord WHERE ~{&KEY-PHRASE} ~
      AND po-ord.company  = ip-company AND ~
ASI.po-ord.opened   = yes        AND ~
ASI.po-ord.vend-no  = ip-vend-no AND ~
ASI.po-ord.due-date = ip-du-date NO-LOCK, ~
      FIRST po-ordl WHERE po-ordl.company = po-ord.company AND ~
ASI.po-ordl.po-no   = po-ord.po-no ~
      AND po-ordl.deleted = no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 po-ord po-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 po-ord
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 po-ordl


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
          "PO#", 1,
"Vendor#", 2,
"PO Date", 3
     SIZE 62 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      po-ord, 
      po-ordl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      po-ord.po-no COLUMN-LABEL "PO#" FORMAT ">>>>>>>>":U WIDTH 12
      po-ord.vend-no FORMAT "x(8)":U
      po-ord.po-date FORMAT "99/99/9999":U
      po-ord.due-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
      po-ord.ship-id FORMAT "x(8)":U WIDTH 12
      po-ordl.i-no FORMAT "x(15)":U WIDTH 22
      po-ordl.i-name FORMAT "x(30)":U
      po-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(9)":U WIDTH 15
      po-ordl.job-no2 COLUMN-LABEL "" FORMAT "999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 145 BY 11.19
         BGCOLOR 8 .


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
     SPACE(134.79) SKIP(2.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Choose PO to Update...".


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
     _TblList          = "ASI.po-ord,ASI.po-ordl WHERE ASI.po-ord  ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "ASI.po-ord.company  = ip-company AND
ASI.po-ord.opened   = yes        AND
ASI.po-ord.vend-no  = ip-vend-no AND
ASI.po-ord.due-date = ip-du-date"
     _JoinCode[2]      = "ASI.po-ordl.company = po-ord.company AND
ASI.po-ordl.po-no   = po-ord.po-no"
     _Where[2]         = "ASI.po-ordl.deleted = no"
     _FldNameList[1]   > ASI.po-ord.po-no
"po-ord.po-no" "PO#" ">>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   = ASI.po-ord.vend-no
     _FldNameList[3]   = ASI.po-ord.po-date
     _FldNameList[4]   > ASI.po-ord.due-date
"po-ord.due-date" "Due Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.po-ord.ship-id
"po-ord.ship-id" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[6]   > ASI.po-ordl.i-no
"po-ordl.i-no" ? ? "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" ""
     _FldNameList[7]   = ASI.po-ordl.i-name
     _FldNameList[8]   > ASI.po-ordl.job-no
"po-ordl.job-no" "Job#" ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[9]   > ASI.po-ordl.job-no2
"po-ordl.job-no2" "" "999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Choose PO to Update... */
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
  RUN select-record.
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
    end.
    APPLY "entry" TO BROWSE {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
  RUN select-record.      
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

    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3} 
    end.      
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

  /*&scoped-define key-phrase {&fld-name-1} >= (ip-cur-val)*/
  &scoped-define sortby-phrase {&sortby-1}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search:SCREEN-VALUE = "".
  END.
  
  RUN enable_UI.

  RUN new-rd-sort.

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
        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3}
    end.    
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  APPLY 'entry' TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-record Dialog-Frame 
PROCEDURE select-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF AVAIL po-ord THEN
  FIND io-po-ord WHERE ROWID(io-po-ord) EQ ROWID(po-ord) NO-LOCK NO-ERROR.
  APPLY "go" TO FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

