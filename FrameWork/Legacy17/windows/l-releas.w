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
/*def input param ip-cust-no like oe-ord.cust-no no-undo.
def input param ip-ship-id like oe-relh.ship-id no-undo.
*/
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-rec-id as recid no-undo.     /* recid output */
&scoped-define SORTBY-1 BY oe-relh.release# descending
&scoped-define SORTBY-2 BY oe-relh.cust-no 
&scoped-define fld-name-1 oe-relh.release#
&scoped-define fld-name-2 oe-relh.cust-no 
&scoped-define datatype-1 int
/*&scoped-define datatype-2 date */

&scoped-define IAMWHAT LOOKUP

def var lv-ord-ok as cha init "R,I,S,P,A,N,U" no-undo.

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
&Scoped-define INTERNAL-TABLES oe-relh oe-ord

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 oe-relh.release# oe-relh.rel-date ~
oe-relh.ord-no oe-relh.cust-no oe-relh.ship-id oe-relh.po-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH oe-relh WHERE ~{&KEY-PHRASE} ~
      AND oe-relh.company eq ip-company ~
  and  oe-relh.posted  eq yes ~
  and  oe-relh.printed eq yes ~
  and  oe-relh.deleted eq no ~
  NO-LOCK, ~
      FIRST oe-ord WHERE     oe-ord.company eq oe-relh.company ~
and oe-ord.ord-no  eq oe-relh.ord-no ~
and oe-ord.stat    ne "C" ~
and oe-ord.stat    ne "D" ~
                               and oe-ord.stat    ne "Z" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH oe-relh WHERE ~{&KEY-PHRASE} ~
      AND oe-relh.company eq ip-company ~
  and  oe-relh.posted  eq yes ~
  and  oe-relh.printed eq yes ~
  and  oe-relh.deleted eq no ~
  NO-LOCK, ~
      FIRST oe-ord WHERE     oe-ord.company eq oe-relh.company ~
and oe-ord.ord-no  eq oe-relh.ord-no ~
and oe-ord.stat    ne "C" ~
and oe-ord.stat    ne "D" ~
                               and oe-ord.stat    ne "Z" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 oe-relh oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 oe-relh
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 oe-ord


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
          "Release#", 1,
"Customer#", 2
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      oe-relh, 
      oe-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      oe-relh.release# FORMAT "->,>>>,>>9":U
      oe-relh.rel-date FORMAT "99/99/9999":U
      oe-relh.ord-no FORMAT ">>>>>9":U
      oe-relh.cust-no FORMAT "x(8)":U
      oe-relh.ship-id FORMAT "x(8)":U
      oe-relh.po-no FORMAT "x(15)":U
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
         TITLE "Release Information".


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
     _TblList          = "ASI.oe-relh,ASI.oe-ord WHERE ASI.oe-relh ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "oe-relh.company eq ip-company
  and  oe-relh.posted  eq yes
  and  oe-relh.printed eq yes
  and  oe-relh.deleted eq no
 "
     _JoinCode[2]      = "    oe-ord.company eq oe-relh.company
and oe-ord.ord-no  eq oe-relh.ord-no
and oe-ord.stat    ne ""C""
and oe-ord.stat    ne ""D""
                               and oe-ord.stat    ne ""Z"""
     _FldNameList[1]   = ASI.oe-relh.release#
     _FldNameList[2]   = ASI.oe-relh.rel-date
     _FldNameList[3]   = ASI.oe-relh.ord-no
     _FldNameList[4]   = ASI.oe-relh.cust-no
     _FldNameList[5]   = ASI.oe-relh.ship-id
     _FldNameList[6]   = ASI.oe-relh.po-no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Release Information */
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
   op-char-val = string(oe-ord.ord-no,">>>>>9") + "," +
                 oe-ord.cust-no
                 .
   op-rec-id = recid(oe-ord).                 
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
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = string(oe-ord.ord-no,">>>>>9") + "," +
                 oe-ord.cust-no
                 .
   op-rec-id = recid(oe-ord).                 

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

    case rd-sort:
        {srtord.i 1}
        {srtord.i 2} 
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
        {srtord.i 1}
        {srtord.i 2}
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

  

  /*&scoped-define key-phrase {&fld-name-1} >= (ip-cur-val)*/
  &scoped-define sortby-phrase {&sortby-1}
  
  RUN enable_UI.
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

