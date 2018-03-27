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
DEF INPUT PARAM ip-cust-no LIKE ar-inv.cust-no NO-UNDO.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-rec-val as recid no-undo.
def var lv-type-dscr as cha no-undo.

&SCOPED-DEFINE ASCDSC-1 DESC
&scoped-define FLD-NAME-1 ar-inv.inv-no
&scoped-define FLD-NAME-2 ar-inv.inv-date
&scoped-define FLD-NAME-3 ar-inv.due
&scoped-define SORTBY-1 BY {&FLD-NAME-1} {&ASCDSC-1}
&scoped-define SORTBY-2 BY {&FLD-NAME-2} {&SORTBY-1}
&scoped-define SORTBY-3 BY {&FLD-NAME-3} {&SORTBY-1}
&SCOPED-DEFINE DATATYPE-1 INT
&SCOPED-DEFINE DATATYPE-2 DATE
&SCOPED-DEFINE DATATYPE-3 DEC
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
&Scoped-define INTERNAL-TABLES ar-inv

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ar-inv.cust-no ar-inv.inv-no ~
ar-inv.inv-date ar-inv.net ar-inv.paid ar-inv.due 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ar-inv WHERE ~{&KEY-PHRASE} ~
      AND ar-inv.posted = yes and ~
ASI.ar-inv.company = ip-company and ~
ar-inv.cust-no = ip-cust-no ~
 ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH ar-inv WHERE ~{&KEY-PHRASE} ~
      AND ar-inv.posted = yes and ~
ASI.ar-inv.company = ip-company and ~
ar-inv.cust-no = ip-cust-no ~
 ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ar-inv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ar-inv


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

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Invoice#", 1,
"Invoice Date", 2,
"Balance Due", 3
     SIZE 63 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ar-inv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      ar-inv.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U WIDTH 12
      ar-inv.inv-no COLUMN-LABEL "Invoice#" FORMAT ">>>>>>>>":U
            WIDTH 12
      ar-inv.inv-date FORMAT "99/99/9999":U WIDTH 15
      ar-inv.net COLUMN-LABEL "Net" FORMAT "->>,>>>,>>9.99":U WIDTH 18
      ar-inv.paid FORMAT "->,>>>,>>9.99":U WIDTH 18
      ar-inv.due COLUMN-LABEL "Balance Due" FORMAT "->>,>>>,>>9.99":U
            WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 69
     bt-cancel AT ROW 14.1 COL 81
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(2.59) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Invoice Information".


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
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.ar-inv"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ar-inv.posted = yes and
ASI.ar-inv.company = ip-company and
ar-inv.cust-no = ip-cust-no

"
     _FldNameList[1]   > ASI.ar-inv.cust-no
"ar-inv.cust-no" "Customer#" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ar-inv.inv-no
"ar-inv.inv-no" "Invoice#" ">>>>>>>>" "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ar-inv.inv-date
"ar-inv.inv-date" ? ? "date" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ar-inv.net
"ar-inv.net" "Net" ? "decimal" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ar-inv.paid
"ar-inv.paid" ? ? "decimal" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ar-inv.due
"ar-inv.due" "Balance Due" ? "decimal" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Invoice Information */
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
   op-char-val = ar-inv.inv-no:screen-value in browse {&browse-name}
                 .
   op-rec-val = recid(ar-inv).              
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
   op-char-val = ar-inv.inv-no:screen-value in browse {&browse-name}.
   op-rec-val = recid(ar-inv).              
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
    &scoped-define where-statement ge lv-search 
       
    IF rd-sort = 3 THEN
        &SCOPED-DEFINE key-phrase ar-inv.due GE DEC(lv-search)
        
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3} 
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
        /*{srtord2.i 1}
        {srtord2.i 2} */

        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3}
       
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

/*
  &scoped-define key-phrase {&FLD-NAME-1} >= ip-cur-val
  &scoped-define sortby-phrase {&sortby-1}
*/  

  RUN enable_UI.
  
  /*{custom/lookpos.i &lookup-file = "job-hdr" &lookup-field = "job-no" }*/
  /*APPLY "value-changed" TO rd-sort IN FRAME {&FRAME-NAME}.*/

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

