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
DEF INPUT PARAM ip-item-type AS LOG NO-UNDO.
def input parameter ip-cur-val as cha no-undo.
DEF OUTPUT PARAM op-chaval AS cha NO-UNDO.
def output parameter op-recid AS RECID no-undo. /* string i-code + i-name */
DEF VAR lv-first-time AS LOG NO-UNDO.
&scoped-define fld-name-1 loadtag.i-no
&scoped-define fld-name-2 loadtag.i-name
&scoped-define SORTBY-1 BY loadtag.i-no
&scoped-define SORTBY-2 BY loadtag.i-name
&global-define IAMWHAT LOOKUP

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
&Scoped-define INTERNAL-TABLES loadtag fg-rctd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 loadtag.tag-no loadtag.i-no ~
loadtag.i-name loadtag.po-no loadtag.ord-no loadtag.job-no loadtag.job-no2 ~
loadtag.loc loadtag.loc-bin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH loadtag WHERE ~{&KEY-PHRASE} ~
      AND loadtag.company = ip-company ~
and loadtag.item-type = ip-item-type NO-LOCK, ~
      EACH fg-rctd WHERE fg-rctd.company = loadtag.company ~
  AND fg-rctd.loc = loadtag.loc  ~
  AND fg-rctd.tag = loadtag.tag-no ~
      AND fg-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH loadtag WHERE ~{&KEY-PHRASE} ~
      AND loadtag.company = ip-company ~
and loadtag.item-type = ip-item-type NO-LOCK, ~
      EACH fg-rctd WHERE fg-rctd.company = loadtag.company ~
  AND fg-rctd.loc = loadtag.loc  ~
  AND fg-rctd.tag = loadtag.tag-no ~
      AND fg-rctd.rita-code = "R" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 loadtag fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 loadtag
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 fg-rctd


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
     SIZE 96 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item #", 1,
"Name", 2
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 140 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      loadtag, 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      loadtag.tag-no FORMAT "X(20)":U
      loadtag.i-no FORMAT "x(15)":U
      loadtag.i-name FORMAT "x(30)":U
      loadtag.po-no FORMAT ">>>>>9":U
      loadtag.ord-no FORMAT ">>>>>9":U
      loadtag.job-no FORMAT "x(6)":U
      loadtag.job-no2 FORMAT ">9":U
      loadtag.loc FORMAT "x(5)":U
      loadtag.loc-bin FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 140 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 119
     bt-cancel AT ROW 14.1 COL 130
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(1.39) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Received Loadtag  Information".


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
     _TblList          = "ASI.loadtag,ASI.fg-rctd WHERE ASI.loadtag ... ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.loadtag.company = ip-company
and loadtag.item-type = ip-item-type"
     _JoinCode[2]      = "ASI.fg-rctd.company = ASI.loadtag.company
  AND ASI.fg-rctd.loc = ASI.loadtag.loc 
  AND ASI.fg-rctd.tag = ASI.loadtag.tag-no"
     _Where[2]         = "ASI.fg-rctd.rita-code = ""R"""
     _FldNameList[1]   = ASI.loadtag.tag-no
     _FldNameList[2]   = ASI.loadtag.i-no
     _FldNameList[3]   = ASI.loadtag.i-name
     _FldNameList[4]   = ASI.loadtag.po-no
     _FldNameList[5]   = ASI.loadtag.ord-no
     _FldNameList[6]   = ASI.loadtag.job-no
     _FldNameList[7]   = ASI.loadtag.job-no2
     _FldNameList[8]   = ASI.loadtag.loc
     _FldNameList[9]   = ASI.loadtag.loc-bin
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Received Loadtag  Information */
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
    op-chaval = loadtag.tag-no.
    op-recid = RECID(loadtag).
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
    end.
        apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-chaval = loadtag.tag-no.
   op-recid = RECID(loadtag).
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
/*
         WHEN 1 THEN DO:
              &scoped-define key-phrase {&fld-name-1} {&Where-statement}
              {&open-query-{&browse-name}}
         END.
         WHEN 2 THEN DO:
              &scoped-define key-phrase {&fld-name-2} {&Where-statement}
              {&open-query-{&browse-name}}
         END.
 */        
    end.      
 /*
    IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
    DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
        lv-search:screen-value = "".
    end.    
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
       
    assign rd-sort.
    case rd-sort:
        {srtord2.i 1}
        {srtord2.i 2}
    end.    
    apply "entry" to {&browse-name}.
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
  

  &scoped-define key-phrase {&fld-name-1} >= ip-cur-val  
  lv-search = ip-cur-val.
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
  ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

