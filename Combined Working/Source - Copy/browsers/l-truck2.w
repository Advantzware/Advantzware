&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: browsers\l-truck2.w

  Description: Truck Browser
   
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input parameter ip-company like itemfg.company no-undo.
DEF INPUT PARAMETER ip-loc AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-carrier AS CHAR NO-UNDO.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */

&scoped-define SORTBY-1 BY truck.truck-code
&SCOPED-DEFINE SORTBY-2 BY truck.truck-desc
&SCOPED-DEFINE SORTBY-3 BY truck.loc
&SCOPED-DEFINE SORTBY-4 BY truck.carrier
&scoped-define fld-name-1 truck.truck-code
&scoped-define fld-name-2 truck.truck-desc
&scoped-define fld-name-3 truck.loc
&scoped-define fld-name-4 truck.carrier
&scoped-define datatype-1 STRING
&scoped-define datatype-2 STRING
&scoped-define datatype-3 STRING
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
&Scoped-define INTERNAL-TABLES truck

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 truck.truck-code truck.truck-desc ~
truck.loc truck.carrier truck.length-inches truck.width-inches ~
truck.height-inches 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH truck WHERE ~{&KEY-PHRASE} ~
      AND truck.company = ip-company and ~
ASI.truck.loc = ip-loc and ~
ASI.truck.carrier = ip-carrier NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH truck WHERE ~{&KEY-PHRASE} ~
      AND truck.company = ip-company and ~
ASI.truck.loc = ip-loc and ~
ASI.truck.carrier = ip-carrier NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 truck
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 truck


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
          "Truck Code", 1,
"Description", 2,
"Location", 3,
"Carrier", 4
     SIZE 53 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      truck SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      truck.truck-code FORMAT "X(10)":U
      truck.truck-desc FORMAT "X(25)":U
      truck.loc FORMAT "x(5)":U
      truck.carrier FORMAT "x(5)":U WIDTH 8.2
      truck.length-inches FORMAT ">>9":U
      truck.width-inches FORMAT ">>9":U
      truck.height-inches FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 11.19
         BGCOLOR 8 .


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
     SPACE(0.59) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Truck Information".


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
     _TblList          = "ASI.truck"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST, FIRST"
     _Where[1]         = "ASI.truck.company = ip-company and
ASI.truck.loc = ip-loc and
ASI.truck.carrier = ip-carrier"
     _FldNameList[1]   = ASI.truck.truck-code
     _FldNameList[2]   = ASI.truck.truck-desc
     _FldNameList[3]   = ASI.truck.loc
     _FldNameList[4]   > ASI.truck.carrier
"truck.carrier" ? ? "character" ? ? ? ? ? ? no ? no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ASI.truck.length-inches
     _FldNameList[6]   = ASI.truck.width-inches
     _FldNameList[7]   = ASI.truck.height-inches
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Truck Information */
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
   op-char-val = truck.truck-code:screen-value in browse {&browse-name} + ","
               + truck.loc:screen-value in browse {&browse-name} + ","
               + truck.carrier:SCREEN-VALUE IN BROWSE {&browse-name}.
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
        {srtord.i 3}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = truck.truck-code:SCREEN-VALUE IN BROWSE {&browse-name} + "," 
               + truck.loc:screen-value in browse {&browse-name} + ","
               + truck.carrier:screen-value in browse {&browse-name}.
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
   /* &scoped-define where-statement >= (lv-search)*/
    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
        {srtord.i 3}
        {srtord.i 4}
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

  

 /* &scoped-define key-phrase {&fld-name-1} >= (ip-cur-val) */
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
  ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

