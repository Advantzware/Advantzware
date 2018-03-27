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
  def var ip-company LIKE eb.company init "001" no-undo.
  def var ip-loc like eb.loc init "MAIN" no-undo.
  DEF VAR ip-est-no LIKE eb.est-no NO-UNDO.
  def var ip-cur-val as ROWID no-undo. /* string i-code + i-name */
&else 
  def input parameter ip-company like itemfg.company no-undo.
  def input parameter ip-loc like est.loc no-undo.
  DEF INPUT PARAMETER ip-est-no LIKE est.est-no NO-UNDO.
  def INPUT-OUTPUT parameter ip-cur-val as ROWID no-undo.
&endif
def var lv-first-time as log init yes no-undo.
def var lv-type-dscr as cha no-undo.
&scoped-define fld-name-1 STRING(eb.form-no)
&scoped-define fld-name-2 STRING(eb.blank-no)
&scoped-define fld-name-3 eb.part-no
&scoped-define fld-name-4 eb.part-dscr1
&scoped-define fld-name-5 eb.stock-no
&scoped-define SORTBY-1 BY eb.form-no BY eb.blank-no
&scoped-define SORTBY-2 BY eb.blank-no BY eb.form-no
&scoped-define SORTBY-3 BY eb.part-no BY eb.form-no BY eb.blank-no
&scoped-define SORTBY-4 BY eb.part-dscr1 BY eb.form-no BY eb.blank-no
&scoped-define SORTBY-5 BY eb.stock-no BY eb.form-no BY eb.blank-no

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
&Scoped-define INTERNAL-TABLES eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 eb.form-no eb.blank-no eb.part-no ~
eb.part-dscr1 eb.stock-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company = ip-company  ~
and eb.loc = ip-loc ~
and eb.est-no = ip-est-no ~
and eb.form-no ne 0 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 eb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 eb


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
          "Form#", 1,
"Blank#", 2,
"Customer Part#", 3,
"Description", 4,
"FG Item#", 5
     SIZE 93 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 107 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      eb.form-no COLUMN-LABEL "Form#" FORMAT ">9":U
      eb.blank-no COLUMN-LABEL "Blank#" FORMAT ">9":U
      eb.part-no COLUMN-LABEL "Customer Part#" FORMAT "x(15)":U
            WIDTH 20
      eb.part-dscr1 COLUMN-LABEL "Description" FORMAT "x(30)":U
            WIDTH 40
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 22 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 85
     bt-cancel AT ROW 14.1 COL 96
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     SPACE(96.79) SKIP(1.84)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Items for Estimate #".


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
     _TblList          = "ASI.eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.eb.company = ip-company 
and eb.loc = ip-loc
and eb.est-no = ip-est-no
and eb.form-no ne 0"
     _FldNameList[1]   > ASI.eb.form-no
"form-no" "Form#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.eb.blank-no
"blank-no" "Blank#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.eb.part-no
"part-no" "Customer Part#" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[4]   > ASI.eb.part-dscr1
"part-dscr1" "Description" ? "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _FldNameList[5]   > ASI.eb.stock-no
"stock-no" "FG Item#" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Items for Estimate # */
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
   ip-cur-val = (ROWID(eb)).
   
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
    end.
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   ip-cur-val = (ROWID(eb)).
   
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
    end.      
    
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
        {srtord2.i 4}
        {srtord2.i 5}
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

  FRAME {&frame-name}:TITLE = TRIM(FRAME {&frame-name}:TITLE) + TRIM(ip-est-no).

  &scoped-define key-phrase eb.company EQ ip-company AND eb.loc EQ ip-loc AND eb.est-no EQ ip-est-no AND eb.form-no NE 0
  &scoped-define sortby-phrase {&sortby-1}
  &scoped-define IAMWHAT LOOKUP

  RUN enable_UI.

  {custom/lookpos1.i &lookup-file="eb"}

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

