&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
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
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}

{custom/globdefs.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

def var lv-qty like fg-rdtlh.qty.
def var lv-first-time as log init yes no-undo.
&scoped-define SORTBY-1 BY tt-fg-rcpth.trans-date-alf
&scoped-define fld-name-1 tt-fg-rcpth.trans-date-alf
&scoped-define IAMWHAT LOOKUP

DEF TEMP-TABLE tt-fg-rcpth NO-UNDO LIKE fg-rcpth
    FIELD qty LIKE fg-rdtlh.qty
    FIELD trans-date-alf AS CHAR
    FIELD row-id AS ROWID.

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
&Scoped-define INTERNAL-TABLES tt-fg-rcpth

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-fg-rcpth.trans-date tt-fg-rcpth.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-fg-rcpth WHERE ~{&KEY-PHRASE}     ~{&SORTBY-PHRASE}.   DEF VAR li AS INT NO-UNDO.  {&browse-name}:DESELECT-ROWS ().  GET LAST {&browse-name}. DO WHILE AVAIL tt-fg-rcpth:   li = li + 1.   IF tt-fg-rcpth.b-no NE 0 THEN {&browse-name}:SELECT-ROW (li) = YES.   GET PREV {&browse-name}. END
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-fg-rcpth WHERE ~{&KEY-PHRASE}     ~{&SORTBY-PHRASE}.   DEF VAR li AS INT NO-UNDO.  {&browse-name}:DESELECT-ROWS ().  GET LAST {&browse-name}. DO WHILE AVAIL tt-fg-rcpth:   li = li + 1.   IF tt-fg-rcpth.b-no NE 0 THEN {&browse-name}:SELECT-ROW (li) = YES.   GET PREV {&browse-name}. END.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-fg-rcpth
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-fg-rcpth


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
          "Date Received", 1
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-fg-rcpth SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-fg-rcpth.trans-date COLUMN-LABEL "Receipt Date" FORMAT "99/99/9999":U
            WIDTH 15
      tt-fg-rcpth.qty        COLUMN-LABEL "Qty Received" FORMAT ">>>,>>>,>>>":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 92 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 13 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 69
     bt-cancel AT ROW 14.1 COL 81
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 12.67 COL 4
     SPACE(81.39) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Receipts for PO / Item".


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
OPEN QUERY {&SELF-NAME} FOR EACH tt-fg-rcpth WHERE ~{&KEY-PHRASE}
    ~{&SORTBY-PHRASE}.


DEF VAR li AS INT NO-UNDO.

{&browse-name}:DESELECT-ROWS ().

GET LAST {&browse-name}.
DO WHILE AVAIL tt-fg-rcpth:
  li = li + 1.
  IF tt-fg-rcpth.b-no NE 0 THEN {&browse-name}:SELECT-ROW (li) = YES.
  GET PREV {&browse-name}.
END.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "    fg-rcpth.company   eq cocode
and fg-rcpth.i-no      eq po-ordl.i-no
and fg-rcpth.po-no     eq trim(string(po-ordl.po-no,"">>>>>>>>>>""))
and fg-rcpth.rita-code eq ""R""
and (fg-rcpth.b-no     eq 0 or
     fg-rcpth.b-no     eq ap-inv.i-no or
     not can-find(first b-ap-inv where b-ap-inv.i-no eq fg-rcpth.b-no))
use-index item-po
"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select Receipts for PO / Item */
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
  APPLY "choose" TO bt-ok.
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
    end.
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR li AS INT NO-UNDO.

  DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
    {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

    IF AVAIL tt-fg-rcpth THEN DO:
      FIND fg-rcpth WHERE ROWID(fg-rcpth) EQ tt-fg-rcpth.row-id NO-ERROR.
      IF AVAIL fg-rcpth THEN fg-rcpth.b-no = ap-invl.i-no.
    END.
  END.

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
    end.      
    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    &scoped-define IAMWHAT LOOKUP
    assign rd-sort.
    case rd-sort:
        {srtord2.i 1}
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

  DEF BUFFER b-ap-inv FOR ap-inv.


  {sys/inc/fgpostgl.i}

  FIND ap-invl WHERE ROWID(ap-invl) EQ ip-rowid NO-LOCK NO-ERROR.

  FIND FIRST ap-inv WHERE ap-inv.i-no EQ ap-invl.i-no NO-LOCK NO-ERROR.

  FIND FIRST po-ordl
      WHERE po-ordl.company   EQ cocode
        AND po-ordl.po-no     EQ ap-invl.po-no
        AND po-ordl.line      EQ {ap/invlline.i -1}
        AND po-ordl.item-type EQ NO
      NO-LOCK NO-ERROR.

  IF AVAIL po-ordl AND fgpostgl NE "None" THEN
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ cocode
        AND fg-rcpth.i-no      EQ po-ordl.i-no
        AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
        AND fg-rcpth.rita-code EQ "R"
        AND (fg-rcpth.b-no     EQ 0 OR
             fg-rcpth.b-no     EQ ap-inv.i-no OR
             NOT CAN-FIND(FIRST b-ap-inv WHERE b-ap-inv.i-no EQ fg-rcpth.b-no))
      USE-INDEX item-po NO-LOCK:
    
    CREATE tt-fg-rcpth.
    BUFFER-COPY fg-rcpth TO tt-fg-rcpth
    ASSIGN
     tt-fg-rcpth.trans-date-alf = STRING(tt-fg-rcpth.trans-date,"99/99/9999").

    FOR EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no NO-LOCK:
      tt-fg-rcpth.qty = tt-fg-rcpth.qty + fg-rdtlh.qty.
    END.
  END.
  RELEASE tt-fg-rcpth.

  FIND FIRST tt-fg-rcpth NO-ERROR.
  IF NOT AVAIL tt-fg-rcpth THEN RETURN.
    
  FRAME {&FRAME-NAME}:TITLE = "Select Receipts for PO / Item:  " +
                              TRIM(string(po-ordl.po-no,">>>>>>>>")) + " / " +
                              TRIM(po-ordl.i-no) + " ".

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

