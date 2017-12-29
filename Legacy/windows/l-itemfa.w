
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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


&SCOPED-DEFINE useMatches

/* Parameters Definitions ---                                           */
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cust-no like itemfg.cust-no no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-recid as recid no-undo.

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

def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-cust-no LIKE itemfg.cust-no NO-UNDO.
DEF VAR lv-part-no LIKE itemfg.part-no NO-UNDO.
DEF VAR cellColumn AS HANDLE NO-UNDO EXTENT 20.
DEF VAR columnCount AS INTEGER NO-UNDO.
DEF VAR idx AS INTEGER NO-UNDO.
DEF VAR useColors AS CHAR NO-UNDO.
DEF VAR gcompany AS CHAR NO-UNDO.

&scoped-define SORTBY-1 BY tt-cust-part.i-no
&scoped-define SORTBY-2 BY tt-cust-part.part-no
&scoped-define SORTBY-3 BY tt-cust-part.i-name
&scoped-define FLD-NAME-1 tt-cust-part.i-no
&scoped-define FLD-NAME-2 tt-cust-part.part-no
&scoped-define FLD-NAME-3 tt-cust-part.i-name
&scoped-define IDXNAME2 cust-part
&scoped-define IDXNAME3 name

&scoped-define IAMWHAT LOOKUP

ll-new-file = CAN-FIND(FIRST _file WHERE _file._file-name EQ "cust-part").

gcompany = ip-company.
{sys/inc/fgbrowse.i}
useColors = sys-ctrl.char-fld.

DEF TEMP-TABLE tt-cust-part    
    FIELD i-no AS CHAR
    FIELD i-name AS CHAR
    FIELD part-no AS CHAR 
    FIELD part-dscr1 AS CHAR
    FIELD part-dscr2 AS CHAR
    FIELD q-onh AS INTEGER
    FIELD cust-no AS CHAR
    FIELD stocked AS LOGICAL
    FIELD r-id  AS RECID .

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
&Scoped-define INTERNAL-TABLES tt-cust-part

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-cust-part.i-no tt-cust-part.i-name tt-cust-part.q-onh tt-cust-part.cust-no tt-cust-part.part-no tt-cust-part.part-dscr1 tt-cust-part.stocked  
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-cust-part WHERE ~{&KEY-PHRASE} ~
NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-cust-part WHERE ~{&KEY-PHRASE} ~
NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-cust-part
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-cust-part


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-1 rd-sort bt-clear lv-search ~
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
     SIZE 10 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 84 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item #", 1,
"Customer Part#", 2,
"Name", 3
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-cust-part SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
     tt-cust-part.i-no COLUMN-LABEL "Item No" FORMAT "x(15)":U WIDTH 23
     tt-cust-part.i-name COLUMN-LABEL "Name" FORMAT "x(15)":U WIDTH 23
     tt-cust-part.q-onh COLUMN-LABEL "Qty on Hand" 
     tt-cust-part.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U WIDTH 15
     tt-cust-part.part-no COLUMN-LABEL "Cust Part #" FORMAT "x(15)":U
            WIDTH 23
     tt-cust-part.part-dscr1 COLUMN-LABEL "Item Description" FORMAT "x(30)":U WIDTH 33
     tt-cust-part.stocked COLUMN-LABEL "Stock Mat'l?"  FORMAT "Yes/No":U
      
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 141 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1.6
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 108
     bt-cancel AT ROW 14.1 COL 120
     "Sort By:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 12.67 COL 2
     RECT-1 AT ROW 12.43 COL 1
     SPACE(0.59) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Finished Goods Information".


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
/* BROWSE-TAB BROWSE-1 RECT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
IF rd-sort = 1 THEN DO:
   OPEN QUERY {&SELF-NAME} FOR EACH tt-cust-part WHERE tt-cust-part.po-no BEGINS lv-search
                                 BY tt-cust-part.part-no.
END.
ELSE DO:
    OPEN QUERY {&SELF-NAME} FOR EACH tt-cust-part WHERE tt-cust-part.po-no BEGINS lv-search
                                  BY tt-cust-part.i-no.
END.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "ASI.oe-rel.company = ip-company
 AND ASI.oe-rel.po-no BEGINS ip-cur-val
 AND ASI.oe-rel.po-no <> """""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* PO Information */
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


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  ASSIGN
   op-recid  = tt-cust-part.r-id
   op-char-val = tt-cust-part.i-no + "," +
                  tt-cust-part.i-name 
                 . 

   APPLY "window-close" TO FRAME {&FRAME-NAME}.
  
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
    assign
     op-recid  = tt-cust-part.r-id
     op-char-val = tt-cust-part.i-no + "," +
                  tt-cust-part.i-name 
                 . 

   APPLY "window-close" TO FRAME {&FRAME-NAME}.
    
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  IF useColors NE '' THEN
  DO idx = 1 TO columnCount:
    CASE useColors:
      WHEN 'Bronze/Maroon' THEN
      ASSIGN
        cellColumn[idx]:FGCOLOR = IF tt-cust-part.stocked AND tt-cust-part.q-onh LE 0 THEN 15 ELSE ?
        cellColumn[idx]:BGCOLOR = IF tt-cust-part.stocked AND tt-cust-part.q-onh LE 0 THEN 4 ELSE ?.
      OTHERWISE
      ASSIGN
        cellColumn[idx]:FGCOLOR = ?
        cellColumn[idx]:BGCOLOR = ?.
    END CASE.
  END. /* do idx */
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

  
  RUN create-tt-cust-part.

  IF ip-cur-val EQ "0" THEN ip-cur-val = "".

  RUN getCellColumns.
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    lv-search = ip-cur-val.

    RELEASE itemfg.
    IF rd-sort:SCREEN-VALUE NE "1" THEN
    FIND itemfg NO-LOCK 
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    BEGINS ip-cur-val
        NO-ERROR.
    IF AVAIL itemfg THEN
      lv-search = IF rd-sort:SCREEN-VALUE EQ "2" THEN tt-cust-part.part-no
                                                 ELSE tt-cust-part.i-name.

    lv-search:SCREEN-VALUE = lv-search.

    IF lv-search EQ "" THEN RUN new-rd-sort.

    ELSE RUN new-search.
  END.

  /*&scoped-define key-phrase {&fld-name-1} >= ip-cur-val
  &scoped-define sortby-phrase {&sortby-1}*/

  /*RUN enable_UI.*/
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-cust-part Dialog-Frame 
PROCEDURE create-tt-cust-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH itemfg NO-LOCK
    WHERE itemfg.company = ip-company
      AND itemfg.stat = "A" BREAK BY itemfg.i-no  :
      IF FIRST-OF(itemfg.i-no) THEN DO:
        CREATE tt-cust-part.
            ASSIGN
                tt-cust-part.i-no       =  itemfg.i-no
                tt-cust-part.i-name     =  itemfg.i-name
                tt-cust-part.q-onh      =  itemfg.q-onh
                tt-cust-part.cust-no    =  itemfg.cust-no
                tt-cust-part.part-no    =  itemfg.part-no
                tt-cust-part.part-dscr1 =  itemfg.part-dscr1
                tt-cust-part.part-dscr2 =  itemfg.part-dscr2
                tt-cust-part.stocked    =  itemfg.stocked
                tt-cust-part.r-id       =    RECID(itemfg).
      END.
      
     FOR EACH cust-part WHERE cust-part.company EQ itemfg.company   
       AND cust-part.i-no = itemfg.i-no AND cust-part.part-no NE ""
          NO-LOCK :
            CREATE tt-cust-part.
            ASSIGN
                tt-cust-part.i-no       =  itemfg.i-no
                tt-cust-part.i-name     =  itemfg.i-name
                tt-cust-part.q-onh      =  itemfg.q-onh
                tt-cust-part.cust-no    =  cust-part.cust-no
                tt-cust-part.part-no    =  cust-part.part-no
                tt-cust-part.part-dscr1 =  itemfg.part-dscr1
                tt-cust-part.part-dscr2 =  itemfg.part-dscr2
                tt-cust-part.stocked    =  itemfg.stocked
                tt-cust-part.r-id       =    RECID(itemfg).
     END.
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
  ENABLE RECT-1 BROWSE-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
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
        {srtord2.i 1}
        {srtord2.i 2}
        {srtord2.i 3}
    end. 
  END.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns Dialog-Frame 
PROCEDURE getCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  columnCount = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
  DO idx = 1 TO columnCount:
    cellColumn[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-search Dialog-Frame 
PROCEDURE new-search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &scoped-define IAMWHAT Search
  &scoped-define where-statement BEGINS lv-search 
   
  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN rd-sort.
    CASE rd-sort:
      {srtord2.i 1}
      {srtord2.i 2}
      {srtord2.i 3}
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
