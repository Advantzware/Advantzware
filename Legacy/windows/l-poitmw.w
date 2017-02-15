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
def input parameter ip-lookup as log no-undo.
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-po-no as char no-undo.
def input parameter ip-item-type like po-ordl.item-type no-undo.
def input parameter ip-cur-val as char no-undo.
def input parameter ip-row-id as rowid no-undo.

def output parameter op-row-id as rowid no-undo.

/* Local Variable Definitions ---                                       */
{windows/l-poitmw.i}

DEF VAR lv-pos-rowid AS ROWID NO-UNDO.
DEF VAR li-count AS INT NO-UNDO.
&scoped-define SORTBY-1 BY string(po-ordl.LINE,'999')
&scoped-define SORTBY-2 BY po-ordl.i-no
&scoped-define SORTBY-3 BY po-ordl.job-no
&scoped-define fld-name-1 string(po-ordl.LINE)
&scoped-define fld-name-2 po-ordl.i-no
&scoped-define fld-name-3 po-ordl.job-no
&scoped-define IAMWHAT LOOKUP
/*&SCOPED-DEFINE useMatches*/

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

def var lv-first-time as log init yes no-undo.

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
&Scoped-define INTERNAL-TABLES tt-report po-ordl


/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 po-ordl.LINE po-ordl.i-no po-ordl.s-num po-ordl.i-name po-ordl.job-no po-ordl.job-no2 po-ordl.s-wid tt-report.s-len   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-report, ~
                                   FIRST po-ordl WHERE ~{&KEY-PHRASE} ~
                                       AND RECID(po-ordl) EQ tt-report.rec-id NO-LOCK ~
                                       ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-report, ~
                                   FIRST po-ordl WHERE ~{&KEY-PHRASE} ~
                                    AND RECID(po-ordl) EQ tt-report.rec-id NO-LOCK ~
                                    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-report po-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-report
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 po-ordl


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort Btn_OK Btn_Cancel ~
bt-clear lv-search 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Line", 1,
"Item", 2,
"Job#", 3
     SIZE 63.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 126 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-report, 
      po-ordl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      po-ordl.LINE     FORMAT ">>>":U
      po-ordl.i-no      FORMAT "x(15)":U    WIDTH 23
      po-ordl.s-num     FORMAT ">>>":U
      po-ordl.i-name    FORMAT "x(30)":U
      po-ordl.job-no    FORMAT "x(6)":U
      po-ordl.job-no2   FORMAT "99":U
      po-ordl.s-wid     FORMAT ">>9.99<<":U
      tt-report.s-len   FORMAT ">>9.99<<":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 136 BY 11.19
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 13 NO-LABEL
     Btn_OK AT ROW 13.91 COL 92.2
     Btn_Cancel AT ROW 13.91 COL 114
     bt-clear AT ROW 13.95 COL 3
     lv-search AT ROW 14.1 COL 23.2 COLON-ALIGNED
     "Sort By:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 12.67 COL 4
     RECT-1 AT ROW 12.48 COL 3 WIDGET-ID 2
     SPACE(8.00) SKIP(1.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Items Not Fully Received for PO#".


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
/* BROWSE-TAB BROWSE-1 TEXT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report,
                            FIRST po-ordl WHERE RECID(po-ordl) EQ tt-report.rec-id NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Where[1]         = "ASI.reftable.reftable = ""Flute"" and 
ASI.reftable.company  = """" and
ASI.reftable.loc = """"
"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Items Not Fully Received for PO# */
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
   op-row-id = ROWID(po-ordl).
   APPLY "window-close" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON START-SEARCH OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  ASSIGN
    lv-search:SCREEN-VALUE = ''
    lv-search.
  RUN startsearch.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  op-row-id = ROWID(po-ordl).
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
  ASSIGN
    lv-search:SCREEN-VALUE = ""
    lv-search
    rd-sort.
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
   
  RUN build-table. 

  IF ip-lookup AND li-count GT 0 THEN DO WITH FRAME {&FRAME-NAME}:
    RELEASE po-ordl.

    IF li-count EQ 1 THEN
    FOR EACH tt-report,
        FIRST po-ordl WHERE RECID(po-ordl) EQ tt-report.rec-id NO-LOCK:
      LEAVE.
    END.

    IF AVAIL po-ordl THEN APPLY "choose" TO Btn_OK.

    ELSE DO:
      FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) +
                                  " " + TRIM(ip-po-no).

      RUN enable_UI.
      {custom/usrprint.i}

      IF lv-pos-rowid NE ? THEN
        REPOSITION {&browse-name} TO ROWID lv-pos-rowid NO-ERROR.

      WAIT-FOR GO OF FRAME {&FRAME-NAME}.
    END.
  END.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-t-rec-qty LIKE po-ordl.t-rec-qty NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.


  ASSIGN
   li-count     = 0
   lv-pos-rowid = ?.
   
  FOR EACH tt-report:
    DELETE tt-report.
  END.

  FOR EACH po-ordl
      WHERE po-ordl.company   EQ ip-company
        AND po-ordl.po-no     EQ INT(ip-po-no)
        AND po-ordl.deleted   EQ NO
        AND po-ordl.stat      NE "C"
        AND po-ordl.item-type EQ ip-item-type
      NO-LOCK:

    lv-t-rec-qty = po-ordl.t-rec-qty.

    IF po-ordl.item-type THEN
    FOR EACH item
        WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK,
        EACH rm-rctd
        WHERE rm-rctd.company EQ po-ordl.company
          AND rm-rctd.po-no   EQ ip-po-no
          AND rm-rctd.i-no    EQ po-ordl.i-no
          AND rm-rctd.job-no  EQ po-ordl.job-no
          AND rm-rctd.job-no2 EQ po-ordl.job-no2
          AND rm-rctd.s-num   EQ po-ordl.s-num
          AND ROWID(rm-rctd)  NE ip-row-id
        NO-LOCK:

      ld = rm-rctd.qty.

      IF rm-rctd.pur-uom NE po-ordl.cons-uom THEN
       run sys/ref/convquom.p(rm-rctd.pur-uom, po-ordl.cons-uom,
                              item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                              ld, output ld).

      lv-t-rec-qty = lv-t-rec-qty + ld.
    END.

    IF lv-t-rec-qty LT po-ordl.cons-qty THEN DO:
      CREATE tt-report.
      ASSIGN
       li-count         = li-count + 1
       tt-report.rec-id = RECID(po-ordl)
       tt-report.key-01 = STRING(po-ordl.cons-qty - lv-t-rec-qty,"9999999999.9999999999")
       tt-report.s-len  = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len.

      IF po-ordl.i-no EQ ip-cur-val AND ip-cur-val NE "" THEN
        lv-pos-rowid = ROWID(tt-report).
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
  ENABLE BROWSE-1 RECT-1 rd-sort Btn_OK Btn_Cancel bt-clear lv-search 
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
  END.
  
  APPLY 'entry' TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

