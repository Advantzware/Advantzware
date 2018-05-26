&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\l-poven.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input param ip-recid AS recid no-undo.
def input param ip-po-no LIKE po-ord.po-no no-undo.
def output param op-rec-val as recid no-undo.
def var v-term as cha no-undo.
def var v-term-2 as cha no-undo.
DEF VAR v-col-move AS LOG NO-UNDO.
{custom/globdefs.i}
{sys/inc/VAR.i "new shared"}
&scoped-define cellcolumnDat l-poven
ASSIGN cocode = g_company
       locode = g_loc.

DEF TEMP-TABLE tt-report NO-UNDO
    FIELD key-01 AS CHAR
    FIELD term-id AS CHAR
    FIELD rec-id AS RECID
    FIELD key-05 AS CHAR
    INDEX term-id term-id key-01
    INDEX key-01 key-01.

DEF VAR lv-ordl-recid AS RECID NO-UNDO.
DEF VAR lv-keyvalue AS cha NO-UNDO.
DEF VAR lv-first-recid AS RECID NO-UNDO.
DEF VAR lc-date AS DATE NO-UNDO.
DEF VAR v-mat-type AS CHAR NO-UNDO .

DEF STREAM st-x.

DEF VAR lv-sort-by AS CHAR INIT "key-01" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "PO Number" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.

&SCOPED-DEFINE sortby-log                             ~
    IF lv-sort-by EQ "key-01"  THEN tt-report.key-01   ELSE     ~
    IF lv-sort-by EQ "lc-date"  THEN STRING((YEAR(getdate()) * 10000) + (MONTH(getdate()) * 100) + DAY(getdate()))  ELSE ~
    IF lv-sort-by EQ "i-no"  THEN po-ordl.i-no  ELSE ~
    IF lv-sort-by EQ "i-name" THEN po-ordl.i-name ELSE ~
    IF lv-sort-by EQ "s-wid" THEN string(po-ordl.s-wid) ELSE ~
    IF lv-sort-by EQ "s-len" THEN string(po-ordl.s-len) ELSE ~
    IF lv-sort-by EQ "ord-qty" THEN string(po-ordl.ord-qty) ELSE ~
    IF lv-sort-by EQ "t-rec-qty" THEN string(po-ordl.t-rec-qty) ELSE ~
    IF lv-sort-by EQ "t-inv-qty" THEN string(po-ordl.t-inv-qty) ELSE ~
    IF lv-sort-by EQ "po-date"  THEN STRING((YEAR(po-ord.po-date) * 10000) + (MONTH(po-ord.po-date) * 100) + DAY(po-ord.po-date))  ELSE ~
    IF lv-sort-by EQ "type" THEN po-ord.type ELSE ~
    IF lv-sort-by EQ "stat" THEN po-ord.stat ELSE ~
    IF lv-sort-by EQ "job-no"  THEN po-ordl.job-no  ELSE ""
    

&SCOPED-DEFINE sortby BY tt-report.key-01

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

/*Pulled from SetCellColumns.i since this dialog is not ADM compatible*/
DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT 200.
DEFINE VARIABLE columnWidth AS DECIMAL NO-UNDO EXTENT 200.
DEFINE VARIABLE cellColumnDat AS CHARACTER NO-UNDO.

/* create a &SCOPED-DEFINE cellColumnDat value prior to this include
   if another file name is desired to store user cell column order */
&IF DEFINED(cellColumnDat) EQ 0 &THEN
&SCOPED-DEFINE cellColumnDat {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
&ENDIF
/*End SetCellColumns.i extract*/

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
&Scoped-define INTERNAL-TABLES tt-report po-ordl po-ord

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-report.key-01 getdate() @ lc-date po-ordl.i-no po-ordl.i-name po-ordl.s-wid po-ordl.s-len po-ordl.ord-qty po-ordl.t-rec-qty po-ordl.t-inv-qty po-ord.po-date po-ord.TYPE po-ord.stat po-ordl.job-no po-ordl.job-no2 NO-LABEL   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-report.key-01 po-ordl.i-no po-ordl.i-name po-ordl.s-wid ~
   po-ordl.s-len po-ordl.ord-qty po-ordl.t-rec-qty po-ordl.t-inv-qty ~
 ~
 ~
  po-ord.po-date ~
   po-ord.TYPE ~
   po-ord.stat po-ordl.job-no
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-report po-ordl po-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-report
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-BROWSE-1 po-ordl
&Scoped-define THIRD-ENABLED-TABLE-IN-QUERY-BROWSE-1 po-ord
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 IF ll-sort-asc THEN    OPEN QUERY {&SELF-NAME} FOR EACH tt-report WHERE tt-report.term-id eq v-term NO-LOCK, ~
                     first po-ordl WHERE RECID(po-ordl) = tt-report.rec-id NO-LOCK, ~
                         FIRST po-ord WHERE po-ord.company = po-ordl.company AND                               po-ord.po-no = po-ordl.po-no ~{&sortby-phrase-asc}. else    OPEN QUERY {&SELF-NAME} FOR EACH tt-report WHERE tt-report.term-id eq v-term NO-LOCK, ~
                     first po-ordl WHERE RECID(po-ordl) = tt-report.rec-id NO-LOCK, ~
                         FIRST po-ord WHERE po-ord.company = po-ordl.company AND                               po-ord.po-no = po-ordl.po-no ~{&sortby-phrase-desc}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-report po-ordl po-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-report
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 po-ordl
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 po-ord


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-clear lv-search bt-ok bt-cancel Btn_move-sort
&Scoped-Define DISPLAYED-OBJECTS lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getdate Dialog-Frame 
FUNCTION getdate RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 11 BY 1.14.

DEFINE BUTTON Btn_move-sort 
     LABEL "Move Columns" 
     SIZE 16 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE BUTTON btn-clear 
     LABEL "Clear Find" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-report, 
      po-ordl, 
      po-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-report.key-01 label "PO Number" LABEL-BGCOLOR 14
      getdate() @ lc-date COLUMN-LABEL "Date Rec'd" FORMAT "99/99/9999":U  
       WIDTH 14.4 LABEL-BGCOLOR 14
      po-ordl.i-no LABEL "Item"   LABEL-BGCOLOR 14 WIDTH 23
      po-ordl.i-name LABEL "RM/FG Item Name" LABEL-BGCOLOR 14
      po-ordl.s-wid LABEL "Width" LABEL-BGCOLOR 14
      po-ordl.s-len LABEL "Length" FORM ">>>>9.9999" LABEL-BGCOLOR 14
      po-ordl.ord-qty LABEL "Quantity" LABEL-BGCOLOR 14
      po-ordl.t-rec-qty LABEL "Received Qty" LABEL-BGCOLOR 14
      po-ordl.t-inv-qty LABEL "Invoiced Qty" LABEL-BGCOLOR 14
      po-ord.po-date LABEL-BGCOLOR 14
      po-ord.TYPE LABEL-BGCOLOR 14
      po-ord.stat LABEL-BGCOLOR 14
      po-ordl.job-no LABEL "JOB#" LABEL-BGCOLOR 14
      po-ordl.job-no2 NO-LABEL 
      ENABLE tt-report.key-01 po-ordl.i-no po-ordl.i-name po-ordl.s-wid
             po-ordl.s-len po-ordl.ord-qty po-ordl.t-rec-qty po-ordl.t-inv-qty             
             po-ord.po-date
             po-ord.TYPE
             po-ord.stat po-ordl.job-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 130 BY 13.81
         BGCOLOR 8 FONT 2 ROW-HEIGHT-CHARS .52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     btn-clear AT ROW 15.05 COL 1
     lv-search AT ROW 15.05 COL 32 COLON-ALIGNED
     Btn_move-sort AT ROW 15.05 COL 87 WIDGET-ID 4
     bt-ok AT ROW 15.05 COL 105
     bt-cancel AT ROW 15.05 COL 117
     SPACE(0.79) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Price History Information".


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
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Price History Information */
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
  
   lv-keyvalue = lv-keyvalue + KEYLABEL(LASTKEY).
   lv-search:SCREEN-VALUE = lv-keyvalue.

   FIND FIRST tt-report WHERE tt-report.term-id = v-term 
                       AND trim(tt-report.key-01)  = lv-keyvalue
                 NO-LOCK NO-ERROR.
   IF AVAIL tt-report THEN DO:
     
      REPOSITION {&browse-name} TO RECID RECID(tt-report).
      lv-keyvalue = "".
      RETURN NO-APPLY.
   END.   
   ELSE do:
       FIND FIRST tt-report WHERE tt-report.term-id = v-term 
                       AND trim(tt-report.key-01)  BEGINS lv-keyvalue
                 NO-LOCK NO-ERROR.
       IF AVAIL tt-report THEN DO:
     
         REPOSITION {&browse-name} TO RECID RECID(tt-report).   
         RETURN NO-APPLY.
       END.
       ELSE APPLY LASTKEY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   BROWSE {&browse-name}:SELECT-FOCUSED-ROW().

   op-rec-val = recid(po-ordl).

   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON START-SEARCH OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  
  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*APPLY "open_query" TO BROWSE {&browse-name}.*/
  {&open-query-{&browse-name}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel Dialog-Frame
ON CHOOSE OF bt-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   op-rec-val = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_move-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_move-sort Dialog-Frame
ON CHOOSE OF Btn_move-sort IN FRAME Dialog-Frame /* Move Columns */
DO:
  RUN move-columns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
    BROWSE {&browse-name}:SELECT-FOCUSED-ROW().
    
    op-rec-val = recid(po-ordl).

    APPLY "window-close" TO FRAME {&FRAME-NAME}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-clear Dialog-Frame
ON CHOOSE OF btn-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      lv-search = "".
      lv-keyvalue = "".
      DISPLAY lv-search.
      REPOSITION {&browse-name} TO RECID lv-first-recid.
      APPLY "entry" TO BROWSE {&browse-name}.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
DO:
    /* gdm - 05290903 */
    ASSIGN  {&SELF-NAME}.

     FIND FIRST tt-report WHERE tt-report.term-id = v-term 
       AND trim(tt-report.key-01)  BEGINS {&SELF-NAME} NO-LOCK NO-ERROR.
     IF AVAIL tt-report THEN DO:
     
       REPOSITION {&browse-name} TO RECID RECID(tt-report).   
     END.
     ELSE APPLY LASTKEY.

  
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

  cellColumnDat = 'users/' + USERID('nosweat') + '/{&cellColumnDat}.dat'.  
  
  {&browse-name}:SET-REPOSITIONED-ROW (1,"CONDITIONAL").

  v-term = string(year(today),"9999") +
            string(month(today),"99")  +
            string(day(today),"99") +
            string(time,"99999").
            
  SESSION:SET-WAIT-STATE("general").

  run reset-tt-report. 
  
  SESSION:SET-WAIT-STATE("").
  RUN enable_UI.
  
  RUN setCellColumns. /*get columns from .dat and display in saved order*/

  IF lv-ordl-recid <> ? THEN REPOSITION {&browse-name} TO RECID lv-ordl-recid.
  APPLY "entry" TO BROWSE {&browse-name}.

  RUN initialize.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN SaveColumns. /*save column positions to .dat*/
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
  DISPLAY lv-search 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 btn-clear lv-search bt-ok bt-cancel Btn_move-sort
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize Dialog-Frame 
PROCEDURE initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN tt-report.key-01:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.i-name:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.s-wid:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.s-len:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.ord-qty:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.t-rec-qty:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.t-inv-qty:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ord.po-date:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ord.TYPE:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ord.stat:READ-ONLY IN BROWSE {&browse-name} = YES
         po-ordl.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
         .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns Dialog-Frame 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         BROWSE-1:COLUMN-MOVABLE = NOT v-col-move
         BROWSE-1:COLUMN-RESIZABLE = NOT v-col-move
         v-col-move = NOT v-col-move
         Btn_move-sort:LABEL = IF NOT v-col-move THEN "Move Columns" ELSE "Sort Columns".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveColumns Dialog-Frame 
PROCEDURE SaveColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE h AS HANDLE NO-UNDO.
  /* check for any columns changes */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    h = cellColumn[i] NO-ERROR.
    IF NOT VALID-HANDLE(h) THEN 
      NEXT.
    h = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i) NO-ERROR.
    IF NOT VALID-HANDLE(h) THEN 
      NEXT.
    IF cellColumn[i]:NAME EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):NAME AND
       columnWidth[i] EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS THEN NEXT.    
    MESSAGE 'Save Column Changes?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE saveChanges AS LOGICAL.
    IF saveChanges THEN DO:
      OUTPUT TO VALUE(cellColumnDat).
      DO j = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        h = {&BROWSE-NAME}:GET-BROWSE-COLUMN(j) NO-ERROR.
        IF NOT VALID-HANDLE(h) THEN 
          NEXT.          
        EXPORT {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):NAME {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):WIDTH-PIXELS.
      END. /* do j */
      OUTPUT CLOSE.
    END. /* if savechanges */
    LEAVE.
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-tt-report Dialog-Frame 
PROCEDURE reset-tt-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var i as int no-undo.
 def var a as INT NO-UNDO.
 DEF VAR v-type AS LOG INIT NO NO-UNDO.

/*{sa/sa-sls01.i}           */ 

   FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode
       AND sys-ctrl.NAME = "APMatTypeExceptions" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN
           v-mat-type  = sys-ctrl.char-fld .
    
   FOR EACH tt-report WHERE tt-report.TERM-id = v-term :
       DELETE tt-report.
   END.

    DEF VAR v-po LIKE po-ord.po-no NO-UNDO.
    DEF VAR v-qty AS DEC NO-UNDO.

    find vend where recid(vend) eq ip-recid no-lock no-error.

    v-po = ip-po-no.

    if avail vend then 
    for each po-ord NO-LOCK
        {po/look/pobyven.i}
          AND po-ord.opened EQ YES
          USE-INDEX opened,
        each po-ordl WHERE
             po-ordl.company EQ po-ord.company AND
             po-ordl.po-no   EQ po-ord.po-no
             no-lock:

      RUN ap/valid-po.p (BUFFER po-ordl, BUFFER ap-invl).
      IF NOT AVAIL po-ordl THEN NEXT.

      RUN po/rec-inv.p (ROWID(po-ordl), OUTPUT v-qty).

      FIND FIRST item
          WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.

       v-type = NO .

       IF AVAIL ITEM AND item.i-code  EQ "R" THEN do:
           DO i = 1 TO LENGTH(v-mat-type) :
               IF SUBSTRING(v-mat-type,i,1) = ITEM.mat-type THEN
                   v-type = YES .
           END.
           
           IF item.stocked THEN
              IF v-qty EQ 0 THEN NEXT .
           IF item.stocked = NO AND NOT v-type THEN NEXT.
       END.
       ELSE IF v-qty = 0 THEN NEXT .
     

      /*if v-qty NE 0 then do:*/ /* task 06301508 */
        create tt-report.
        assign
         tt-report.term-id = v-term
         tt-report.key-01  = string(po-ord.po-no,">>>>>>")
         tt-report.rec-id  = recid(po-ordl)
         tt-report.key-05 = STRING(RECID(po-ordl))
         lv-first-recid = IF lv-first-recid = ? THEN RECID(tt-report) ELSE lv-first-recid
           .
          
        if po-ord.po-no ge v-po and v-po ne 0 then
          assign
           lv-ordl-recid = recid(tt-report)
           v-po   = 0.          
      /*end.*/
    end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCellColumns Dialog-Frame 
PROCEDURE SetCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE userColumn AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-index AS INT NO-UNDO.
  
  IF SEARCH(cellColumnDat) NE ? THEN DO:
     /* get user cell column order */
     INPUT FROM VALUE(cellColumnDat) NO-ECHO.
     REPEAT:
        IMPORT userColumn[j] columnWidth[j].
        j = j + 1.
     END. /* repeat */
     INPUT CLOSE.
     /* change default columns to user order */
     DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
     END.
    
     j = j - 1.
     DO i = 1 TO j:
     
        DO k = 1 TO j:
           IF userColumn[i] EQ cellColumn[k]:NAME THEN
              LEAVE.
        END.

        IF columnWidth[i] NE cellColumn[k]:WIDTH-PIXELS THEN
           cellColumn[k]:WIDTH-PIXELS = columnWidth[i].

        IF userColumn[i] NE cellColumn[i]:NAME THEN DO:
    
           {&BROWSE-NAME}:MOVE-COLUMN(k,i) IN FRAME {&FRAME-NAME}.
          
           DO v-index = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
              cellColumn[v-index] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(v-index).
           END.
        END.
     END. /* do i */

  END. /* search */
  /* read new order to check for changes when exiting */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    ASSIGN
      cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
      columnWidth[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS.    
  END. /* do i */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getdate Dialog-Frame 
FUNCTION getdate RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-result AS DATE NO-UNDO.
    /* IF oe-ordl.whsed THEN lc-result = "X".*/

    FOR EACH fg-rcpth WHERE fg-rcpth.company = cocode
        AND fg-rcpth.rita-code  = "R" AND fg-rcpth.i-no = po-ordl.i-no
            AND fg-rcpth.po-no =  tt-report.key-01 NO-LOCK:
        ASSIGN
            lc-result = fg-rcpth.trans-date .
        LEAVE.
    END.
    IF lc-result  = ? THEN
    FOR EACH rm-rcpth WHERE rm-rcpth.company = cocode
        AND rm-rcpth.rita-code  = "R" AND rm-rcpth.i-no = po-ordl.i-no
            AND rm-rcpth.po-no =  tt-report.key-01 NO-LOCK:
        ASSIGN
            lc-result = rm-rcpth.trans-date .
        LEAVE.
    END.
              
    RETURN lc-result.   /* Function return value. */  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
