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

&SCOPED-DEFINE yellowColumnsName l-pofg

/* Parameters Definitions ---                                           */
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */

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

def var lv-first-time as log init yes no-undo.

&scoped-define SORTBY-1 BY po-ordl.po-no DESC BY po-ordl.LINE
&scoped-define SORTBY-2 BY po-ordl.vend-i-no {&SORTBY-1}
&scoped-define SORTBY-3 BY po-ordl.i-no {&SORTBY-1}
&scoped-define FLD-NAME-1 po-ordl.po-no
&scoped-define FLD-NAME-2 po-ordl.vend-i-no
&scoped-define FLD-NAME-3 po-ordl.i-no
&scoped-define DATATYPE-1 INTEGER
&scoped-define ASCDSC-1 DESC
&scoped-define IAMWHAT LOOKUP
&SCOPED-DEFINE useMatches

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
&Scoped-define INTERNAL-TABLES po-ordl po-ord

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 po-ordl.po-no po-ordl.LINE ~
po-ordl.i-no po-ordl.i-name po-ordl.job-no po-ordl.job-no2 ~
po-ordl.vend-i-no po-ord.vend-no po-ordl.s-wid po-ordl.s-len ~
po-ordl.ord-qty po-ordl.cost
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH po-ordl WHERE ~{&KEY-PHRASE} ~
      AND po-ordl.company eq ip-company and ~
po-ordl.deleted = no and ~
po-ordl.opened eq yes and ~
po-ordl.item-type = no NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company eq po-ordl.company and ~
po-ord.po-no eq po-ordl.po-no ~
      AND po-ord.stat ne "H" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH po-ordl WHERE ~{&KEY-PHRASE} ~
      AND po-ordl.company eq ip-company and ~
po-ordl.deleted = no and ~
po-ordl.opened eq yes and ~
po-ordl.item-type = no NO-LOCK, ~
      FIRST po-ord WHERE po-ord.company eq po-ordl.company and ~
po-ord.po-no eq po-ordl.po-no ~
      AND po-ord.stat ne "H" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 po-ordl po-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 po-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 po-ord


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 rd-sort bt-clear lv-search bt-ok ~
bt-cancel RECT-1 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search fi_sortby 

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

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 116.8 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PO#", 1,
"Vendor Item", 2,
"Item#", 3
     SIZE 45 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 163.8 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      po-ordl, 
      po-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      po-ordl.po-no COLUMN-LABEL "PO#" FORMAT ">>>>>>>9":U COLUMN-FONT 0
            LABEL-BGCOLOR 14
      po-ordl.LINE FORMAT ">>9":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      po-ordl.i-no FORMAT "x(15)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      po-ordl.i-name FORMAT "x(30)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      po-ordl.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U COLUMN-FONT 0
            LABEL-BGCOLOR 14
      po-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U COLUMN-FONT 0
      po-ordl.vend-i-no FORMAT "x(15)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      po-ord.vend-no FORMAT "x(8)":U COLUMN-FONT 0 LABEL-BGCOLOR 14
      po-ordl.s-wid COLUMN-LABEL "Sheet!Wid" FORMAT ">>9.9999":U
            COLUMN-FONT 0 LABEL-BGCOLOR 14
      po-ordl.s-len COLUMN-LABEL "Sheet!Len" FORMAT ">>9.9999":U
            COLUMN-FONT 0 LABEL-BGCOLOR 14
     po-ordl.ord-qty COLUMN-LABEL "Qty" FORMAT ">>>>>>>9.99":U
            COLUMN-FONT 0 LABEL-BGCOLOR 14
     po-ordl.cost COLUMN-LABEL "Cost" FORMAT ">>>>>>>9.99<<":U
            COLUMN-FONT 0 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 164 BY 11.43
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.86 COL 14.2 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.29 COL 21.2 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 141.2
     bt-cancel AT ROW 14.1 COL 152.8
     fi_sortby AT ROW 12.86 COL 100 COLON-ALIGNED
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 13.1 COL 4.2
     RECT-1 AT ROW 12.62 COL 1.2
     SPACE(0.00) SKIP(1.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Purchase Order Information (Finished Goods)".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.po-ordl,ASI.po-ord WHERE ASI.po-ordl ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST, FIRST"
     _Where[1]         = "po-ordl.company eq ip-company and
po-ordl.deleted = no and
po-ordl.opened eq yes and
po-ordl.item-type = no"
     _JoinCode[2]      = "po-ord.company eq po-ordl.company and
po-ord.po-no eq po-ordl.po-no"
     _Where[2]         = "po-ord.stat ne ""H"""
     _FldNameList[1]   > ASI.po-ordl.po-no
"po-ordl.po-no" "PO#" ? "integer" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.po-ordl.line
"po-ordl.line" ? ? "Integer" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.po-ordl.i-no
"po-ordl.i-no" ? ? "character" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.po-ordl.i-name
"po-ordl.i-name" ? ? "character" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.po-ordl.job-no
"po-ordl.job-no" "Job #" ? "character" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.po-ordl.job-no2
"po-ordl.job-no2" "" ? "integer" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.po-ordl.vend-i-no
"po-ordl.vend-i-no" ? ? "character" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.po-ord.vend-no
"po-ord.vend-no" ? ? "character" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.po-ordl.s-wid
"po-ordl.s-wid" "Sheet!Wid" ? "decimal" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.po-ordl.s-len
"po-ordl.s-len" "Sheet!Len" ? "decimal" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.po-ordl.ord-qty
"po-ordl.ord-qty" "Qty" ? "decimal" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[12]   > ASI.po-ordl.cost
"po-ordl.cost" "Cost" ? "decimal" ? ? 0 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Purchase Order Information (Finished Goods) */
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
   op-char-val = po-ordl.po-no:screen-value in browse {&browse-name} + "," + 
                 po-ordl.i-no:screen-value in browse {&browse-name} + "," +
                 po-ordl.i-name:screen-value in browse {&browse-name} + "," +
                 po-ordl.job-no:screen-value in browse {&browse-name} + "," +
                 po-ordl.job-no2:screen-value in browse {&browse-name} + "," +
                 string(po-ordl.line)
                 .
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON START-SEARCH OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN startSearch.
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
        {srtord2.i 2}
        {srtord2.i 3}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
  op-char-val = po-ordl.po-no:screen-value in browse {&browse-name} + "," + 
                po-ordl.i-no:screen-value in browse {&browse-name} + "," +
                po-ordl.i-name:screen-value in browse {&browse-name} + "," +
                po-ordl.job-no:screen-value in browse {&browse-name} + "," +
                po-ordl.job-no2:screen-value in browse {&browse-name} + "," +
                string(po-ordl.line)
                 .
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON ENTER OF lv-search IN FRAME Dialog-Frame /* Search */
DO:
APPLY 'tab' TO SELF. 
/*   IF BROWSE {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN DO: */
/*       APPLY 'tab' TO SELF. /* Move over to OK button */    */
/*   END.                                                     */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
or return of lv-search
DO:
  ASSIGN lv-search.

  RUN new-search.     
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
  
  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    
	IF ip-cur-val NE "" THEN DO:
		lv-search = STRING(INT(ip-cur-val),">>>>>>>>>>") NO-ERROR.
		IF ERROR-STATUS:ERROR THEN 
			lv-search = ip-cur-val.
	END.

    RELEASE itemfg.
    IF rd-sort:SCREEN-VALUE EQ "2" THEN DO: 
      FIND po-ordl NO-LOCK 
        WHERE po-ordl.company EQ cocode
          AND po-ordl.po-no   EQ INT(ip-cur-val)
        NO-ERROR.
      IF AVAIL itemfg THEN lv-search = po-ordl.vend-i-no.
    END.
    ELSE IF rd-sort:SCREEN-VALUE = "3" THEN DO:
        FIND po-ordl NO-LOCK 
          WHERE po-ordl.company EQ cocode
            AND po-ordl.i-no  EQ ip-cur-val
          NO-ERROR.
        IF AVAIL itemfg THEN lv-search = po-ordl.i-no.
    END.
    lv-search:SCREEN-VALUE = lv-search.

    IF lv-search EQ "" THEN RUN new-rd-sort. 

    ELSE  RUN new-search.
  END.

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
  DISPLAY rd-sort lv-search fi_sortby 
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
        {srtord2.i 2}
        {srtord2.i 3}
    end.    
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  APPLY 'entry' TO BROWSE {&browse-name}.

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
      {srtord.i 1}
      {srtord2.i 2}
      {srtord2.i 3}
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

