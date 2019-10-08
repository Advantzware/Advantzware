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
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cust-no like itemfg.cust-no no-undo.
def input parameter ip-cur-val as cha no-undo.
DEFINE INPUT PARAMETER ip-vendor AS CHARACTER NO-UNDO .
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */
def output param op-recid as recid no-undo.

/* Local Variable Definitions ---                                       */
def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.
DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR lv-cust-no LIKE itemfg.cust-no NO-UNDO.
DEF VAR lv-part-no LIKE itemfg.part-no NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHAR NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lDefaultFilteras AS LOGICAL NO-UNDO .

&scoped-define SORTBY-1 BY itemfg.i-no
&scoped-define SORTBY-2 BY itemfg.i-name
&scoped-define fld-name-1 itemfg.i-no
&scoped-define fld-name-2 itemfg.i-name
&SCOPED-DEFINE useMatches
&scoped-define IAMWHAT LOOKUP

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

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
&Scoped-define INTERNAL-TABLES itemfg e-itemfg-vend

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 itemfg.i-no itemfg.i-name ~
itemfg.q-onh get-cust () @ itemfg.cust-no itemfg.cust-no ~
get-part () @ itemfg.part-no itemfg.part-no itemfg.part-dscr1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH itemfg WHERE ~{&KEY-PHRASE} ~
      AND itemfg.company = ip-company AND itemfg.stat EQ "A" NO-LOCK, ~
      first e-itemfg-vend NO-LOCK ~
        where e-itemfg-vend.company eq itemfg.company ~
          and e-itemfg-vend.i-no    eq itemfg.i-no ~
          and ((e-itemfg-vend.vend-no EQ ip-vendor AND rd-filter EQ 1) OR rd-filter EQ 2 ) ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH itemfg WHERE ~{&KEY-PHRASE} ~
      AND itemfg.company = ip-company AND itemfg.stat EQ "A" NO-LOCK, ~
      first e-itemfg-vend NO-LOCK ~
        where e-itemfg-vend.company eq itemfg.company ~
          and e-itemfg-vend.i-no    eq itemfg.i-no ~
          and ((e-itemfg-vend.vend-no EQ ip-vendor AND rd-filter EQ 1) OR rd-filter EQ 2 ) ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 itemfg e-itemfg-vend
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 itemfg


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 rd-sort rd-filter bt-clear lv-search bt-ok ~
bt-cancel RECT-1 
&Scoped-Define DISPLAYED-OBJECTS rd-sort rd-filter lv-search lv-label

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cust Dialog-Frame 
FUNCTION get-cust RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-part Dialog-Frame 
FUNCTION get-part RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 11 BY 1.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12.4 BY 1.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 11 BY 1.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item #", 1,
"Name", 2
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE rd-filter AS INTEGER 
     LABEL "Filter By:" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PO Vendor", 1,
          "All Vendors", 2
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE lv-label AS CHARACTER FORMAT "X(9)":U 
     LABEL "Filter By" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 141 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      itemfg,
      e-itemfg-vend SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      itemfg.i-no FORMAT "x(15)":U WIDTH 23
      itemfg.i-name FORMAT "x(30)":U
      itemfg.q-onh FORMAT "->>,>>>,>>9.999":U
      get-cust () @ itemfg.cust-no
      itemfg.cust-no FORMAT "x(8)":U WIDTH 10
      get-part () @ itemfg.part-no
      itemfg.part-no FORMAT "x(15)":U
      itemfg.part-dscr1 FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 141 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     rd-filter AT ROW 12.67 COL 99 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 22 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 118
     bt-cancel AT ROW 14.1 COL 131
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 12.67 COL 4
     lv-label AT ROW 12.67 COL 89
     SPACE(38.59) SKIP(1.56)
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
     _TblList          = "ASI.itemfg,ASI.e-itemfg-vend"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.itemfg.company = ip-company AND itemfg.stat EQ 'A'"
     _Where[2]         = "e-itemfg-vend.company eq itemfg.company 
                          and e-itemfg-vend.i-no eq itemfg.i-no 
                          and ((e-itemfg-vend.vend-no NE "" AND rd-filter EQ 1) OR rd-filter EQ 2 )"
     _FldNameList[1]   > ASI.itemfg.i-no
"itemfg.i-no" ? ? "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _FldNameList[2]   = ASI.itemfg.i-name
     _FldNameList[3]   = ASI.itemfg.q-onh
     _FldNameList[4]   > "_<CALC>"
"get-cust () @ itemfg.cust-no" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.itemfg.cust-no
"itemfg.cust-no" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"get-part () @ itemfg.part-no" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > ASI.itemfg.part-no
"itemfg.part-no" ? "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   = ASI.itemfg.part-dscr1
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Finished Goods Information */
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
   op-char-val = itemfg.i-no:screen-value in browse {&browse-name} + "," +
                 itemfg.i-name:screen-value in browse {&browse-name} 
                 .
   op-recid  = recid(itemfg).               
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel Dialog-Frame
ON CHOOSE OF bt-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   ASSIGN op-char-val = ""
          op-recid = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    assign lv-search:screen-value = "".
           lv-search = "".
    
  RUN pSearchData .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = itemfg.i-no:screen-value in browse {&browse-name} + "," +
                 itemfg.i-name:screen-value in browse {&browse-name} 
                 .
   op-recid  = recid(itemfg).                 
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
    
    RUN pSearchData .
    
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
    
    RUN pSearchData .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rd-filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-filter Dialog-Frame
ON VALUE-CHANGED OF rd-filter IN FRAME Dialog-Frame
DO:
    /* redefined for lookup */
    &scoped-define IAMWHAT LOOKUP   
    ASSIGN rd-filter .     
    assign rd-sort.
    
    RUN pSearchData .
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
  
  IF PROGRAM-NAME(2) MATCHES "*po/d-poordl.*" THEN do:
      RUN sys/ref/nk1look.p (INPUT ip-company, "POItemFilterDefault", "L" /* Logical */, NO /* check by cust */, 
                             INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                             OUTPUT cRtnChar, OUTPUT lRecFound).
      IF lRecFound THEN
          lDefaultFilteras = LOGICAL(cRtnChar) NO-ERROR.
      IF lDefaultFilteras THEN
          ASSIGN rd-filter = 1 .
      ELSE
          rd-filter = 2.
  END.
  ELSE DO:
      ASSIGN rd-filter = 2 .
      rd-filter:HIDDEN in FRAME {&FRAME-NAME} = YES .
  END.
      
  &scoped-define key-phrase {&fld-name-1} >= ip-cur-val
  &scoped-define sortby-phrase {&sortby-1}
  
  RUN enable_UI.

  IF ip-vendor EQ "" THEN do:
      ASSIGN rd-filter = 2 .
      rd-filter:HIDDEN in FRAME {&FRAME-NAME} = YES .
      lv-label:HIDDEN in FRAME {&FRAME-NAME} = YES  .
  END.
  APPLY "value-changed" TO rd-sort.

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
  DISPLAY rd-sort rd-filter lv-search lv-label
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 rd-sort rd-filter bt-clear lv-search bt-ok bt-cancel RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-cust-part Dialog-Frame 
PROCEDURE get-cust-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cp-part-no LIKE itemfg.part-no NO-UNDO.
  DEF VAR cp-rowid AS ROWID NO-UNDO.


  IF AVAIL itemfg AND ll-new-file AND ip-cust-no NE "" THEN DO:
    ASSIGN
     cp-part-no = ""
     cp-rowid   = ROWID(itemfg).

    RUN custom/getcpart.p (ip-company, ip-cust-no,
                           INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
    IF cp-part-no NE "" THEN
      ASSIGN
       lv-part-no = cp-part-no
       lv-cust-no = ip-cust-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSearchData Dialog-Frame 
PROCEDURE pSearchData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
       IF rd-filter EQ 1 THEN do:
            case rd-sort:
            {srtord2.i 1}
            {srtord2.i 2}
            END.
      END.
      ELSE IF rd-filter EQ 2 THEN do:
           
       IF rd-sort EQ 1 THEN do:
        OPEN QUERY {&browse-name}
          FOR EACH itemfg WHERE {&KEY-PHRASE}
              and itemfg.company = ip-company AND itemfg.stat EQ "A"
              AND itemfg.i-no BEGINS lv-search  
              AND itemfg.i-no NE "" NO-LOCK,
              first e-itemfg-vend NO-LOCK 
              where e-itemfg-vend.company eq itemfg.company 
              and e-itemfg-vend.i-no    eq itemfg.i-no 
              and e-itemfg-vend.vend-no EQ ip-vendor  OUTER-JOIN
              {&sortby-1}.
          END.
          ELSE IF rd-sort EQ 2 THEN do:
              OPEN QUERY {&browse-name}
              FOR EACH itemfg WHERE {&KEY-PHRASE}
              and itemfg.company = ip-company AND itemfg.stat EQ "A"
              AND itemfg.i-name BEGINS lv-search  
              AND itemfg.i-no NE ""    NO-LOCK,
              first e-itemfg-vend NO-LOCK 
              where e-itemfg-vend.company eq itemfg.company 
              and e-itemfg-vend.i-no    eq itemfg.i-no 
              and e-itemfg-vend.vend-no EQ ip-vendor  OUTER-JOIN
              {&sortby-2}.
          END.

          IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN DO:
              MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
                  VIEW-AS ALERT-BOX.
              APPLY "ENTRY" TO {&BROWSE-NAME}.
          END.
      END. /* all po */

     apply "entry" to {&browse-name}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cust Dialog-Frame 
FUNCTION get-cust RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  lv-cust-no = itemfg.cust-no.

  RUN get-cust-part.

  RETURN lv-cust-no.            /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-part Dialog-Frame 
FUNCTION get-part RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  lv-part-no = itemfg.part-no.

  RUN get-cust-part.

  RETURN lv-part-no.            /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

