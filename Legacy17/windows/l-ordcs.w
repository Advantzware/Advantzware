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

/* params Definitions ---                                           */
def input param ip-company like itemfg.company no-undo.
def input param ip-cust-no like oe-ord.cust-no no-undo.
def input param ip-ship-id like oe-relh.ship-id no-undo.
def input param ip-cur-val as char no-undo.
def input param ip-type as int no-undo.
def output param op-char-val as char no-undo.
def output param op-rec-id as recid no-undo.     /* recid output */

/* Local Variable Definitions ---                                       */
&scoped-define SORTBY-1 BY oe-rell.ord-no DESC
&scoped-define SORTBY-2 BY oe-rell.i-no {&sortby-1}
&scoped-define fld-name-1 oe-rell.ord-no
&scoped-define fld-name-2 oe-rell.i-no 
&scoped-define datatype-1 INT

&scoped-define IAMWHAT LOOKUP

/*def var lv-ord-ok as cha init "R,I,S,P,A,N,U" no-undo.*/

DEF TEMP-TABLE tt-rell FIELD tt-rowid AS ROWID
                       FIELD tt-ord-no LIKE oe-rell.ord-no
                       FIELD tt-i-no LIKE oe-rell.i-no
                       FIELD tt-est-no LIKE oe-ordl.est-no
                       FIELD tt-ord-date LIKE oe-ord.ord-date.

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
&Scoped-define INTERNAL-TABLES tt-rell oe-rell oe-relh

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 oe-rell.ord-no oe-rell.po-no oe-rell.i-no oe-rell.rel-no oe-rell.b-ord-no oe-relh.release# oe-rell.job-no oe-rell.job-no2 tt-est-no tt-ord-date   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-rell, ~
                                   FIRST oe-rell WHERE ROWID(oe-rell) EQ tt-rowid NO-LOCK, ~
                                   FIRST oe-relh OF oe-rell NO-LOCK                             ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-rell, ~
                                   FIRST oe-rell WHERE ROWID(oe-rell) EQ tt-rowid NO-LOCK, ~
                                   FIRST oe-relh OF oe-rell NO-LOCK                             ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-rell oe-rell oe-relh
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-rell
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 oe-rell
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 oe-relh


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
     SIZE 10 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order#", 1,
"FGItem#", 2
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 104 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-rell, 
      oe-rell, 
      oe-relh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      oe-rell.ord-no    LABEL "Order#"
      oe-rell.po-no     LABEL "Customer PO#"
      oe-rell.i-no      LABEL "FG Item#"
      oe-rell.rel-no    LABEL "Order Rel#"
      oe-rell.b-ord-no  LABEL "BO#"
      oe-relh.release#  LABEL "Release#"
      oe-rell.job-no    LABEL "Job#"
      oe-rell.job-no2   LABEL ""
      tt-est-no         LABEL "    Est#"
                        FORMAT "x(8)"
      tt-ord-date       LABEL "Order Date"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 83
     bt-cancel AT ROW 14.1 COL 94
     RECT-1 AT ROW 12.43 COL 1
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     SPACE(93.00) SKIP(2.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Available Releases for".


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
OPEN QUERY {&SELF-NAME} FOR EACH tt-rell,
                            FIRST oe-rell WHERE ROWID(oe-rell) EQ tt-rowid NO-LOCK,
                            FIRST oe-relh OF oe-rell NO-LOCK
                            ~{&SORTBY-PHRASE}
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.oe-ord.company = ip-company
and (oe-ord.cust-no = ip-cust-no or ip-cust-no = """")
 AND lookup(oe-ord.stat,lv-ord-ok) > 0 "
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Available Releases for */
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
   op-char-val = string(oe-rell.ord-no,">>>>>9") + "," + oe-rell.i-no
                 .
   op-rec-id = recid(oe-rell).

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
   op-char-val = string(oe-rell.ord-no,">>>>>9") + "," + oe-rell.i-no
                 .
   op-rec-id = recid(oe-rell).

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

    case rd-sort:
        {srtord.i 1}
        {srtord.i 2} 
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

  ASSIGN
   FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) +
                               " Customer#/ShipID: " +
                               TRIM(ip-cust-no) + "/" + TRIM(ip-ship-id)
   rd-sort = ip-type.

  RUN build-table.

  RUN enable_UI.
  
  DO WITH FRAME {&FRAME-NAME}:
    {&browse-name}:SET-REPOSITIONED-ROW(INT({&browse-name}:DOWN / 2),"always").
  
    IF rd-sort EQ 1 THEN
      FOR EACH tt-rell
          WHERE tt-ord-no LE INT(ip-cur-val)
          NO-LOCK
          {&sortby-1}:
        LEAVE.
      END.
    ELSE
      FOR EACH tt-rell
          WHERE tt-i-no GE ip-cur-val
          NO-LOCK
          {&sortby-2}:
         LEAVE.
      END.

    APPLY "value-changed" TO rd-sort.

    IF AVAIL tt-rell THEN 
      REPOSITION {&browse-name} TO ROWID ROWID(tt-rell) NO-ERROR.    
  END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ ip-company
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.cust-no EQ ip-cust-no
      USE-INDEX opened NO-LOCK,

      FIRST oe-ord OF oe-ordl NO-LOCK,

      EACH oe-rell
      WHERE oe-rell.company  EQ oe-ordl.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no)
      NO-LOCK,

      FIRST oe-relh
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        AND oe-relh.posted  EQ YES
        AND oe-relh.printed EQ YES
        AND oe-relh.deleted EQ NO
        AND oe-relh.ship-id EQ ip-ship-id
      NO-LOCK
      
      BREAK BY oe-rell.ord-no
            BY oe-rell.line
            BY oe-rell.i-no
            BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no:

    IF FIRST-OF(oe-rell.b-ord-no) THEN DO:
      CREATE tt-rell.
      ASSIGN
       tt-rowid    = ROWID(oe-rell)
       tt-ord-no   = oe-rell.ord-no
       tt-i-no     = oe-rell.i-no
       tt-est-no   = oe-ordl.est-no
       tt-ord-date = oe-ord.ord-date.
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
  ENABLE BROWSE-1 rd-sort bt-clear lv-search bt-ok bt-cancel RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

