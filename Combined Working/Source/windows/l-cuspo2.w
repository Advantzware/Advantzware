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

/* Local Variable Definitions ---                                       */
DEF INPUT PARAMETER ip-company      LIKE itemfg.company NO-UNDO.
DEF INPUT PARAMETER ip-cur-val      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-char-val    AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-oe-rel LIKE oe-rel
   FIELD selected-flag AS LOG INIT FALSE.

&scoped-define SORTBY-1 BY tt-oe-rel.po-no
&scoped-define SORTBY-2 BY tt-oe-rel.i-no 
&scoped-define fld-name-1 tt-oe-rel.po-no
&scoped-define fld-name-2 tt-oe-rel.i-no 

&scoped-define IAMWHAT LOOKUP

DEF VAR lv-ord-ok AS CHAR INIT "R,I,S,P,A,N,U" NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt-oe-rel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-oe-rel.po-no tt-oe-rel.i-no tt-oe-rel.ord-no tt-oe-rel.line tt-oe-rel.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 IF rd-sort = 1 THEN DO:    OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-rel WHERE tt-oe-rel.po-no BEGINS lv-search                                  BY tt-oe-rel.po-no. END. ELSE DO:     OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-rel WHERE tt-oe-rel.po-no BEGINS lv-search                                   BY tt-oe-rel.i-no. END.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-oe-rel
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-oe-rel


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
     SIZE 52.8 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer PO#", 1,
"Item#", 2
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98.2 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-oe-rel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-oe-rel.po-no FORMAT "x(15)":U WIDTH 21.2
tt-oe-rel.i-no FORMAT "x(15)":U WIDTH 29.2
tt-oe-rel.ord-no FORMAT ">>>>>9":U WIDTH 19.2
tt-oe-rel.line FORMAT "99":U WIDTH 6.2
tt-oe-rel.qty COLUMN-LABEL "Quantity"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 11.19
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1.6
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 76.4
     bt-cancel AT ROW 14.1 COL 88.4
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1.6
     SPACE(0.19) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "PO Information".


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
   OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-rel WHERE tt-oe-rel.po-no BEGINS lv-search
                                 BY tt-oe-rel.po-no.
END.
ELSE DO:
    OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-rel WHERE tt-oe-rel.po-no BEGINS lv-search
                                  BY tt-oe-rel.i-no.
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
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-char-val = tt-oe-rel.po-no 
               + "," + tt-oe-rel.i-no 
               + "," + STRING(tt-oe-rel.ord-no) 
               + "," + STRING(tt-oe-rel.line).

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
   op-char-val = tt-oe-rel.po-no 
               + "," + tt-oe-rel.i-no 
               + "," + STRING(tt-oe-rel.ord-no) 
               + "," + STRING(tt-oe-rel.line).

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

  

  /*&scoped-define key-phrase {&fld-name-1} >= (ip-cur-val)*/
  &scoped-define sortby-phrase {&sortby-1}
  lv-search = ip-cur-val.
  RUN create-tt-oe-rel.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-oe-rel Dialog-Frame 
PROCEDURE create-tt-oe-rel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-oe-ordl FOR oe-ordl.

/* FOR EACH oe-ordl NO-LOCK WHERE oe-ordl.company = ip-company                                           */
/*                            AND oe-ordl.po-no BEGINS ip-cur-val                                        */
/*                            AND oe-ordl.po-no <> ""                                                    */
/*                            AND oe-ordl.i-no  <> "":                                                   */
/*    FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-ordl.company                   */
/*                                  AND b-vend-whse-item.cust-no     = oe-ordl.cust-no                   */
/*                                  AND b-vend-whse-item.fg-item-no  = oe-ordl.i-no NO-LOCK NO-ERROR.    */
/*    IF NOT AVAILABLE(b-vend-whse-item) THEN DO:                                                        */
/*       FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-ordl.company                */
/*                                     AND b-vend-whse-item.fg-item-no  = oe-ordl.i-no NO-LOCK NO-ERROR. */
/*    END.                                                                                               */
/*    IF AVAILABLE(b-vend-whse-item) THEN DO:                                                            */
/*          CREATE tt-oe-ordl.                                                                           */
/*          BUFFER-COPY oe-ordl TO tt-oe-ordl NO-ERROR.                                                  */
/*    END.                                                                                               */
/* END.                                                                                                  */

FOR EACH oe-rel NO-LOCK WHERE oe-rel.company = ip-company
                          AND (oe-rel.stat    = "C" OR oe-rel.stat    = "Z")
                          AND oe-rel.po-no   <> ""
                          AND oe-rel.i-no    <> "":
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = oe-rel.company
                          AND b-oe-ordl.ord-no  = oe-rel.ord-no
                          AND b-oe-ordl.LINE    = oe-rel.LINE
                          AND b-oe-ordl.i-no    = oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                    AND b-vend-whse-item.cust-no     = oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                       AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE(b-vend-whse-item) THEN DO:
            CREATE tt-oe-rel.
            BUFFER-COPY oe-rel TO tt-oe-rel NO-ERROR.
      END.
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

