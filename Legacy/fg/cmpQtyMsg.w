&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


/* -------------------------------------------------- fg/checkset.p 08/99 JLF */
/* Check qty on hand for components of a set                                  */
/* -------------------------------------------------------------------------- */

DEF INPUT        PARAM ip-rowid1    AS   ROWID              NO-UNDO.
DEF INPUT        PARAM ip-rowid2    AS   ROWID              NO-UNDO.
DEF INPUT        PARAM ip-job-no    LIKE fg-rctd.job-no     NO-UNDO.
DEF INPUT        PARAM ip-job-no2   LIKE fg-rctd.job-no2    NO-UNDO.
DEF INPUT        PARAM io-set-qty   AS   INT                NO-UNDO.
DEF INPUT        PARAM ip-loc       AS   CHAR               NO-UNDO.
DEF OUTPUT       PARAM oplUseMax    AS   LOG                NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b2-fg-rctd FOR fg-rctd.
DEF BUFFER use-job FOR reftable.

DEF VAR v-set           LIKE itemfg.i-no NO-UNDO.
DEF VAR v-comp          LIKE itemfg.i-no NO-UNDO.
DEF VAR lv-q-onh        LIKE itemfg.q-onh NO-UNDO.
DEF VAR lv-q-alloc      LIKE itemfg.q-alloc NO-UNDO.
DEF VAR lv-partset      AS   INT NO-UNDO.
DEF VAR v-set-use       AS   INT NO-UNDO.
DEF VAR v-max-qty       AS   INT NO-UNDO.

{fg/fullset.i NEW}

DEF TEMP-TABLE tt-set NO-UNDO
                      FIELD comp-desc AS CHAR
                      FIELD comp LIKE itemfg.i-no
                      FIELD alloc AS INT 
                      FIELD onhand AS INT
                      FIELD required AS INT
                      FIELD qty-set AS INT
                      FIELD setqty AS INT.

DO TRANSACTION:
  {sys/inc/fgsetrec.i}
END.

DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGetBin AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-set

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-set.comp tt-set.comp-desc tt-set.qty-set tt-set.required tt-set.alloc tt-set.onhand tt-set.setqty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-set
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-set.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-set
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-set


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_use-job Btn_OK btCancel btUseMaxQty 
&Scoped-Define DISPLAYED-OBJECTS tb_use-job 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btUseMaxQty 
     LABEL "Use Max Quantity" 
     SIZE 27 BY 1.14.

DEFINE VARIABLE tb_use-job AS LOGICAL INITIAL no 
     LABEL "Use Job#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-set SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-set.comp   LABEL "Component"       FORMAT "X(20)"
      tt-set.comp-desc LABEL "Name" FORMAT "x(30)"
      tt-set.qty-set LABEL "Qty Per Set"
      tt-set.required LABEL "Qty Req'd"
      tt-set.alloc  LABEL "Qty Allocated"
      tt-set.onhand LABEL "Qty On-Hand"
      tt-set.setqty LABEL "Max Sets"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 10.71
         BGCOLOR 8 FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 1.24 COL 2
     tb_use-job AT ROW 12.43 COL 3
     Btn_OK AT ROW 16.24 COL 64.8
     btCancel AT ROW 16.24 COL 81.8 WIDGET-ID 8
     btUseMaxQty AT ROW 16.29 COL 35.8 WIDGET-ID 6
     "There is insufficient inventory for some set components." VIEW-AS TEXT
          SIZE 66 BY .95 AT ROW 12.38 COL 40.8 WIDGET-ID 2
          FONT 6
     "Please review the quantities available for components highlighted in yellow." VIEW-AS TEXT
          SIZE 87 BY .95 AT ROW 13.43 COL 29.8 WIDGET-ID 4
     SPACE(22.59) SKIP(3.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Set / Components"
         DEFAULT-BUTTON Btn_OK.

DEFINE FRAME FRAME-A
     "Choose the 'Use Max Quantity' button to create receipt with the maximum quantity" VIEW-AS TEXT
          SIZE 94 BY 1.19 AT ROW 1 COL 5.8 WIDGET-ID 2
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 14.57
         SIZE 109 BY 1.19 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 TEXT-2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BROWSE-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       FRAME FRAME-A:SENSITIVE        = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-set.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Set / Components */
DO:
  /*APPLY "END-ERROR":U TO SELF.*/
  APPLY "choose" TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME Dialog-Frame
DO:
  io-set-qty = tt-set.setqty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-2 IN FRAME Dialog-Frame
DO:
  RUN set-bg-color.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel Dialog-Frame
ON CHOOSE OF btCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  FIND CURRENT use-job NO-ERROR.
  IF AVAIL use-job THEN use-job.val[1] = INT(tb_use-job).
  FIND CURRENT use-job NO-LOCK NO-ERROR.

  io-set-qty = v-max-qty.
  APPLY 'GO' TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  FIND CURRENT use-job NO-ERROR.
  IF AVAIL use-job THEN use-job.val[1] = INT(tb_use-job).
  FIND CURRENT use-job NO-LOCK NO-ERROR.

  io-set-qty = v-max-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUseMaxQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUseMaxQty Dialog-Frame
ON CHOOSE OF btUseMaxQty IN FRAME Dialog-Frame /* Use Max Quantity */
DO:
  oplUseMax = TRUE.
  APPLY 'GO' TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_use-job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_use-job Dialog-Frame
ON VALUE-CHANGED OF tb_use-job IN FRAME Dialog-Frame /* Use Job#? */
DO:
  ASSIGN {&self-name}.
  RUN recheckset.
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
  
  FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid2 EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL fg-rctd THEN DO:
    ASSIGN fg-rctd.use-job = fgsetrec-log.

    IF ip-rowid1 EQ ? THEN DO:
      FIND FIRST itemfg
          WHERE itemfg.company EQ fg-rctd.company
            AND itemfg.i-no    EQ fg-rctd.i-no
          NO-LOCK NO-ERROR.
      tb_use-job = fgsetrec-log.
    END.

    ELSE DO:
      FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid1 NO-LOCK NO-ERROR.
      tb_use-job = fgsetrec-log.
    END.
    RELEASE fg-rctd.
  END.

  IF AVAIL itemfg THEN DO:
    v-set = itemfg.i-no.
   
    FOR EACH b-fg-rctd FIELDS(t-qty)
        WHERE b-fg-rctd.company   EQ cocode   
          AND b-fg-rctd.i-no      EQ itemfg.i-no
          AND b-fg-rctd.rita-code EQ "R"
          AND ROWID(b-fg-rctd)    NE ip-rowid2
        NO-LOCK:
      v-set-use = v-set-use + b-fg-rctd.t-qty.
    END.
    
    IF itemfg.alloc EQ YES THEN
    FOR EACH fg-bin FIELDS(qty)
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.job-no  EQ ip-job-no
          AND fg-bin.job-no2 EQ ip-job-no2
        NO-LOCK:      
      v-set-use = v-set-use + fg-bin.qty.
    END.

    /* Yes is unassembled */
    IF itemfg.alloc EQ YES THEN tb_use-job = YES.

    RUN checkset.

    IF CAN-FIND(FIRST tt-set) THEN DO:
      FRAME {&FRAME-NAME}:TITLE = "Set: " + TRIM(CAPS(v-set)) +
                                  " / Components" + " " +
                                  (IF ip-job-no NE "" THEN 
                                     "For Job#: " + TRIM(ip-job-no) + "-" + STRING(ip-job-no2,"99")
                                   ELSE "").
      RUN enable_UI.

      /* For assembled sets,    */
      /* if fgsetrec-log  then search only for current job (with checkbox option) */
      /* otherwise, always search all jobs (so checkbox not needed) */
      IF itemfg.alloc EQ NO /* Assembled */ 
        OR fgsetrec-log EQ NO /* All jobs */ THEN DO WITH FRAME {&FRAME-NAME}:

        tb_use-job:HIDDEN = YES.
      
        IF lFGSetAssembly THEN
          ASSIGN btUseMaxQty:HIDDEN   = YES
                 btCancel:HIDDEN      = YES
                 FRAME FRAME-A:HIDDEN = YES.
        ELSE
          ASSIGN btn_Ok:HIDDEN        = YES. 

        IF BROWSE browse-2:NUM-SELECTED-ROWS GT 0 THEN
          BROWSE BROWSE-2:DESELECT-SELECTED-ROW ( 1 ) NO-ERROR.
        tt-set.alloc:VISIBLE IN BROWSE browse-2 = FALSE.
      
      END. /* Do with frame */

      WAIT-FOR GO OF FRAME {&FRAME-NAME}.

    END. /* If can find first tt-set */
  END. /* If avail itemfg */
END. /* Do...*/

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkset Dialog-Frame 
PROCEDURE checkset :
/*------------------------------------------------------------------------------
  Purpose:     from checkset.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-int AS INT NO-UNDO.
           
  v-max-qty = io-set-qty.

  RUN fg/fullset.p (ROWID(itemfg)).

  FOR EACH tt-fg-set,
      FIRST b-itemfg
      WHERE b-itemfg.company EQ itemfg.company
        AND b-itemfg.i-no    EQ tt-fg-set.part-no
        AND b-itemfg.i-no    NE itemfg.i-no
      NO-LOCK:  

    ASSIGN
     lv-q-onh   = 0
     lv-q-alloc = 0.

    FOR EACH fg-bin FIELDS(qty)
        WHERE fg-bin.company   EQ b-itemfg.company
          AND fg-bin.i-no      EQ b-itemfg.i-no
          AND ((fg-bin.job-no  EQ ip-job-no AND
                fg-bin.job-no2 EQ ip-job-no2) OR
               NOT tb_use-job) 
          AND (IF lFGSetAssembly THEN  fg-bin.loc EQ ip-loc ELSE TRUE)
          AND (IF lFGSetAssembly  THEN fg-bin.loc-bin EQ cFGSetAssembly ELSE TRUE)
        NO-LOCK:
      lv-q-onh = lv-q-onh + fg-bin.qty.
    END.

    IF itemfg.alloc NE YES THEN
      lv-q-alloc = b-itemfg.q-alloc + (IF PROGRAM-NAME(2) BEGINS "oe/oe-ordlu." THEN
                                         (io-set-qty * tt-fg-set.part-qty-dec)
                                       ELSE 0).

    IF ip-rowid1 NE ? THEN
    FOR EACH b-fg-rctd FIELDS(t-qty r-no)
        WHERE b-fg-rctd.company   EQ cocode   
          AND b-fg-rctd.i-no      EQ b-itemfg.i-no
          AND b-fg-rctd.rita-code EQ "R"
          AND ((b-fg-rctd.job-no  EQ ip-job-no AND
                b-fg-rctd.job-no2 EQ ip-job-no2) OR
               NOT tb_use-job)
   AND (IF lFGSetAssembly AND AVAIL(fg-rctd) THEN b-fg-rctd.loc EQ fg-rctd.loc ELSE TRUE)
   AND (IF lFGSetAssembly THEN b-fg-rctd.loc-bin EQ cFGSetAssembly ELSE TRUE)
          NO-LOCK:



       IF AVAIL fg-rctd AND fg-rctd.SetHeaderRno NE 0 THEN
          DO:
           v-int = b-fg-rctd.r-no.
           IF NOT CAN-FIND(FIRST b2-fg-rctd WHERE
             b2-fg-rctd.r-no EQ v-int AND
             b2-fg-rctd.rita-code NE "P"
             AND ROWID(b2-fg-rctd)    NE ip-rowid2) THEN
             NEXT.
          END.   

      /* v-set-use already includes this number, unless there is another */
      /* positive receipt for the component separate from the set header */
      IF b-fg-rctd.t-qty GT 0 THEN
        lv-q-onh = lv-q-onh + b-fg-rctd.t-qty.
    END.
    
    ASSIGN
     lv-q-onh   = lv-q-onh - (v-set-use * tt-fg-set.part-qty-dec)
     lv-partset = TRUNC(lv-q-onh / tt-fg-set.part-qty-dec,0).

    IF TRUE /* (io-set-qty * tt-fg-set.part-qty-dec) GT lv-q-onh */ THEN DO:
      IF ip-rowid1 NE ? THEN DO:
        CREATE tt-set.
        ASSIGN
         tt-set.comp      = b-itemfg.i-no
         tt-set.comp-desc = b-itemfg.i-name
         tt-set.onhand    = lv-q-onh
         tt-set.alloc     = lv-q-alloc
         tt-set.required  = (io-set-qty * tt-fg-set.part-qty-dec)
         tt-set.qty-set   = tt-fg-set.part-qty-dec
         tt-set.setqty    = lv-partset.
      END.
      
      IF lv-partset LT v-max-qty THEN v-max-qty = lv-partset.
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
  HIDE FRAME FRAME-A.
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
  DISPLAY tb_use-job 
      WITH FRAME Dialog-Frame.
  ENABLE tb_use-job Btn_OK btCancel btUseMaxQty 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  VIEW FRAME FRAME-A.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  FRAME FRAME-A:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recheckset Dialog-Frame 
PROCEDURE recheckset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE tt-set.

  RUN checkset.

  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-FIND(FIRST tt-set) THEN DO:
      {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
    END.

    ELSE APPLY "choose" TO Btn_OK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-bg-color Dialog-Frame 
PROCEDURE set-bg-color :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL(tt-set) AND (tt-set.onhand 
     + tt-set.alloc
      LT tt-set.required) THEN
  ASSIGN tt-set.onhand:BGCOLOR IN BROWSE {&BROWSE-NAME}         = 14
         tt-set.comp-desc:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 14
         tt-set.comp:BGCOLOR IN BROWSE {&BROWSE-NAME}           = 14
         tt-set.alloc:BGCOLOR IN BROWSE {&BROWSE-NAME}          = 14
         tt-set.qty-set:BGCOLOR IN BROWSE {&BROWSE-NAME}        = 14
         tt-set.setqty:BGCOLOR IN BROWSE {&BROWSE-NAME}         = 14
         tt-set.required:BGCOLOR IN BROWSE {&BROWSE-NAME}       = 14.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

