&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: po\d-clspo.w

  Description: Close/Reopen POs

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{sys/inc/var.i SHARED}
DEF VAR ll-close AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 begin_ord end_ord begin_date end_date ~
tb_only btn_ok 
&Scoped-Define DISPLAYED-OBJECTS begin_ord end_ord begin_date end_date ~
tb_only 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_ok AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 6.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 6.67.

DEFINE VARIABLE tb_only AS LOGICAL INITIAL yes 
     LABEL "Close only those orders where all invoiced qtys have been shipped?" 
     VIEW-AS TOGGLE-BOX
     SIZE 69 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     begin_ord AT ROW 2.19 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Order#"
     end_ord AT ROW 2.19 COL 63 COLON-ALIGNED HELP
          "Enter Beginning Order#"
     begin_date AT ROW 4.1 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_date AT ROW 4.1 COL 63 COLON-ALIGNED HELP
          "Enter Endng Order Date"
     tb_only AT ROW 6 COL 9
     btn_ok AT ROW 8.62 COL 35
     RECT-1 AT ROW 1.24 COL 1
     SPACE(0.19) SKIP(2.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Close Orders"
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Close Orders */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date D-Dialog
ON LEAVE OF begin_date IN FRAME D-Dialog /* Beginning Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord D-Dialog
ON LEAVE OF begin_ord IN FRAME D-Dialog /* Beginning Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok D-Dialog
ON CHOOSE OF btn_ok IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  MESSAGE "Are you sure you want to " +
          TRIM(STRING(ll-close,"close/reopen")) +
          " the selected Orders?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN DO WITH FRAME {&FRAME-NAME}:
    SESSION:SET-WAIT-STATE("general").
    
    order-close:
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company     EQ cocode
          AND oe-ord.opened      EQ ll-close
          AND oe-ord.ord-no      GE begin_ord
          AND oe-ord.ord-no      LE end_ord
          AND oe-ord.ord-date    GE begin_date
          AND oe-ord.ord-date    LE end_date
        USE-INDEX opened.

      IF ll-close THEN DO:
        IF tb_only THEN
        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ oe-ord.company
              AND oe-ordl.ord-no  EQ oe-ord.ord-no:
          IF oe-ordl.t-ship-qty LT oe-ordl.t-inv-qty THEN NEXT order-close.
        END.

        lv-msg = "".

        IF lv-msg EQ "" THEN
        FOR EACH inv-line NO-LOCK
            WHERE inv-line.company EQ oe-ord.company
              AND inv-line.ord-no  EQ oe-ord.ord-no,
            FIRST inv-head WHERE inv-head.r-no EQ inv-line.r-no NO-LOCK:
          lv-msg = "Invoice" +
                   (IF inv-head.inv-no EQ 0 THEN
                      " for BOL# " + TRIM(STRING(inv-head.bol-no,">>>>>>>>>>"))
                    ELSE
                      "# " + TRIM(STRING(inv-head.inv-no,">>>>>>>>>>"))) +
                   " with FG Item# " + TRIM(inv-line.i-no).
          LEAVE.
        END.

        IF lv-msg EQ "" THEN
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-ord.company
              AND oe-boll.ord-no  EQ oe-ord.ord-no,
            FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.b-no   EQ oe-boll.b-no
              AND oe-bolh.posted EQ NO:
          lv-msg = "BOL# " + TRIM(STRING(oe-bolh.bol-no,">>>>>>>>>>")) +
                   " with FG Item# " + TRIM(oe-boll.i-no).
          LEAVE.
        END.

        IF lv-msg EQ "" THEN
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company EQ oe-ord.company
              AND oe-rell.ord-no  EQ oe-ord.ord-no,
            FIRST oe-relh NO-LOCK
            WHERE oe-relh.r-no   EQ oe-rell.r-no
              AND oe-relh.posted EQ NO:
          lv-msg = "Release# " + TRIM(STRING(oe-relh.release#,">>>>>>>>>>")) +
                   " with FG Item# " + TRIM(oe-rell.i-no).
          LEAVE.
        END.

        IF lv-msg NE "" THEN DO:
          ll = NO.
          MESSAGE "Unposted " + TRIM(lv-msg) + " exists for Order# " +
                  TRIM(STRING(oe-ord.ord-no,">>>>>>>>>>")) + ", close anyway?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              UPDATE ll.
          IF NOT ll THEN NEXT order-close.
        END.
      END.

      RUN oe/close.p (RECID(oe-ord), ll-close).
    END.

    SESSION:SET-WAIT-STATE("").
        
    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date D-Dialog
ON LEAVE OF end_date IN FRAME D-Dialog /* Ending Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord D-Dialog
ON LEAVE OF end_ord IN FRAME D-Dialog /* Ending Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_only D-Dialog
ON VALUE-CHANGED OF tb_only IN FRAME D-Dialog /* Close only those orders where all invoiced qtys have been shipped? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL oe-ord THEN RETURN.

ASSIGN
 begin_ord  = oe-ord.ord-no
 end_ord    = oe-ord.ord-no
 begin_date = oe-ord.ord-date
 end_date   = oe-ord.ord-date
 ll-close   = INDEX("CDZ",oe-ord.stat) EQ 0

 FRAME {&FRAME-NAME}:TITLE = (IF ll-close THEN "Close" ELSE "Reopen") + " Orders".

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY begin_ord end_ord begin_date end_date tb_only 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 begin_ord end_ord begin_date end_date tb_only btn_ok 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ll-close THEN DISABLE tb_only.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

