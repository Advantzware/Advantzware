&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cust

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 cust.cust-no cust.type   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 cust.type   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 cust
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 cust
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH cust                             WHERE cust.company    EQ cocode                               AND cust.cust-no    GE begin_cust-no                               AND cust.cust-no    LE end_cust-no                               AND TRIM(cust.type) EQ ""
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH cust                             WHERE cust.company    EQ cocode                               AND cust.cust-no    GE begin_cust-no                               AND cust.cust-no    LE end_cust-no                               AND TRIM(cust.type) EQ "".
&Scoped-define TABLES-IN-QUERY-BROWSE-2 cust
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 cust


/* Definitions for DIALOG-BOX D-Dialog                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no btn_go BROWSE-2 ~
Btn_process Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 38 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_go 
     LABEL "Go" 
     SIZE 11 BY 1.

DEFINE BUTTON Btn_process 
     LABEL "Set the rest to 'OTHER'" 
     SIZE 38 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      cust SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      cust.cust-no LABEL "Cust#" WIDTH 15
     cust.type    LABEL "Type"  WIDTH 10
     ENABLE cust.type
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 38 BY 15
         BGCOLOR 8 FONT 6 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     begin_cust-no AT ROW 1.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 27 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     btn_go AT ROW 3.86 COL 19
     BROWSE-2 AT ROW 5.29 COL 6
     Btn_process AT ROW 20.76 COL 6
     Btn_Cancel AT ROW 22.19 COL 6
     SPACE(3.99) SKIP(0.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Set Blank Customer Type"
         DEFAULT-BUTTON btn_go CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
/* BROWSE-TAB BROWSE-2 btn_go D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME D-Dialog     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME D-Dialog     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH cust
                            WHERE cust.company    EQ cocode
                              AND cust.cust-no    GE begin_cust-no
                              AND cust.cust-no    LE end_cust-no
                              AND TRIM(cust.type) EQ ""
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Set Blank Customer Type */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no D-Dialog
ON LEAVE OF begin_cust-no IN FRAME D-Dialog /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go D-Dialog
ON CHOOSE OF btn_go IN FRAME D-Dialog /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN build-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_process D-Dialog
ON CHOOSE OF Btn_process IN FRAME D-Dialog /* Set the rest to 'OTHER' */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  SESSION:SET-WAIT-STATE ("general").


  ll = NO.
  MESSAGE "Are you sure you wish set Type to 'OTHER' for all Customers not updated?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.

  IF ll THEN DO:
    FOR EACH cust
        WHERE cust.company    EQ cocode
          AND cust.cust-no    GE begin_cust-no
          AND cust.cust-no    LE end_cust-no
          AND TRIM(cust.type) EQ "":
      cust.type = "OTHER".
    END.
  END.

  SESSION:SET-WAIT-STATE ("").

  IF ll THEN MESSAGE "Process Completed..." VIEW-AS ALERT-BOX.

  RUN build-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no D-Dialog
ON LEAVE OF end_cust-no IN FRAME D-Dialog /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


ON 'leave':U OF cust.type IN BROWSE {&browse-name}
DO:
  cust.type:SCREEN-VALUE IN BROWSE {&browse-name} =
      CAPS(cust.type:SCREEN-VALUE IN BROWSE {&browse-name}).

  IF cust.type:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
     NOT CAN-FIND(FIRST custype
                  WHERE custype.company EQ cocode
                    AND custype.custype EQ cust.type:SCREEN-VALUE IN BROWSE {&browse-name})
  THEN DO:
    MESSAGE "Invalid Customer Type, try help..."
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO cust.type IN BROWSE {&browse-name}.
    RETURN NO-APPLY.
  END.
END.

ON 'help':U OF BROWSE {&browse-name}
DO:
  DEF VAR lv-handle AS HANDLE NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.

  CASE FOCUS:NAME :
    WHEN "type" THEN DO:
      RUN windows/l-csttyp.w (cocode, cust.type:SCREEN-VALUE IN BROWSE {&browse-name}, OUTPUT char-val).
      IF char-val NE "" THEN
        cust.type:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* ***************************  Main Block  *************************** */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  SESSION:SET-WAIT-STATE ("general").

  {&OPEN-QUERY-{&BROWSE-NAME}}

  SESSION:SET-WAIT-STATE ("").

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
  DISPLAY begin_cust-no end_cust-no 
      WITH FRAME D-Dialog.
  ENABLE begin_cust-no end_cust-no btn_go BROWSE-2 Btn_process Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

