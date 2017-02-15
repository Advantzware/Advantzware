&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
DEF INPUT PARAM ipcCompany AS CHARACTER NO-UNDO.
DEF INPUT PARAM ip-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF INPUT PARAM ip-i-no LIKE oe-ordl.i-no NO-UNDO.
DEF INPUT PARAM ip-po-no LIKE oe-rel.po-no NO-UNDO.
DEF INPUT PARAM ip-job-no AS cha NO-UNDO.
DEF INPUT PARAM ip-qty LIKE oe-rel.qty NO-UNDO.
DEF OUTPUT PARAM op-select AS cha NO-UNDO.  /*All,One,No Tag"*/

/* Local Variable Definitions ---                                       */

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
&Scoped-Define ENABLED-OBJECTS Btn_yes Btn_job Btn_no Btn_notag 
&Scoped-Define DISPLAYED-OBJECTS v-order v-fgitem v-po-no v-job-no v-qty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getJobOnly D-Dialog 
FUNCTION getJobOnly RETURNS LOGICAL
  (ipcCompany AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_job AUTO-GO 
     LABEL "Select Bin/Tags" 
     SIZE 50 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_no AUTO-END-KEY 
     LABEL "Select Bin/Tags via FIFO" 
     SIZE 50 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_notag AUTO-GO 
     LABEL "No Tags" 
     SIZE 50 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_yes AUTO-GO 
     LABEL "Select Bin/Tags (All Jobs)" 
     SIZE 50 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE v-fgitem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE v-job-no AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE v-order AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE v-po-no AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE v-qty AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     v-order AT ROW 1.24 COL 1 COLON-ALIGNED NO-LABEL
     Btn_yes AT ROW 1.24 COL 55
     v-fgitem AT ROW 2.43 COL 1 COLON-ALIGNED NO-LABEL
     Btn_job AT ROW 2.67 COL 55
     v-po-no AT ROW 3.62 COL 1 COLON-ALIGNED NO-LABEL
     Btn_no AT ROW 4.1 COL 55
     v-job-no AT ROW 4.81 COL 1 COLON-ALIGNED NO-LABEL
     Btn_notag AT ROW 5.52 COL 55
     v-qty AT ROW 6 COL 1 COLON-ALIGNED NO-LABEL
     SPACE(51.00) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Bins/Tags?"
         DEFAULT-BUTTON Btn_yes CANCEL-BUTTON Btn_no.


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

/* SETTINGS FOR FILL-IN v-fgitem IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-job-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-order IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-po-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-qty IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Select Bins/Tags? */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  op-select = "Cancel".
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_job D-Dialog
ON CHOOSE OF Btn_job IN FRAME D-Dialog /* Select Bin/Tags */
DO:
  op-select = "ONE".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_no D-Dialog
ON CHOOSE OF Btn_no IN FRAME D-Dialog /* Select Bin/Tags via FIFO */
DO:
  op-select = "ONE".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_notag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_notag D-Dialog
ON CHOOSE OF Btn_notag IN FRAME D-Dialog /* No Tags */
DO:
  op-select = "NoTag".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_yes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_yes D-Dialog
ON CHOOSE OF Btn_yes IN FRAME D-Dialog /* Select Bin/Tags (All Jobs) */
DO:
  op-select = "ALL".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


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
  DISPLAY v-order v-fgitem v-po-no v-job-no v-qty 
      WITH FRAME D-Dialog.
  ENABLE Btn_yes Btn_job Btn_no Btn_notag 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-order  = "Order#: " + string(ip-ord-no,">>>>>>>>>>")
     v-fgitem = "FG Item#: " + ip-i-no 
     v-po-no  = "PO#: " + ip-po-no
     v-job-no = "Job#: " + ip-job-no
     v-qty    = "Qty: " + TRIM(STRING(ip-qty,"->>>,>>>,>>9")).
    
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF getJobOnly(INPUT ipcCompany) THEN
        ASSIGN 
            btn_yes:SENSITIVE = NO
            btn_no:SENSITIVE = NO
        .
    IF TRIM(ip-job-no) EQ "" THEN
        Btn_job:SENSITIVE = NO.
    ELSE
        Btn_job:LABEL = TRIM(Btn_job:LABEL) +
                     " (For Job#: " + TRIM(ip-job-no) + " Only)".
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getJobOnly D-Dialog 
FUNCTION getJobOnly RETURNS LOGICAL
  (ipcCompany AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  REturns whether the tag must be limited to the job only based on 
  RELMERGE integer value = 0.
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lReturn AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (INPUT ipcCompany, 
                           INPUT "RELMERGE", 
                           INPUT "I", 
                           INPUT NO, 
                           INPUT NO, 
                           INPUT "", 
                           INPUT "", 
                           OUTPUT cReturn, 
                           OUTPUT lReturn).
    
    lReturn = INT(cReturn) EQ 0.

    RETURN lReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

