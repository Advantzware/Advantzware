&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: crmCustomers.w

  Description: import contacts from ZOHO CRM

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 6.24.2016

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcCompany AS CHARACTER NO-UNDO INITIAL "001".
&ENDIF

/* Local Variable Definitions ---                                       */

{CRM/ttCRMCustomers.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME crmAccounts

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCRMCustomers

/* Definitions for BROWSE crmAccounts                                   */
&Scoped-define FIELDS-IN-QUERY-crmAccounts ttCRMCustomers.tickerSymbol ttCRMCustomers.crmName ttCRMCustomers.crmPhone ttCRMCustomers.xxApplyAction ttCRMCustomers.action ttCRMCustomers.custName ttCRMCustomers.custAreaCode ttCRMCustomers.custPhone ttCRMCustomers.custStreet ttCRMCustomers.custStreet2 ttCRMCustomers.custCity ttCRMCustomers.custState ttCRMCustomers.custCode   
&Scoped-define ENABLED-FIELDS-IN-QUERY-crmAccounts ttCRMCustomers.xxApplyAction   
&Scoped-define ENABLED-TABLES-IN-QUERY-crmAccounts ttCRMCustomers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-crmAccounts ttCRMCustomers
&Scoped-define SELF-NAME crmAccounts
&Scoped-define QUERY-STRING-crmAccounts FOR EACH ttCRMCustomers
&Scoped-define OPEN-QUERY-crmAccounts OPEN QUERY {&SELF-NAME} FOR EACH ttCRMCustomers.
&Scoped-define TABLES-IN-QUERY-crmAccounts ttCRMCustomers
&Scoped-define FIRST-TABLE-IN-QUERY-crmAccounts ttCRMCustomers


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svSelect crmAccounts btnApply btnReset ~
btnSave btnCancel 
&Scoped-Define DISPLAYED-OBJECTS svSelect svStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 Dialog-Frame btnReset btnSave btnCancel 
&Scoped-define List-4 Dialog-Frame btnReset btnSave btnCancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply 
     IMAGE-UP FILE "CRM/images/apply.jpg":U
     LABEL "&Apply" 
     SIZE 4.4 BY 1 TOOLTIP "Apply Selected Actions".

DEFINE BUTTON btnCancel AUTO-GO 
     IMAGE-UP FILE "CRM/images/cancel.jpg":U
     LABEL "&Cancel" 
     SIZE 4.4 BY 1 TOOLTIP "Cancel".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "CRM/images/reset.jpg":U
     LABEL "&Reset" 
     SIZE 4.4 BY 1 TOOLTIP "Reset".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "CRM/images/save.jpg":U
     LABEL "&Save" 
     SIZE 4.4 BY 1 TOOLTIP "Save Selected Actions".

DEFINE VARIABLE svStatus AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 49 BY .62 NO-UNDO.

DEFINE VARIABLE svSelect AS LOGICAL INITIAL no 
     LABEL "Select" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY crmAccounts FOR 
      ttCRMCustomers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE crmAccounts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS crmAccounts Dialog-Frame _FREEFORM
  QUERY crmAccounts DISPLAY
      ttCRMCustomers.tickerSymbol
    ttCRMCustomers.crmName
    ttCRMCustomers.crmPhone
    ttCRMCustomers.xxApplyAction VIEW-AS TOGGLE-BOX
    ttCRMCustomers.action
    ttCRMCustomers.custName
    ttCRMCustomers.custAreaCode
    ttCRMCustomers.custPhone
    ttCRMCustomers.custStreet
    ttCRMCustomers.custStreet2
    ttCRMCustomers.custCity
    ttCRMCustomers.custState
    ttCRMCustomers.custCode
    ENABLE
    ttCRMCustomers.xxApplyAction
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 225 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     svSelect AT ROW 1 COL 67 WIDGET-ID 2
     crmAccounts AT ROW 1.95 COL 2 WIDGET-ID 200
     btnApply AT ROW 7.19 COL 95 HELP
          "Apply Selected Actions" WIDGET-ID 76
     btnReset AT ROW 7.19 COL 100 HELP
          "Reset" WIDGET-ID 16
     btnSave AT ROW 7.19 COL 105 HELP
          "Save Selected Actions" WIDGET-ID 18
     btnCancel AT ROW 7.19 COL 110 HELP
          "Cancel" WIDGET-ID 4
     svStatus AT ROW 7.19 COL 2 NO-LABEL WIDGET-ID 78
     "CRM Accounts" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 26 WIDGET-ID 80
     "Advantzware Customers" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 1.24 COL 126 WIDGET-ID 82
     SPACE(77.00) SKIP(6.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ZOHO CRM (Customers)" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME 3 4                                                       */
/* BROWSE-TAB crmAccounts svSelect Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME Dialog-Frame
   3 4                                                                  */
ASSIGN 
       btnCancel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnReset IN FRAME Dialog-Frame
   3 4                                                                  */
ASSIGN 
       btnReset:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnSave IN FRAME Dialog-Frame
   3 4                                                                  */
ASSIGN 
       btnSave:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "WinKitRibbon".

/* SETTINGS FOR FILL-IN svStatus IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE crmAccounts
/* Query rebuild information for BROWSE crmAccounts
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCRMCustomers.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE crmAccounts */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* ZOHO CRM (Customers) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply Dialog-Frame
ON CHOOSE OF btnApply IN FRAME Dialog-Frame /* Apply */
DO:
    RUN pApplyCRM.
    BROWSE crmAccounts:REFRESH() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
    RUN pGetCRM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    RUN pApplyCRM.
    RUN pSave.
    RUN pGetCRM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSelect Dialog-Frame
ON VALUE-CHANGED OF svSelect IN FRAME Dialog-Frame /* Select */
DO:
  ASSIGN {&SELF-NAME}.
  FOR EACH ttCRMCustomers:
      ttCRMCustomers.xxApplyAction = {&SELF-NAME}.
      IF ttCRMCustomers.action EQ "" THEN
      ttCRMCustomers.xxApplyAction = NO.
  END. /* each ttCRMCustomers */
  BROWSE crmAccounts:REFRESH() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME crmAccounts
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
  RUN pGetCRM.
  IF RETURN-VALUE NE "" THEN RETURN.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{CRM/crmCustomers.i}

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
  DISPLAY svSelect svStatus 
      WITH FRAME Dialog-Frame.
  ENABLE svSelect crmAccounts btnApply btnReset btnSave btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCRM Dialog-Frame 
PROCEDURE pGetCRM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iRows   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iHeight AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        svStatus:SCREEN-VALUE = "Retreiving ZOHO CRM ...".
        EMPTY TEMP-TABLE ttCRMCustomers.
        RUN pZohoCRM (ipcCompany, OUTPUT iRows).
        IF RETURN-VALUE NE "" THEN DO:
            MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
            RETURN RETURN-VALUE.
        END.
        IF iRows GT 30 THEN iRows = 30.
        IF iRows GT 5 THEN DO:
            ASSIGN
                iHeight = 20 + iRows * 17
                FRAME {&FRAME-NAME}:HEIGHT-PIXELS = 80 + iHeight
                btnApply:Y  = BROWSE crmAccounts:Y + iHeight + 5
                btnReset:Y  = btnApply:Y
                btnSave:Y   = btnApply:Y
                btnCancel:Y = btnApply:Y
                svStatus:Y  = btnApply:Y
                .
            BROWSE crmAccounts:HEIGHT-PIXELS = iHeight.
        END. /* irows gt 5 */
        {&OPEN-QUERY-crmAccounts}
        svStatus:SCREEN-VALUE = "".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

