&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: sys/ref/CustListManager.w

  Description: UserInterface for new "CustList" fucntionality

  Input Parameters:
      ipcCompany = Company Code
      ipcMode:
      Mode can be "Selection" (Future), 
      Specific CustList Char Value (e.g "AR4")
      If left blank, it will just display contents of temp table.
      Otherwise it will rebuild the temp table
        
  Output Parameters:
      <none>

  Author: BV

  Created:  9/17/2014
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcMode AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{sys/ref/CustList.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME brsCustList

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustList cust

/* Definitions for BROWSE brsCustList                                   */
&Scoped-define FIELDS-IN-QUERY-brsCustList ttCustList.cust-no cust.name ttCustList.cSource   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brsCustList   
&Scoped-define SELF-NAME brsCustList
&Scoped-define QUERY-STRING-brsCustList FOR EACH ttCustList NO-LOCK, ~
           FIRST cust NO-LOCK         WHERE cust.company EQ ipcCompany           AND cust.cust-no EQ ttCustList.cust-no
&Scoped-define OPEN-QUERY-brsCustList OPEN QUERY {&SELF-NAME}     FOR EACH ttCustList NO-LOCK, ~
           FIRST cust NO-LOCK         WHERE cust.company EQ ipcCompany           AND cust.cust-no EQ ttCustList.cust-no.
&Scoped-define TABLES-IN-QUERY-brsCustList ttCustList cust
&Scoped-define FIRST-TABLE-IN-QUERY-brsCustList ttCustList
&Scoped-define SECOND-TABLE-IN-QUERY-brsCustList cust


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-brsCustList}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brsCustList Btn_OK Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brsCustList FOR 
      ttCustList, 
      cust SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brsCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brsCustList Dialog-Frame _FREEFORM
  QUERY brsCustList DISPLAY
      ttCustList.cust-no FORMAT "X(8)" LABEL "Cust #" WIDTH 10
cust.name FORMAT "X(30)" LABEL "Customer Name" 
ttCustList.cSource FORMAT "X(40)" LABEL "Source"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 89 BY 11.67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     brsCustList AT ROW 1 COL 1 WIDGET-ID 200
     Btn_OK AT ROW 12.91 COL 30
     Btn_Cancel AT ROW 12.91 COL 46
     SPACE(29.59) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Customer List"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
   FRAME-NAME                                                           */
/* BROWSE-TAB brsCustList 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brsCustList
/* Query rebuild information for BROWSE brsCustList
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH ttCustList NO-LOCK,
    FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no EQ ttCustList.cust-no.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brsCustList */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Customer List */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Move Order Qty to Qty/Unit */
DO:
    DEF VAR counter AS INT NO-UNDO.
    DEF VAR vcount AS CHAR NO-UNDO.
     do counter = 1 to BROWSE brsCustList:NUM-SELECTED-ROWS:
      BROWSE brsCustList:FETCH-SELECTED-ROW(counter).
      vcount = vcount + ttCustList.cust-no + "," .
     end.
     IF vcount <> "" THEN
     FOR EACH ttCustList NO-LOCK:
         IF LOOKUP(ttCustList.cust-no,vcount) <> 0 THEN
             ttCustList.log-fld = YES.
         ELSE ttCustList.log-fld = NO .
     END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brsCustList
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF ipcMode EQ "Selection" THEN DO:
        /*Initialize Selection*/
    END.
    ELSE IF ipcMode NE "" THEN DO:
        RUN sys/ref/CustList.p(INPUT ipcCompany,
                               INPUT ipcMode,
                               INPUT YES,
                               OUTPUT lActive).
    END.

  RUN enable_UI.
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
  ENABLE brsCustList Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

