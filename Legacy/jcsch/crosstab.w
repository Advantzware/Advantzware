 
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*------------------------------------------------------------------------
 
  File:             crosstab.w
 
  Description:      Shows how to create a Crosstab Browse
 
  Input Parameters:
      <none>
 
  Output Parameters:
      <none>
 
  Author:
 
  Created:          05/04/2000
 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.   */
/*----------------------------------------------------------------------*/
 
/* Create an unnamed pool to store all the widgets created
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
 
CREATE WIDGET-POOL.
 
/* ***************************  Definitions ************************** */
 
/* Parameters Definitions --- */
 
/* Local Variable Definitions ---*/
 
DEFINE VARIABLE hXTab AS HANDLE NO-UNDO.
DEFINE VARIABLE hTTBuff AS HANDLE NO-UNDO.
DEFINE VARIABLE hQry AS HANDLE     NO-UNDO.
 
DEFINE TEMP-TABLE tt_Summary NO-UNDO 
  FIELD State LIKE Customer.State 
  FIELD SalesRep LIKE Customer.SalesRep 
  FIELD deAmount AS DECIMAL DECIMALS 2 FORMAT ">>>,>>>,>>9.99-" 
  INDEX udx IS UNIQUE PRIMARY State SalesRep.
 
DEFINE TEMP-TABLE tt_SalesRep NO-UNDO
  FIELD SalesRep LIKE Customer.SalesRep
  INDEX udx IS UNIQUE PRIMARY SalesRep.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK
 
/* ********************  Preprocessor Definitions ******************** */
 
&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no
 
/* Name of first Frame and/or Browse and/or first Query*/
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br_XTab
 
/* Definitions for FRAME DEFAULT-FRAME*/
 
/* Standard List Definitions*/
&Scoped-Define ENABLED-OBJECTS br_XTab
 
/* Custom List Definitions*/
/* List-1,List-2,List-3,List-4,List-5,List-6*/
 
/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME
 
 
 
/* ***********************  Control Definitions********************** */
 
/* Define the widget handle for the window*/
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
 
/* Definitions of the field level widgets*/
 
/* Browse definitions*/
DEFINE BROWSE br_XTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_XTab C-Win_STRUCTURED
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 12.62 EXPANDABLE.
 
 
/* ************************  Frame Definitions *********************** */
 
DEFINE FRAME DEFAULT-FRAME
     br_XTab AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 92.8 BY 12.86.
 
 
/* *********************** Procedure Settings *************************/
 
&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS
 
/* *************************  Create Window ************************** */
 
&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Crosstab Example"
         HEIGHT             = 12.86
         WIDTH              = 92.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 207.2
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 207.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION*/
&ANALYZE-RESUME
 
/* ***********  Runtime Attributes and AppBuilder Settings*********** */
 
&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win VISIBLE,,RUN-PERSISTENT */
/* SETTINGS FOR FRAME DEFAULT-FRAME*/
/* BROWSE-TAB br_XTab 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.
 
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME                              
 
/* ************************  Control Triggers************************ */
 
&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Crosstab Example */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not,the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Crosstab Example */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define BROWSE-NAME br_XTab
&UNDEFINE SELF-NAME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win
 
 
/* ***************************  Main Block *************************** */
 
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.*/
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
 
/* The CLOSE event can be used from inside or outside the procedure to*/
/* terminate it.*/
ON CLOSE OF THIS-PROCEDURE
   RUN disable_UI.
 
/* Best default for GUI applications is...*/
PAUSE 0 BEFORE-HIDE.
 
/* Now enable the interface and wait for the exit condition.*/
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.*/
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN initializeObject.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
/* **********************  Internal Procedures*********************** */
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createSummary C-Win
PROCEDURE createSummary :
/*--------------------------------------------------------------------
  Purpose:     Builds the cross tab dynamic temp-table
  Parameters:  <none>
  Notes:
---------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hField AS HANDLE     NO-UNDO.
 
  /* Iterate through the customers and create/update the summary
     records. There's no rocket science in this. */
  FOR EACH Customer NO-LOCK:
     FIND tt_Summary
       WHERE tt_Summary.State = Customer.State
         AND tt_Summary.SalesRep = Customer.SalesRep
       NO-ERROR.
     IF NOT AVAILABLE(tt_Summary) THEN
     DO:
       CREATE tt_Summary.
       ASSIGN
         tt_Summary.SalesRep = Customer.SalesRep
         tt_Summary.State = Customer.State.
     END.
     ASSIGN
       tt_Summary.deAmount = tt_Summary.deAmount + Customer.Balance.
 
     /* We need a list of the sales rep columns to create. Use a temp-table
        to make sure you have them all. */
     IF NOT CAN-FIND(tt_SalesRep
        WHERE tt_SalesRep.SalesRep = Customer.SalesRep)
     THEN
     DO:
       CREATE tt_SalesRep.
       ASSIGN
         tt_SalesRep.SalesRep = Customer.SalesRep.
     END.
  END.
 
  /* Now we create the CrossTab temp-table */
  CREATE TEMP-TABLE hXTab.
 
  /* The first field is the state */
  hXTab:ADD-LIKE-FIELD("State","Customer.State").
 
  /* Now add a field for each sales rep */
  FOR EACH tt_SalesRep:
    hXTab:ADD-NEW-FIELD(tt_SalesRep.SalesRep, "DECIMAL",0,">>>,>>>,>>9.99-",0).
  END.
 
  /* Index the stuff on the state field */
  hXTab:ADD-NEW-INDEX("udx",true,true).
  hXTab:ADD-INDEX-FIELD("udx","State").
 
  /* Prepare the temp-table */
  hXTab:TEMP-TABLE-PREPARE("CrossTab").
 
  /* Get the buffer handle for the temp-table */
  hTTBuff = hXTab:DEFAULT-BUFFER-HANDLE.
 
  /* This replaces the column label of the field with the SalesReps name. */
  DO iCount = 2 TO hTTBuff:NUM-FIELDS:
    hField = hTTBuff:BUFFER-FIELD(iCount).
    FIND SalesRep NO-LOCK
      WHERE SalesRep.SalesRep = hField:NAME NO-ERROR.
    IF AVAILABLE(SalesRep) THEN
      hField:LABEL = SalesRep.RepName.
  END.
 
  /* Now we iterate through the summary table */
  FOR EACH tt_Summary
    BREAK BY tt_Summary.State
          BY tt_Summary.SalesRep:
 
    /* If this is the first of the state, create a record in the
       Cross tab table for the state */
    IF FIRST-OF(tt_Summary.State) THEN
    DO:
      hTTBuff:BUFFER-CREATE().
      hField = hTTBuff:BUFFER-FIELD("State").
      hField:BUFFER-VALUE = tt_Summary.State.
    END.
 
    /* Get the field in the crosstab table that is this sales reps field */
    hField = hTTBuff:BUFFER-FIELD(tt_Summary.SalesRep).
    /* Set the amount */
    hField:BUFFER-VALUE = tt_Summary.deAmount.
 
    /* If this is the last of the state, release the record */
    IF LAST-OF(tt_Summary.State) THEN
    DO:
      hTTBuff:BUFFER-RELEASE().
    END.
  END.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win_DEFAULT-DISABLE
PROCEDURE disable_UI :
/*-------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
---------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win_DEFAULT-ENABLE
PROCEDURE enable_UI :
/*-------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other
               Settings" section of the widget Property Sheets.
--------------------------------------------------------------------------*/
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject C-Win
PROCEDURE initializeObject :
/*------------------------------------------------------------------------
  Purpose:     Initializes the temp-table and the browse
  Parameters:  <none>
  Notes:
-------------------------------------------------------------------------*/
  /* Get the cross-tab information */                              
    RUN createSummary.
 
  /* Create a query based on the CrossTab temp-table */
  CREATE QUERY hQry.
  hQry:ADD-BUFFER(hTTBuff).
  hQry:QUERY-PREPARE("FOR EACH " + hTTBuff:NAME).
  hQry:QUERY-OPEN().
 
  /* Set the Browse's query to be query just created, and add the
     columns to the Browse */
  DO WITH FRAME {&FRAME-NAME}:
    br_XTab:QUERY = hQry.
    br_XTab:ADD-COLUMNS-FROM(hTTBuff).
    ENABLE br_XTab.
  END.
 
 
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
