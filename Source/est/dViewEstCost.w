&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: est/dViewEstCost.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
{est/ttEstSysConfig.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.

/* Local Variable Definitions ---                                       */
   
DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTotalCost    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iGroupLevelID AS INTEGER   NO-UNDO.
DEFINE VARIABLE cGroupID      AS CHARACTER NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME CostDetailBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES estCostDetail ttEstCostCategory ~
estCostSummary ttEstCostGroup ttEstCostGroupLevel

/* Definitions for BROWSE CostDetailBrowse                              */
&Scoped-define FIELDS-IN-QUERY-CostDetailBrowse estCostDetail.company estCostDetail.costTotal estCostDetail.estCostBlankID estCostDetail.estCostCategoryID estCostDetail.estCostDetailDesc estCostDetail.estCostDetailID estCostDetail.estCostFormID estCostDetail.estCostHeaderID estCostDetail.estimateNo estCostDetail.hasBeenProcessed estCostDetail.profitPercent estCostDetail.profitPercentType estCostDetail.rec_key estCostDetail.sourceID estCostDetail.sourceType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-CostDetailBrowse   
&Scoped-define SELF-NAME CostDetailBrowse
&Scoped-define QUERY-STRING-CostDetailBrowse FOR EACH estCostDetail       WHERE estCostDetail.estCostHeaderID EQ ipiEstCostHeaderID NO-LOCK, ~
             FIRST ttEstCostCategory       WHERE ttEstCostCategory.estCostCategoryID EQ estCostDetail.estCostCategoryID         AND ttEstCostCategory.estCostGroupID    EQ cGroupID INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-CostDetailBrowse OPEN QUERY {&SELF-NAME} FOR EACH estCostDetail       WHERE estCostDetail.estCostHeaderID EQ ipiEstCostHeaderID NO-LOCK, ~
             FIRST ttEstCostCategory       WHERE ttEstCostCategory.estCostCategoryID EQ estCostDetail.estCostCategoryID         AND ttEstCostCategory.estCostGroupID    EQ cGroupID INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-CostDetailBrowse estCostDetail ~
ttEstCostCategory
&Scoped-define FIRST-TABLE-IN-QUERY-CostDetailBrowse estCostDetail
&Scoped-define SECOND-TABLE-IN-QUERY-CostDetailBrowse ttEstCostCategory


/* Definitions for BROWSE CostSummaryBrowse                             */
&Scoped-define FIELDS-IN-QUERY-CostSummaryBrowse estCostSummary.estCostGroupID ttEstCostGroup.estCostGroupDesc estCostSummary.costTotal estCostSummary.costTotalPerMFinished estCostSummary.estCostHeaderID estCostSummary.estCostSummaryID estCostSummary.estimateNo estCostSummary.rec_key estCostSummary.scopeRecKey estCostSummary.scopeType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-CostSummaryBrowse   
&Scoped-define SELF-NAME CostSummaryBrowse
&Scoped-define QUERY-STRING-CostSummaryBrowse FOR EACH estCostSummary       WHERE estCostSummary.estCostHeaderID EQ ipiEstCostHeaderID, ~
             FIRST ttEstCostGroup       WHERE ttEstCostGroup.estCostGroupID EQ estCostSummary.estCostGroupID         AND ttEstCostGroup.estCostGroupLevelID EQ iGroupLevelID NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-CostSummaryBrowse OPEN QUERY {&SELF-NAME} FOR EACH estCostSummary       WHERE estCostSummary.estCostHeaderID EQ ipiEstCostHeaderID, ~
             FIRST ttEstCostGroup       WHERE ttEstCostGroup.estCostGroupID EQ estCostSummary.estCostGroupID         AND ttEstCostGroup.estCostGroupLevelID EQ iGroupLevelID NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-CostSummaryBrowse estCostSummary ~
ttEstCostGroup
&Scoped-define FIRST-TABLE-IN-QUERY-CostSummaryBrowse estCostSummary
&Scoped-define SECOND-TABLE-IN-QUERY-CostSummaryBrowse ttEstCostGroup


/* Definitions for BROWSE GroupLevelBrowse                              */
&Scoped-define FIELDS-IN-QUERY-GroupLevelBrowse ttEstCostGroupLevel.estCostGroupLevelID ttEstCostGroupLevel.estCostGroupLevelDesc fGetGroupLevelCost(ttEstCostGroupLevel.estCostGroupLevelID) @ dTotalCost   
&Scoped-define ENABLED-FIELDS-IN-QUERY-GroupLevelBrowse   
&Scoped-define SELF-NAME GroupLevelBrowse
&Scoped-define QUERY-STRING-GroupLevelBrowse FOR EACH ttEstCostGroupLevel
&Scoped-define OPEN-QUERY-GroupLevelBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttEstCostGroupLevel.
&Scoped-define TABLES-IN-QUERY-GroupLevelBrowse ttEstCostGroupLevel
&Scoped-define FIRST-TABLE-IN-QUERY-GroupLevelBrowse ttEstCostGroupLevel


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-CostDetailBrowse}~
    ~{&OPEN-QUERY-CostSummaryBrowse}~
    ~{&OPEN-QUERY-GroupLevelBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS GroupLevelBrowse CostSummaryBrowse ~
CostDetailBrowse 
&Scoped-Define DISPLAYED-OBJECTS fiEstimate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetGroupLevelCost C-Win 
FUNCTION fGetGroupLevelCost RETURNS DECIMAL
  ( ipiEstCostGroupLevelID AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiEstimate AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY CostDetailBrowse FOR 
      estCostDetail, 
      ttEstCostCategory SCROLLING.

DEFINE QUERY CostSummaryBrowse FOR 
      estCostSummary, 
      ttEstCostGroup SCROLLING.

DEFINE QUERY GroupLevelBrowse FOR 
      ttEstCostGroupLevel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE CostDetailBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS CostDetailBrowse C-Win _FREEFORM
  QUERY CostDetailBrowse NO-LOCK DISPLAY
      estCostDetail.company FORMAT "x(3)":U
      estCostDetail.costTotal FORMAT "->,>>>,>>9.99":U
      estCostDetail.estCostBlankID FORMAT ">>>>>>>>>9":U WIDTH 12.6
      estCostDetail.estCostCategoryID FORMAT "x(20)":U
      estCostDetail.estCostDetailDesc FORMAT "x(100)":U WIDTH 66.4
      estCostDetail.estCostDetailID FORMAT ">>>>>>>>>9":U
      estCostDetail.estCostFormID FORMAT ">>>>>>>>>9":U
      estCostDetail.estCostHeaderID FORMAT ">>>>>>>>>9":U
      estCostDetail.estimateNo FORMAT "x(8)":U WIDTH 16.6
      estCostDetail.hasBeenProcessed FORMAT "yes/no":U
      estCostDetail.profitPercent FORMAT "->>9.99<<":U
      estCostDetail.profitPercentType FORMAT "x(10)":U
      estCostDetail.rec_key FORMAT "x(26)":U
      estCostDetail.sourceID FORMAT ">>>>>>>>>9":U
      estCostDetail.sourceType FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 235 BY 13.33
         TITLE "Estimate Cost Detail" FIT-LAST-COLUMN.

DEFINE BROWSE CostSummaryBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS CostSummaryBrowse C-Win _FREEFORM
  QUERY CostSummaryBrowse NO-LOCK DISPLAY
      estCostSummary.estCostGroupID FORMAT "x(20)":U
      ttEstCostGroup.estCostGroupDesc FORMAT "x(60)":U
      estCostSummary.costTotal FORMAT "->,>>>,>>9.99":U
      estCostSummary.costTotalPerMFinished FORMAT "->,>>>,>>9.99":U
      estCostSummary.estCostHeaderID FORMAT ">>>>>>>>>9":U
      estCostSummary.estCostSummaryID FORMAT ">>>>>>>>>9":U
      estCostSummary.estimateNo FORMAT "x(8)":U
      estCostSummary.rec_key FORMAT "x(26)":U
      estCostSummary.scopeRecKey FORMAT "x(26)":U
      estCostSummary.scopeType FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 236 BY 12.14
         TITLE "Estimate Cost Summary" FIT-LAST-COLUMN.

DEFINE BROWSE GroupLevelBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS GroupLevelBrowse C-Win _FREEFORM
  QUERY GroupLevelBrowse DISPLAY
      ttEstCostGroupLevel.estCostGroupLevelID
ttEstCostGroupLevel.estCostGroupLevelDesc
fGetGroupLevelCost(ttEstCostGroupLevel.estCostGroupLevelID) @ dTotalCost COLUMN-LABEL "Total Cost"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 235.8 BY 6.19
         TITLE "Estimate Cost Level" ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiEstimate AT ROW 1.43 COL 15 COLON-ALIGNED WIDGET-ID 2
     GroupLevelBrowse AT ROW 2.91 COL 5.2 WIDGET-ID 400
     CostSummaryBrowse AT ROW 9.33 COL 5 WIDGET-ID 300
     CostDetailBrowse AT ROW 21.95 COL 5 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 241.4 BY 34.48
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Estimate Costs"
         HEIGHT             = 34.48
         WIDTH              = 241.4
         MAX-HEIGHT         = 34.48
         MAX-WIDTH          = 241.4
         VIRTUAL-HEIGHT     = 34.48
         VIRTUAL-WIDTH      = 241.4
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB GroupLevelBrowse fiEstimate DEFAULT-FRAME */
/* BROWSE-TAB CostSummaryBrowse GroupLevelBrowse DEFAULT-FRAME */
/* BROWSE-TAB CostDetailBrowse CostSummaryBrowse DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fiEstimate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE CostDetailBrowse
/* Query rebuild information for BROWSE CostDetailBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH estCostDetail
      WHERE estCostDetail.estCostHeaderID EQ ipiEstCostHeaderID NO-LOCK,
      FIRST ttEstCostCategory
      WHERE ttEstCostCategory.estCostCategoryID EQ estCostDetail.estCostCategoryID
        AND ttEstCostCategory.estCostGroupID    EQ cGroupID INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ASI.estCostDetail.estCostHeaderID EQ ipiEstCostHeaderID"
     _Query            is OPENED
*/  /* BROWSE CostDetailBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE CostSummaryBrowse
/* Query rebuild information for BROWSE CostSummaryBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH estCostSummary
      WHERE estCostSummary.estCostHeaderID EQ ipiEstCostHeaderID,
      FIRST ttEstCostGroup
      WHERE ttEstCostGroup.estCostGroupID EQ estCostSummary.estCostGroupID
        AND ttEstCostGroup.estCostGroupLevelID EQ iGroupLevelID NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ASI.estCostSummary.estCostHeaderID EQ ipiEstCostHeaderID"
     _Query            is OPENED
*/  /* BROWSE CostSummaryBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE GroupLevelBrowse
/* Query rebuild information for BROWSE GroupLevelBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttEstCostGroupLevel.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE GroupLevelBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Estimate Costs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Estimate Costs */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME CostSummaryBrowse
&Scoped-define SELF-NAME CostSummaryBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CostSummaryBrowse C-Win
ON VALUE-CHANGED OF CostSummaryBrowse IN FRAME DEFAULT-FRAME /* Estimate Cost Summary */
DO:
    IF AVAILABLE estCostSummary THEN
        cGroupID = estCostSummary.estCostGroupID.
        
    {&OPEN-QUERY-CostDetailBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME GroupLevelBrowse
&Scoped-define SELF-NAME GroupLevelBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL GroupLevelBrowse C-Win
ON VALUE-CHANGED OF GroupLevelBrowse IN FRAME DEFAULT-FRAME /* Estimate Cost Level */
DO:
    IF AVAILABLE ttEstCostGroupLevel THEN
        iGroupLevelID = ttEstCostGroupLevel.estCostGroupLevelID.
        
    {&OPEN-QUERY-CostSummaryBrowse}
    
    APPLY "VALUE-CHANGED" TO BROWSE CostSummaryBrowse.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME CostDetailBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN pInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiEstimate 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE GroupLevelBrowse CostSummaryBrowse CostDetailBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dSourceTotalCost AS DECIMAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN Estimate_GetSystemDataForEstimate (
        INPUT  cCompany,
        OUTPUT TABLE ttEstCostCategory,
        OUTPUT TABLE ttEstCostGroup,
        OUTPUT TABLE ttEstCostGroupLevel
        ).
    
    {&OPEN-QUERY-GroupLevelBrowse}

    APPLY "VALUE-CHANGED" TO BROWSE GroupLevelBrowse.
                
    FIND FIRST estCostHeader NO-LOCK
         WHERE estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
         NO-ERROR.
    IF AVAILABLE estCostHeader THEN
        fiEstimate:SCREEN-VALUE = estCostHeader.estimateNo.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetGroupLevelCost C-Win 
FUNCTION fGetGroupLevelCost RETURNS DECIMAL
  ( ipiEstCostGroupLevelID AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSourceRecKey AS CHARACTER NO-UNDO.

    FIND FIRST estCostHeader NO-LOCK
         WHERE estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
         NO-ERROR.
    IF AVAILABLE estCostHeader THEN
        cSourceRecKey = estCostHeader.rec_key.

    FOR EACH ttEstCostGroup
        WHERE ttEstCostGroup.estCostGroupLevelID LE ipiEstCostGroupLevelID:
        FOR EACH estCostSummary
            WHERE estCostSummary.estcostHeaderID EQ ipiEstCostHeaderID
              AND estCostSummary.estCostGroupID  EQ ttEstCostGroup.estCostGroupID
              AND estCostSummary.scopeRecKey     EQ cSourceRecKey:
            dCost = dCost + estCostSummary.costTotal.
        END.
    END.
    
    RETURN dCost.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

