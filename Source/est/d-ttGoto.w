&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: est/d-ttGoto.w

  Description: Screen to view/update the yields of an estimate

  Input Parameters:
      ipcCompany     : Company ID (Character)
      ipcEstNo       : Estimate No (Character)

  Output Parameters:
      oplChangesMade : Changes made in estimate (Logical)

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est/ttGoto.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEstNo       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplChangesMade AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE lSaveUpdate AS LOGICAL    NO-UNDO.
DEFINE VARIABLE riPrevRowID AS ROWID      NO-UNDO.
DEFINE VARIABLE hdEstimateProcs AS HANDLE NO-UNDO.

RUN est/EstimateProcs.p PERSISTENT SET hdEstimateProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME brGoto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttGoto

/* Definitions for BROWSE brGoto                                        */
&Scoped-define FIELDS-IN-QUERY-brGoto ttGoto.formNo ttGoto.blankNo ttGoto.partNo ttGoto.reqQty ttGoto.effectiveYldQty ttGoto.reqQtyAdj ttGoto.numWid ttGoto.numLen ttGoto.numUp ttGoto.yieldRequest ttGoto.sheetsRequired ttGoto.maxSheetsPerForm ttGoto.calcYldQty ttGoto.surplusQty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brGoto   
&Scoped-define SELF-NAME brGoto
&Scoped-define QUERY-STRING-brGoto FOR EACH ttGoto
&Scoped-define OPEN-QUERY-brGoto OPEN QUERY {&SELF-NAME} FOR EACH ttGoto.
&Scoped-define TABLES-IN-QUERY-brGoto ttGoto
&Scoped-define FIRST-TABLE-IN-QUERY-brGoto ttGoto


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-brGoto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 brGoto btSaveAll ~
btResetAll btClose btUpdate 
&Scoped-Define DISPLAYED-OBJECTS fiFormNo fiBlankNo fiEffYldQty fiReqQty ~
fiReqQtyAdj fiSheetsRequired fiNumWid fiNumLen fiNumUp fiMaxSheetsPerForm ~
rdYieldRequest fiPartNo fiCalcYldQty fiSurplusQty 

/* Custom List Definitions                                              */
/* ENABLE-FIELDS,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define ENABLE-FIELDS fiFormNo fiBlankNo fiReqQty fiReqQtyAdj ~
fiNumWid fiNumLen rdYieldRequest fiPartNo btCancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.52.

DEFINE BUTTON btClose AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 21 BY 1.52.

DEFINE BUTTON btResetAll 
     LABEL "Reset All" 
     SIZE 21 BY 1.52.

DEFINE BUTTON btSaveAll AUTO-GO 
     LABEL "Save All" 
     SIZE 21 BY 1.52.

DEFINE BUTTON btUpdate 
     LABEL "Update" 
     SIZE 15 BY 1.52.

DEFINE VARIABLE fiBlankNo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Blank" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiCalcYldQty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Calculated Yield Qty" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiEffYldQty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Effective Yield Qty" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiFormNo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Form" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiMaxSheetsPerForm AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Max Sheets Per Form" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumLen AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "#on Length" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumUp AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "#on Up" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumWid AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "#on Width" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiPartNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer Part#" 
     VIEW-AS FILL-IN 
     SIZE 43.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiReqQty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Required Qty" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiReqQtyAdj AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Req Qty Adjust" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiSheetsRequired AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Sheets Required" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiSurplusQty AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Surplus Qty" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rdYieldRequest AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yield", yes,
"Request", no
     SIZE 23.4 BY .86 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 75 BY 7.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 38 BY 1.91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 48 BY 7.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brGoto FOR 
      ttGoto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brGoto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brGoto Dialog-Frame _FREEFORM
  QUERY brGoto DISPLAY
      ttGoto.formNo           FORMAT ">>9":U COLUMN-LABEL "Form" 
      ttGoto.blankNo          FORMAT ">>9":U COLUMN-LABEL "Blank" 
      ttGoto.partNo           FORMAT "X(20)":U
      ttGoto.reqQty           FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Required!Qty" WIDTH 12
      ttGoto.effectiveYldQty  FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Effective!Yield Qty" WIDTH 12
      ttGoto.reqQtyAdj        FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Req Qty!Adjustment" WIDTH 14
      ttGoto.numWid           FORMAT ">9":U COLUMN-LABEL "# on! Width" 
      ttGoto.numLen           FORMAT ">9":U COLUMN-LABEL "# on! Length"
      ttGoto.numUp            FORMAT ">>9":U
      ttGoto.yieldRequest     FORMAT "Yield/Request" COLUMN-LABEL "Yield/!Request"
      ttGoto.sheetsRequired   FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Sheets!Required" WIDTH 12
      ttGoto.maxSheetsPerForm FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Max Sheets!Per Form" WIDTH 13
      ttGoto.calcYldQty       FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Calculated!Yield Qty" WIDTH 12
      ttGoto.surplusQty       FORMAT "->>>,>>>,>>9":U COLUMN-LABEL "Surplus!Qty"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 165 BY 12.62
         FONT 6 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     brGoto AT ROW 1.24 COL 2 WIDGET-ID 200
     btSaveAll AT ROW 14.14 COL 128 WIDGET-ID 34 NO-TAB-STOP 
     fiFormNo AT ROW 14.29 COL 20.4 COLON-ALIGNED WIDGET-ID 4
     fiBlankNo AT ROW 14.29 COL 56.8 COLON-ALIGNED WIDGET-ID 6
     fiEffYldQty AT ROW 14.29 COL 103 COLON-ALIGNED WIDGET-ID 16
     fiReqQty AT ROW 15.33 COL 20.4 COLON-ALIGNED WIDGET-ID 8
     fiReqQtyAdj AT ROW 15.33 COL 56.8 COLON-ALIGNED WIDGET-ID 10
     fiSheetsRequired AT ROW 15.33 COL 103 COLON-ALIGNED WIDGET-ID 18
     btResetAll AT ROW 15.76 COL 128 WIDGET-ID 36 NO-TAB-STOP 
     fiNumWid AT ROW 16.38 COL 20.4 COLON-ALIGNED WIDGET-ID 12
     fiNumLen AT ROW 16.38 COL 56.8 COLON-ALIGNED WIDGET-ID 14
     fiNumUp AT ROW 16.38 COL 103 COLON-ALIGNED WIDGET-ID 20
     btClose AT ROW 17.38 COL 128 WIDGET-ID 38
     fiMaxSheetsPerForm AT ROW 17.43 COL 103 COLON-ALIGNED WIDGET-ID 22
     rdYieldRequest AT ROW 17.52 COL 22.6 NO-LABEL WIDGET-ID 46
     fiPartNo AT ROW 18.52 COL 20.4 COLON-ALIGNED WIDGET-ID 40
     fiCalcYldQty AT ROW 18.52 COL 103 COLON-ALIGNED WIDGET-ID 24
     fiSurplusQty AT ROW 19.62 COL 103 COLON-ALIGNED WIDGET-ID 26
     btUpdate AT ROW 20 COL 22.4 WIDGET-ID 28
     btCancel AT ROW 20 COL 41.6 WIDGET-ID 30
     "Price By:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 17.57 COL 12.2 WIDGET-ID 50
     RECT-1 AT ROW 14.1 COL 2 WIDGET-ID 2
     RECT-2 AT ROW 19.81 COL 20.4 WIDGET-ID 32
     RECT-3 AT ROW 14.1 COL 78.6 WIDGET-ID 44
     SPACE(40.99) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE "Estimate Form" WIDGET-ID 100.


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
/* BROWSE-TAB brGoto RECT-3 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       brGoto:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE
       brGoto:COLUMN-MOVABLE IN FRAME Dialog-Frame         = TRUE.

/* SETTINGS FOR BUTTON btCancel IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiBlankNo IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiCalcYldQty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEffYldQty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFormNo IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiMaxSheetsPerForm IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNumLen IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiNumUp IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNumWid IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiPartNo IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiReqQty IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiReqQtyAdj IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiSheetsRequired IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSurplusQty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rdYieldRequest IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brGoto
/* Query rebuild information for BROWSE brGoto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttGoto.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brGoto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Form */
DO:
    IF VALID-HANDLE(hdEstimateProcs) THEN
        DELETE PROCEDURE hdEstimateProcs.
        
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brGoto
&Scoped-define SELF-NAME brGoto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brGoto Dialog-Frame
ON VALUE-CHANGED OF brGoto IN FRAME Dialog-Frame
DO: 
    IF AVAILABLE ttGoto THEN DO:
        IF lSaveUpdate AND riPrevRowID NE ROWID(ttGoto) THEN DO:
            MESSAGE "Please save or cancel the selected"
                    " record before selecting another record"
                VIEW-AS ALERT-BOX ERROR.
            REPOSITION {&BROWSE-NAME} TO ROWID riPrevRowID.
            RETURN NO-APPLY.
        END.

        ASSIGN
            fiFormNo:SCREEN-VALUE           = STRING(ttGoto.formNo)
            fiBlankNo:SCREEN-VALUE          = STRING(ttGoto.blankNo)
            fiReqQty:SCREEN-VALUE           = STRING(ttGoto.reqQty)
            fiReqQtyAdj:SCREEN-VALUE        = STRING(ttGoto.reqQtyAdj)
            fiNumWid:SCREEN-VALUE           = STRING(ttGoto.numWid)
            fiNumLen:SCREEN-VALUE           = STRING(ttGoto.numLen)
            fiNumUp:SCREEN-VALUE            = STRING(ttGoto.numUp)
            fiEffYldQty:SCREEN-VALUE        = STRING(ttGoto.effectiveYldQty)
            fiSheetsRequired:SCREEN-VALUE   = STRING(ttGoto.sheetsRequired)
            fiMaxSheetsPerForm:SCREEN-VALUE = STRING(ttGoto.maxSheetsPerForm)
            fiCalcYldQty:SCREEN-VALUE       = STRING(ttGoto.calcYldQty)
            fiSurplusQty:SCREEN-VALUE       = STRING(ttGoto.surplusQty)
            rdYieldRequest:SCREEN-VALUE     = STRING(ttGoto.yieldRequest)
            fiPartNo:SCREEN-VALUE           = ttGoto.partNo
            riPrevRowID                     = ROWID(ttGoto)
            .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel Dialog-Frame
ON CHOOSE OF btCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    DISABLE {&ENABLE-FIELDS} WITH FRAME {&FRAME-NAME}.

    ASSIGN
        lSaveUpdate    = NOT lSaveUpdate   
        btUpdate:LABEL = "Update"         
        .
        
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btResetAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btResetAll Dialog-Frame
ON CHOOSE OF btResetAll IN FRAME Dialog-Frame /* Reset All */
DO:
    MESSAGE "All the changes made will be discarded. Are you sure?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lContinue AS LOGICAL.

    IF NOT lContinue THEN
        RETURN NO-APPLY.
        
    IF lSaveUpdate THEN
        APPLY "CHOOSE" TO btCancel.
        
    RUN pLoadEstimate.
        
    RUN pReCalculateYields.

    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSaveAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSaveAll Dialog-Frame
ON CHOOSE OF btSaveAll IN FRAME Dialog-Frame /* Save All */
DO:
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
        
    IF lSaveUpdate THEN DO:
        MESSAGE "Please save or cancel the selected"
                " record before committing the changes"
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    
    RUN pReNumberFormsAndBlanks.

    RUN pReCalculateYields.

    RUN pUpdateEstimate.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
    
    oplChangesMade = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate Dialog-Frame
ON CHOOSE OF btUpdate IN FRAME Dialog-Frame /* Update */
DO: 
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    lSaveUpdate = NOT lSaveUpdate.

    IF lSaveUpdate THEN DO:
        ENABLE {&ENABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
        SELF:LABEL = "Save".
        APPLY "ENTRY" TO fiFormNo.
    END.
    ELSE DO:
        RUN pUpdatettGoto (
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
        
        IF NOT lSuccess THEN DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
            lSaveUpdate = NOT lSaveUpdate.
            RETURN NO-APPLY.
        END.

        DISABLE {&ENABLE-FIELDS} WITH FRAME {&FRAME-NAME}.    
        SELF:LABEL = "Update".        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNumLen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNumLen Dialog-Frame
ON LEAVE OF fiNumLen IN FRAME Dialog-Frame /* #on Length */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) LE 0 THEN
        SELF:SCREEN-VALUE = "1".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNumWid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNumWid Dialog-Frame
ON LEAVE OF fiNumWid IN FRAME Dialog-Frame /* #on Width */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) LE 0 THEN
        SELF:SCREEN-VALUE = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiReqQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiReqQty Dialog-Frame
ON LEAVE OF fiReqQty IN FRAME Dialog-Frame /* Required Qty */
DO: 
    IF ttGoto.estType NE 2 AND ttGoto.estType NE 5 AND ttGoto.estType NE 6 AND
       INTEGER(SELF:SCREEN-VALUE) LE 0 THEN DO:
        MESSAGE "Request Qty may not be zero..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
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
    RUN enable_UI.
    FRAME {&FRAME-NAME}:TITLE = "Estimate Form for " + ipcEstNo.
    RUN pInit.
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
  DISPLAY fiFormNo fiBlankNo fiEffYldQty fiReqQty fiReqQtyAdj fiSheetsRequired 
          fiNumWid fiNumLen fiNumUp fiMaxSheetsPerForm rdYieldRequest fiPartNo 
          fiCalcYldQty fiSurplusQty 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 RECT-3 brGoto btSaveAll btResetAll btClose btUpdate 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit Dialog-Frame 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pLoadEstimate.
    RUN pReCalculateYields.

    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadEstimate Dialog-Frame 
PROCEDURE pLoadEstimate PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Estimate_LoadEstToTT IN hdEstimateProcs (
        INPUT  ipcCompany,
        INPUT  ipcEstNo,
        OUTPUT TABLE ttGoto
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReCalculateYields Dialog-Frame 
PROCEDURE pReCalculateYields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dMaxSheets AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSheetsReq AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttGoto FOR ttGoto.
    
    FOR EACH ttGoto:
        ASSIGN
            ttGoto.effectiveYldQty = IF ttGoto.reqQtyAdj GT 0 THEN
                                         ttGoto.reqQtyAdj
                                     ELSE
                                         ttGoto.reqQty
            ttGoto.numUp           = ttGoto.numWid * ttGoto.numLen
            dSheetsReq             = IF ttGoto.numUp GT 0 THEN
                                         ttGoto.effectiveYldQty / ttGoto.numUp
                                     ELSE
                                         0
            .
        
        {sys/inc/roundup.i dSheetsReq}
        
        ttGoto.sheetsRequired = dSheetsReq.
    END.
    
    FOR EACH ttGoto 
        BREAK BY ttGoto.formNo:    
        IF FIRST-OF(ttGoto.formNo) THEN 
        DO:
            /* Calculate maximum sheets requested for this form */
            dMaxSheets = 0.
            FOR EACH bf-ttGoto 
                WHERE bf-ttGoto.formNo EQ ttGoto.formNo:
                IF bf-ttGoto.sheetsRequired GT dMaxSheets THEN 
                    dMaxSheets = bf-ttGoto.sheetsRequired.
            END.
        END.

        ASSIGN 
            ttGoto.maxSheetsPerForm = dMaxSheets 
            ttGoto.calcYldQty       = ttGoto.numUp * ttGoto.maxSheetsPerForm        
            ttGoto.surplusQty       = IF ttGoto.calcYldQty GT 0 THEN
                                          ttGoto.calcYldQty - ttGoto.effectiveYldQty
                                      ELSE
                                          0
            .
    END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReNumberFormsAndBlanks Dialog-Frame 
PROCEDURE pReNumberFormsAndBlanks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iFormCnt  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankCnt AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-ttGoto FOR ttGoto.

    FOR EACH ttGoto 
        BREAK BY ttGoto.formNo:        
        IF FIRST-OF(ttGoto.formNo) THEN 
        DO:
            IF ttGoto.formNo NE 0 THEN
                iFormCnt = iFormCnt + 1.

            IF ttGoto.formNo NE iFormCnt THEN 
            DO:
                FOR EACH bf-ttGoto 
                    WHERE bf-ttGoto.formNo EQ ttGoto.formNo
                    BY bf-ttGoto.formNo:            
                    bf-ttGoto.formNo = iFormCnt.
                END.
            END.
            
            iBlankCnt = 0.
            
            FOR EACH bf-ttGoto 
                WHERE bf-ttGoto.formNo EQ iFormCnt
                BY bf-ttGoto.blankNo:
                iBlankCnt = iBlankCnt + 1.

                IF bf-ttGoto.blankNo NE iBlankCnt THEN DO:
                    bf-ttGoto.blankNo = iBlankCnt.
                END.
            END. /* each blank of form */
        END. /* First-of form */
    END. /* each ttGoto */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateEstimate Dialog-Frame 
PROCEDURE pUpdateEstimate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Estimate_UpdateEstFromTT IN hdEstimateProcs (
        INPUT TABLE ttGoto
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdatettGoto Dialog-Frame 
PROCEDURE pUpdatettGoto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iBlankCnt AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttGoto FOR ttGoto.
    
    DO WITH FRAME  {&FRAME-NAME}:
    END.

    IF INTEGER(fiNumWid:SCREEN-VALUE) LE 0 THEN
        fiNumWid:SCREEN-VALUE = "1".

    IF INTEGER(fiNumLen:SCREEN-VALUE) LE 0 THEN
        fiNumLen:SCREEN-VALUE = "1".

    IF INTEGER(fiBlankNo:SCREEN-VALUE) EQ 0 THEN 
    DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Blank Number may not be zero..."
            .
        APPLY "ENTRY" TO fiBlankNo.
        RETURN.
    END.

    IF AVAILABLE ttGoto THEN DO:                
        ASSIGN
            ttGoto.reqQty       = INTEGER(fiReqQty:SCREEN-VALUE)
            ttGoto.reqQtyAdj    = INTEGER(fiReqQtyAdj:SCREEN-VALUE)
            ttGoto.numWid       = INTEGER(fiNumWid:SCREEN-VALUE)
            ttGoto.numLen       = INTEGER(fiNumLen:SCREEN-VALUE)
            ttGoto.partNo       = fiPartNo:SCREEN-VALUE
            ttGoto.yieldRequest = LOGICAL(rdYieldRequest:SCREEN-VALUE, "Yield/Request")
            .

        IF ttGoto.formNo NE INTEGER(fiFormNo:SCREEN-VALUE) THEN DO:
            FOR EACH bf-ttGoto
                 WHERE bf-ttGoto.formNo EQ INTEGER(fiFormNo:SCREEN-VALUE):
                iBlankCnt = iBlankCnt + 1.
            END.
            
            IF iBlankCnt EQ 0 THEN
                ttGoto.blankNo = 1.
            ELSE
                ttGoto.blankNo = iBlankCnt.
        END.

        ttGoto.formNo = INTEGER(fiFormNo:SCREEN-VALUE).

        RUN pReNumberFormsAndBlanks.

        RUN pReCalculateYields.

        {&OPEN-QUERY-{&BROWSE-NAME}}
    
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.                   
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

