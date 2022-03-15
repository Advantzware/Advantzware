&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/smartobj/f-poprint.w 

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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
DEFINE TEMP-TABLE ttPOLine NO-UNDO
    LIKE po-ordl
    FIELD   quantity            AS INTEGER     
    FIELD   quantityInSubUnit   AS INTEGER
    FIELD   subUnitsPerUnit     AS INTEGER
    .
    
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.

DEFINE VARIABLE oKeyboard AS system.Keyboard NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttPOLine

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttPOLine.line ttPOLine.i-no ttPOLine.i-name ttPOLine.ord-qty ttPOLine.pr-qty-uom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttPOLine
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttPOLine.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttPOLine
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttPOLine


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiPOLine BROWSE-2 btDelete 
&Scoped-Define DISPLAYED-OBJECTS fiPOLine 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_qtyunits AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCreate 
     LABEL "CREATE TAGS" 
     SIZE 27.2 BY 2.14.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 7.6 BY 1.81 TOOLTIP "Delete currently selected record".

DEFINE BUTTON btnKeyboardPO 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "UPDATE" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE fiPOLine AS CHARACTER FORMAT "X(256)":U 
     LABEL "PO and LINE" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttPOLine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 F-Frame-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttPOLine.line
    ttPOLine.i-no WIDTH 38
    ttPOLine.i-name WIDTH 50
    ttPOLine.ord-qty
    ttPOLine.pr-qty-uom COLUMN-LABEL "UOM"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 131.8 BY 7.48
         BGCOLOR 15 FGCOLOR 0 FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btCreate AT ROW 1.14 COL 109 WIDGET-ID 6
     btnKeyboardPO AT ROW 1.43 COL 67.6 WIDGET-ID 136 NO-TAB-STOP 
     fiPOLine AT ROW 1.48 COL 27 COLON-ALIGNED WIDGET-ID 4
     BROWSE-2 AT ROW 3.52 COL 4.2 WIDGET-ID 200
     btUpdate AT ROW 11.48 COL 136.8 WIDGET-ID 66
     btDelete AT ROW 3.48 COL 136.8 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.6 BY 19.38
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 19.33
         WIDTH              = 152.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-2 fiPOLine F-Main */
/* SETTINGS FOR BUTTON btCreate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnKeyboardPO IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnKeyboardPO:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btUpdate IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPOLine.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 F-Frame-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME F-Main
DO: 
    btUpdate:SENSITIVE = FALSE.
    
    IF AVAILABLE ttPOLine THEN DO:
        {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(INPUT ttPOLine.quantity, INPUT ttPOLine.quantityInSubUnit, INPUT ttPOLine.subUnitsPerUnit, INPUT 0)"}
        
        btUpdate:SENSITIVE = TRUE.
    END.
    ELSE
        {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(INPUT 0, INPUT 0, INPUT 0, INPUT 0)"}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreate F-Frame-Win
ON CHOOSE OF btCreate IN FRAME F-Main /* CREATE TAGS */
DO:
    RUN new-state ("create-tag-po").
    
    EMPTY TEMP-TABLE ttPOLine.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    
    APPLY "ENTRY" TO fiPOLine.
    
    fiPOLine:SET-SELECTION(1, -1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete F-Frame-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
    RUN pDeletePOLine.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardPO F-Frame-Win
ON CHOOSE OF btnKeyboardPO IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiPoLine.    
    
    oKeyboard:OpenKeyboardOverride (fiPOLine:HANDLE, "Qwerty"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate F-Frame-Win
ON CHOOSE OF btUpdate IN FRAME F-Main /* UPDATE */
DO:
    DEFINE VARIABLE iQuantity          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit   AS INTEGER NO-UNDO.
    
    IF SELF:LABEL = "Update" THEN DO:
        SELF:LABEL = "Save".
        {methods/run_link.i "QTY-SOURCE" "EnableQuantities"}
        
    END.
    ELSE DO:
        SELF:LABEL = "Update".

        {methods/run_link.i "QTY-SOURCE" "GetQuantities" "(OUTPUT iQuantity, OUTPUT iQuantityInSubUnit, OUTPUT iSubUnitsPerUnit)"}

        {methods/run_link.i "QTY-SOURCE" "DisableQuantities"}
  
        IF AVAILABLE ttPOLine THEN
            ASSIGN
                ttPOLine.quantity          = iQuantity
                ttPOLine.quantityInSubUnit = iQuantityInSubUnit
                ttPOLine.subUnitsPerUnit   = iSubUnitsPerUnit
                .
           
        BROWSE {&BROWSE-NAME}:REFRESH().
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPOLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPOLine F-Frame-Win
ON ENTRY OF fiPOLine IN FRAME F-Main /* PO and LINE */
DO:
    SELF:BGCOLOR = 30.

    IF VALID-OBJECT (oKeyboard) THEN DO:
        oKeyboard:FocusField = SELF.    
        
        oKeyboard:OpenKeyboard (SELF, "Qwerty").
    END.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPOLine F-Frame-Win
ON LEAVE OF fiPOLine IN FRAME F-Main /* PO and LINE */
DO:   
    IF (((LASTKEY LT 609 OR LASTKEY GT 652) AND LASTKEY NE -1) OR (VALID-OBJECT (oKeyboard) AND oKeyboard:IsKeyboardOpen())) AND SELF:SCREEN-VALUE NE "" THEN
        RUN pScanPO (SELF:SCREEN-VALUE).
    
    SELF:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/qtyunits.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_qtyunits ).
       RUN set-position IN h_qtyunits ( 11.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.29 , 131.40 ) */

       /* Links to SmartObject h_qtyunits. */
       RUN add-link IN adm-broker-hdl ( h_qtyunits , 'QTY':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_qtyunits ,
             BROWSE-2:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
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
  DISPLAY fiPOLine 
      WITH FRAME F-Main.
  ENABLE fiPOLine BROWSE-2 btDelete 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDesignConfig F-Frame-Win 
PROCEDURE GetDesignConfig :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoDesignConfig AS system.Config NO-UNDO.
    
    opoDesignConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTagPODesign").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMessageAndType F-Frame-Win 
PROCEDURE GetMessageAndType :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiStatusMessageType AS INTEGER   NO-UNDO.
    
    ASSIGN
        opcStatusMessage     = cStatusMessage
        opiStatusMessageType = iStatusMessageType
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPOLineTT F-Frame-Win 
PROCEDURE GetPOLineTT :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttPOLine.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable F-Frame-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildPOLines F-Frame-Win 
PROCEDURE pBuildPOLines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOID    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine  AS INTEGER   NO-UNDO.    
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-eb      FOR eb.
    
    EMPTY TEMP-TABLE ttPOLine.
    
    FOR EACH bf-po-ordl NO-LOCK
        WHERE bf-po-ordl.company   EQ ipcCompany
          AND bf-po-ordl.po-no     EQ ipiPOID
          AND bf-po-ordl.item-type EQ FALSE
          AND (bf-po-ordl.line     EQ ipiPOLine OR ipiPOLine EQ 0):
        CREATE ttPOLine.
        BUFFER-COPY bf-po-ordl TO ttPOLine.
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ bf-po-ordl.company
               AND bf-itemfg.i-no    EQ bf-po-ordl.i-no
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN
            NEXT.
            
        ASSIGN
            ttPOLine.quantityInSubUnit = bf-itemfg.case-count
            ttPOLine.subUnitsPerUnit   = IF bf-itemfg.case-pall NE 0 THEN 
                                             bf-itemfg.case-pall 
                                         ELSE 
                                             1
            .

        IF AVAILABLE bf-itemfg AND bf-itemfg.est-no NE '' THEN
            FIND FIRST bf-eb NO-LOCK 
                 WHERE bf-eb.company  EQ bf-itemfg.company
                   AND bf-eb.est-no   EQ bf-itemfg.est-no
                   AND bf-eb.stock-no EQ bf-itemfg.i-no 
                 NO-ERROR.

        IF AVAILABLE bf-eb THEN
            ASSIGN
                ttPOLine.quantityInSubUnit = bf-eb.cas-cnt
                ttPOLine.subUnitsPerUnit   = bf-eb.cas-pal
                .
                
        ttPOLine.quantity = ttPOLine.quantityInSubUnit * ttPOLine.subUnitsPerUnit.        
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeletePOLine F-Frame-Win 
PROCEDURE pDeletePOLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE ttPOLine THEN DO:
        DELETE ttPOLine.
        
        {custom/askdel.i}
        
        {&OPEN-QUERY-{&BROWSE-NAME}}
        
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit F-Frame-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lShowKeyboard AS LOGICAL NO-UNDO.
    
    {methods/run_link.i "CONTAINER-SOURCE" "GetKeyboard" "(OUTPUT oKeyboard)"}
    {methods/run_link.i "CONTAINER-SOURCE" "ShowKeyboard" "(OUTPUT lShowKeyboard)"}
    {methods/run_link.i "QTY-SOURCE" "SetKeyboard" "(INPUT oKeyboard)"}

    IF lShowKeyboard THEN
        RUN ShowKeyboard.
    
    RUN Set-Focus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanPO F-Frame-Win 
PROCEDURE pScanPO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPOAndLine AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidPOLine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iPOID        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPOLine      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantity    AS DECIMAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        btUpdate:SENSITIVE      = FALSE
        btCreate:SENSITIVE      = FALSE
        .
    
    RUN new-state ("empty-message").
    
    ASSIGN
        iPOID     = INTEGER(ENTRY(1, ipcPOAndLine, "-"))
        iPOLine   = INTEGER(ENTRY(2, ipcPOAndLine, "-"))
        dQuantity = DECIMAL(ENTRY(3, ipcPOAndLine, "-"))
        NO-ERROR.
        
    RUN pBuildPOLines (
        INPUT cCompany,
        INPUT iPOID,
        INPUT iPOLine
        ) NO-ERROR.

    IF NOT TEMP-TABLE ttPOLine:HAS-RECORDS THEN DO:
        RUN pSendError ("Invalid PO # '" + ipcPOAndLine + "' or finished good lines").
        
        RETURN.
    END.
    
    IF dQuantity NE 0 AND dQuantity NE ? THEN DO:
        FIND FIRST ttPOLine
             WHERE ttPOLine.po-no EQ iPOID
               AND ttPOLine.line  EQ iPOLine
             NO-ERROR.
        IF AVAILABLE ttPOLine THEN
            ASSIGN
                ttPOLine.ord-qty    = dQuantity
                ttPOLine.pr-qty-uom = "EA"   
                .
    END.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}                        
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    
    ASSIGN
        btCreate:SENSITIVE = TRUE
        btUpdate:SENSITIVE = TRUE
        .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSendError F-Frame-Win 
PROCEDURE pSendError :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcStatusMessage AS CHARACTER NO-UNDO.    
    
    ASSIGN
        cStatusMessage     = ipcStatusMessage
        iStatusMessageType = 3
        .

    RUN new-state (
        "po-error"
        ).

    ASSIGN
        cStatusMessage     = ""
        iStatusMessageType = 0
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset F-Frame-Win 
PROCEDURE Reset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    EMPTY TEMP-TABLE ttPOLine.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    {methods/run_link.i "QTY-SOURCE" "SetQuantities" "(0,0,0,0)"}
    
    fiPOLine:SCREEN-VALUE = "".
        
    APPLY "ENTRY" TO fiPOLine.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttPOLine"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus F-Frame-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    APPLY "ENTRY" TO fiPOLine IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard F-Frame-Win 
PROCEDURE ShowKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        btnKeyboardPO:VISIBLE   = TRUE
        btnKeyboardPO:SENSITIVE = TRUE
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
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

