&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: viewers/estMiscControl.w

  Description: from VIEWER.W - Template for SmartViewer Objects

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcByLevelList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcByGroupList    AS CHARACTER NO-UNDO.    
DEFINE VARIABLE cCalcByCategoryList AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES estMiscControl
&Scoped-define FIRST-EXTERNAL-TABLE estMiscControl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR estMiscControl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS estMiscControl.costDescription ~
estMiscControl.estCostCategoryID estMiscControl.flatFeeCharge ~
estMiscControl.estCostCalcBy estMiscControl.estCostCalcSource ~
estMiscControl.chargePercent 
&Scoped-define ENABLED-TABLES estMiscControl
&Scoped-define FIRST-ENABLED-TABLE estMiscControl
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS estMiscControl.costDescription ~
estMiscControl.estCostCategoryID estMiscControl.flatFeeCharge ~
estMiscControl.estCostCalcBy estMiscControl.estCostCalcSource ~
estMiscControl.chargePercent 
&Scoped-define DISPLAYED-TABLES estMiscControl
&Scoped-define FIRST-DISPLAYED-TABLE estMiscControl
&Scoped-Define DISPLAYED-OBJECTS rsCostMethod 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE rsCostMethod AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Flat Fee", 1,
"Charge Percent", 2
     SIZE 42 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 6.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 3.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     estMiscControl.costDescription AT ROW 1.76 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     estMiscControl.estCostCategoryID AT ROW 3.14 COL 21 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS COMBO-BOX INNER-LINES 10
          DROP-DOWN-LIST
          SIZE 42 BY 1
     rsCostMethod AT ROW 5.05 COL 23 NO-LABEL WIDGET-ID 28
     estMiscControl.flatFeeCharge AT ROW 6.48 COL 21 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     estMiscControl.estCostCalcBy AT ROW 6.48 COL 21 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Level","Group","Category" 
          DROP-DOWN-LIST
          SIZE 42 BY 1
     estMiscControl.estCostCalcSource AT ROW 7.67 COL 21 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS COMBO-BOX INNER-LINES 10
          DROP-DOWN-LIST
          SIZE 42 BY 1
     estMiscControl.chargePercent AT ROW 8.95 COL 21 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RECT-1 AT ROW 4.81 COL 2 WIDGET-ID 24
     RECT-2 AT ROW 1.33 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.estMiscControl
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 10.19
         WIDTH              = 69.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET rsCostMethod IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME estMiscControl.estCostCalcBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMiscControl.estCostCalcBy V-table-Win
ON VALUE-CHANGED OF estMiscControl.estCostCalcBy IN FRAME F-Main /* Calculate Cost By */
DO:
    RUN pUpdateUI(estMiscControl.estCostCalcBy:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsCostMethod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsCostMethod V-table-Win
ON VALUE-CHANGED OF rsCostMethod IN FRAME F-Main
DO:
    RUN pUpdateUI(estMiscControl.estCostCalcBy:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "estMiscControl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "estMiscControl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iSequence AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-estMiscControl FOR estMiscControl.

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */ 
    IF estMiscControl.sequenceID EQ 0 THEN DO:
        FOR EACH bf-estMiscControl NO-LOCK
            WHERE bf-estMiscControl.company    EQ cCompany
            BY bf-estMiscControl.sequenceID DESCENDING:
            iSequence = bf-estMiscControl.sequenceID.
            LEAVE.
        END.
        
        estMiscControl.sequenceID = iSequence + 1.
    END.

    estMiscControl.company = cCompany.
    
    IF rsCostMethod:SCREEN-VALUE EQ "1"  THEN
        ASSIGN
            estMiscControl.estCostCalcSource = ""
            estMiscControl.estCostCalcBy     = ""
            estMiscControl.chargePercent     = 0
            .    
    ELSE
        estMiscControl.flatFeeCharge = 0.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win
PROCEDURE local-delete-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {custom/askdel.i}
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        rsCostMethod:SENSITIVE                     = FALSE
        estMiscControl.flatFeeCharge:SENSITIVE     = FALSE
        estMiscControl.estCostCalcSource:SENSITIVE = FALSE
        estMiscControl.estCostCalcBy:SENSITIVE     = FALSE
        estMiscControl.chargePercent:SENSITIVE     = FALSE        
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE estMiscControl THEN
        RUN pUpdateUI(estMiscControl.estCostCalcBy).
    ELSE    
        RUN pUpdateUI("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    rsCostMethod:SENSITIVE = TRUE.
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE estMiscControl AND estMiscControl.flatFeeCharge NE 0 THEN
        rsCostMethod:SCREEN-VALUE = "1".
    ELSE
        rsCostMethod:SCREEN-VALUE = "2".
    
    RUN pUpdateUI(estMiscControl.estCostCalcBy:SCREEN-VALUE).    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pValidate(OUTPUT lError, OUTPUT cMessage).
    IF lError THEN DO:
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
        
        RETURN ERROR.
    END.
                    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pComboBoxes V-table-Win
PROCEDURE pUpdateUI:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcEstCostCalcBy AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostGroupSystem      FOR estCostGroupSystem.
    DEFINE BUFFER bf-estCostGroupLevelSystem FOR estCostGroupLevelSystem.
    DEFINE BUFFER bf-estCostCategorySystem   FOR estCostCategorySystem.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipcEstCostCalcBy EQ "Level" THEN DO:
        IF cCalcByLevelList EQ "" THEN DO:
            FOR EACH bf-estCostGroupLevelSystem NO-LOCK:
                cCalcByLevelList = cCalcByLevelList + "," + bf-estCostGroupLevelSystem.estCostGroupLevelDesc + "," + STRING(bf-estCostGroupLevelSystem.estCostGroupLevelID).
            END.
            
            cCalcByLevelList = TRIM(cCalcByLevelList, ",").
        END.

        estMiscControl.estCostCalcSource:LIST-ITEM-PAIRS = cCalcByLevelList.
    END.
    ELSE IF ipcEstCostCalcBy EQ "Group" THEN DO:
        IF cCalcByGroupList EQ "" THEN DO:
            FOR EACH bf-estCostGroupSystem NO-LOCK:
                cCalcByGroupList = cCalcByGroupList + "," + bf-estCostGroupSystem.estCostGroupDesc + "," + STRING(bf-estCostGroupSystem.estCostGroupID).
            END.    
            
            cCalcByGroupList = TRIM(cCalcByGroupList, ",").
        END.

        estMiscControl.estCostCalcSource:LIST-ITEM-PAIRS = cCalcByGroupList.
    END.
    ELSE IF ipcEstCostCalcBy EQ "Category" THEN DO:
        IF cCalcByCategoryList EQ "" THEN DO:
            FOR EACH bf-estCostCategorySystem NO-LOCK:
                cCalcByCategoryList = cCalcByCategoryList + "," + bf-estCostCategorySystem.estCostCategoryDesc + "," + STRING(bf-estCostCategorySystem.estCostCategoryID).
            END.    
            
            cCalcByCategoryList = TRIM(cCalcByCategoryList, ",").
        END.

        estMiscControl.estCostCalcSource:LIST-ITEM-PAIRS = cCalcByCategoryList.
    END.
    
    IF AVAILABLE estMiscControl THEN
        estMiscControl.estCostCalcSource:SCREEN-VALUE = estMiscControl.estCostCalcSource.

    ASSIGN
        estMiscControl.flatFeeCharge:SENSITIVE     = rsCostMethod:SENSITIVE
        estMiscControl.estCostCalcSource:SENSITIVE = rsCostMethod:SENSITIVE
        estMiscControl.estCostCalcBy:SENSITIVE     = rsCostMethod:SENSITIVE
        estMiscControl.chargePercent:SENSITIVE     = rsCostMethod:SENSITIVE
        .
    
    IF SELF:SCREEN-VALUE EQ "1" THEN DO:
        ASSIGN
            estMiscControl.flatFeeCharge:VISIBLE     = TRUE
            estMiscControl.estCostCalcSource:VISIBLE = FALSE
            estMiscControl.estCostCalcBy:VISIBLE     = FALSE
            estMiscControl.chargePercent:VISIBLE     = FALSE
            .
    END.
    ELSE DO:
        ASSIGN
            estMiscControl.flatFeeCharge:VISIBLE     = FALSE
            estMiscControl.estCostCalcSource:VISIBLE = TRUE
            estMiscControl.estCostCalcBy:VISIBLE     = TRUE
            estMiscControl.chargePercent:VISIBLE     = TRUE
            .    
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit V-table-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-estCostCategorySystem FOR estCostCategorySystem.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    FOR EACH bf-estCostCategorySystem NO-LOCK:
        cCalcByCategoryList = cCalcByCategoryList + "," + bf-estCostCategorySystem.estCostCategoryDesc + "," + STRING(bf-estCostCategorySystem.estCostCategoryID).
    END.    
    
    cCalcByCategoryList = TRIM(cCalcByCategoryList, ",").

    estMiscControl.estCostCategoryID:LIST-ITEM-PAIRS = cCalcByCategoryList.
    
    IF AVAILABLE estMiscControl THEN        
        RUN pUpdateUI(estMiscControl.estCostCalcBy).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidate V-table-Win 
PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estMiscControl FOR estMiscControl.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE estMiscControl THEN DO:
        IF estMiscControl.costDescription:SCREEN-VALUE EQ "" THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Cost Description cannot be empty"
                .
            
            RETURN.            
        END.

        IF estMiscControl.estCostCategoryID:SCREEN-VALUE EQ "" OR estMiscControl.estCostCategoryID:SCREEN-VALUE EQ ? THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Category cannot be empty"
                .
            
            RETURN.            
        END.

        IF rsCostMethod:SCREEN-VALUE EQ "1" THEN DO:
            IF  DECIMAL(estMiscControl.flatFeeCharge:SCREEN-VALUE) EQ 0 THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Flat Fee cannot be 0".
                    .
                
                RETURN.
            END.            

            FIND FIRST bf-estMiscControl NO-LOCK
                 WHERE bf-estMiscControl.company           EQ cCompany
                   AND bf-estMiscControl.estCostCategoryID EQ estMiscControl.estCostCategoryID:SCREEN-VALUE
                   AND ROWID(bf-estMiscControl)            NE ROWID(estMiscControl)
                 NO-ERROR.
            IF AVAILABLE bf-estMiscControl THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "A record already exists for same configuration"
                    .
                
                RETURN.
            END.        
        END.

        IF rsCostMethod:SCREEN-VALUE EQ "2" THEN DO:
            IF DECIMAL(estMiscControl.chargePercent:SCREEN-VALUE) EQ 0 THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Charge Percent cannot be 0".
                    .
                
                RETURN.
            END.            

            IF estMiscControl.estCostCalcBy:SCREEN-VALUE EQ "" OR estMiscControl.estCostCalcBy:SCREEN-VALUE EQ ? THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Calculate Cost By cannot be empty"
                    .
                
                RETURN.            
            END.            

            IF estMiscControl.estCostCalcSource:SCREEN-VALUE EQ "" OR estMiscControl.estCostCalcSource:SCREEN-VALUE EQ ? THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Cost Source cannot be empty"
                    .
                
                RETURN.
            END.

            FIND FIRST bf-estMiscControl NO-LOCK
                 WHERE bf-estMiscControl.company           EQ cCompany
                   AND bf-estMiscControl.estCostCalcBy     EQ estMiscControl.estCostCalcBy:SCREEN-VALUE
                   AND bf-estMiscControl.estCostCalcSource EQ estMiscControl.estCostCalcSource:SCREEN-VALUE
                   AND bf-estMiscControl.estCostCategoryID EQ estMiscControl.estCostCategoryID:SCREEN-VALUE
                   AND ROWID(bf-estMiscControl)            NE ROWID(estMiscControl)
                 NO-ERROR.
            IF AVAILABLE bf-estMiscControl THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "A record already exists for same configuration"
                    .
                
                RETURN.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "estMiscControl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

