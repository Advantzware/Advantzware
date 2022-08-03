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

  File: viewers/estMisc.w

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
DEFINE VARIABLE cEstimateNo         AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES estMisc
&Scoped-define FIRST-EXTERNAL-TABLE estMisc


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR estMisc.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS estMisc.costDescription estMisc.estCostCalcBy ~
estMisc.estCostCalcSource estMisc.estCostCategoryID estMisc.flatFeeCharge ~
estMisc.chargePercent 
&Scoped-define ENABLED-TABLES estMisc
&Scoped-define FIRST-ENABLED-TABLE estMisc
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS estMisc.costDescription ~
estMisc.estCostCalcBy estMisc.estCostCalcSource estMisc.estCostCategoryID ~
estMisc.flatFeeCharge estMisc.chargePercent 
&Scoped-define DISPLAYED-TABLES estMisc
&Scoped-define FIRST-DISPLAYED-TABLE estMisc


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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 2.69.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 67 BY 5.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     estMisc.costDescription AT ROW 1.96 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     estMisc.estCostCalcBy AT ROW 3.15 COL 21 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Level","Group","Category" 
          DROP-DOWN-LIST
          SIZE 42 BY .92
     estMisc.estCostCalcSource AT ROW 4.35 COL 21 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS COMBO-BOX INNER-LINES 10
          DROP-DOWN-LIST
          SIZE 42 BY 1
     estMisc.estCostCategoryID AT ROW 5.54 COL 21 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS COMBO-BOX INNER-LINES 10
          DROP-DOWN-LIST
          SIZE 42 BY 1
     estMisc.flatFeeCharge AT ROW 7.46 COL 21 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 18.83 BY 1
     estMisc.chargePercent AT ROW 8.58 COL 21 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 18.83 BY 1
     RECT-1 AT ROW 7.19 COL 2 WIDGET-ID 24
     RECT-2 AT ROW 1.46 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.estMisc
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
         HEIGHT             = 9.31
         WIDTH              = 70.5.
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

&Scoped-define SELF-NAME estMisc.estCostCalcBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMisc.estCostCalcBy V-table-Win
ON VALUE-CHANGED OF estMisc.estCostCalcBy IN FRAME F-Main /* Calculate Cost By */
DO:
    DEFINE BUFFER bf-estCostGroupSystem      FOR estCostGroupSystem.
    DEFINE BUFFER bf-estCostGroupLevelSystem FOR estCostGroupLevelSystem.
    DEFINE BUFFER bf-estCostCategorySystem   FOR estCostCategorySystem.
    
    IF estMisc.estCostCalcBy:SCREEN-VALUE EQ "Level" THEN DO:
        IF cCalcByLevelList EQ "" THEN DO:
            FOR EACH bf-estCostGroupLevelSystem NO-LOCK:
                cCalcByLevelList = cCalcByLevelList + "," + bf-estCostGroupLevelSystem.estCostGroupLevelDesc + "," + STRING(bf-estCostGroupLevelSystem.estCostGroupLevelID).
            END.
            
            cCalcByLevelList = TRIM(cCalcByLevelList, ",").
        END.

        estMisc.estCostCalcSource:LIST-ITEM-PAIRS = cCalcByLevelList.
    END.
    ELSE IF estMisc.estCostCalcBy:SCREEN-VALUE EQ "Group" THEN DO:
        IF cCalcByGroupList EQ "" THEN DO:
            FOR EACH bf-estCostGroupSystem NO-LOCK:
                cCalcByGroupList = cCalcByGroupList + "," + bf-estCostGroupSystem.estCostGroupDesc + "," + STRING(bf-estCostGroupSystem.estCostGroupID).
            END.    
            
            cCalcByGroupList = TRIM(cCalcByGroupList, ",").
        END.

        estMisc.estCostCalcSource:LIST-ITEM-PAIRS = cCalcByGroupList.
    END.
    ELSE IF estMisc.estCostCalcBy:SCREEN-VALUE EQ "Category" THEN DO:
        IF cCalcByCategoryList EQ "" THEN DO:
            FOR EACH bf-estCostCategorySystem NO-LOCK:
                cCalcByCategoryList = cCalcByCategoryList + "," + bf-estCostCategorySystem.estCostCategoryDesc + "," + STRING(bf-estCostCategorySystem.estCostCategoryID).
            END.    
            
            cCalcByCategoryList = TRIM(cCalcByCategoryList, ",").
        END.

        estMisc.estCostCalcSource:LIST-ITEM-PAIRS = cCalcByCategoryList.
    END.
        
    IF AVAILABLE estMisc THEN
        estMisc.estCostCalcSource:SCREEN-VALUE = estMisc.estCostCalcSource.
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
  {src/adm/template/row-list.i "estMisc"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "estMisc"}

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

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */ 
    ASSIGN
        estMisc.company    = cCompany
        estMisc.estimateNo = cEstimateNo
        .

    IF DECIMAL(estMisc.flatFeeCharge:SCREEN-VALUE) NE 0  THEN
        ASSIGN
            estMisc.estCostCalcSource = ""
            estMisc.estCostCalcBy     = ""
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
    APPLY "VALUE-CHANGED" TO estMisc.estCostCalcBy.
    APPLY "VALUE-CHANGED" TO estMisc.flatFeeCharge.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit V-table-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-estCostCategorySystem FOR estCostCategorySystem.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    {methods/run_link.i "CONTAINER-SOURCE" "GetEstimateNo" "(OUTPUT cEstimateNo)"}

    FOR EACH bf-estCostCategorySystem NO-LOCK:
        cCalcByCategoryList = cCalcByCategoryList + "," + bf-estCostCategorySystem.estCostCategoryDesc + "," + STRING(bf-estCostCategorySystem.estCostCategoryID).
    END.    
    
    cCalcByCategoryList = TRIM(cCalcByCategoryList, ",").

    estMisc.estCostCategoryID:LIST-ITEM-PAIRS = cCalcByCategoryList.
    
    APPLY "VALUE-CHANGED" TO estMisc.estCostCalcBy.
    APPLY "VALUE-CHANGED" TO estMisc.flatFeeCharge.
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

    DEFINE BUFFER bf-estMisc FOR estMisc.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE estMisc THEN DO:
        IF estMisc.costDescription:SCREEN-VALUE EQ "" THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Cost Description cannot be empty"
                .
            
            RETURN.            
        END.

        IF DECIMAL(estMisc.chargePercent:SCREEN-VALUE) EQ 0 AND DECIMAL(estMisc.flatFeeCharge:SCREEN-VALUE) EQ 0 THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Both Charge Percent and Flat Fee cannot be 0".
                .
            
            RETURN.            
        END.
        
        IF estMisc.estCostCalcBy:SCREEN-VALUE EQ "" OR estMisc.estCostCalcBy:SCREEN-VALUE EQ ? THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Calculate Cost By cannot be empty"
                .
            
            RETURN.            
        END.

        IF estMisc.estCostCategoryID:SCREEN-VALUE EQ "" OR estMisc.estCostCategoryID:SCREEN-VALUE EQ ? THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Category cannot be empty"
                .
            
            RETURN.            
        END.
        
        IF DECIMAL(estMisc.chargePercent:SCREEN-VALUE) NE 0 THEN DO:
            IF estMisc.estCostCalcBy:SCREEN-VALUE EQ "" OR estMisc.estCostCalcBy:SCREEN-VALUE EQ ? THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Calculate Cost By cannot be empty"
                    .
                
                RETURN.            
            END.            

            IF estMisc.estCostCalcSource:SCREEN-VALUE EQ "" OR estMisc.estCostCalcSource:SCREEN-VALUE EQ ? THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Cost Source cannot be empty"
                    .
                
                RETURN.
            END.
        END.
                
        FIND FIRST bf-estMisc NO-LOCK
             WHERE bf-estMisc.company           EQ cCompany
               AND bf-estMisc.estimateNo        EQ cEstimateNo
               AND bf-estMisc.estCostCalcBy     EQ estMisc.estCostCalcBy:SCREEN-VALUE
               AND bf-estMisc.estCostCategoryID EQ estMisc.estCostCategoryID:SCREEN-VALUE
               AND bf-estMisc.estCostCalcSource EQ estMisc.estCostCalcSource:SCREEN-VALUE
               AND ROWID(bf-estMisc)        NE ROWID(estMisc)
             NO-ERROR.
        IF AVAILABLE bf-estMisc THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "A record already exists for same configuration"
                .
            
            RETURN.
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
  {src/adm/template/snd-list.i "estMisc"}

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

