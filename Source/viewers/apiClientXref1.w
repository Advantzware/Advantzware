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

  File: viewers/apiClientXref1.w

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
{custom/globdefs.i}
{sys/inc/var.i}

ASSIGN
    cocode = g_company
    locode = g_loc
    .
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cAPIClientXrefAny AS CHARACTER NO-UNDO INITIAL "_ANY_".

/* The below variables are used in run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

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
&Scoped-define EXTERNAL-TABLES apiClientXref apiClient
&Scoped-define FIRST-EXTERNAL-TABLE apiClientXref


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR apiClientXref, apiClient.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS apiClientXref.scopeID apiClientXref.inactive 
&Scoped-define ENABLED-TABLES apiClientXref
&Scoped-define FIRST-ENABLED-TABLE apiClientXref
&Scoped-Define ENABLED-OBJECTS RECT-5 cbScopeType 
&Scoped-Define DISPLAYED-FIELDS apiClientXref.scopeID ~
apiClientXref.inactive 
&Scoped-define DISPLAYED-TABLES apiClientXref
&Scoped-define FIRST-DISPLAYED-TABLE apiClientXref
&Scoped-Define DISPLAYED-OBJECTS cbScopeType cbTriggerID 

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
DEFINE VARIABLE cbScopeType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scope Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE cbTriggerID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trigger ID" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 88.2 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cbScopeType AT ROW 1.24 COL 25 COLON-ALIGNED WIDGET-ID 8
     apiClientXref.scopeID AT ROW 2.38 COL 25 COLON-ALIGNED WIDGET-ID 4 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     cbTriggerID AT ROW 3.52 COL 25 COLON-ALIGNED WIDGET-ID 18
     apiClientXref.inactive AT ROW 4.76 COL 27 WIDGET-ID 16
          VIEW-AS TOGGLE-BOX
          SIZE 14.4 BY .81
     RECT-5 AT ROW 1 COL 1 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.apiClientXref,ASI.apiClient
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
         HEIGHT             = 4.76
         WIDTH              = 88.2.
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

/* SETTINGS FOR COMBO-BOX cbTriggerID IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN apiClientXref.scopeID IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME cbScopeType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbScopeType V-table-Win
ON VALUE-CHANGED OF cbScopeType IN FRAME F-Main /* Scope Type */
DO:
    IF cbScopeType:SCREEN-VALUE EQ cAPIClientXrefAny THEN 
        ASSIGN 
            scopeID:SCREEN-VALUE = cAPIClientXrefAny
            scopeID:SENSITIVE    = FALSE
            .
    ELSE
        scopeID:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME apiClientXref.scopeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL apiClientXref.scopeID V-table-Win
ON HELP OF apiClientXref.scopeID IN FRAME F-Main /* Scope ID */
DO:
    DEFINE VARIABLE cReturnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE riRecVal      AS RECID     NO-UNDO.
    DEFINE VARIABLE iSubjectID    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustomerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    
    IF cbScopeType:SCREEN-VALUE EQ "" OR cbScopeType:SCREEN-VALUE EQ ? OR
       cbScopeType:SCREEN-VALUE EQ cAPIClientXrefAny THEN DO:
        MESSAGE "Please select any of the following scope types in Scope Type field for help:" SKIP
            TRIM(REPLACE(cbScopeType:LIST-ITEMS,cAPIClientXrefAny,""),",")
            VIEW-AS ALERT-BOX INFORMATION.
        RETURN.
    END.
    
    CASE cbScopeType:SCREEN-VALUE:
        WHEN "Customer" THEN
            iSubjectID = 23. /* Customer Lookup subject ID */
        WHEN "Vendor" THEN
            iSubjectID = 32. /* Vendor Lookup Subject ID */
        WHEN "ShipTo" THEN
            iSubjectID = 122. /* ShipTo lookup subject ID */
        OTHERWISE
            RETURN.            
    END CASE.
    
    RUN system/openlookup.p (
        INPUT  "", 
        INPUT  "", /* lookup field */
        INPUT  iSubjectID,   /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT cReturnFields, 
        OUTPUT cLookupField, 
        OUTPUT riRecVal
        ). 
                
    IF cLookupField NE "" AND cReturnFields NE "" THEN DO:
        ASSIGN
            cCustomerID = ""
            cShipToID   = ""
            .
            
        IF cbScopeType:SCREEN-VALUE EQ "ShipTo" THEN DO:
            DO iIndex = 1 TO NUM-ENTRIES(cReturnFields, "|"):
                IF ENTRY(iIndex, cReturnFields, "|") EQ "ship-id" THEN
                    cShipToID = ENTRY(iIndex + 1, cReturnFields, "|").
                
                IF ENTRY(iIndex, cReturnFields, "|") EQ "cust-no" THEN
                    cCustomerID = ENTRY(iIndex + 1, cReturnFields, "|").
                 
            END.
            SELF:SCREEN-VALUE = cCustomerID + "|" + cShipToID.
        END.
        ELSE
            SELF:SCREEN-VALUE = cLookupField.
    END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  {sys/inc/f3help.i}

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
  {src/adm/template/row-list.i "apiClientXref"}
  {src/adm/template/row-list.i "apiClient"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "apiClientXref"}
  {src/adm/template/row-find.i "apiClient"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAPIAndClientID V-table-Win 
PROCEDURE GetAPIAndClientID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcAPIID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcClientID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.    
            
    {methods/run_link.i "RECORD-SOURCE" "GetAPIAndClientID" "(OUTPUT opcAPIID, OUTPUT opcClientID)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAPIID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientID AS CHARACTER NO-UNDO.
        
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    
    RUN GetAPIAndClientID (
        OUTPUT cAPIID,
        OUTPUT cClientID
        ).

    ASSIGN
        apiClientXref.company   = cocode
        apiClientXref.apiID     = cAPIID
        apiClientXref.clientID  = cClientID
        apiClientXref.scopeType = cbScopeType:SCREEN-VALUE
        apiClientXref.triggerID = cbTriggerID:SCREEN-VALUE
        .

    IF apiClientXref.scopeID EQ "" THEN
        apiClientXref.scopeID = cAPIClientXrefAny.

    IF cbScopeType:SCREEN-VALUE EQ "" OR cbScopeType:SCREEN-VALUE EQ ? THEN
        apiClientXref.scopeType = cAPIClientXrefAny.

    IF cbTriggerID:SCREEN-VALUE EQ "" OR cbTriggerID:SCREEN-VALUE EQ ? THEN
        apiClientXref.triggerID = cAPIClientXrefAny.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        cbScopeType:SENSITIVE = FALSE
        cbTriggerID:SENSITIVE = FALSE.
        .
    
    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE apiClientXref THEN DO:
        ASSIGN
            cbScopeType:SCREEN-VALUE = apiClientXref.scopeType
            cbTriggerID:SCREEN-VALUE = apiClientXref.triggerID
            NO-ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        cbScopeType:SENSITIVE = TRUE
        cbTriggerID:SENSITIVE = TRUE
        .

    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}
    
    APPLY "VALUE-CHANGED" TO cbScopeType.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-reset-record V-table-Win 
PROCEDURE local-reset-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'reset-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    APPLY "VALUE-CHANGED" TO cbScopeType IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.    
    DEFINE VARIABLE cRowState AS CHARACTER NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pUpdateTriggerList.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

    RUN pGetRowState (
        OUTPUT cRowState
        ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    {methods/run_link.i "TABLEIO-SOURCE" "set-buttons" "(INPUT cRowState)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pFieldValidations (
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.        
    END.
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFieldValidations V-table-Win 
PROCEDURE pFieldValidations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCustomerID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToID   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-cust   FOR cust.
    DEFINE BUFFER bf-vend   FOR vend.
    DEFINE BUFFER bf-shipTo FOR shipTo.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE apiClientXref THEN DO:
        /* If a scope ID is enetered, force the user to enter a valid scopeType */
        IF apiClientXref.scopeID:SCREEN-VALUE NE "" AND
           apiClientXref.scopeID:SCREEN-VALUE NE cAPIClientXrefAny THEN DO:
            IF cbScopeType:SCREEN-VALUE EQ cAPIClientXrefAny OR
               cbScopeType:SCREEN-VALUE EQ ? OR
               cbScopeType:SCREEN-VALUE EQ "" THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Please specify a valid Scope Type for the given Scope ID"
                    .
                RETURN.
            END.
            
            IF cbScopeType:SCREEN-VALUE EQ "Customer" THEN DO:
                FIND FIRST bf-cust NO-LOCK
                     WHERE bf-cust.company EQ g_company
                       AND bf-cust.cust-no EQ apiClientXref.scopeID:SCREEN-VALUE
                     NO-ERROR.
                IF NOT AVAILABLE bf-cust THEN DO:
                    ASSIGN
                        oplSuccess = FALSE
                        opcMessage = "Invalid customer # entered in Scope ID"
                        .
                    RETURN.                
                END.
            END.
            IF cbScopeType:SCREEN-VALUE EQ "Vendor" THEN DO:
                FIND FIRST bf-vend NO-LOCK
                     WHERE bf-vend.company EQ g_company
                       AND bf-vend.vend-no EQ apiClientXref.scopeID:SCREEN-VALUE
                     NO-ERROR.
                IF NOT AVAILABLE bf-vend THEN DO:
                    ASSIGN
                        oplSuccess = FALSE
                        opcMessage = "Invalid vendor # entered in Scope ID"
                        .
                    RETURN.                
                END.
            END.
            IF cbScopeType:SCREEN-VALUE EQ "ShipTo" THEN DO:
                IF NUM-ENTRIES(apiClientXref.scopeID:SCREEN-VALUE, "|") GE 2 THEN
                    cShipToID = ENTRY(2, apiClientXref.scopeID:SCREEN-VALUE, "|").
                
                cCustomerID = ENTRY(1, apiClientXref.scopeID:SCREEN-VALUE, "|").

                FIND FIRST bf-shipTo NO-LOCK
                     WHERE bf-shipTo.company EQ g_company
                       AND bf-shipTo.cust-no EQ cCustomerID
                       AND bf-shipTo.ship-id EQ cShipToID
                     NO-ERROR.
                IF NOT AVAILABLE bf-shipTo THEN DO:
                    ASSIGN
                        oplSuccess = FALSE
                        opcMessage = "Invalid ship to entered in Scope ID"
                        .
                    RETURN.                
                END.
            END.            
        END.        
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRowState V-table-Win 
PROCEDURE pGetRowState :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRowState AS CHARACTER NO-UNDO.

    DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.    

    DEFINE VARIABLE lAPIClientRecordAvailable AS LOGICAL NO-UNDO.

    {methods/run_link.i "RECORD-SOURCE" "IsAPIClientRecordAvailable" "(OUTPUT lAPIClientRecordAvailable)"}    
    
    IF lAPIClientRecordAvailable THEN DO:
        IF AVAILABLE apiClientXref THEN
            opcRowState = "initial".
        ELSE
            opcRowState = "add-only".    
    END.        
    ELSE
        opcRowState = "disable-all".    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit V-table-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScopeTypeList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pUpdateTriggerList.
    
    RUN Outbound_GetScopeTypeList IN hdOutboundProcs (
        OUTPUT cScopeTypeList    
        ).
            
    cbScopeType:LIST-ITEMS = cScopeTypeList.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateTriggerList V-table-Win 
PROCEDURE pUpdateTriggerList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTriggerList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientID      AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN GetAPIAndClientID (
        OUTPUT cAPIID,
        OUTPUT cClientID
        ).
    
    RUN Outbound_GetAPITriggersList IN hdOutboundProcs (
        INPUT  g_company,
        INPUT  cAPIID,
        INPUT  cClientID,
        OUTPUT cTriggerList
        ).
        
    cbTriggerID:LIST-ITEMS = "_ANY_" + "," + cTriggerList.
    
    IF AVAILABLE apiClientXref THEN
        cbTriggerID:SCREEN-VALUE = apiClientXref.triggerID NO-ERROR.
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
  {src/adm/template/snd-list.i "apiClientXref"}
  {src/adm/template/snd-list.i "apiClient"}

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

