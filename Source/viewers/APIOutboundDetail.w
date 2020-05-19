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

  File: viewers/APIOutboundDetail.w

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
DEFINE VARIABLE lSuperAdmin AS LOGICAL NO-UNDO.

DEFINE VARIABLE hdPgmMstrSecur AS HANDLE NO-UNDO.
RUN system/PgmMstrSecur.p PERSISTENT SET hdPgmMstrSecur.

RUN epCanAccess IN hdPgmMstrSecur (
    INPUT  "viewers/APIOutboundDetail.w", /* Program Name */
    INPUT  "",                            /* Function */
    OUTPUT lSuperAdmin
    ).
    
DELETE PROCEDURE hdPgmMstrSecur.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES APIOutboundDetail APIOutbound
&Scoped-define FIRST-EXTERNAL-TABLE APIOutboundDetail


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR APIOutboundDetail, APIOutbound.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS APIOutboundDetail.detailID ~
APIOutboundDetail.parentID 
&Scoped-define ENABLED-TABLES APIOutboundDetail
&Scoped-define FIRST-ENABLED-TABLE APIOutboundDetail
&Scoped-Define ENABLED-OBJECTS edData 
&Scoped-Define DISPLAYED-FIELDS APIOutboundDetail.apiID ~
APIOutboundDetail.clientID APIOutboundDetail.detailID ~
APIOutboundDetail.parentID 
&Scoped-define DISPLAYED-TABLES APIOutboundDetail
&Scoped-define FIRST-DISPLAYED-TABLE APIOutboundDetail
&Scoped-Define DISPLAYED-OBJECTS edData 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-ASSIGN-FIELDS APIOutboundDetail.apiID ~
APIOutboundDetail.clientID 

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
DEFINE VARIABLE edData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 101 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 116 BY 10.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     APIOutboundDetail.apiID AT ROW 1.24 COL 13 COLON-ALIGNED WIDGET-ID 2
          LABEL "API ID" FORMAT "x(32)"
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 3 FGCOLOR 15 
     APIOutboundDetail.clientID AT ROW 1.24 COL 73 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 3 FGCOLOR 15 
     APIOutboundDetail.detailID AT ROW 3.14 COL 13 COLON-ALIGNED WIDGET-ID 4
          LABEL "Detail ID" FORMAT "x(32)"
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 15 
     APIOutboundDetail.parentID AT ROW 5.1 COL 13.2 COLON-ALIGNED WIDGET-ID 6
          LABEL "Parent ID" FORMAT "x(32)"
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 15 
     edData AT ROW 6.95 COL 15 NO-LABEL WIDGET-ID 8
     "Data:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 6.95 COL 8 WIDGET-ID 10
     RECT-6 AT ROW 1 COL 1 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.APIOutboundDetail,ASI.APIOutbound
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
         HEIGHT             = 10.24
         WIDTH              = 116.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN APIOutboundDetail.apiID IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
ASSIGN 
       APIOutboundDetail.apiID:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN APIOutboundDetail.clientID IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN APIOutboundDetail.detailID IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       edData:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN APIOutboundDetail.parentID IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
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
  {src/adm/template/row-list.i "APIOutboundDetail"}
  {src/adm/template/row-list.i "APIOutbound"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "APIOutboundDetail"}
  {src/adm/template/row-find.i "APIOutbound"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   /* Code placed here will execute PRIOR to standard behavior. */
       
   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   RUN pSetDefaults.
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
    RUN pUpdateFields.
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pDisableFields.
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

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pDisplayFields.
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

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pEnableFields.
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
    DEFINE VARIABLE lValidParent AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail FOR APIOutboundDetail.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.           
    
    lValidParent = IF AVAILABLE APIOutbound
                       AND APIOutbound.apiID EQ APIOutboundDetail.parentID:SCREEN-VALUE THEN
                       TRUE
                   ELSE
                       FALSE.
    
    IF NOT lValidParent THEN
        lValidParent = CAN-FIND(
                       FIRST bf-APIOutboundDetail
                       WHERE bf-APIOutboundDetail.apiOutboundID EQ APIOutbound.apiOutboundID
                         AND bf-APIOutboundDetail.detailID      EQ APIOutboundDetail.parentID:SCREEN-VALUE
                       ).

    IF NOT lValidParent THEN DO:
        MESSAGE "Invalid parent ID value [" APIOutboundDetail.parentID:SCREEN-VALUE
                "], please enter a valid Parent ID" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
                       
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableFields V-table-Win 
PROCEDURE pDisableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    edData:READ-ONLY = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayFields V-table-Win 
PROCEDURE pDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    IF AVAILABLE APIOutboundDetail THEN
        edData:SCREEN-VALUE = STRING(APIOutboundDetail.data).
    ELSE
        edData:SCREEN-VALUE = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableFields V-table-Win 
PROCEDURE pEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRowState AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pGetRowState (
        OUTPUT cRowState
        ).
    
    IF cRowState EQ "update-only" THEN
        ASSIGN
            apiOutboundDetail.detailID:SENSITIVE = FALSE
            apiOutboundDetail.parentID:SENSITIVE = FALSE
            .

    edData:READ-ONLY = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRowState V-table-Win 
PROCEDURE pGetRowState PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRowState AS CHARACTER NO-UNDO.

    IF AVAILABLE APIOutboundDetail THEN DO:
        IF APIOutboundDetail.apiOutboundID GT 5000 THEN
            opcRowState = "update-only".
        ELSE
            opcRowState = "disable-all".
    END.
    ELSE
        opcRowState = "disable-all".
        
    IF lSuperAdmin THEN DO:
        IF AVAILABLE APIoutboundDetail THEN
            opcRowState = "initial".
        ELSE
            opcRowState = "add-only".
    END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDefaults V-table-Win 
PROCEDURE pSetDefaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
   END.

   edData:SCREEN-VALUE = "".
   
   IF AVAILABLE APIOutbound THEN
       ASSIGN
           APIOutboundDetail.apiID:SCREEN-VALUE    = APIOutbound.apiID
           APIOutboundDetail.clientID:SCREEN-VALUE = APIOutbound.clientID
           .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFields V-table-Win 
PROCEDURE pUpdateFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF AVAILABLE APIOutboundDetail THEN        
        ASSIGN
            APIOutboundDetail.company       = APIOutbound.company
            APIOutboundDetail.apiOutboundID = APIOutbound.apiOutboundID
            APIOutboundDetail.data          = edData:SCREEN-VALUE
            .
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
  {src/adm/template/snd-list.i "APIOutboundDetail"}
  {src/adm/template/snd-list.i "APIOutbound"}

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

