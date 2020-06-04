&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File:  browsers/apiClientXref1.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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
DEFINE VARIABLE cScopeID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScopeType AS CHARACTER NO-UNDO INITIAL "ALL".
DEFINE VARIABLE cTriggerID AS CHARACTER NO-UNDO INITIAL "ALL".
DEFINE VARIABLE cAPIID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cClientID  AS CHARACTER NO-UNDO.

DEFINE VARIABLE cAPIClientXrefAny AS CHARACTER NO-UNDO INITIAL "_ANY_".

DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES apiClient
&Scoped-define FIRST-EXTERNAL-TABLE apiClient


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR apiClient.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES apiClientXref

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table apiClientXref.scopeType apiClientXref.scopeID apiClientXref.triggerId apiClientXref.inactive   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH apiClientXref       WHERE apiClientXref.company  EQ cocode         AND apiClientXref.clientID EQ apiClient.clientID         AND apiClientXref.apiID    EQ cAPIID         AND apiClientXref.scopeID  BEGINS cScopeID         AND (cScopeType EQ "ALL" OR              apiClientXref.scopeType EQ cScopeType)         AND (cTriggerID EQ "ALL" OR              apiClientXref.triggerID EQ cTriggerID)       BY apiClientXref.scopeID       BY apiClientXref.scopeType       BY apiClientXref.triggerID
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH apiClientXref       WHERE apiClientXref.company  EQ cocode         AND apiClientXref.clientID EQ apiClient.clientID         AND apiClientXref.apiID    EQ cAPIID         AND apiClientXref.scopeID  BEGINS cScopeID         AND (cScopeType EQ "ALL" OR              apiClientXref.scopeType EQ cScopeType)         AND (cTriggerID EQ "ALL" OR              apiClientXref.triggerID EQ cTriggerID)       BY apiClientXref.scopeID       BY apiClientXref.scopeType       BY apiClientXref.triggerID       .
&Scoped-define TABLES-IN-QUERY-br_table apiClientXref
&Scoped-define FIRST-TABLE-IN-QUERY-br_table apiClientXref


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbTriggerID fiScopeID cbScopeType btGo ~
RECT-4 br_table 
&Scoped-Define DISPLAYED-OBJECTS cbTriggerID fiScopeID cbScopeType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
clientID||y|ASI.apiClient.clientID
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "clientID"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btGo 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbScopeType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbTriggerID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE fiScopeID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 123 BY 12.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      apiClientXref SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      apiClientXref.scopeType FORMAT "x(32)":U WIDTH 27.2 LABEL-BGCOLOR 14
      apiClientXref.scopeID FORMAT "x(12)":U WIDTH 29.2 LABEL-BGCOLOR 14
      apiClientXref.triggerId FORMAT "x(32)":U WIDTH 42.2
      apiClientXref.inactive COLUMN-LABEL "Status" FORMAT "Inactive/Active":U WIDTH 16.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 122 BY 10.62
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cbTriggerID AT ROW 1.86 COL 58.6 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fiScopeID AT ROW 1.86 COL 30.6 NO-LABEL WIDGET-ID 2
     cbScopeType AT ROW 1.86 COL 2.2 NO-LABEL WIDGET-ID 8
     btGo AT ROW 1.76 COL 104.8 WIDGET-ID 12
     br_table AT ROW 2.95 COL 2
     "Trigger ID" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.14 COL 61 WIDGET-ID 16
          FGCOLOR 1 
     "Scope Type" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.14 COL 2.6 WIDGET-ID 10
          FGCOLOR 1 
     "Scope ID" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.14 COL 31 WIDGET-ID 4
          FGCOLOR 1 FONT 6
     RECT-4 AT ROW 1 COL 1 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.apiClient
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 12.86
         WIDTH              = 123.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br_table RECT-4 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cbScopeType IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiScopeID IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH apiClientXref
      WHERE apiClientXref.company  EQ cocode
        AND apiClientXref.clientID EQ apiClient.clientID
        AND apiClientXref.apiID    EQ cAPIID
        AND apiClientXref.scopeID  BEGINS cScopeID
        AND (cScopeType EQ "ALL" OR
             apiClientXref.scopeType EQ cScopeType)
        AND (cTriggerID EQ "ALL" OR
             apiClientXref.triggerID EQ cTriggerID)
      BY apiClientXref.scopeID
      BY apiClientXref.scopeType
      BY apiClientXref.triggerID
      .
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.apiClientXref.company = ASI.apiClient.company
  AND ASI.apiClientXref.clientID = ASI.apiClient.clientID"
     _Where[1]         = "ASI.apiClientXref.company EQ cocode AND
ASI.apiClientXref.clientID EQ ASI.apiClient.clientID AND

ASI.apiClientXref.apiID EQ cAPIID AND
ASI.apiClientXref.scopeID BEGINS cScopeID AND 
 (cScopeType EQ ""ALL"" OR ASI.apiClientXref.scopeType BEGINS cScopeType) AND
 ASI.apiClientXref.triggerID BEGINS cTriggerID
 BY ASI.apiClientXref.scopeID
 BY ASI.apiClientXref.scopeType
 BY ASI.apiClientXref.triggerID"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo B-table-Win
ON CHOOSE OF btGo IN FRAME F-Main /* Go */
DO:
    RUN dispatch (
        INPUT "open-query":U
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiScopeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeID B-table-Win
ON HELP OF fiScopeID IN FRAME F-Main
DO:
    DEFINE VARIABLE cReturnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE riRecVal      AS RECID     NO-UNDO.
    DEFINE VARIABLE iSubjectID    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustomerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    
    IF cbScopeType:SCREEN-VALUE EQ "" OR cbScopeType:SCREEN-VALUE EQ ? OR
       cbScopeType:SCREEN-VALUE EQ "ALL" OR
       cbScopeType:SCREEN-VALUE EQ cAPIClientXrefAny THEN DO:
        MESSAGE "Please select any of the following scope types in Scope Type field for help:" SKIP
            TRIM(REPLACE(REPLACE(cbScopeType:LIST-ITEMS,cAPIClientXrefAny,""),"ALL",""),",")
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{methods/ctrl-a_browser.i}
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "apiClient"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "apiClient"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAPIAndClientID B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IsAPIClientRecordAvailable B-table-Win 
PROCEDURE IsAPIClientRecordAvailable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplIsAPIClientAvailable AS LOGICAL NO-UNDO.

    DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.    
            
    {methods/run_link.i "RECORD-SOURCE" "IsAPIClientRecordAvailable" "(OUTPUT oplIsAPIClientAvailable)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        cScopeID   = fiScopeID:SCREEN-VALUE
        cScopeType = cbScopeType:SCREEN-VALUE
        cTriggerID = cbTriggerID:SCREEN-VALUE
        .
            
    RUN GetAPIAndClientID (
        OUTPUT cAPIID,
        OUTPUT cClientID
        ).
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pUpdateTriggerList.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit B-table-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScopeTypeList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN Outbound_GetScopeTypeList IN hdOutboundProcs (
        OUTPUT cScopeTypeList    
        ).
    
    cScopeTypeList = "ALL," + cScopeTypeList.
    
    ASSIGN
        cbScopeType:LIST-ITEMS   = cScopeTypeList
        cbScopeType:SCREEN-VALUE = ENTRY(1,cScopeTypeList)
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateTriggerList B-table-Win 
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
        
    ASSIGN
        cbTriggerID:LIST-ITEMS   = "ALL" + "," + "_ANY_" + "," + cTriggerList
        cbTriggerID:SCREEN-VALUE = "ALL"
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "clientID" "apiClient" "clientID"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "apiClient"}
  {src/adm/template/snd-list.i "apiClientXref"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

