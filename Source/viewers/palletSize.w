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

  File: viewers/palletSize.w

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
&Scoped-define EXTERNAL-TABLES storageCost palletSize
&Scoped-define FIRST-EXTERNAL-TABLE storageCost


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR storageCost, palletSize.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS palletSize.upToWidth palletSize.upToLength 
&Scoped-define ENABLED-TABLES palletSize
&Scoped-define FIRST-ENABLED-TABLE palletSize
&Scoped-Define DISPLAYED-FIELDS palletSize.positions palletSize.upToWidth ~
palletSize.upToLength 
&Scoped-define DISPLAYED-TABLES palletSize
&Scoped-define FIRST-DISPLAYED-TABLE palletSize
&Scoped-Define DISPLAYED-OBJECTS fiWidthValidRange fiLengthValidRange 

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
DEFINE VARIABLE fiLengthValidRange AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valid Range" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiWidthValidRange AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valid Range" 
     VIEW-AS FILL-IN 
     SIZE 19.4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     palletSize.positions AT ROW 1.24 COL 16 COLON-ALIGNED WIDGET-ID 2 FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     palletSize.upToWidth AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 6 FORMAT ">>>>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     fiWidthValidRange AT ROW 2.43 COL 42.6 COLON-ALIGNED WIDGET-ID 8
     palletSize.upToLength AT ROW 3.62 COL 16 COLON-ALIGNED WIDGET-ID 4 FORMAT ">>>>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     fiLengthValidRange AT ROW 3.62 COL 42.6 COLON-ALIGNED WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.storageCost,ASI.palletSize
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
         HEIGHT             = 6.86
         WIDTH              = 66.
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

/* SETTINGS FOR FILL-IN fiLengthValidRange IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiLengthValidRange:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiWidthValidRange IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiWidthValidRange:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN palletSize.positions IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN palletSize.upToLength IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN palletSize.upToWidth IN FRAME F-Main
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
  {src/adm/template/row-list.i "storageCost"}
  {src/adm/template/row-list.i "palletSize"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "storageCost"}
  {src/adm/template/row-find.i "palletSize"}

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
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT AVAILABLE storageCost THEN DO:
        MESSAGE "No storage cost exists. Please enter storage cost and try again"
            VIEW-AS ALERT-BOX ERROR.    
        RETURN.
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
   
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE storageCost THEN
        palletSize.positions:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(storageCost.positions).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE storageCost THEN
        ASSIGN
            palletSize.company   = storageCost.company
            palletSize.location  = storageCost.location
            palletSize.positions = storageCost.positions
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT adm-new-record THEN DO:
        {custom/askdel.i}    
    END.
    
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
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        fiLengthValidRange:VISIBLE = FALSE
        fiWidthValidRange:VISIBLE  = FALSE
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
    /* Code placed here will execute PRIOR to standard behavior. */
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
            
    /* Code placed here will execute AFTER standard behavior.    */
    IF NOT AVAILABLE palletSize THEN
        ASSIGN
            palletSize.positions:SCREEN-VALUE  = "0"
            palletSize.upToWidth:SCREEN-VALUE  = "0.00"
            palletSize.upToLength:SCREEN-VALUE = "0.00"
            .    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dValidWidthRangeBegin  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidWidthRangeEnd    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeBegin AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeEnd   AS DECIMAL NO-UNDO.

    DEFINE VARIABLE cValidRange       AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-palletSize FOR palletSize.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN
        fiLengthValidRange:VISIBLE      = TRUE
        fiWidthValidRange:VISIBLE       = TRUE
        fiLengthValidRange:SCREEN-VALUE = ""
        fiWidthValidRange:SCREEN-VALUE  = ""
        .

    RUN pGetValidRange(OUTPUT dValidWidthRangeBegin,OUTPUT dValidWidthRangeEnd,OUTPUT dValidLengthRangeBegin,OUTPUT dValidLengthRangeEnd).
    
    cValidRange = STRING(dValidWidthRangeBegin) + " - " + STRING(dValidWidthRangeEnd).
                      
    fiWidthValidRange:SCREEN-VALUE = cValidRange.

    cValidRange = STRING(dValidLengthRangeBegin) + " - " + STRING(dValidLengthRangeEnd).
                      
    ASSIGN
        fiLengthValidRange:SCREEN-VALUE = cValidRange
        .
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dValidWidthRangeBegin  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidWidthRangeEnd    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeBegin AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValidLengthRangeEnd   AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-palletSize FOR palletSize.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pGetValidRange(OUTPUT dValidWidthRangeBegin,OUTPUT dValidWidthRangeEnd,OUTPUT dValidLengthRangeBegin,OUTPUT dValidLengthRangeEnd).
    
    IF DECIMAL (palletSize.upToWidth:SCREEN-VALUE) GT dValidWidthRangeEnd OR DECIMAL (palletSize.upToLength:SCREEN-VALUE) LT dValidWidthRangeBegin OR
        DECIMAL (palletSize.upToLength:SCREEN-VALUE) GT dValidLengthRangeEnd OR DECIMAL (palletSize.upToLength:SCREEN-VALUE) LT dValidLengthRangeBegin THEN DO:
        MESSAGE "Pallet size conflicts with another record. Please enter a different dimensions"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    IF DECIMAL(palletSize.upToWidth:SCREEN-VALUE) EQ 0 THEN
         palletSize.upToWidth:SCREEN-VALUE = "9999.99".

    IF DECIMAL(palletSize.upToLength:SCREEN-VALUE) EQ 0 THEN
         palletSize.upToLength:SCREEN-VALUE = "9999.99".
         
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetValidRange V-table-Win
PROCEDURE pGetValidRange PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opdValidWidthRangeBegin  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValidWidthRangeEnd    AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValidLengthRangeBegin AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValidLengthRangeEnd   AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-palletSize FOR palletSize.
        
    FOR EACH bf-palletSize NO-LOCK
        WHERE bf-palletSize.company    EQ storageCost.company
          AND bf-palletSize.location   EQ storageCost.location
          AND bf-palletSize.positions  LE storageCost.positions
          USE-INDEX companyLoc
          BY bf-palletSize.positions
          BY bf-palletSize.upToWidth:
        IF NOT adm-new-record AND ROWID(bf-palletSize) EQ ROWID(palletSize) THEN
            NEXT.
                  
        opdValidWidthRangeBegin = bf-palletSize.upToWidth + 0.01.
        IF bf-palletSize.upToWidth GE 999999.99 THEN
            opdValidWidthRangeBegin = 999999.99.
    END.
    
    IF opdValidWidthRangeBegin EQ 0 THEN
        opdValidWidthRangeBegin = 1.00.
        
    FIND FIRST bf-palletSize NO-LOCK
         WHERE bf-palletSize.company    EQ storageCost.company
           AND bf-palletSize.location   EQ storageCost.location
           AND bf-palletSize.positions  GT storageCost.positions
         NO-ERROR.
    IF AVAILABLE bf-palletSize THEN
        opdValidWidthRangeEnd = bf-palletSize.upToWidth - 0.01.
    ELSE
        opdValidWidthRangeEnd = 9999.99.
        
    FOR EACH bf-palletSize NO-LOCK
        WHERE bf-palletSize.company    EQ storageCost.company
          AND bf-palletSize.location   EQ storageCost.location
          AND bf-palletSize.positions  LE storageCost.positions
          USE-INDEX companyLoc
          BY bf-palletSize.positions
          BY bf-palletSize.upToLength:
        IF NOT adm-new-record AND ROWID(bf-palletSize) EQ ROWID(palletSize) THEN
            NEXT.
              
        opdValidLengthRangeBegin = bf-palletSize.upToLength + 0.01.
        IF bf-palletSize.upToLength GE 999999.99 THEN
            opdValidLengthRangeBegin = 999999.99.        
    END.

    IF opdValidLengthRangeBegin EQ 0 THEN
        opdValidLengthRangeBegin = 1.00.
                
    FIND FIRST bf-palletSize NO-LOCK
         WHERE bf-palletSize.company    EQ storageCost.company
           AND bf-palletSize.location   EQ storageCost.location
           AND bf-palletSize.positions  GT storageCost.positions
         NO-ERROR.
    IF AVAILABLE bf-palletSize THEN
        opdValidLengthRangeEnd = bf-palletSize.upToLength - 0.01.
    ELSE
        opdValidLengthRangeEnd = 9999.99.

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
  {src/adm/template/snd-list.i "storageCost"}
  {src/adm/template/snd-list.i "palletSize"}

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

