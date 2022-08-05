&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: est/dAdditionalCost.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: DEVA$!

  Created: 
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
DEFINE INPUT  PARAMETER ipriEstimate AS ROWID NO-UNDO.

DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceType    AS CHARACTER NO-UNDO INITIAL "estMisc".
DEFINE VARIABLE hdEstMiscProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE iFormNo AS INTEGER NO-UNDO.

RUN est/estMiscProcs.p PERSISTENT SET hdEstMiscProcs.
/* Local Variable Definitions ---                                       */

DEFINE BUFFER bf-eb FOR eb.

FIND FIRST bf-eb NO-LOCK
     WHERE ROWID(bf-eb) EQ ipriEstimate
     NO-ERROR.
IF AVAILABLE bf-eb THEN
    ASSIGN
        cCompany    = bf-eb.company
        cEstimateNo = bf-eb.est-no
        iFormNo     = bf-eb.form-no
        .

RELEASE bf-eb.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btReset 
&Scoped-Define DISPLAYED-OBJECTS fiEstimateNo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_estmisc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_estmisc-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btReset 
     LABEL "Reset Cost From Misc Control" 
     SIZE 37 BY 1.62
     FONT 6.

DEFINE VARIABLE fiEstimateNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 26.33 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fiEstimateNo AT ROW 1.46 COL 13.67 COLON-ALIGNED WIDGET-ID 2
     btReset AT ROW 16.23 COL 133 WIDGET-ID 4
     SPACE(17.39) SKIP(4.81)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Additional Costs" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiEstimateNo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Additional Costs */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  IF VALID-HANDLE(hdEstMiscProcs) THEN
        DELETE PROCEDURE hdEstMiscProcs.
        
    APPLY "END-ERROR":U TO SELF.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReset D-Dialog
ON CHOOSE OF btReset IN FRAME D-Dialog /* Reset Cost From Misc Control */
DO:
    DEFINE BUFFER bf-estMisc        FOR estMisc.
    DEFINE BUFFER bf-estMiscControl FOR estMiscControl.
    
    IF cEstimateNo EQ "" THEN
        RETURN.
        
    MESSAGE "Are you sure you want to reset? This will delete the costs configuration for the estimate and reset to default costs"  
    VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
    
    IF NOT lChoice THEN
        RETURN.
    
    RUN EstMisc_ResetEstMiscForEstimate IN hdEstMiscProcs (cCompany, cEstimateNo).
    
    RUN dispatch IN h_estmisc ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
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
             INPUT  'browsers/estmisc.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_estmisc ).
       RUN set-position IN h_estmisc ( 2.91 , 3.00 ) NO-ERROR.
       RUN set-size IN h_estmisc ( 19.29 , 113.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/estmisc.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_estmisc-2 ).
       RUN set-position IN h_estmisc-2 ( 2.95 , 117.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.95 , 68.00 ) */

       /* Links to SmartViewer h_estmisc-2. */
       RUN add-link IN adm-broker-hdl ( h_estmisc , 'Record':U , h_estmisc-2 ).
       
       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_estmisc ,
             fiEstimateNo:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_estmisc-2 ,
             h_estmisc , 'AFTER':U ).        
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fiEstimateNo 
      WITH FRAME D-Dialog.
  ENABLE btReset 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetEstimateNo D-Dialog 
PROCEDURE GetEstimateNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFormNo AS INTEGER NO-UNDO.
    
    opcEstimateNo = cEstimateNo.
    opiFormNo = iFormNo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit D-Dialog 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiEstimateNo:SCREEN-VALUE = cEstimateNo
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetEstMiscProcsHandle W-Win 
PROCEDURE GetEstMiscProcsHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ophdEstMiscProcs AS HANDLE NO-UNDO.
    
    ophdEstMiscProcs = hdEstMiscProcs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSourceType W-Win 
PROCEDURE GetSourceType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETE opcSourceType AS CHARACTER NO-UNDO.
    
    opcSourceType = cSourceType.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

