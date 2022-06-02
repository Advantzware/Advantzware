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

  File: sharpshooter/d-poInfo.w 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

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
DEFINE INPUT-OUTPUT PARAMETER iopiPOID        AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiPOLine      AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdQuantity    AS DECIMAL   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcQuantityUOM AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplValid        AS LOGICAL   NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btCancel fiPOID fiPOLine fiQuantity fiUOM ~
btOk 
&Scoped-Define DISPLAYED-OBJECTS fiPOID fiPOLine fiQuantity fiUOM ~
statusMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Ok" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btOk AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Ok" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE fiPOID AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "PO #" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1.57 TOOLTIP "Enter PO#"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiPOLine AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Line #" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1.57 TOOLTIP "Enter PO Line#"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiQuantity AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1.57 TOOLTIP "Enter Quantity To Receive"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(8)":U 
     LABEL "UOM" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1.57 TOOLTIP "Enter Quantity UOM"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 96 BY 1.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btCancel AT ROW 6.71 COL 89 WIDGET-ID 12
     fiPOID AT ROW 1.57 COL 26 COLON-ALIGNED WIDGET-ID 2
     fiPOLine AT ROW 3.38 COL 26 COLON-ALIGNED WIDGET-ID 4
     fiQuantity AT ROW 5.19 COL 26 COLON-ALIGNED WIDGET-ID 6
     fiUOM AT ROW 7 COL 26 COLON-ALIGNED WIDGET-ID 8
     btOk AT ROW 6.71 COL 75 WIDGET-ID 10
     statusMessage AT ROW 9.33 COL 2.2 NO-LABEL WIDGET-ID 28
     SPACE(0.79) SKIP(1.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 38
         TITLE "Enter PO Information" WIDGET-ID 100.


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

/* SETTINGS FOR FILL-IN statusMessage IN FRAME D-Dialog
   NO-ENABLE ALIGN-L                                                    */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Enter PO Information */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk D-Dialog
ON CHOOSE OF btOk IN FRAME D-Dialog /* Ok */
DO:
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pValidate (OUTPUT lError, OUTPUT cMessage).
    IF lError THEN DO:
        RUN pStatusMessage(cMessage,INPUT 3).
        RETURN NO-APPLY.
    END.
    
    ASSIGN fiPOID fiPOLine fiQuantity fiUOM.
    
    ASSIGN
        iopiPOID        = fiPOID
        iopiPOLine      = fiPOLine
        iopdQuantity    = fiQuantity
        iopcQuantityUOM = fiUOM
        oplValid        = TRUE
        . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPOID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPOID D-Dialog
ON LEAVE OF fiPOID IN FRAME D-Dialog /* PO # */
DO:
    RUN pPopulateQuantityAndUOM.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPOLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPOLine D-Dialog
ON LEAVE OF fiPOLine IN FRAME D-Dialog /* Line # */
DO:
    RUN pPopulateQuantityAndUOM.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}
{sharpshooter/pStatusMessage.i}

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
  DISPLAY fiPOID fiPOLine fiQuantity fiUOM statusMessage 
      WITH FRAME D-Dialog.
  ENABLE btCancel fiPOID fiPOLine fiQuantity fiUOM btOk 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit D-Dialog 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF iopiPOID EQ ? THEN
        iopiPOID = 0.

    IF iopiPOLine EQ ? THEN
        iopiPOLine = 0.

    IF iopdQuantity EQ ? THEN
        iopdQuantity = 0.

    IF iopcQuantityUOM EQ ? THEN
        iopcQuantityUOM = "".
    
    RUN spGetSessionParam("Company", OUTPUT cCompany).
            
    ASSIGN
        fiPOID:SCREEN-VALUE     = STRING(iopiPOID)
        fiPOLine:SCREEN-VALUE   = STRING(iopiPOLine)
        fiQuantity:SCREEN-VALUE = STRING(iopdQuantity)
        fiUOM:SCREEN-VALUE      = STRING(iopcQuantityUOM)
        . 
    
    RUN pPopulateQuantityAndUOM.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPopulateQuantityAndUOM D-Dialog 
PROCEDURE pPopulateQuantityAndUOM PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN fiPOID fiPOLine.
    
    IF fiPOID EQ 0 OR fiPOID EQ ? OR fiPOLine EQ ? THEN
        RETURN.
                
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ cCompany
           AND bf-po-ordl.po-no   EQ fiPOID
           AND bf-po-ordl.line    EQ fiPOLine
         NO-ERROR.
    IF AVAILABLE bf-po-ordl THEN
        ASSIGN
            fiQuantity:SCREEN-VALUE = STRING(bf-po-ordl.ord-qty)
            fiUOM:SCREEN-VALUE      = bf-po-ordl.pr-qty-uom
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidate D-Dialog 
PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN fiPOID fiPOLine fiQuantity fiUOM.
    
    IF fiPOID EQ 0 THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "PO # cannot be 0"
            .
        
        RETURN. 
    END.

    IF fiPOLine EQ 0 THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "PO Line cannot be 0"
            .
        
        RETURN. 
    END.

    IF fiQuantity EQ 0 THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Quantity cannot be 0"
            .
        
        RETURN. 
    END.
    
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
