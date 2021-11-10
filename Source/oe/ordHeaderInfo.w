&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: oe/ordHeaderInfo

  Description: 

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

/* Local Variable Definitions ---                                       */



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCustNo fiShipTo fiCustPo fiEstNo 
&Scoped-Define DISPLAYED-OBJECTS fiCustNo fiShipToLabel fiShipTo ~
fiCustPoLabel fiCustPo fiEstLabel fiEstNo fiCustNoLabel

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiCustNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.48
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiCustNoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Customer Id:" 
     VIEW-AS FILL-IN 
     SIZE 23.5 BY 1.48 NO-UNDO.

DEFINE VARIABLE fiCustPo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.48
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiCustPoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Customer PO:" 
     VIEW-AS FILL-IN 
     SIZE 25.5 BY 1.48 NO-UNDO.

DEFINE VARIABLE fiEstLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Estimate:" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.48 NO-UNDO.

DEFINE VARIABLE fiEstNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.48
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiShipTo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.48
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiShipToLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Ship To:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.48 NO-UNDO.
        
/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiEstLabel AT ROW 1.29 COL 2 NO-LABEL WIDGET-ID 2
     fiEstNo AT ROW 1.29 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiCustNoLabel AT ROW 1.29 COL 45.6 NO-LABEL WIDGET-ID 16
     fiCustNo AT ROW 1.29 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 186
     fiShipToLabel AT ROW 1.29 COL 105 NO-LABEL WIDGET-ID 20
     fiShipTo AT ROW 1.29 COL 120 COLON-ALIGNED NO-LABEL WIDGET-ID 188
     fiCustPoLabel AT ROW 1.29 COL 155 NO-LABEL WIDGET-ID 184
     fiCustPo AT ROW 1.24 COL 180 COLON-ALIGNED NO-LABEL WIDGET-ID 190
     
     
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 2.05
         WIDTH              = 182.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiCustNoLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCustPoLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiEstLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiShipToLabel IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCustNo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiCustPo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiEst IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiShipTo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */   
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

&Scoped-define SELF-NAME fiCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustNo s-object
ON ANY-KEY OF fiCustNo IN FRAME F-Main
DO:
    IF KEYLABEL(LASTKEY) EQ "ENTER" THEN
        APPLY 'LEAVE' TO SELF. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

 RUN DisableAll.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableAll s-object 
PROCEDURE DisableAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        DISABLE fiCustNo fiShipTo fiCustPo fiEstNo.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFillValue s-object 
PROCEDURE pFillValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcEstNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcShipTo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustPo AS CHARACTER NO-UNDO.    
    
    DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN
        fiCustNo:SCREEN-VALUE = ipcCustNo
        fiShipTo:SCREEN-VALUE = ipcShipTo
        fiCustPo:SCREEN-VALUE = ipcCustPo
        fiEstNo:SCREEN-VALUE  = ipcEstNo.
        DISPLAY fiShipToLabel fiCustPoLabel fiEstLabel fiCustNoLabel  .
    END.
    
    RUN DisableAll.
    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE No-Resize s-object 
PROCEDURE No-Resize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



