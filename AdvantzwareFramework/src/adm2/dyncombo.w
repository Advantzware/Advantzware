&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: dyncombo.w 

  Description: from FIELD.W - Template for ADM2 SmartDataField combo object

  Created: ICF Object - August 2001 -- Progress Version 9.1C
           Uses new ICF combo class

  Modified:  Moved property sheet to ry/uib.
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

&IF "{&xcInstanceProperties}":U NE "":U &THEN
    &GLOB xcInstanceProperties {&xcInstanceProperties},
  &ENDIF

  &IF "{&xcTranslatableProperties}":U NE "":U &THEN
    &GLOB xcTranslatableProperties {&xcTranslatableProperties},
  &ENDIF
  &GLOB xcTranslatableProperties {&xcTranslatableProperties}~
SelectionLabel,OptionalString

&IF DEFINED (ADM-PROPERTY-DLG) = 0 &THEN
  &SCOP ADM-PROPERTY-DLG adeuib/_dynamiccombod.w
&ENDIF
/* tell smart.i that we can use the default destroyObject */ 
&SCOPED-DEFINE include-destroyobject
{src/adm2/ttdcombo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataField
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME frCombo

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frCombo
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50 BY 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataField
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
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 1
         WIDTH              = 50.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm2/combo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frCombo
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME frCombo:SCROLLABLE       = FALSE
       FRAME frCombo:HIDDEN           = TRUE
       FRAME frCombo:SELECTABLE       = TRUE
       FRAME frCombo:MOVABLE          = TRUE
       FRAME frCombo:RESIZABLE        = TRUE
       FRAME frCombo:PRIVATE-DATA     = 
                "nolookups".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frCombo
/* Query rebuild information for FRAME frCombo
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME frCombo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME frCombo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frCombo sObject
ON SELECTION OF FRAME frCombo
DO:
  DEFINE VARIABLE cMode AS CHARACTER  NO-UNDO.
  
  {get UIBMode cMode}.
  IF cMode = "":U THEN /* Run Time */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frCombo sObject
ON START-MOVE OF FRAME frCombo
DO:
  DEFINE VARIABLE cMode AS CHARACTER  NO-UNDO.
  
  {get UIBMode cMode}.
  IF cMode = "":U THEN /* Run Time */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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
  HIDE FRAME frCombo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamicCombo sObject 
PROCEDURE dynamicCombo :
/*------------------------------------------------------------------------------
  Purpose:     This procedure is being checked for by adm2/viewer.p.
               The reason for adding this procedure is to allow viewer.p to 
               scan all the SDF procedure and not run {set DataValue} for 
               dynamic combos since it will result in the combo flashing in the
               viewer. The displayCombo procedure thakes care of assigning the
               correct value of the data field in the combo.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* DO NOT REMOVE THIS PROCEDURE */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject sObject 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose: Override of initializeObject in order to subscribe to 
           the ChangedEvent if defined and initializeSelection      
  Parameters:  <none>
  Notes:   This is in here rather than the super procedure as when put
           in lookup.p it did not work and the target-procedure in the
           subscribed procedures was wrong.
---------------------------------------------------------------------------*/

  DEFINE VARIABLE cUIBMode              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hContainer            AS HANDLE     NO-UNDO.

  {get ContainerSource hContainer}. /* SDV */    
  {get UIBMode cUIBMode}.    
  
  /* subscribe ib containing viewer to events that will populate the combo */
  IF NOT (cUIBMode BEGINS "DESIGN":U)  THEN
  DO:
    SUBSCRIBE TO "getComboQuery":U IN hContainer.  /* add to lookup tt */
    SUBSCRIBE TO "displayCombo":U IN hContainer.   /* display lookup fields */
  END.

  RUN SUPER.
 
  RUN initializeCombo IN TARGET-PROCEDURE.
  
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

