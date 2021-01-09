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

  File: sharpshooter/smartobj/qtyUnits.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Mithun Porandla
  Created: 12/21/2020

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
DEFINE VARIABLE iPrevPartialValue AS INTEGER NO-UNDO.
DEFINE VARIABLE iOversPct         AS DECIMAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-36 
&Scoped-Define DISPLAYED-OBJECTS fiQuantityLabel fiQuantity fiSubUnitsLabel ~
fiSubUnits fiSubUnitsPerUnitLabel fiSubUnitsPerUnit ~
fiQuantityPerPalletLabel fiQuantityPerPallet fiPartialLabel fiPartial ~
fiPalletTagsLabel fiPalletTags fiCopiesLabel fiCopies 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiCopies AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiCopiesLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Copies:" 
     VIEW-AS FILL-IN 
     SIZE 11.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiPalletTags AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiPalletTagsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Pallet Tags:" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiPartial AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiPartialLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Partial:" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiQuantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiQuantityLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Quantity:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiQuantityPerPallet AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiQuantityPerPalletLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Quantity Per Pallet:" 
     VIEW-AS FILL-IN 
     SIZE 28.4 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiSubUnits AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiSubUnitsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sub Units:" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiSubUnitsPerUnit AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiSubUnitsPerUnitLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sub Units Per Unit:" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.38 NO-UNDO.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 68.2 BY 10.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiQuantityLabel AT ROW 1.24 COL 16.8 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     fiQuantity AT ROW 1.24 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     fiSubUnitsLabel AT ROW 2.71 COL 15.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiSubUnits AT ROW 2.71 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fiSubUnitsPerUnitLabel AT ROW 4.19 COL 2.8 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fiSubUnitsPerUnit AT ROW 4.19 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     fiQuantityPerPalletLabel AT ROW 5.67 COL 2.4 COLON-ALIGNED NO-LABEL WIDGET-ID 46 DEBLANK  DISABLE-AUTO-ZAP 
     fiQuantityPerPallet AT ROW 5.67 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     fiPartialLabel AT ROW 7.14 COL 19.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fiPartial AT ROW 7.14 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fiPalletTagsLabel AT ROW 8.62 COL 13.4 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiPalletTags AT ROW 8.62 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fiCopiesLabel AT ROW 10.1 COL 19.4 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     fiCopies AT ROW 10.1 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     RECT-36 AT ROW 1 COL 1.2 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 36 WIDGET-ID 100.


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
         HEIGHT             = 10.86
         WIDTH              = 73.2.
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

/* SETTINGS FOR FILL-IN fiCopies IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCopiesLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPalletTags IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPalletTagsLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPartial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPartialLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQuantity IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQuantityLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQuantityPerPallet IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQuantityPerPalletLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSubUnits IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSubUnitsLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSubUnitsPerUnit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSubUnitsPerUnitLabel IN FRAME F-Main
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

&Scoped-define SELF-NAME fiPalletTags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPalletTags s-object
ON LEAVE OF fiPalletTags IN FRAME F-Main
DO:
    DEFINE VARIABLE iSubUnitsPerUnit AS INTEGER NO-UNDO.       
    
    iSubUnitsPerUnit = INTEGER(((INTEGER(fiQuantity:SCREEN-VALUE) + INTEGER(fiPartial:SCREEN-VALUE)) / INTEGER(SELF:SCREEN-VALUE) / INTEGER(fiSubUnits:SCREEN-VALUE))).
    
    RUN pUpdateQuantities (
        INPUT INTEGER(fiQuantity:SCREEN-VALUE),
        INPUT iOversPct,
        INPUT INTEGER(fiSubUnits:SCREEN-VALUE),
        INPUT iSubUnitsPerUnit,
        INPUT 0,
        INPUT INTEGER(fiCopies:SCREEN-VALUE)
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPartial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPartial s-object
ON ENTRY OF fiPartial IN FRAME F-Main
DO:
    iPrevPartialValue = INTEGER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPartial s-object
ON LEAVE OF fiPartial IN FRAME F-Main
DO:    
    IF iPrevPartialValue EQ INTEGER(SELF:SCREEN-VALUE) THEN
        RETURN.
        
    RUN pUpdateQuantities (
        INPUT INTEGER(fiQuantity:SCREEN-VALUE),
        INPUT iOversPct,
        INPUT INTEGER(fiSubUnits:SCREEN-VALUE),
        INPUT INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE),
        INPUT INTEGER(SELF:SCREEN-VALUE) - iPrevPartialValue,
        INPUT INTEGER(fiCopies:SCREEN-VALUE)
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiQuantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQuantity s-object
ON LEAVE OF fiQuantity IN FRAME F-Main
DO:    
    IF INTEGER(SELF:SCREEN-VALUE) EQ 0 OR INTEGER(SELF:SCREEN-VALUE) EQ ? THEN DO:
        MESSAGE "Quantity cannot be " + SELF:SCREEN-VALUE
        VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "1".    
    END.
     
    RUN pUpdateQuantities (
        INPUT INTEGER(SELF:SCREEN-VALUE),
        INPUT iOversPct,
        INPUT INTEGER(fiSubUnits:SCREEN-VALUE),
        INPUT INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE),
        INPUT 0,  /* Partial */
        INPUT INTEGER(fiCopies:SCREEN-VALUE)
        ).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiQuantityPerPallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQuantityPerPallet s-object
ON LEAVE OF fiQuantityPerPallet IN FRAME F-Main
DO:
    RUN pUpdateQuantities (
        INPUT INTEGER(fiQuantity:SCREEN-VALUE),
        INPUT iOversPct,
        INPUT INTEGER(SELF:SCREEN-VALUE),
        INPUT 1,  /* Sub Units Per Unit */
        INPUT 0,  /* Partial */
        INPUT INTEGER(fiCopies:SCREEN-VALUE)
        ).      
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

RUN pInit.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableQuantities s-object 
PROCEDURE DisableQuantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiQuantity:SENSITIVE             = FALSE
        fiQuantityPerPallet:SENSITIVE    = FALSE
        fiPartial:SENSITIVE              = FALSE
        fiPalletTags:SENSITIVE           = FALSE
        fiCopies:SENSITIVE               = FALSE
        fiQuantity:SCREEN-VALUE          = "1"
        fiSubUnits:SCREEN-VALUE          = "1"
        fiSubUnitsPerUnit:SCREEN-VALUE   = "1"
        fiQuantityPerPallet:SCREEN-VALUE = "1"
        fiPartial:SCREEN-VALUE           = "0"
        fiPalletTags:SCREEN-VALUE        = "1"
        fiCopies:SCREEN-VALUE            = "1"          
        .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableQuantities s-object 
PROCEDURE EnableQuantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiQuantity:SENSITIVE          = TRUE
        fiQuantityPerPallet:SENSITIVE = TRUE
        fiPartial:SENSITIVE           = TRUE
        fiPalletTags:SENSITIVE        = TRUE
        fiCopies:SENSITIVE            = TRUE
        .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetQuantities s-object 
PROCEDURE GetQuantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiQuantity          AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSubUnits          AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSubUnitsPerUnit   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityPerPallet AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPartial           AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPalletTags        AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCopies            AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        opiQuantity          = INTEGER(fiQuantity:SCREEN-VALUE)
        opiSubUnits          = INTEGER(fiSubUnits:SCREEN-VALUE)
        opiSubUnitsPerUnit   = INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE)
        opiQuantityPerPallet = INTEGER(fiQuantityPerPallet:SCREEN-VALUE)
        opiPartial           = INTEGER(fiPartial:SCREEN-VALUE)
        opiPalletTags        = INTEGER(fiPalletTags:SCREEN-VALUE)
        opiCopies            = INTEGER(fiCopies:SCREEN-VALUE)
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiQuantityLabel:SCREEN-VALUE          = "Quantity:"
        fiSubUnitsLabel:SCREEN-VALUE          = "Sub Units:"
        fiSubUnitsPerUnitLabel:SCREEN-VALUE   = "Sub Units Per Unit:"
        fiQuantityPerPalletLabel:SCREEN-VALUE = "Quantity Per Pallet:"
        fiPartialLabel:SCREEN-VALUE           = "Partial:"
        fiPalletTagsLabel:SCREEN-VALUE        = "Pallet Tags:"
        fiCopiesLabel:SCREEN-VALUE            = "Copies:"
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateQuantities s-object 
PROCEDURE pUpdateQuantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiBaseQuantity    AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOvers           AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnits        AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPartial         AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies          AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iQuantity          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityPerPallet AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPalletTags        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartial           AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipiBaseQuantity EQ ? THEN
        ipiBaseQuantity = 0.
        
    IF ipiOvers EQ ? THEN
        ipiOvers = 0.
    
    IF ipiCopies EQ ? OR ipiCopies EQ 0 THEN
        ipiCopies = 1.

    ASSIGN
        iQuantity = TRUNCATE(ipiBaseQuantity + (ipiBaseQuantity * ipiOvers / 100 ), 0)
        iQuantity = iQuantity + ipiPartial
        . 
    
    IF ipiSubUnits EQ ? OR ipiSubUnits EQ 0 THEN
        ipiSubUnits = 1.

    IF ipiSubUnitsPerUnit EQ ? OR ipiSubUnitsPerUnit EQ 0 THEN
        ipiSubUnitsPerUnit = 1.

    ASSIGN
        iQuantityPerPallet = ipiSubUnits * ipiSubUnitsPerUnit.
        iPalletTags        = (iQuantity / iQuantityPerPallet) + INTEGER((iQuantity MOD iQuantityPerPallet) GT 0)
        .
    
    iPartial = iQuantity MOD iQuantityPerPallet.
    
    ASSIGN
        fiQuantity:SCREEN-VALUE          = STRING(iQuantity)
        fiSubUnits:SCREEN-VALUE          = STRING(ipiSubUnits)
        fiSubUnitsPerUnit:SCREEN-VALUE   = STRING(ipiSubUnitsPerUnit)
        fiQuantityPerPallet:SCREEN-VALUE = STRING(iQuantityPerPallet)
        fiPartial:SCREEN-VALUE           = STRING(iPartial)
        fiPalletTags:SCREEN-VALUE        = STRING(iPalletTags)
        fiCopies:SCREEN-VALUE            = STRING(ipiCopies)        
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetOversPercent s-object
PROCEDURE SetOversPercent:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiOversPct AS INTEGER NO-UNDO.

    iOversPct = ipiOversPct.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetQuantities s-object
PROCEDURE SetQuantities:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiBaseQuantity    AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOvers           AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnits        AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPartial         AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies          AS INTEGER NO-UNDO.

    RUN pUpdateQuantities(
        INPUT ipiBaseQuantity,
        INPUT ipiOvers,
        INPUT ipiSubUnits,
        INPUT ipiSubUnitsPerUnit,
        INPUT ipiPartial,
        INPUT ipiCopies
        ).
    
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

