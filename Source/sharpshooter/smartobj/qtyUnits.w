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
DEFINE VARIABLE lIsQuantityVisible             AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsQuantityOfUnitsVisible      AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsQuantityInSubUnitVisible    AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsQuantityOfSubUnitsVisible   AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsSubUnitsPerUnitVisible      AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsPartialVisible              AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsTotalTagsVisible            AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsFullTagsVisible             AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsPartialTagsVisible          AS LOGICAL NO-UNDO INITIAL TRUE.

DEFINE VARIABLE lIsQuantitySensitive           AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsQuantityOfUnitsSensitive    AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lIsQuantityInSubUnitSensitive  AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsQuantityOfSubUnitsSensitive AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lIsSubUnitsPerUnitSensitive    AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE lIsPartialSensitive            AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lIsTotalTagsSensitive          AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lIsFullTagsSensitive           AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lIsPartialTagsSensitive        AS LOGICAL NO-UNDO INITIAL FALSE.

DEFINE VARIABLE dOversPct AS DECIMAL NO-UNDO.

DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rQuantitiesRectangle rUnitCountsRectangle 
&Scoped-Define DISPLAYED-OBJECTS fiUnitCountsRectangleLabel ~
fiQuantitiesRectangleLabel fiQuantity fiQuantityOfSubUnits ~
fiQuantityInSubUnit fiTotalTags fiSubUnitsPerUnit fiFullTags fiPartial ~
fiPartialTags fiQuantityOfUnits 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiFullTags AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Full Tags" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total number of full loadtags" NO-UNDO.

DEFINE VARIABLE fiPartial AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Partail Quantity" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 NO-UNDO.

DEFINE VARIABLE fiPartialTags AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Partial Tags" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total num ber of partial tags" NO-UNDO.

DEFINE VARIABLE fiQuantitiesRectangleLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Quantities" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiQuantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total Quantity" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total Quantity" NO-UNDO.

DEFINE VARIABLE fiQuantityInSubUnit AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Per Case" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Quantity in a single case" NO-UNDO.

DEFINE VARIABLE fiQuantityOfSubUnits AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cases" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total number of cases for the quantity" NO-UNDO.

DEFINE VARIABLE fiQuantityOfUnits AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total number of Pallets" NO-UNDO.

DEFINE VARIABLE fiSubUnitsPerUnit AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cases Per Pallet" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total number of cases in a pallet" NO-UNDO.

DEFINE VARIABLE fiTotalTags AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Tags" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38 TOOLTIP "Total number of tags including partial tags" NO-UNDO.

DEFINE VARIABLE fiUnitCountsRectangleLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Unit Counts" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY .95
     FONT 6 NO-UNDO.

DEFINE RECTANGLE rQuantitiesRectangle
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 66 BY 8.81.

DEFINE RECTANGLE rUnitCountsRectangle
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 63.4 BY 8.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiUnitCountsRectangleLabel AT ROW 1 COL 69.6 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     fiQuantitiesRectangleLabel AT ROW 1.05 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     fiQuantity AT ROW 1.86 COL 31.2 COLON-ALIGNED WIDGET-ID 26
     fiQuantityOfSubUnits AT ROW 1.86 COL 96.2 COLON-ALIGNED WIDGET-ID 32
     fiQuantityInSubUnit AT ROW 3.33 COL 31.2 COLON-ALIGNED WIDGET-ID 60
     fiTotalTags AT ROW 3.38 COL 96.2 COLON-ALIGNED WIDGET-ID 52
     fiSubUnitsPerUnit AT ROW 4.81 COL 31.2 COLON-ALIGNED WIDGET-ID 38
     fiFullTags AT ROW 4.91 COL 96.2 COLON-ALIGNED WIDGET-ID 68
     fiPartial AT ROW 6.33 COL 31.2 COLON-ALIGNED WIDGET-ID 48
     fiPartialTags AT ROW 6.38 COL 96.2 COLON-ALIGNED WIDGET-ID 72
     fiQuantityOfUnits AT ROW 7.86 COL 31 COLON-ALIGNED WIDGET-ID 64
     rQuantitiesRectangle AT ROW 1.48 COL 1.2 WIDGET-ID 74
     rUnitCountsRectangle AT ROW 1.48 COL 69 WIDGET-ID 80
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
         HEIGHT             = 9.29
         WIDTH              = 132.
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

/* SETTINGS FOR FILL-IN fiFullTags IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiFullTags:PRIVATE-DATA IN FRAME F-Main     = 
                "FullTags".

/* SETTINGS FOR FILL-IN fiPartial IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiPartial:PRIVATE-DATA IN FRAME F-Main     = 
                "Partial".

/* SETTINGS FOR FILL-IN fiPartialTags IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiPartialTags:PRIVATE-DATA IN FRAME F-Main     = 
                "PartialTags".

/* SETTINGS FOR FILL-IN fiQuantitiesRectangleLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiQuantitiesRectangleLabel:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantitiesRectangleLabel".

/* SETTINGS FOR FILL-IN fiQuantity IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiQuantity:PRIVATE-DATA IN FRAME F-Main     = 
                "Quantity".

/* SETTINGS FOR FILL-IN fiQuantityInSubUnit IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiQuantityInSubUnit:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantityInSubUnit".

/* SETTINGS FOR FILL-IN fiQuantityOfSubUnits IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiQuantityOfSubUnits:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantityOfSubUnits".

/* SETTINGS FOR FILL-IN fiQuantityOfUnits IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiQuantityOfUnits:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantityOfUnits".

/* SETTINGS FOR FILL-IN fiSubUnitsPerUnit IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiSubUnitsPerUnit:PRIVATE-DATA IN FRAME F-Main     = 
                "SubUnitsPerUnit".

/* SETTINGS FOR FILL-IN fiTotalTags IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiTotalTags:PRIVATE-DATA IN FRAME F-Main     = 
                "TotalTags".

/* SETTINGS FOR FILL-IN fiUnitCountsRectangleLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiUnitCountsRectangleLabel:PRIVATE-DATA IN FRAME F-Main     = 
                "UnitCountsRectangleLabel".

ASSIGN 
       rQuantitiesRectangle:PRIVATE-DATA IN FRAME F-Main     = 
                "QuantitiesRectangle".

ASSIGN 
       rUnitCountsRectangle:PRIVATE-DATA IN FRAME F-Main     = 
                "UnitCountsRectangle".

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

&Scoped-define SELF-NAME fiQuantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQuantity s-object
ON LEAVE OF fiQuantity IN FRAME F-Main /* Total Quantity */
DO:  
    IF INTEGER(SELF:SCREEN-VALUE) EQ 0 OR INTEGER(SELF:SCREEN-VALUE) EQ ? THEN DO:
        MESSAGE "Quantity cannot be " + SELF:SCREEN-VALUE
        VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "1".    
    END.
         
    RUN pUpdateQuantities (
        INPUT INTEGER(fiQuantity:SCREEN-VALUE),
        INPUT INTEGER(fiQuantityInSubUnit:SCREEN-VALUE),
        INPUT INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE),
        INPUT dOversPct
        ).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiQuantityInSubUnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiQuantityInSubUnit s-object
ON LEAVE OF fiQuantityInSubUnit IN FRAME F-Main /* Per Case */
DO:  
    IF INTEGER(SELF:SCREEN-VALUE) EQ 0 OR INTEGER(SELF:SCREEN-VALUE) EQ ? THEN DO:
        MESSAGE "Value cannot be " + SELF:SCREEN-VALUE
        VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "1".    
    END.
         
    RUN pUpdateQuantities (
        INPUT INTEGER(fiQuantity:SCREEN-VALUE),
        INPUT INTEGER(fiQuantityInSubUnit:SCREEN-VALUE),
        INPUT INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE),
        INPUT dOversPct
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSubUnitsPerUnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubUnitsPerUnit s-object
ON LEAVE OF fiSubUnitsPerUnit IN FRAME F-Main /* Cases Per Pallet */
DO:
    IF INTEGER(SELF:SCREEN-VALUE) EQ 0 OR INTEGER(SELF:SCREEN-VALUE) EQ ? THEN DO:
        MESSAGE "Value cannot be " + SELF:SCREEN-VALUE
        VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "1".    
    END.
         
    RUN pUpdateQuantities (
        INPUT INTEGER(fiQuantity:SCREEN-VALUE),
        INPUT INTEGER(fiQuantityInSubUnit:SCREEN-VALUE),
        INPUT INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE),
        INPUT dOversPct
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
        fiQuantity:SENSITIVE           = FALSE
        fiQuantityOfUnits:SENSITIVE    = FALSE
        fiQuantityInSubUnit:SENSITIVE  = FALSE
        fiQuantityOfSubUnits:SENSITIVE = FALSE
        fiSubUnitsPerUnit:SENSITIVE    = FALSE
        fiPartial:SENSITIVE            = FALSE
        fiTotalTags:SENSITIVE          = FALSE
        fiFullTags:SENSITIVE           = FALSE
        fiPartialTags:SENSITIVE        = FALSE
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
        fiQuantity:SENSITIVE           = lIsQuantitySensitive
        fiQuantityOfUnits:SENSITIVE    = lIsQuantityOfUnitsSensitive
        fiQuantityInSubUnit:SENSITIVE  = lIsQuantityInSubUnitSensitive
        fiQuantityOfSubUnits:SENSITIVE = lIsQuantityOfSubUnitsSensitive
        fiSubUnitsPerUnit:SENSITIVE    = lIsSubUnitsPerUnitSensitive
        fiPartial:SENSITIVE            = lIsPartialSensitive
        fiTotalTags:SENSITIVE          = lIsTotalTagsSensitive
        fiFullTags:SENSITIVE           = lIsFullTagsSensitive
        fiPartialTags:SENSITIVE        = lIsPartialTagsSensitive
        fiQuantity:HIDDEN              = lIsQuantityVisible
        fiQuantityOfUnits:HIDDEN       = lIsQuantityOfUnitsVisible
        fiQuantityInSubUnit:HIDDEN     = lIsQuantityInSubUnitVisible
        fiQuantityOfSubUnits:HIDDEN    = lIsQuantityOfSubUnitsVisible
        fiSubUnitsPerUnit:HIDDEN       = lIsSubUnitsPerUnitVisible
        fiPartial:HIDDEN               = lIsPartialVisible
        fiTotalTags:HIDDEN             = lIsTotalTagsVisible
        fiFullTags:HIDDEN              = lIsFullTagsVisible
        fiPartialTags:HIDDEN           = lIsPartialTagsVisible
        .  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQuantities s-object 
PROCEDURE GetQuantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiQuantity          AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSubUnitsPerUnit   AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        opiQuantity          = INTEGER(fiQuantity:SCREEN-VALUE)
        opiQuantityInSubUnit = INTEGER(fiQuantityInSubUnit:SCREEN-VALUE)
        opiSubUnitsPerUnit   = INTEGER(fiSubUnitsPerUnit:SCREEN-VALUE)
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable s-object 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE oSSLoadTagJobDesignConfig AS system.Config NO-UNDO.
     
    DEFINE VARIABLE hdWidget AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    {methods/run_link.i "CONTAINER-SOURCE" "GetDesignConfig" "(OUTPUT oSSLoadTagJobDesignConfig)"}
    
    ASSIGN
        fiUnitCountsRectangleLabel:SCREEN-VALUE = "Units Count"
        fiQuantitiesRectangleLabel:SCREEN-VALUE = "Quantities"
        .

    IF VALID-OBJECT(oSSLoadTagJobDesignConfig) THEN DO:
        hdWidget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.
        DO WHILE VALID-HANDLE(hdWidget):
            IF hdWidget:PRIVATE-DATA NE "" AND hdWidget:PRIVATE-DATA NE ? THEN DO:

                IF oSSLoadTagJobDesignConfig:IsAttributeAvailable("Quantities", hdWidget:PRIVATE-DATA, "visible") THEN
                    hdWidget:HIDDEN = NOT LOGICAL(oSSLoadTagJobDesignConfig:GetAttributeValue("Quantities", hdWidget:PRIVATE-DATA, "visible")).

                IF oSSLoadTagJobDesignConfig:IsAttributeAvailable("Quantities", hdWidget:PRIVATE-DATA, "label") THEN
                    hdWidget:LABEL = oSSLoadTagJobDesignConfig:GetAttributeValue("Quantities", hdWidget:PRIVATE-DATA, "label").

                IF oSSLoadTagJobDesignConfig:IsAttributeAvailable("Quantities", hdWidget:PRIVATE-DATA, "sensitive") THEN
                    hdWidget:SENSITIVE = LOGICAL(oSSLoadTagJobDesignConfig:GetAttributeValue("Quantities", hdWidget:PRIVATE-DATA, "sensitive")).
                
                CASE hdWidget:PRIVATE-DATA:
                    WHEN "Quantity" THEN 
                        ASSIGN 
                            lIsQuantityVisible   = hdWidget:HIDDEN
                            lIsQuantitySensitive = hdWidget:SENSITIVE
                            .
                    WHEN "QuantityOfUnits" THEN 
                        ASSIGN 
                            lIsQuantityOfUnitsVisible   = hdWidget:HIDDEN
                            lIsQuantityOfUnitsSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "QuantityInSubUnit" THEN 
                        ASSIGN 
                            lIsQuantityInSubUnitVisible   = hdWidget:HIDDEN
                            lIsQuantityInSubUnitSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "QuantityOfSubUnits" THEN 
                        ASSIGN 
                            lIsQuantityOfSubUnitsVisible   = hdWidget:HIDDEN
                            lIsQuantityOfSubUnitsSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "SubUnitsPerUnit" THEN 
                        ASSIGN 
                            lIsSubUnitsPerUnitVisible   = hdWidget:HIDDEN
                            lIsSubUnitsPerUnitSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "Partial" THEN 
                        ASSIGN 
                            lIsPartialVisible   = hdWidget:HIDDEN
                            lIsPartialSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "TotalTags" THEN 
                        ASSIGN 
                            lIsTotalTagsVisible   = hdWidget:HIDDEN
                            lIsTotalTagsSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "FullTags" THEN 
                        ASSIGN 
                            lIsFullTagsVisible   = hdWidget:HIDDEN
                            lIsFullTagsSensitive = hdWidget:SENSITIVE
                            .
                    WHEN "PartialTags" THEN 
                        ASSIGN 
                            lIsPartialTagsVisible   = hdWidget:HIDDEN
                            lIsPartialTagsSensitive = hdWidget:SENSITIVE
                            .
                END CASE.
            END.

            hdWidget = hdWidget:NEXT-SIBLING.
        END.
    END.
            
    /* Sets the visiblilty */
    RUN EnableQuantities.
    
    /* Sets the sensitivity to default */
    RUN DisableQuantities.
    
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
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdOvers             AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInUnit     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartial            AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotalTags          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iFullTags           AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartialTags        AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipiQuantity EQ ? OR ipiQuantity EQ 0 THEN
        ipiQuantity = 1.
        
    IF ipiQuantityInSubUnit EQ ? OR ipiQuantityInSubUnit EQ 0 THEN
        ipiQuantityInSubUnit = 1.

    IF ipiSubUnitsPerUnit EQ ? OR ipiSubUnitsPerUnit EQ 0 THEN
        ipiSubUnitsPerUnit = 1.
        
    IF ipdOvers EQ ? THEN
        ipdOvers = 0.

    ipiQuantity = TRUNCATE(ipiQuantity + (ipiQuantity * ipdOvers / 100 ), 0).

    ASSIGN
        iQuantityofSubUnits = TRUNCATE(ipiQuantity / ipiQuantityInSubUnit, 0)
        iQuantityInUnit     = ipiQuantityInSubUnit * ipiSubUnitsPerUnit
        iTotalTags          = TRUNCATE(ipiQuantity / iQuantityInUnit, 0) + INTEGER(NOT (ipiQuantity MOD iQuantityInUnit EQ 0))
        iFullTags           = TRUNCATE(ipiQuantity / iQuantityInUnit, 0)
        iPartialTags        = iTotalTags - iFullTags
        iPartial            = ipiQuantity - (iFullTags * iQuantityInUnit)
        .
    
    ASSIGN
        fiQuantity:SCREEN-VALUE           = STRING(ipiQuantity)
        fiQuantityInSubUnit:SCREEN-VALUE  = STRING(ipiQuantityInSubUnit)
        fiSubUnitsPerUnit:SCREEN-VALUE    = STRING(ipiSubUnitsPerUnit)
        fiQuantityOfSubUnits:SCREEN-VALUE = STRING(iQuantityofSubUnits)
        fiQuantityOfUnits:SCREEN-VALUE    = STRING(iQuantityInUnit)
        fiPartial:SCREEN-VALUE            = STRING(iPartial)
        fiTotalTags:SCREEN-VALUE          = STRING(iTotalTags)
        fiFullTags:SCREEN-VALUE           = STRING(iFullTags)
        fiPartialTags:SCREEN-VALUE        = STRING(iPartialTags)       
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetOvers s-object 
PROCEDURE SetOvers :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdOversPct AS DECIMAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    dOversPct = ipdOversPct.
    
    APPLY "LEAVE" TO fiQuantity.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetQuantities s-object 
PROCEDURE SetQuantities :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdOvers             AS DECIMAL NO-UNDO.

    RUN pUpdateQuantities(
        INPUT ipiQuantity,
        INPUT ipiQuantityInSubUnit,
        INPUT ipiSubUnitsPerUnit,
        INPUT ipdOvers
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

