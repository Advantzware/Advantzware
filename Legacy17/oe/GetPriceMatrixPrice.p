&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : BusinessLogic/OrderEntry/GetPriceMatrixPrice.p 
    Purpose     : Gets the price and UOM from the price matrix based on
                  qty and/or level inputs.

    Syntax      : Run GetPriceMatrixPrice(BUFFER oe-prmtx, 
                                     BUFFER cust, 
                                     INPUT iQty,
                                     INPUT iLevelOverride,
                                     OUTPUT dPrice,
                                     OUTPUT cUOM)

    Description :

    Author(s)   :  BV
    Created     :  1/14
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*main matrix buffer - must be passed avail*/
DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.

/*to get price matrix appropriate for qty*/
DEFINE INPUT PARAMETER ipxQty LIKE oe-ordl.qty NO-UNDO.

/*for use to get specific price level from matrix - set to 0 to determine based on qty*/
DEFINE INPUT PARAMETER ipiLevelOverride AS INTEGER NO-UNDO. 

/*cust.cust-level for higher starting level*/
DEFINE INPUT PARAMETER ipiCustLevel AS INTEGER NO-UNDO.

/*for calculation of discount method*/
DEFINE INPUT PARAMETER ipxItemSellPrice LIKE itemfg.sell-price NO-UNDO.
DEFINE INPUT PARAMETER ipxItemSellPrUom LIKE itemfg.sell-uom NO-UNDO.

/*Outputs*/
DEFINE OUTPUT PARAMETER opxPrice LIKE oe-ordl.price NO-UNDO.
DEFINE OUTPUT PARAMETER opxUom LIKE oe-ordl.pr-uom NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE iLevel AS INTEGER     NO-UNDO.

IF NOT AVAIL ipbf-oe-prmtx THEN RETURN.

IF ipiLevelOverride NE 0 THEN DO:
    IF ipiCustLevel GT ipiLevelOverride THEN
        iLevel = ipiCustLevel.
    ELSE
        iLevel = ipiLevelOverride.
    RUN GetPrice(BUFFER ipbf-oe-prmtx,
                     INPUT iLevel,
                     INPUT ipxItemSellPrice,
                     INPUT ipxItemSellPrUom,
                     OUTPUT opxPrice,
                     OUTPUT opxUom).
END.
ELSE DO iLevel = (IF ipiCustLevel EQ 0 THEN 1 ELSE ipiCustLevel) TO 10:
    /* IF customer has higher starting level set otherwise start with 1st level*/
    IF ipxQty LE ipbf-oe-prmtx.qty[iLevel] THEN DO:
        RUN GetPrice(BUFFER ipbf-oe-prmtx,
                     INPUT iLevel,
                     INPUT ipxItemSellPrice,
                     INPUT ipxItemSellPrUom,
                     OUTPUT opxPrice,
                     OUTPUT opxUom).
        iLevel = 99.
        LEAVE.
    END. /*Qty LE oe-prmtx qty*/
END. /*do i  (loop through each level)*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetPrice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPrice Procedure 
PROCEDURE GetPrice :
/*------------------------------------------------------------------------------
  Purpose:     Gets the Price and UOM from a matrix element
  Parameters:  ipbf-oe-prmtx - matrix buffer
               ipiLevel = matrix element to return
               ipxItemSellPrice & ipxItemSellUom - fg item pricing info for 
                discount method.
              opxPrice & opxUom - output
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-oe-prmtx FOR oe-prmtx.
DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipxItemSellPrice LIKE itemfg.sell-price NO-UNDO.
DEFINE INPUT PARAMETER ipxItemSellUom LIKE itemfg.sell-uom NO-UNDO.
DEFINE OUTPUT PARAMETER opxPrice LIKE oe-ordl.price NO-UNDO.
DEFINE OUTPUT PARAMETER opxUom LIKE oe-ordl.pr-uom NO-UNDO.

IF ipbf-oe-prmtx.meth THEN
    ASSIGN 
        opxPrice = ipbf-oe-prmtx.price[ipiLevel]
        opxUom = ipbf-oe-prmtx.uom[ipiLevel].
ELSE /*discount method*/
    ASSIGN 
        opxPrice = ipxItemSellPrice - 
            ROUND((ipxItemSellPrice * ipbf-oe-prmtx.discount[ipiLevel]) / 100, 2)
        opxUom = ipxItemSellPrUom.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

