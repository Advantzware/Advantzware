&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : BusinessLogic/OrderEntry/GetPriceTotal.p 
    Purpose     : Gets the total price based on qty, pric, uom and other inputs
    
    Syntax      : Run GetPriceTotal(Quantity, 
                                    Price, 
                                    Uom,
                                    CaseCount,
                                    Discount,
                                    OUTPUT TotalPrice)

    Description :

    Author(s)   :  BV
    Created     :  1/14
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipxQty LIKE oe-ordl.qty.
DEFINE INPUT PARAMETER ipxPrice LIKE oe-ordl.price.
DEFINE INPUT PARAMETER ipxPriceUom LIKE oe-ordl.pr-uom.
DEFINE INPUT PARAMETER ipxCaseCount LIKE itemfg.case-count.
DEFINE INPUT PARAMETER ipxDisc LIKE oe-ordl.disc.
DEFINE OUTPUT PARAMETER opxTotalPrice LIKE oe-ordl.t-price.

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

DEFINE VARIABLE cFGUomList AS CHARACTER   NO-UNDO.
DEFINE BUFFER bf-uom FOR uom.

RUN sys/ref/uom-ea.p (OUTPUT cFGUomList).

IF ipxQty EQ 0 THEN
    opxTotalPrice = 0.
ELSE IF ipxPriceUom BEGINS "L" AND ipxPriceUom NE "LB" THEN
    opxTotalPrice = ipxPrice * IF ipxQty LT 0 THEN -1 ELSE 1.
ELSE IF ipxPriceUom EQ "CS" AND ipxCaseCount NE 0 THEN
    opxTotalPrice = /*ipxPrice **/ ipxQty / ipxCaseCount * ipxPrice.
ELSE IF ipxPriceUom EQ "C" THEN
    opxTotalPrice = ipxQty / 100 * ipxPrice.
ELSE IF ipxPriceUom EQ "M" THEN
    opxTotalPrice = ipxQty / 1000 * ipxPrice.
ELSE IF LOOKUP(ipxPriceUom, cFGUomList) GT 0 THEN
    opxTotalPrice = ipxQty * ipxPrice.
ELSE DO:
    FIND FIRST bf-uom
        WHERE bf-uom.uom  EQ ipxPriceUom
          AND bf-uom.mult NE 0
        NO-LOCK NO-ERROR.
    IF AVAIL bf-uom THEN
        opxTotalPrice = ipxQty / bf-uom.mult * ipxPrice.
    ELSE
        opxTotalPrice = ipxQty * ipxPrice.
END.

opxTotalPrice = ROUND(opxTotalPrice - opxTotalPrice * ipxDisc / 100, 2).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


