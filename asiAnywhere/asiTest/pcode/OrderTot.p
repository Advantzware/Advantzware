

/*------------------------------------------------------------------------
    File        : OrderTot.p
    Purpose     : OrderOnHand

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : sewa
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttOrderTot NO-UNDO 
        
        FIELD Weight1 AS Decimal
        FIELD Taxes AS Decimal
        FIELD Freig AS Decimal
        FIELD BillF AS Logical
        FIELD Tot AS Decimal
        FIELD Cos AS Decimal
        FIELD Com AS DEC
        FIELD v-fr-tax LIKE oe-ctrl.f-tax
        FIELD v-tax-rate AS DEC
        FIELD v-frt-tax-rate LIKE v-tax-rate
    FIELD vItem LIKE oe-ordl.i-no.


DEFINE DATASET dsOrderTot FOR ttOrderTot .
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmWeight  AS Decimal NO-UNDO.
DEFINE INPUT PARAMETER prmTax  AS Decimal NO-UNDO.
DEFINE INPUT PARAMETER prmFreight  AS Decimal NO-UNDO.
DEFINE INPUT PARAMETER prmFbill  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmRevenue  AS Decimal NO-UNDO.
DEFINE INPUT PARAMETER prmCost  AS Decimal NO-UNDO.
DEFINE INPUT PARAMETER prmComm  AS DEC NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderTot.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEFINE VARIABLE frighiout AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-freight LIKE oe-ord.t-freight NO-UNDO.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    g_company = prmComp
    g_loc      = "MAIN".

/* ********************  Preprocessor Definitions  ******************** */


IF prmAction = "CalcFreight" THEN DO:    
    FIND FIRST oe-ord where oe-ord.company EQ prmComp AND
         oe-ord.ord-no = INT(prmOrderNum) EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN DO:
    
    FOR EACH oe-ordl OF oe-ord :
      RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT frighiout).
      ld = ld + frighiout .
   END.
   ASSIGN
       oe-ord.t-freight = ld .
    
  END.
  ASSIGN 
      prmAction = "Select".
  
END.

IF prmAction = "Update" THEN DO:
   FIND FIRST oe-ord where oe-ord.company EQ prmComp AND
              oe-ord.ord-no = INT(prmOrderNum) EXCLUSIVE-LOCK NO-ERROR.

    FIND  FIRST  oe-ordl WHERE oe-ordl.ord-no = oe-ord.ord-no AND oe-ordl.company = prmComp  EXCLUSIVE-LOCK NO-ERROR.
    
 ASSIGN    lv-freight = oe-ord.t-freight.

    IF AVAIL oe-ord THEN
        ASSIGN
        
            oe-ord.tax          = prmTax
            oe-ord.t-freight    = prmFreight          
            oe-ord.f-bill       = IF prmFbill = "true" THEN TRUE ELSE FALSE
            oe-ord.t-revenue    = prmRevenue
            oe-ord.t-cost       = prmCost
            oe-ord.t-comm       = prmComm .   
        
            ASSIGN  oe-ordl.t-weight  = prmWeight .
            ASSIGN
                oe-ord.t-weight = oe-ord.t-weight + oe-ordl.t-weight. 
            
            ASSIGN prmAction = "Select".
END.




IF prmAction = "Select" THEN DO:
FIND FIRST oe-ord WHERE
    oe-ord.company EQ prmComp AND
    oe-ord.ord-no = INT(prmOrderNum)  NO-LOCK NO-ERROR.

               
    IF AVAIL oe-ord THEN DO:
   
   CREATE ttOrderTot.
   ASSIGN
           
            ttOrderTot.Weight1   = oe-ord.t-weight
            ttOrderTot.Taxes     = oe-ord.tax
            ttOrderTot.Freig     = oe-ord.t-freight
            ttOrderTot.BillF     = oe-ord.f-bill
            ttOrderTot.Tot       = oe-ord.t-revenue
            ttOrderTot.Cos       = oe-ord.t-cost
            ttOrderTot.Com       = oe-ord.t-comm.
         END.
IF AVAIL oe-ord THEN
     RUN ar/cctaxrt.p (prmComp, oe-ord.tax-gr,
                    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).
    
   
FIND FIRST ce-ctrl
    WHERE ce-ctrl.company EQ prmComp
      AND ce-ctrl.loc     EQ "MAIN"
    NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ prmComp NO-LOCK NO-ERROR.
IF AVAIL oe-ctrl THEN v-fr-tax = oe-ctrl.f-tax.
END.
