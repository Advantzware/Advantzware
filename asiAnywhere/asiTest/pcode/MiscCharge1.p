
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : OrderOnHand

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MiscCharge1.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSell as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMiscCharge1.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmSell = ? THEN ASSIGN prmSell = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        
        RUN build-qry .
        
        
        DATASET dsMiscCharge1:FILL().
    END.
    
END CASE.    
    
PROCEDURE build-qry:

FOR EACH oe-ordm where
    oe-ordm.company EQ prmComp AND
    oe-ordm.amt = int(prmSell)
    NO-LOCK,
    first oe-ord WHERE 
          oe-ord.company EQ prmComp AND
          oe-ord.ord-no EQ oe-ordm.ord-no
          NO-LOCK:
     	
    create ttMiscCharge1.
    assign  
       ttMiscCharge1.Weight   = oe-ord.t-weight
       ttMiscCharge1.Tax1      = oe-ord.tax
       ttMiscCharge1.Freight  = oe-ord.t-freight
       ttMiscCharge1.BillFre  = oe-ord.f-bill
       ttMiscCharge1.Total    = oe-ord.t-revenue
       ttMiscCharge1.Cost     = oe-ord.t-cost.
      
 END.   /*FOR EACH oe-ordm*/
 
 END PROCEDURE.
