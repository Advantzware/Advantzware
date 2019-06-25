            /*------------------------------------------------------------------------
    File        : ItemHistoryDetail.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all item

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemHistDetail NO-UNDO 
   
    FIELD hisDate       AS DATE
    FIELD hisCost       AS DECIMAL
    FIELD hisSell       AS DECIMAL
    FIELD hisUom        AS CHAR
    FIELD hisQuantity       AS INT
    FIELD hisProfit     AS DECIMAL
   .
DEFINE DATASET dsItemHistDetail FOR ttItemHistDetail.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemHistDetail.
       
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmItem = ? THEN ASSIGN prmItem = "".
IF prmCust  = ? THEN ASSIGN prmCust = "".

IF INDEX(prmItem ,'&quot;',1) > 0 THEN ASSIGN
            prmItem  = REPLACE(prmItem ,'&quot;','"').

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
def NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def NEW SHARED var locode     as   char  format "x(5)"  no-undo.
ASSIGN
    cocode = prmComp.

DO TRANSACTION:
   {sys/inc/fgsecur.i}
END.

FUNCTION get-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  def var v-price as dec no-undo.
 v-price = oe-ordl.price.
  return v-price.  
END FUNCTION.

FUNCTION get-prof RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  def var v-price as dec no-undo.
  def var v-prof as dec no-undo.
  
  v-price = get-price().

  IF oe-ordl.pr-uom NE "M" THEN
    RUN sys/ref/convcuom.p(oe-ordl.pr-uom, "M", 0, 0, 0, 0,
                           v-price, OUTPUT v-price).

  v-prof = (v-price - oe-ordl.cost) / v-price * 100.
  if v-prof = ? then v-prof = 0.
  
  return v-prof.

END FUNCTION.


if prmAction = "Detail" then do:

    FOR EACH oe-ordl WHERE oe-ordl.company EQ prmComp AND oe-ordl.i-no EQ prmItem 
        AND oe-ordl.qty NE 0 NO-LOCK, 
        EACH oe-ord OF oe-ordl WHERE oe-ord.cust-no = prmCust NO-LOCK 
            BY oe-ord.ord-date DESCENDING BY oe-ord.ord-no DESCENDING BY oe-ordl.line DESCENDING:
                  CREATE ttItemHistDetail .
                    assign                                         
                        ttItemHistDetail.hisDate         =  oe-ord.ord-date
                        ttItemHistDetail.hisCost         =  oe-ordl.cost   
                        ttItemHistDetail.hisSell         =  get-price()
                        ttItemHistDetail.hisUom          =  oe-ordl.pr-uom  
                        ttItemHistDetail.hisQuantity     =  oe-ordl.qty
                        ttItemHistDetail.hisProfit       =  get-prof()
               
                 .
      
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 




