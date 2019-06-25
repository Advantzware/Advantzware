



/*------------------------------------------------------------------------
    File        : ItemPriceLook.p
    Purpose     : Price

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
def temp-table tt-rpt 
                      field tt-recid as recid
                      field tt-cost as dec
                      field tt-price as dec
                      field tt-uom-price as dec
                      field tt-qty as dec
                      field tt-prof as dec
                      FIELD tt-selldate AS DATE    .
                                           
    
DEFINE DATASET dsItemPriceLook FOR tt-rpt .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUom       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemPriceLook.
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
def var v-term as CHAR no-undo.      
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE TEMP-TABLE  ttreport 

                      field vTerm-id as CHARACTER
                      field vKey-02 as CHARACTER
                      FIELD vKey-03 as CHARACTER
                      field vKey-04 as CHARACTER
                      field vRec-id as recid
                         .

IF prmAction    = ? THEN ASSIGN prmAction   = "".
IF prmUser      = ? THEN ASSIGN prmUser     = "".
IF prmItem      = ? THEN ASSIGN prmItem     = "".
IF prmCust      = ? THEN ASSIGN prmCust     = "".
IF prmUom       = ? THEN ASSIGN prmUom      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp.


if prmAction = "select" then do:

      v-term = string(year(today),"9999") +
             string(month(today),"99")  +
             string(day(today),"99") +
             string(time,"99999").
            
  IF prmUom EQ "" THEN prmUom = "M".
        
      RUN reset-report
                   .
        
END.  /*ifif prmAction <> "select" */

   PROCEDURE reset-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
7------------------------------------------------------------------------------*/
   def var i as int no-undo.
 
def var a as int.


/*{sa/sa-sls02.i} */

for each oe-ordl where oe-ordl.company eq prmComp
                   and oe-ordl.i-no    eq prmItem
                   use-index item no-lock,
    first oe-ord where oe-ord.company eq prmComp
                   and oe-ord.cust-no eq prmCust
                   and oe-ord.ord-no  eq oe-ordl.ord-no
             use-index cust no-lock:
  create ttreport.
  assign ttreport.vTerm-id = v-term
         ttreport.vKey-02  = string(year(oe-ord.ord-date),"9999") +
                    string(month(oe-ord.ord-date),"99")  +
                    string(day(oe-ord.ord-date),"99")
         ttreport.vKey-03  = string(oe-ord.ord-no,"9999999999")
         ttreport.vKey-04  = string(month(oe-ord.ord-date),"99")  + "/" +
                    string(day(oe-ord.ord-date),"99")    + "/" +
                    substr(string(year(oe-ord.ord-date),"9999"),3,2)
         ttreport.vRec-id  = recid(oe-ordl).

   create tt-rpt.   
   assign /*tt-rpt.tt-recid = recid(report)*/
          tt-rpt.tt-selldate = oe-ord.ord-date.
   
   tt-rpt.tt-qty = oe-ordl.qty.
   tt-rpt.tt-cost = oe-ordl.cost / 1000.
   if tt-rpt.tt-cost eq ? then tt-rpt.tt-cost = 0.
   if oe-ordl.pr-uom eq "EA" then 
   tt-rpt.tt-price = oe-ordl.price.
   else run sys/ref/convcuom.p(oe-ordl.pr-uom, "EA", 0, 0, 0, 0,
                               oe-ordl.price, output tt-rpt.tt-price).
   if oe-ordl.pr-uom eq prmUom then tt-rpt.tt-uom-price = oe-ordl.price.
   else run sys/ref/convcuom.p(oe-ordl.pr-uom, prmUom, 0, 0, 0, 0,
                               oe-ordl.price, output tt-rpt.tt-uom-price).
   tt-rpt.tt-prof = (1 - (tt-rpt.tt-cost / tt-rpt.tt-price)) * 100 .
   if tt-rpt.tt-prof eq ? then tt-rpt.tt-prof = 0 .

end.

  
  
END PROCEDURE. 

