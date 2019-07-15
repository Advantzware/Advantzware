
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
{OrderRel.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderRel.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR relstat AS CHARACTER.
DEFINE VAR relout AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
/*IF prmFgItem   = ? THEN ASSIGN prmFgItem   = "".*/

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum) no-lock.
IF available oe-ordl then do:
          create ttOrderRel.
        assign 
          ttOrderRel.disc = oe-ordl.disc
          ttOrderRel.t-price = oe-ordl.t-price.
         
      FOR EACH oe-rel WHERE oe-rel.company = oe-ordl.company AND
                          oe-rel.ord-no = oe-ordl.ord-no AND oe-rel.i-no = oe-ordl.i-no:
                                  relstat = "".
                                  FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
                                  {oi/rel-stat.i relstat}
                                             
               CASE relstat:
                   WHEN "P" THEN relout = "Bill of Lading Processed".
                   WHEN "A" THEN relout = "Released".
                   WHEN "B" THEN relout = "Partial Shipment Rescheduled".
                   WHEN "I" THEN relout = "Past Whse Terms".
                   WHEN "L" THEN relout = "Late Not Scheduled".
                   WHEN "S" THEN relout = "Scheduled".
                   WHEN "C" THEN relout = "Delivery Completed".
                   OTHERWISE relout = relstat.
               END CASE.
                      assign ttOrderRel.Vstatus = relout.
               FIND FIRST oe-relh  where oe-relh.company  eq oe-rel.company
                                  and oe-relh.ord-no   eq oe-rel.ord-no  
                                  and oe-relh.rel-no   eq oe-rel.rel-no
                                  and oe-relh.b-ord-no eq oe-rel.b-ord-no
                                  and oe-relh.cust-no  eq oe-rel.cust-no
                                  use-index order NO-LOCK NO-ERROR.
               IF NOT AVAILABLE oe-relh THEN DO:
             
                  IF (TODAY + (5 * 365)) < oe-rel.rel-date THEN DO:
        
                            assign ttOrderRel.Vdate = "Scheduled".
        
                  END.
        
                  ELSE DO:
                             assign ttOrderRel.Vdate = string(oe-rel.rel-date).

        
                  END.
               END.
               ELSE DO:
                  IF (TODAY + (5 * 365)) < oe-relh.rel-date THEN DO:
        
                          assign ttOrderRel.Vdate = "Scheduled".

        
                  END.
        
                  ELSE DO:
                           assign ttOrderRel.Vdate = string(oe-relh.rel-date).

 
                  
        
                  END.
               END.
               message "relstat" relstat.
               IF relstat NE "C" AND relstat NE "Z" THEN DO:
                              assign
                                  /*ttOrderRel.printed =
                                  ttOrderRel.s-code  =*/
                                  ttOrderRel.ship-id = oe-rel.ship-id
                                  ttOrderRel.carrier = oe-rel.carrier
                                  ttOrderRel.tot-qty = oe-rel.tot-qty
                                  ttOrderRel.qty     = oe-rel.qty
                                  /*ttOrderRel.po-no = oe-rel.po-no
                                  ttOrderRel.lot-no  =
                                  ttOrderRel.key-02  =*/
                                  ttOrderRel.ship-addr = STRING(oe-rel.ship-addr[1] + " " + oe-rel.ship-addr[2])
                                  ttOrderRel.ship-city = oe-rel.ship-city
                                  ttOrderRel.ship-state = oe-rel.ship-state
                                  .
                END.
                
          END.
 END.     

