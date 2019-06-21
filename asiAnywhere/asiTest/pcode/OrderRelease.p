
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : QuoteDetail

    Syntax      :

    Description : Return a Dataset of all Quote Inquiry

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrderRelease.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER Part-no as character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderRelease.

DEFINE VARIABLE quotedate      AS DATE.
ASSIGN quotedate = CalcQuoteDate().

DEFINE VARIABLE custx          AS CHARACTER.
DEFINE VAR relstat AS CHARACTER.
DEFINE VAR relout AS CHARACTER.
DEFINE VAR quotehdrowid AS ROWID.

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

find first oe-ordl where
     oe-ordl.company EQ prmComp AND
     oe-ordl.part-no = Part-no
     no-lock.

IF available oe-ordl then do:
   create ttOrderRelease.
   assign 
     ttOrderRelease.Part-no = oe-ordl.part-no
     ttOrderRelease.ord-no = string(oe-ordl.ord-no) 
     ttOrderRelease.line = string(oe-ordl.line)
     ttOrderRelease.descriptiond = oe-ordl.part-dscr1 + ", " + oe-ordl.part-dscr2 + ", " + oe-ordl.part-dscr3.
   FOR EACH oe-rel WHERE
       oe-rel.company = oe-ordl.company AND
       oe-rel.ord-no = oe-ordl.ord-no AND
       oe-rel.i-no = oe-ordl.i-no
       NO-LOCK:
       
       relstat = "".
       FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
       {oe/rel-stat.i relstat}
                                          
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
       
       assign ttOrderRelease.Vstatus = relout.
       FIND FIRST oe-relh  where oe-relh.company  eq oe-rel.company
                          and oe-relh.ord-no   eq oe-rel.ord-no  
                          and oe-relh.rel-no   eq oe-rel.rel-no
                          and oe-relh.b-ord-no eq oe-rel.b-ord-no
                          and oe-relh.cust-no  eq oe-rel.cust-no
                          use-index order NO-LOCK NO-ERROR.
       IF NOT AVAILABLE oe-relh THEN DO:
       
          IF (TODAY + (5 * 365)) < oe-rel.rel-date THEN
             assign ttOrderRelease.Vdate = "Scheduled".
          ELSE
             assign ttOrderRelease.Vdate = string(oe-rel.rel-date).
          
       END.
       ELSE DO:
          IF (TODAY + (5 * 365)) < oe-relh.rel-date THEN
             assign ttOrderRelease.Vdate = "Scheduled".
          ELSE
             assign ttOrderRelease.Vdate = string(oe-relh.rel-date).
       END.
       
       IF relstat NE "C" AND relstat NE "Z" THEN DO:
          assign
           ttOrderRelease.qty = string(oe-rel.qty)
           ttOrderRelease.po-no = STRING(oe-rel.po-no)
           ttOrderRelease.ship-no = STRING(oe-rel.ship-no)
           ttOrderRelease.addr = STRING(oe-rel.ship-addr[1] + " " + oe-rel.ship-addr[2])
           ttOrderRelease.city = string(oe-rel.ship-city)
           ttOrderRelease.state = string(oe-rel.ship-state)
          .
        END.
             
       END.
 END.     







