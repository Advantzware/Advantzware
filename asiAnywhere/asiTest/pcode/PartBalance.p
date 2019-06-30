
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
{PartBalance.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmItem as character no-undo.
DEFINE INPUT PARAMETER vRefresh  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vStartdate    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vEnddate      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPartBalance.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VARIABLE quotedate      AS DATE.
ASSIGN quotedate = CalcQuoteDate().
DEFINE VARIABLE custx          AS CHARACTER.
DEFINE VAR relstat AS CHARACTER.
DEFINE VAR relout AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE VAR totalbin AS INTEGER.
DEFINE VAR totalOrdered AS INTEGER.
DEFINE VAR totalOnhand AS INTEGER.
DEFINE VAR totalAvailable AS INTEGER.
DEFINE VAR totalReleased AS INTEGER.
DEFINE VAR totalShipped AS INTEGER.
DEFINE VAR isrow AS LOGICAL.
DEFINE VAR v-pallets AS DECIMAL.
DEFINE VAR total-pallets AS INTEGER.
DEFINE VAR v-qty-pal AS DECIMAL.
DEFINE VAR  v-palletsrnd AS DECIMAL.
DEFINE VAR qty-pallet AS DECIMAL.
DEFINE VAR all-total-pallets AS INTEGER.
DEFINE VAR v-shipped AS DECIMAL.
DEFINE VAR total-shipped AS DECIMAL.
DEFINE VAR thisreleased AS INTEGER.

DEFINE BUFFER oeordlb FOR oe-ordl.
DEFINE BUFFER oeordb FOR oe-ord.

DEFINE VAR Vqtypal AS DECIMAL.
DEFINE VAR Vpallets AS DECIMAL.
DEFINE VAR Vpalletsrnd AS DECIMAL.

DEFINE VAR startdate AS CHARACTER.
DEFINE VAR enddate AS CHARACTER.


DEFINE VAR quotehdrowid AS ROWID.

IF vRefresh = ""  THEN DO:
      ASSIGN startdate = STRING(TODAY - 30).
END.
ELSE DO:
      ASSIGN startdate = string(date("vStartdate")).
END.
IF vEnddate = "" THEN DO:
      ASSIGN enddate = STRING(TODAY).
END.
ELSE DO:
      ASSIGN enddate = STRING(DATE("vEnddate")).
END.



find first itemfg where itemfg.part-no = prmItem no-lock.
if available itemfg then do:
          create ttPartBalance.
          assign
          ttPartBalance.Part-no = itemfg.part-no 
          ttPartBalance.descriptiond = itemfg.part-dscr1 + ", " + itemfg.part-dscr2 + ", " + itemfg.part-dscr3
           .
          
          for each oeordb WHERE oeordb.company = prmComp       AND
                                oeordb.cust-no = prmCust       AND
                                oeordb.stat ne 'z' and oeordb.stat ne 'c'
                                
                              
                                
              , EACH oeordlb OF oeordb WHERE  oeordlb.i-no    eq itemfg.i-no
                              and  oeordlb.cust-no eq cust.cust-no
                              no-lock use-index item break by oeordlb.ord-no
                              by oeordlb.i-no:
                              
                     totalbin = 0.
                     v-pallets = 0.
                     ASSIGN total-pallets = 0.
                     FOR EACH fg-bin WHERE fg-bin.company = prmCust        AND fg-bin.i-no = oeordlb.i-no  AND fg-bin.job-no = string(oeordlb.job-no)  NO-LOCK:
                                   ASSIGN totalbin = totalbin + fg-bin.qty.
                                                    
                          assign
                           v-qty-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                                       (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                                       (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
                           v-pallets =  fg-bin.qty / v-qty-pal. 
                            
                
                        
                
                            v-palletsrnd = ROUND(v-pallets,0).
                            IF v-palletsrnd < v-pallets  THEN do:
                                v-pallets = v-palletsrnd + 1 .
                            end.
                            ELSE do:
                                v-pallets = v-palletsrnd.
                            END.
                        
                        if v-pallets lt 0 then v-pallets = v-pallets * -1.

                        total-pallets = total-pallets + v-pallets.
                    END.  /*FOR EACH fg-bin */
                     v-shipped = 0.
                     for each oe-bolh where oe-bolh.company eq oeordlb.company
                                        and oe-bolh.ord-no  eq oeordlb.ord-no
                                        and oe-bolh.posted  eq yes
                                        use-index order-no no-lock,
                               each oe-boll
                                    where oe-boll.company eq oeordlb.company
                                      and oe-boll.b-no    eq oe-bolh.b-no
                                      and oe-boll.i-no    eq oeordlb.i-no
                                      and oe-boll.line    eq oeordlb.line
                                      and oe-boll.s-code  ne "I"
                                      no-lock:
                                      v-shipped = v-shipped + oe-boll.qty.
                      end.
                      for each ar-invl
                            where ar-invl.company eq oeordlb.company
                              and ar-invl.ord-no  eq oeordlb.ord-no
                              and ar-invl.i-no    eq oeordlb.i-no
                              and ar-invl.line    eq oeordlb.line
                              no-lock,
                              first ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock,
                                    each oe-reth
                                          where oe-reth.company eq oeordlb.company
                                            and oe-reth.posted  eq yes
                                            and oe-reth.applied eq yes
                                            and oe-reth.cust-no eq ar-inv.cust-no
                                            and oe-reth.inv-no  eq ar-inv.inv-no
                                             no-lock,
                                             each oe-retl
                                                   where oe-retl.company eq oeordlb.company
                                                     and oe-retl.r-no    eq oe-reth.r-no
                                                     and oe-retl.i-no    eq ar-invl.i-no
                                                     no-lock:
                                                      v-shipped = v-shipped - oe-retl.tot-qty-return.  
                      END.  /*for each ar-invl */

                        total-shipped = total-shipped + v-shipped.
                        



            IF AVAILABLE oeordlb  THEN DO:
                            ASSIGN cocode = oeordlb.company.
                            FOR EACH oe-rel WHERE oe-rel.company = oeordlb.company AND
                                                  oe-rel.ord-no = oeordlb.ord-no AND
                                                  oe-rel.i-no = oeordlb.i-no NO-LOCK:
                                                  relstat = "".
                                                   FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.

                                                   {oe/rel-stat.i relstat}
                                                   IF relstat = "a" THEN totalreleased = totalreleased + oe-rel.qty.
                                                   IF relstat = "a" THEN thisreleased = thisreleased + oe-rel.qty.
                            END.
                            isrow = YES.
                            totalOrdered = totalOrdered + oeordlb.qty.
                            totalOnhand = totalOnhand + totalbin.
                            totalavailable = totalAvailable + (totalbin - thisreleased).
                                  totalshipped = totalshipped + oeordlb.t-ship-qty.
            END. /* IF AVAILABLE oeordlb  THEN DO: */

              assign
              ttPartBalance.orderno = string(oeordlb.ord-no)
              ttPartBalance.qtyship = string(v-shipped)
              ttPartBalance.qtyr    = string(thisreleased)
              ttPartBalance.qtyoh   = IF (v-shipped = 0 AND totalbin = 0) THEN STRING(oeordlb.prom-date) ELSE string(totalbin)
              ttPartBalance.paloh   = string(total-pallets)
              ttPartBalance.qtypal  = string(v-qty-pal)
              ttPartBalance.qtyoo   = string(oeordlb.qty)
              ttPartBalance.qtya    = string(IF (totalbin - totalreleased) GE 0 THEN (string(totalbin - totalreleased)) ELSE "In Process")
              .
              all-total-pallets = all-total-pallets + total-pallets.
              
              
      END. /*for each oeordb */

      IF isrow THEN DO:
              assign
              ttPartBalance.orderno = "TOTAL"
              ttPartBalance.qtyoh   = STRING(totalOnhand)
              ttPartBalance.qtyship = STRING(total-shipped)
              ttPartBalance.qtyr    = STRING(totalreleased)
             
              ttPartBalance.paloh   = STRING(all-total-pallets)
              ttPartBalance.qtypal  = string(v-qty-pal)
              ttPartBalance.qtyoo   = string(totalordered)
              ttPartBalance.qtya    = IF totalAvailable GE 0 THEN string(totalAvailable) ELSE "In Process"
             .
      END. /*IF isrow THEN DO:*/

      FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company = itemfg.company and
                                      fg-rcpth.i-no = itemfg.i-no and
                                      fg-rcpth.trans-date >= date(startdate) and
                                      fg-rcpth.trans-date <= date(enddate) and
                                      fg-rcpth.rita-code = "S" 
                                      BY trans-date DESCENDING:
           FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = itemfg.company AND fg-rdtlh.r-no = fg-rcpth.r-no NO-LOCK NO-ERROR.
           find first fg-bin
                    where  fg-bin.company eq itemfg.company
                      and  fg-bin.i-no    eq fg-rcpth.i-no
                      and fg-bin.job-no  eq fg-rcpth.job-no
                      and fg-bin.job-no2 eq fg-rcpth.job-no2
                      and fg-bin.loc     eq fg-rdtlh.loc
                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                      and fg-bin.tag     eq fg-rdtlh.tag
                      no-lock no-error.  
            if avail fg-bin then
                      assign
                      Vqtypal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                                   (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                                   (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
                      Vpallets = fg-rdtlh.qty / Vqtypal.
            else
                      assign
                      Vpallets = 1
                      Vqtypal = fg-rdtlh.qty.
                      Vpalletsrnd = ROUND(Vpallets,0).
            IF Vpalletsrnd < Vpallets  THEN do:
                Vpallets = Vpalletsrnd + 1 .
            end.
            ELSE do:
                Vpallets = Vpalletsrnd.
            END.
            if Vpallets lt 0 then Vpallets = Vpallets * -1.
            assign
            ttPartBalance.pono = string(fg-rcpth.po-no)
            ttPartBalance.shipdated = string(fg-rcpth.trans-date)
            ttPartBalance.shipqtyd  = STRING(fg-rdtlh.qty)
            ttPartBalance.palletsd  = STRING(Vpallets)
            ttPartBalance.palletqtyd = STRING(Vqtypal)
            .
            
      END.  /*FOR EACH fg-rcpth NO-LOCK */


  END. /*if available itemfg then do:*/
  






