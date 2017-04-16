/* ---------------------------------------------- oe/rep/bolmwbox.i 12/99 FWK */
/* PRINT Midwest Box BOL                                                           */
/* -------------------------------------------------------------------------- */


assign
 v-tot-wt = 0
 v-tot-cases = 0.

for each report where report.term-id eq v-term-id,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break by report.key-01
          by report.key-02:

  v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
               if oe-boll.partial gt 0 then 1 else 0.

  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  release oe-rel.
  find first oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-boll.r-no
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        and oe-rell.i-no    eq oe-boll.i-no
        and oe-rell.line    eq oe-boll.line
      no-lock no-error.
  if avail oe-rell then do:
    find first oe-relh of oe-rell no-lock.
    find first oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-rell.ord-no
          and oe-rel.line    eq oe-rell.line
          and oe-rel.link-no eq oe-rell.r-no
          and oe-rel.ship-no eq oe-relh.ship-no
          and oe-rel.i-no    eq oe-rell.i-no
        no-lock no-error.
    if not avail oe-rel then
      find first oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-rell.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.rel-date eq oe-relh.rel-date
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.
  end.

  if last-of(report.key-02) then do:
    assign
     i = 0
     j = 0.

    for each w2 break by w2.cas-cnt:
      if first(w2.cas-cnt) then do:
        assign
         v-ord-qty = 0
         v-bol-qty = 0
         v-bol-wt  = 0
         v-ship-qty = 0.

        for each oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq int(report.key-02)
              and oe-ordl.i-no    eq report.key-01
            no-lock:

          assign
           v-ord-qty  = v-ord-qty  + oe-ordl.qty
           v-ship-qty = v-ship-qty + oe-ordl.ship-qty.

        end.

        for each xreport
            where xreport.term-id eq v-term
              and xreport.key-01  eq report.key-01
              and xreport.key-02  eq report.key-02
            no-lock,

            first xoe-boll where recid(xoe-boll) eq xreport.rec-id no-lock:

          assign
           v-bol-qty = v-bol-qty + xoe-boll.qty
           v-bol-wt  = v-bol-wt  + xoe-boll.weight.

          if xoe-boll.weight eq 0 then
            v-bol-wt = v-bol-wt + (xoe-boll.qty / 100 * itemfg.weight-100).
        end.
      end.

      assign
       i           = i + 1
       v-part-comp = if v-ship-qty + v-bol-qty ge v-ord-qty or oe-boll.p-c
                     then "C" else "P"   
       v-part-dscr = "".
    end.
    
    for each w2:
      delete w2.
    end.
  end.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq int(report.key-02)
        and oe-ordl.i-no    eq report.key-01
      no-lock no-error.

  v-job-no = "".
  if avail oe-ordl and oe-ordl.job-no ne "" then
    v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
               trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

    IF v-printline >= 36 THEN DO:
     v-printline = 0.
     PAGE {1}.
     /*{oe/rep/bolmwbx2.i } */
     PUT {1}
         "<FArial>"  SKIP
          "<P14><C+40><B>Bill Of Lading</B> " SKIP
          "<C5><R3><#1><R+7><C+75><IMAGE#1=" ls-full-img1  SKIP 
          "<=1><R+7>"
            "<FCourier New><B><P12>                          PHONE (216)281-3980" SKIP
             "                              FAX   (216)281-5707<P10>" SKIP(1)
             "<FCourier New><P10>"
               "    Sold To:" SPACE(40) "Ship To:"  SKIP
             SPACE(9) v-comp-name v-ship-name AT 56 skip
             SPACE(9) v-comp-addr[1] v-ship-addr[1] AT 56 SKIP
           SPACE(9) v-comp-addr[2] v-ship-addr[2] AT 56 SKIP
             SPACE(9) v-comp-addr3 v-ship-addr3 AT 56 SKIP
         space(9) v-phone-num SKIP
      "<R5><C50><#3>" SKIP
      "<FArial><P14><=#3>" /*<C-20><R-2> <B>Bill Of Lading</B> */ "<P10>" SKIP
              "<=#3><B>BOL #: " oe-bolh.bol-no "</B>" SKIP
              "<=#3><R+1><B>Page #: " 
                  string(trim(string(page-number - v-last-page,">9")) + " of " +
                  trim(string(v-page-tot,">9")))
                         format "x(8)" "</B>" SKIP
              "<=#3><R+2>Date: " oe-bolh.bol-date        SKIP
              "<=#3><R+3>" /*Ship Date:" oe-bolh.ship-date        */ SKIP
               SKIP     
              "<|10><R19><C1><#4><FROM><R23><C81><RECT>" SKIP
              "<R21><C1><FROM><R21><C81><LINE>" SKIP    
              "<R19><C12><FROM><R23><C12><LINE>" SKIP
            /*  "<R19><C25><FROM><R23><C25><LINE>" SKIP      
              "<R19><C34><FROM><R23><C34><LINE>" SKIP */
              "<R19><C46><FROM><R23><C46><LINE>" SKIP
              "<R19><C66><FROM><R23><C66><LINE>" SKIP
              /*"<FArial><=4><R+1>    Date                    PO#                               JOB#                 FOB                  Carrier                                                 Freight Terms" SKIP */
              "<FArial><=4><R+1>    Date                    FOB                                                                                   Carrier                                            Freight Terms" SKIP 
              "<FCourier New><=4><R+3> " oe-bolh.bol-date SPACE(3) /*v-po-no FORM "x(15)" space(2) v-job-no*/ v-fob space(30) carrier.dscr v-frt-terms SKIP
              "<|10><R24><C1><#5><FROM><R26><C81><RECT>" SKIP    
              "<R24><C13><FROM><R26><C13><LINE>" SKIP
              "<R24><C26><FROM><R26><C26><LINE>" SKIP
              "<R24><C39><FROM><R26><C39><LINE>" SKIP
              "<R24><C56><FROM><R26><C56><LINE>" SKIP  
              "<R24><C65><FROM><R26><C65><LINE>" SKIP
              "<R24><C76><FROM><R26><C76><LINE>" SKIP            
          "<FArial><=5><R+1> Part#                        PO#                            Finished Good#        Our Order#                          Unit-Quantity Partial/Complete Weight" SKIP(1)
          "<FCourier New>"                                  
          .

     v-printline = v-printline + 16.
  END.

  DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.po-no 
          oe-boll.i-no 
          oe-ordl.i-name  FORM "x(22)"
          oe-boll.cases FORM ">>,>>>" AT 72 "@"
          oe-boll.qty-case FORM "->>>>>Z" SKIP          
          oe-ordl.part-dscr1 AT 33 FORM "x(25)"
          v-1    FORM ">>,>>9"  when oe-boll.partial gt 0 AT 72 "@"
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP 
          "====================" AT 68 SKIP
          v-tot-pkgs AT 72 FORM ">>,>>9"  "="
          oe-boll.qty FORM "->>>>>z" SPACE(2)
          v-part-comp  SPACE(1)
          oe-boll.weight  SKIP          
          with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
  down {1} with frame bol-mid2.

  v-printline = v-printline + 5.
  put {1} skip(1).

  assign
   v-tot-wt = v-tot-wt + oe-boll.weight
   v-tot-cases = v-tot-cases + v-tot-pkgs.

  v-tot-pkgs = 0.

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
  

end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
