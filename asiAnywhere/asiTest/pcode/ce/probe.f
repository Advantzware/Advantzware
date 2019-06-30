/* ------------------------------------------------------ ce/probe.f 11/93 cd */
/* Probe ('What if') - FORM Statement                                         */
/* -------------------------------------------------------------------------- */

def var v-contrib-fac as dec.

form probe.est-qty      format ">>>>>>>9"
     probe.freight      format ">9"
     probe.fact-cost    format "->>>>9.99"
     probe.full-cost    format "->>>>9.99"
     probe.gross-profit format "->>9.99"
     probe.net-profit   format "->>9.99"
     probe.sell-price   format "->>>>9.99"
     v-contrib-fac      format ">>9.99"
     probe.gsh-qty      format ">>>>>>>9"
     probe.tot-lbs      format ">>>>>9"
     header
     "     E S T I M A T E  " + "#" + trim(xest.est-no) +
                "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip
     "             Dir.Fact      Full"
     space(17)
     "     Sell  Contr    Total   Ship"
     skip
     "     Qty  R      Cost      Cost"
     fill(" ",7 - length(trim(ce-ctrl.hd-gross))) + trim(ce-ctrl.hd-gross) format "x(7)"
     fill(" ",7 - length(trim(ce-ctrl.hd-net))) + trim(ce-ctrl.hd-net)     format "x(7)"
     "    Price   Fact   Sheets Weight"
     skip

    with width 80 stream-io frame probe down no-labels no-underline no-attr-space no-box
         /*title
           "E S T I M A T E  " + "#" + trim(xest.est-no) +
           "   A N A L Y S I S   P e r  T h o u s a n d"*/.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
