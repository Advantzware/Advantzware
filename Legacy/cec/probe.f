/* ----------------------------------------------------- cec/probe.f 11/93 cd */
/* Probe ('What if') - FORM Statement                                         */
/* -------------------------------------------------------------------------- */

def var voverall     as   dec    /*format "->>,>>>,>>9.99"*/ .
def var vtot-msf     as   dec    format ">>>>9.9<<<<".
DEFINE VARIABLE dContPerManHr AS DECIMAL format ">>>>9.99".
def var vfont4xprint as cha no-undo.

/* moved to probeu3.p  
put unformatted
     "     E S T I M A T E  " + "#" + trim(xest.est-no) +
     "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip
     .

find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "CEPrint" no-lock no-error.
put unformatted
    (if avail sys-ctrl and sys-ctrl.char-fld ne 'Text' then "<P10>" else "")
     "               Tot.Fact         Full"
     space(17)
     "         Sell          Price  Total   Total" skip
     "    Qty  R         Cost         Cost"
     fill(" ",8 - length(trim(ce-ctrl.hd-gross))) + trim(ce-ctrl.hd-gross) format "x(8)"
     fill(" ",8 - length(trim(ce-ctrl.hd-net))) + trim(ce-ctrl.hd-net)     format "x(8)"
     "         Price           /BSF Sheets     MSF"
     skip.
*/

form
     probe.est-qty          format ">>>>>>9"
     probe.freight          format ">9"
     probe.fact-cost        format ">>>>>9.99"
     probe.full-cost        format ">>>>>9.99"
     probe.gross-profit     format "->>9.99"
     probe.net-profit       format "->>9.99"
     probe.sell-price       format "->>>>9.99"
     
     voverall               format "->>>9.99"
     probe.gsh-qty format ">>>>>9"
     vtot-msf
     skip
     space(5) cm-disp format "x(14)" 
     cmah-disp format "x(14)" 
     cmoh-disp format "x(14)"
     cm%-disp format "x(18)"
     
     /*header
     "     E S T I M A T E  " + "#" + trim(xest.est-no) +
                "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip
     "               Tot.Fact         Full"
     space(17)
     "         Sell          Price  Total   Total" skip
     "    Qty  R         Cost         Cost"
     fill(" ",7 - length(trim(ce-ctrl.hd-gross))) + trim(ce-ctrl.hd-gross) format "x(7)"
     fill(" ",7 - length(trim(ce-ctrl.hd-net))) + trim(ce-ctrl.hd-net)     format "x(7)"
     "        Price           /BSF Sheets     MSF"
     skip*/
     with width 80 stream-io frame probe down no-labels no-underline no-attr-space no-box.
form
     probe.est-qty          format ">>>>>>9"
     probe.freight          format ">9"
     probe.fact-cost        format ">,>>>,>>9.99"
     probe.full-cost        format ">,>>>,>>9.99"
     probe.gross-profit     format "->>9.99"
     probe.net-profit       format "->>9.99"
     probe.sell-price       format "->,>>>,>>9.99"
     
     voverall               format "->>,>>>,>>9.99"
     probe.gsh-qty format ">>>>>9"
     vtot-msf
     skip

     space(5) cm-disp format "x(12)" 
     cmah-disp format "x(14)" 
     cmoh-disp format "x(16)"
     
     /*header
     "     E S T I M A T E  " + "#" + trim(xest.est-no) +
                "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip
     "               Tot.Fact         Full"
     space(17)
     "         Sell          Price  Total   Total" skip
     "    Qty  R         Cost         Cost"
     fill(" ",7 - length(trim(ce-ctrl.hd-gross))) + trim(ce-ctrl.hd-gross) format "x(7)"
     fill(" ",7 - length(trim(ce-ctrl.hd-net))) + trim(ce-ctrl.hd-net)     format "x(7)"
     "        Price           /BSF Sheets     MSF"
     skip*/
     with width 130 stream-io frame probe-big down no-labels no-underline no-attr-space no-box.
form
     probe.est-qty          format ">>>>>9"
     probe.freight          format ">9"
     reftable.val[2]        format "->>>>9.99"
     reftable.val[3]        format "->>9.99"
     reftable.val[5]        format ">>>>>>9.99"
     dContPerManHR         format ">>>>9.99"
     probe.sell-price       format "->>>>9.99"
     voverall               format "->>>9.99"
     probe.gsh-qty format ">>>>>9"
     vtot-msf
     skip
     header
     "     E S T I M A T E  " + "#" + trim(xest.est-no) +
     "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip
     "                              "
     space(10)
     "              Board              Total   Contb/      Sell    Price  Total   Total" skip
     "   Qty  R       $/M Board %      Contb   Man Hr     Price     /MSF Sheets     MSF"
     /*skip*/
     with width 90 stream-io frame probe-peach down no-labels no-underline no-attr-space no-box.
      
form
     probe.est-qty          format ">>>>>>9"
     probe.freight          format ">9"
     reftable.val[2]        format "->,>>>,>>9.99"
     reftable.val[3]        format "->>9.99"
     reftable.val[5]        format ">>>,>>9.99"
     dContPerManHR         format ">,>>>,>>9.99"
     probe.sell-price       format "->>>>,>>9.99"
     voverall               format "->>>>9.99"
     probe.gsh-qty format ">>>>>9"
     vtot-msf
     skip
     header
     "     E S T I M A T E  " + "#" + trim(xest.est-no) +
     "   A N A L Y S I S   P e r  T h o u s a n d     " format "x(78)" skip
     "                              "
     space(10)
     "                                     Total         Contb/        Sell     Price  Total   Total" skip
     "    Qty  R     Board $/M Board %     Contb         Man Hr       Price      /MSF Sheets     MSF" 
     /*skip*/
     with width 130 stream-io frame probe-peach-big down no-labels no-underline no-attr-space no-box.
           

/* end ---------------------------------- copr. 1993  advanced software, inc. */
