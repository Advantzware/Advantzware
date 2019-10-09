/* ------------------------------------------------- ce/tan/pr4-mis.p 4/93 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}
{sys/inc/ceprepprice.i}

mis-tot = 0. /* zero array */
do i = 1 to 6:
  if index("IM",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
  
  put skip(1)
      "Miscellaneous Cost"
      "Mat/F"             to 30
      "Lab/F"             to 40
      "Mat/M"             to 50
      "Lab/M"             to 60
      "Mrkup%"            to 69
      "Total Cost"        to 80
      skip.
        
  leave.
end.

do i = 1 to 6 with frame ad2 down no-labels no-box:
  /* only (i)ntegrate and (m)aintenance lines are done here */
  if index("IM",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
  
  IF xef.mis-simon[i] = 'M' THEN DO:
        mis-tot[5] = xef.mis-matf[i] + (xef.mis-matm[i] * qty / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[5].
        mis-tot[6] = xef.mis-labf[i] + (xef.mis-labm[i] * qty / 1000).
        dMCostToExcludeMisc = dMCostToExcludeMisc + mis-tot[6].
        IF ceprepprice-chr EQ 'Profit' THEN 
            ASSIGN 
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] / (1 - (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] / (1 - (xef.mis-mkup[i] / 100))
                .
        ELSE 
            ASSIGN 
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[5] * (1 + (xef.mis-mkup[i] / 100))
                dMPriceToAddMisc = dMPriceToAddMisc + mis-tot[6] * (1 + (xef.mis-mkup[i] / 100))
                .
     END.
  ELSE IF ceprepprice-chr EQ "Profit" THEN
     ASSIGN
        mis-tot[5] = (xef.mis-matf[i] + (xef.mis-matm[i] * (qty / 1000))) /
                                        (1 - (xef.mis-mkup[i] / 100))
        mis-tot[6] = (xef.mis-labf[i] + (xef.mis-labm[i] * (qty / 1000))) /
                                        (1 - (xef.mis-mkup[i] / 100)).
  ELSE
     ASSIGN
        mis-tot[5] = (xef.mis-matf[i] + (xef.mis-matm[i] * (qty / 1000))) *
                                        (1 + (xef.mis-mkup[i] / 100))
        mis-tot[6] = (xef.mis-labf[i] + (xef.mis-labm[i] * (qty / 1000))) *
                                        (1 + (xef.mis-mkup[i] / 100)).

  assign
   mis-tot[1] = mis-tot[1] + mis-tot[5]
   mis-tot[2] = mis-tot[2] + (mis-tot[5] / (qty / 1000))
   mis-tot[3] = mis-tot[3] + mis-tot[6]
   mis-tot[4] = mis-tot[4] + (mis-tot[6] / (qty / 1000)).

  if mis-tot[5] ne 0 then do:
    create xprep.
    assign
     xprep.frm      = xef.mis-snum[i]
     xprep.blank-no = xef.mis-bnum[i]
     xprep.qty      = 1
     xprep.std-cost = mis-tot[5]
     xprep.ml       = yes
     xprep.cost-m   = mis-tot[5] / (qty / 1000)
     xprep.simon    = xef.mis-simon[i]
     xprep.code     = "MISM" + string(i,"9").
  end.

  if mis-tot[6] ne 0 then do:
    create xprep.
    assign
     xprep.frm      = xef.mis-snum[i]
     xprep.blank-no = xef.mis-bnum[i]
     xprep.qty      = 1
     xprep.std-cost = mis-tot[6]
     xprep.ml       = no
     xprep.cost-m   = mis-tot[6] / (qty / 1000)
     xprep.simon    = xef.mis-simon[i]
     xprep.code     = "MISL" + string(i,"9").
  end.
  
  display xef.mis-cost[i]         format "x(20)"
          xef.mis-matf[i]         format ">>,>>9.99"   to 30
          xef.mis-labf[i]         format ">>,>>9.99"   to 40
          xef.mis-matm[i]         format ">>,>>9.99"   to 50
          xef.mis-labm[i]         format ">>,>>9.99"   to 60
          xef.mis-mkup[i]         format " >>9.99%"    to 69
          mis-tot[5] + mis-tot[6] format ">>>>,>>9.99" to 80
          SKIP  WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
