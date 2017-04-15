/* -------------------------------------------------- ce/pr4-mis2.p 07/96 JLF */

{sys/inc/var.i shared}
/*
{sys/inc/debug.i "In ce/print4.p"} /* CTS */
*/

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
def new shared buffer xop for est-op.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

{ce/msfcalc.i}

def var v as INT NO-UNDO.
def var v-mat-cost as dec NO-UNDO.
def var v-lab-cost as dec NO-UNDO.

DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

DEF VAR v-orig-prep-mat LIKE prep-mat NO-UNDO.
DEF VAR v-orig-prep-lab LIKE prep-lab NO-UNDO.

output to value(outfile2).

find first est-prep WHERE est-prep.company = xest.company
                      AND est-prep.est-no = xest.est-no
                      AND index("SON",est-prep.simon) gt 0
                      and est-prep.code ne ""
                      no-lock no-error.
      
if (xeb.chg-method ne "P" and fr-tot ne 0) or avail est-prep then
  put skip(1)
      space(24) "B I L L A B L E    C H A R G E S"
      skip
      "Prep Description"
      "Mat'l"             to 30
      "Labor"             to 40
      "Addt'l"            to 50
      "Amtz"              to 60
      "Cost/M"            to 69
      "Total Cost"        to 80
      skip.

assign
 tprep-mat = 0
 tprep-lab = 0
 tprep-tot = 0.

for each est-prep  WHERE est-prep.company = xest.company
                     AND est-prep.est-no = xest.est-no
                     AND index("SON",est-prep.simon) gt 0
                     and est-prep.code ne ""
                     with frame ag STREAM-IO no-box no-labels:
      
  assign
   prep-add = est-prep.mkup / 100
   prep-atz = if est-prep.amtz ne 0 then est-prep.amtz / 100 else 1.
     
  if est-prep.ml THEN
     ASSIGN
        prep-lab = 0
        prep-mat = est-prep.cost * est-prep.qty.
  ELSE
    ASSIGN
       prep-mat = 0
       prep-lab = est-prep.cost * est-prep.qty.
    
  ASSIGN
     v-orig-prep-mat = prep-mat
     v-orig-prep-lab = prep-lab
     prep-tot = prep-mat + prep-lab.

  IF ceprepprice-chr EQ "Profit" THEN
     prep-tot  = prep-tot / (1 - prep-add) * prep-atz.
  ELSE
     prep-tot  = prep-tot * (1 + prep-add) * prep-atz.

  IF ceprep-cha EQ "FiveDollar" AND
     prep-tot ne 0 THEN DO:
     ld-fac = prep-tot.
     {sys/inc/roundupfive.i prep-tot}
     ASSIGN
        ld-fac = prep-tot / ld-fac
        prep-mat = prep-mat * ld-fac
        prep-lab = prep-lab * ld-fac.
  END.

  IF ceprepprice-chr EQ "Profit" THEN
     ASSIGN
        tprep-mat = tprep-mat + (prep-mat / (1 - prep-add) * prep-atz)
        tprep-lab = tprep-lab + (prep-lab / (1 - prep-add) * prep-atz).
  ELSE
     ASSIGN
        tprep-mat = tprep-mat + (prep-mat * (1 + prep-add) * prep-atz)
        tprep-lab = tprep-lab + (prep-lab * (1 + prep-add) * prep-atz).

  tprep-tot = tprep-tot + prep-tot.

  create xprep.
  assign
   xprep.frm      = est-prep.s-num
   xprep.blank-no = est-prep.b-num
   xprep.qty      = est-prep.qty
   xprep.std-cost = est-prep.cost
   xprep.ml       = est-prep.ml
   xprep.cost-m   = prep-tot / (qty / 1000)
   xprep.simon    = est-prep.simon
   xprep.code     = est-prep.code.
     
  display est-prep.dscr           format "x(20)"       to 20
          v-orig-prep-mat         format "->>>>>9.99"   to 30
          v-orig-prep-lab         format "->>>>>9.99"   to 40
          est-prep.mkup           format "  >>9.99%"   to 50
          est-prep.amtz           format "  >>9.99%"   to 60
          prep-tot / (qty / 1000) format "->>>>9.99"    to 69
          prep-tot                format "->>>>>>9.99" to 80
            when index("SO",est-prep.simon) gt 0
          "      N/C "
            when est-prep.simon eq "N" @ prep-tot
          SKIP  WITH STREAM-IO.
end.

if xeb.chg-method ne "P" and fr-tot ne 0 then do:
  put "Freight"
      fr-tot                      format ">>>9.99"     to 30
      (fr-tot / (qty / 1000))     format ">>>>9.99"    to 69
      fr-tot                      format ">>>>,>>9.99" to 80
      skip.
          
  assign
   tprep-mat = tprep-mat + fr-tot
   tprep-tot = tprep-tot + fr-tot.
end.

do i = 1 to 6:
  if index("SON",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
  
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
  
do i = 1 to 6 with frame ah down no-labels no-box:
  if index("SON",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
  
  {ce/refest5a.i MAT-QTY i "no-lock no-error"}

  if avail reftable then do v = 1 to EXTENT(reftable.val):
    if qty le reftable.val[v] then leave.
    if v eq EXTENT(reftable.val) then do:
      v = 0.
      release reftable.
      leave.
    end.
  end.

  if avail reftable then
    {ce/refest5a.i MAT-CST i "no-lock no-error"}

  v-mat-cost = if avail reftable then reftable.val[v] else 0.

  IF ceprepprice-chr EQ "Profit" THEN
     mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qty / 1000))) /
                                     (1 - (xef.mis-mkup[i] / 100)).
  ELSE
     mis-tot[5] = (xef.mis-matf[i] + (v-mat-cost * (qty / 1000))) *
                                       (1 + (xef.mis-mkup[i] / 100)).

  {ce/refest5a.i LAB-QTY i "no-lock no-error"}

  if avail reftable then do v = 1 to EXTENT(reftable.val):
    if qty le reftable.val[v] then leave.
    if v eq EXTENT(reftable.val) then do:
      v = 0.
      release reftable.
      leave.
    end.
  end.

  if avail reftable then
    {ce/refest5a.i LAB-CST i "no-lock no-error"}

  v-lab-cost = if avail reftable then reftable.val[v] else 0.

  IF ceprepprice-chr EQ "Profit" THEN
     mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qty / 1000))) /
                                     (1 - (xef.mis-mkup[i] / 100)).
  ELSE
     mis-tot[6] = (xef.mis-labf[i] + (v-lab-cost * (qty / 1000))) *
                                     (1 + (xef.mis-mkup[i] / 100)).
     
  prep-tot = mis-tot[5] + mis-tot[6].

  IF ceprep-cha EQ "FiveDollar" AND
     prep-tot ne 0 THEN DO:
     ld-fac = prep-tot.
     {sys/inc/roundupfive.i prep-tot}
     ASSIGN
        ld-fac = prep-tot / ld-fac
        mis-tot[5] = mis-tot[5] * ld-fac
        mis-tot[6] = mis-tot[6] * ld-fac.
  END.

  if mis-tot[5] ne 0 then do:
	create xprep.
	assign xprep.frm      = xef.mis-snum[i]
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
	assign xprep.frm      = xef.mis-snum[i]
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
          v-mat-cost              format ">>,>>9.99"   to 50
          v-lab-cost              format ">>,>>9.99"   to 60
          xef.mis-mkup[i]         format " >>9.99%"    to 69
          mis-tot[5] + mis-tot[6] format ">>>>,>>9.99" to 80
          skip WITH STREAM-IO.
end.

put skip(2).
  
output close.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
