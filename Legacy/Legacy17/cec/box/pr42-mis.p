/* ----------------------------------------------- cec/com/pr42-mis.p 7/92 cd */

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def shared var qty as int NO-UNDO.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

def shared var v-qtty like qtty.

def var v as int NO-UNDO.
def var v-mat-cost as dec NO-UNDO.
def var v-lab-cost as dec NO-UNDO.
def var v-yld as dec NO-UNDO.
DEF VAR v-qty LIKE qty NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}
{sys/inc/cerun.i C}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

assign
 mis-tot = 0
 save-qty = qty.

/* misc. */
if line-counter > 52 then page.
setup1:
for each xef where xef.company = xest.company
               and xef.est-no = xest.est-no
               and (xef.form-no eq v-form-no or (not vmclean2)):
   do i = 1 to 6:
      if index("IM",xef.mis-simon[i]) > 0 and xef.mis-cost[i] ne "" then do:
         display skip(1) (IF cerunc = "Protagon" THEN
                "Miscellaneous Cost    Mat/F   Lab/F     Mat/M     Lab/M Charge    OH% Total Cost"
           ELSE "Miscellaneous Cost    Mat/F   Lab/F     Mat/M     Lab/M Charge Mrkup% Total Cost") FORMAT "X(80)"
          skip with frame wywy no-labels no-box stream-io . 
   leave setup1.
      end.
   end.
end.

for each xef where xef.company = xest.company
               and xef.est-no    eq xest.est-no
               and (xef.form-no eq v-form-no or (not vmclean2)):

   assign
    mis-tot[2] = 0
    mis-tot[4] = 0
    mis-tot[5] = 0
    mis-tot[6] = 0
   v-qty = tt-blk.

   {cec/pr4-mis.i}

         if xef.mis-bnum[i] = 0 then
         for each blk where blk.snum = xef.mis-snum[i]:
            find first xjob
                where xjob.i-no eq blk.id
                  and xjob.qty  eq blk.qreq.
/*
                  and xjob.qty  eq qtty[vmcl].
*/
            assign
             xjob.lab = xjob.lab + (mis-tot[6] * blk.pct)
             xjob.mat = xjob.mat + (mis-tot[5] * blk.pct)
             blk.lab  = blk.lab  + (mis-tot[6] * blk.pct)
             blk.cost = blk.cost + ((mis-tot[5] + mis-tot[6]) * blk.pct).
         end.

         else do:
            find first blk where blk.snum = xef.mis-snum[i] and
                                 blk.bnum = xef.mis-bnum[i] no-error.
            find first xjob
                where xjob.i-no eq blk.id
                  and xjob.qty  eq blk.qreq.
/*
                  and xjob.qty  eq qtty[vmcl].
*/
            if available blk then do:
               assign xjob.lab = xjob.lab + mis-tot[6]
                      xjob.mat = xjob.mat + mis-tot[5]
                      blk.lab  = blk.lab  + mis-tot[6]
                      blk.cost = blk.cost + (mis-tot[5] + mis-tot[6]).
            end.
         end.

         display xef.mis-cost[i]  format "x(19)"
                 xef.mis-matf[i]  format ">>>9.99"
                 xef.mis-labf[i]  format ">>>9.99"
                 v-mat-cost       format ">>>>>9.99" to 45
                 v-lab-cost       format ">>>>>9.99" to 55
                 xef.mis-simon[i] format "X" to 59
                 xef.mis-mkup[i]  to 71
                 mis-tot[5] + mis-tot[6] to 80 format ">>>>>9.99" skip with stream-io.
      end.
   end.
end.

qty = save-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
