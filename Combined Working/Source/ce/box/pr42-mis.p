/* ------------------------------------------------ ce/box/pr42-mis.p 7/92 cd */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}

DEF SHARED VAR qty AS INT NO-UNDO.

def var v as int.
def var v-mat-cost as dec.
def var v-lab-cost as dec.

DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

mis-tot = 0. /* zero array */

head-blok:
for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no:
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
        
    leave head-blok.
  end.  
end.

for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no:
  find first xeb
      where xeb.company eq xef.company
        and xeb.est-no  eq xef.est-no
        and xeb.form-no eq xef.form-no 
      no-lock.
                    
  assign
   mis-tot[2] = 0
   mis-tot[4] = 0
   mis-tot[5] = 0
   mis-tot[6] = 0
   qty        = tt-blk.

  {ce/pr4-mis.i}

      if xef.mis-bnum[i] eq 0 then
      for each blk where blk.snum = xef.mis-snum[i]:
        find first xjob
            where xjob.i-no eq BLK.id
              and xjob.qty  eq qtty[vmcl].
        assign
         xjob.lab = xjob.lab + (mis-tot[6] * blk.pct)
         xjob.mat = xjob.mat + (mis-tot[5] * blk.pct)
         blk.lab  = blk.lab  +  (mis-tot[6] * blk.pct)
         blk.cost = blk.cost + ((mis-tot[5] + mis-tot[6]) * blk.pct).
      end.

      else do:
        find first blk
            where blk.snum eq xef.mis-snum[i]
              and blk.bnum eq xef.mis-bnum[i]
            no-error.
        find first xjob
            where xjob.i-no eq BLK.id
              and xjob.qty  eq qtty[vmcl].
              
        if avail blk then do:
          assign
           xjob.lab = xjob.lab + mis-tot[6]
           xjob.mat = xjob.mat + mis-tot[5]
           blk.lab  = blk.lab  + mis-tot[6]
           blk.cost = blk.cost + (mis-tot[5] + mis-tot[6]).
        end.
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
  end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
