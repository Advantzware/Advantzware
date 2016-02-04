/* ---------------------------------------------------- ce/kmr-run.p 10/94 gb */
/* routing seq. kalk from matrices                                            */
/* -------------------------------------------------------------------------- */
{sys/inc/var.i shared}
define shared buffer xest for est.
define shared buffer xef  for ef.
define shared buffer xeb  for eb.

def var prev-board-cal like ef.cal no-undo.
def shared var  xcal as de NO-UNDO.
def shared var maxco as int NO-UNDO.
def shared var r-spo as de NO-UNDO.         
def shared var spo   as de NO-UNDO.
def shared var v-2 as logical init false NO-UNDO.
DEF SHARED VAR CALL_id AS RECID NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF SHARED VAR fil_id AS RECID NO-UNDO.

find mach where recid(mach) = call_id no-lock no-error.
find first est-op where recid(est-op) = fil_id no-error.
/* find mstd of machine */
find first mstd where mstd.company  = cocode      and
                      mstd.loc      = locode      and
                      mstd.m-code   = mach.m-code and
                      mstd.style    = xeb.style
                      no-lock no-error.

/* maybe there's only a blank style mstd */
if not available mstd then
find first mstd where mstd.company  = cocode      and
                      mstd.loc      = locode      and
                      mstd.m-code   = mach.m-code
                      no-lock no-error.

/* we should now have a mstd. kxy sets global vars x & y */

call_id = recid(xef).
find first xef where recid(xef) = call_id no-error.

if available mstd then do:
   x = 0. y = 0.
   find first mmty of mstd no-lock no-error.
   {ce/kxy.i &fil=mmty &fld=mr-x &fld2=mr-y}
   est-op.op-mr = mmty.vals[(10 * y) + x].
   find first mmtx  of mstd no-lock no-error.
   find first mmtx2 of mmtx no-lock no-error.

   {ce/kxy.i &fil=mmtx &fld=rs-x &fld2=rs-y}

   if x > 15 then est-op.op-speed = mmtx2.vals[(10 * y) + x].
   else est-op.op-speed =  mmtx.vals[(10 * y) + x].
   zzz = est-op.op-speed.

   /* 9 = last extent in mstd.. */
   do k = 1 to 9:
      if ( (mach.therm = true and mach.p-type = "R") and
           (((mstd.run-qty[k] / (xef.nsh-len / 12)) * xeb.num-up) > qty ))
      or ( (mach.p-type = "S" or (mach.therm = false and mach.p-type = "R")) and
           (mstd.run-qty[k] * xeb.num-up > qty))
      or  (mach.p-type = "B" and mstd.run-qty[k] > qty) then leave.
      else if mstd.x-sheets[k] ne 0
      then est-op.op-speed = zzz * (1 + (mstd.x-sheets[k] / 100)).
   end.
   {sys/inc/roundup.i est-op.op-speed }
   tmpstore = "no".
   find first item {sys/look/item.w} and item.i-code = xef.board
   no-lock no-error.
   if available item then do k = 1 to 10:
      if item.dept-name[k] = est-op.dept and item.speed%[k] ne 0 then do:
         assign est-op.op-speed = est-op.op-speed * (1 - (item.speed%[k] / 100))
                tmpstore = "Yes".
         leave.
      end.
   end.
   prev-board-cal = 0.
   if tmpstore = "NO" then do k = 1 to 9:
      if xef.cal gt prev-board-cal and
         xef.cal le mstd.board-cal[k]
      then
      do:
         est-op.op-speed = est-op.op-speed * (1 - (mstd.spd-reduc[k] / 100)) .
         leave.
      end.
      prev-board-cal = mstd.board-cal[k].
   end.
end.
else
   /*run sys/msg/wemsg.p(" NO STANDARDS FILE AVAILABLE", " E R R O R ", 4). */
    MESSAGE "NO STANDARDS FILE AVAILABLE" VIEW-AS ALERT-BOX ERROR.

assign xef.cal = xxx
       fil_id = recid(xef).
find first xef where recid(xef) = fil_id no-error.
fil_id = recid(est-op).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
