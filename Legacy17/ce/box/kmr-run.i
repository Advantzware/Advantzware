/* ------------------------------------------------ ce/tan/kmr-run.i 4/92 cd  */
/*                                                                            */
/* routing seq. kalk from matrices                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def var prev-board-cal like ef.cal no-undo.

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

/* we should now have a mstd.
   kxy sets global vars x & y,
   krocol sets estimate fields */

call_id = recid(xef).
find first xef where recid(xef) = call_id no-error.
xcal = xef.cal.
if (xef.medium ne "" or xef.flute ne "" ) and xef.trim-pen ne 0
then do:
   find first dept where dept.code = "LM" no-lock no-error.
   if available dept then i = dept.fc.
   find first dept where dept.code = mach.dept[1] no-lock no-error.
   if available dept and dept.fc > i then do:
      xcal = xef.trim-pen / 1000. /* trim-pen stored as integer */
      if xef.cal ne .014 then
      xcal = xcal - .014 + xef.cal.
   end.
end.
xxx = xef.cal.
xef.cal = xcal.
if available mstd then do:
   x = 0. y = 0.
   find first mmty of mstd no-lock no-error.
   {ce/com/kxy.i &fil=mmty &fld=mr-x &fld2=mr-y}
   if x = 0 or y = 0 then do:
      MESSAGE "Can't process machine " + mach.m-code + ". Please verify Standards File!."
          VIEW-AS ALERT-BOX ERROR.
      next.
   end.
   {&fil}.{&fld} = mmty.vals[(10 * y) + x].
   find first mmtx  of mstd no-lock no-error.
   find first mmtx2 of mmtx no-lock no-error.
   {ce/com/kxy.i &fil=mmtx &fld=rs-x &fld2=rs-y}
   if x > 15 then
	{&fil2}.{&fld2} = mmtx2.vals[(10 * y) + x].
   else {&fil2}.{&fld2} =  mmtx.vals[(10 * y) + x].
   /*
   /* added 11/13/92 to convert lin.ft to #sheet for roll fed machines. */
   if mach.therm = true
   then {&fil2}.{&fld2} = {&fil2}.{&fld2} / (xef.gsh-len / 12).*/

   zzz = {&fil2}.{&fld2}.
   /* 9 = last extent in mstd.. */
   do k = 1 to 9:
      if ( (mach.therm = true and mach.p-type = "R") and
	   (((mstd.run-qty[k] / (xef.gsh-len / 12)) * xeb.num-up) > qty ))
      or ( (mach.p-type = "S" or
	    (mach.therm = false and mach.p-type = "R")) and
	   (mstd.run-qty[k] * xeb.num-up > qty))
      or  (mach.p-type = "B" and mstd.run-qty[k] > qty)
      then leave.
      else
      if mstd.x-sheets[k] ne 0 then do:
	 {&fil2}.{&fld2} = zzz * (1 + (mstd.x-sheets[k] / 100)).
      end.
   end.
   tmpstore = "no".
   find first item {sys/look/itemW.i} and item.i-code = xef.board
   no-lock no-error.
   if available item then
   do k = 1 to 10:
      if item.dept-name[k] = {&fil2}.dept and item.speed%[k] ne 0
      then do:
	 {&fil2}.{&fld2} = {&fil2}.{&fld2} * (1 - (item.speed%[k] / 100)) .
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
	 {&fil2}.{&fld2} = {&fil2}.{&fld2} * (1 - (mstd.spd-reduc[k] / 100)) .
	 leave.
      end.
      prev-board-cal = mstd.board-cal[k].
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
