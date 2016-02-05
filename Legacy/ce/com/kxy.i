/* ---------------------------------------------------- ce/com/kxy.i 10/94 gb */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

/* find col & row numbers for given mstd.code */

/* fil = mmtx or mmty
   fld = mr-x rs-x
   fld2= mr-y rs-y
   fil3.fld3 = file/field to set with grid value */

/* --------------------------------- c o l   ----- */
do i = 1 to 10:
   if mstd.{&fld} = 1 then
      if {&fil}.col-value[i] >= maxco then do: x = i. leave. end.
   else if mstd.{&fld} = 2 then
      if {&fil}.col-value[i] >= xeb.len then do: x = i. leave. end.
   else if mstd.{&fld} = 3 then
      if {&fil}.col-value[i] >= xeb.wid then do: x = i. leave. end.
   else if mstd.{&fld} = 4 then
      if {&fil}.col-value[i] >= xeb.t-len then do: x = i. leave. end.
   else if mstd.{&fld} = 5 then
      if {&fil}.col-value[i] >= xeb.t-wid then do: x = i. leave. end.
   else if mstd.{&fld} = 6 then
      if {&fil}.col-value[i] >= xeb.gluelap then do: x = i. leave. end.
   else if mstd.{&fld} = 7 then
      if {&fil}.col-value[i] >= xeb.t-sqin then do: x = i. leave. end.
   else if mstd.{&fld} = 8 then
      if {&fil}.col-value[i] >= xef.cal then do: x = i. leave. end.
   else if mstd.{&fld} = 9 then
      if {&fil}.col-value[i] >= xef.weight then do: x = i. leave. end.
   else if mstd.{&fld} = 10 then
      if {&fil}.col-value[i] >= xef.roll-wid then do: x = i. leave. end.
   else if mstd.{&fld} = 11 then
      if {&fil}.col-value[i] >= xef.trim-l then do: x = i. leave. end.
   else if mstd.{&fld} = 12 then
      if {&fil}.col-value[i] >= xef.trim-w then do: x = i. leave. end.
   else if mstd.{&fld} = 13 then
      if {&fil}.col-value[i] >= xeb.num-up then do: x = i. leave. end.
   else if mstd.{&fld} = 14 then
      if {&fil}.col-value[i] >= xef.leaf-l[1] then do: x = i. leave. end.
   else if mstd.{&fld} = 15 then
      if {&fil}.col-value[i] >= xef.leaf-w[1] then do: x = i. leave. end.
   else if mstd.{&fld} = 16 then
      if {&fil}.col-value[i] >= xef.nsh-len then do: x = i. leave. end.
   else if mstd.{&fld} = 17 then
      if {&fil}.col-value[i] >= xef.nsh-wid then do: x = i. leave. end.
   else if mstd.{&fld} = 18 then
      if {&fil}.col-value[i] >= qty then do: x = i. leave. end.
   else if mstd.{&fld} = 19 then
      if {&fil}.col-value[i] >= xef.die-in then do: x = i. leave. end.
   else if mstd.{&fld} = 20 then
      if {&fil}.col-value[i] >= qty / z then do: x = i. leave. end.
   else if mstd.{&fld} = 21 then
      if {&fil}.col-value[i] >= (xef.trim-l * xef.trim-w) then do:
	 x = i. leave.
      end.
   else if mstd.{&fld} = 22 then
      if {&fil}.col-value[i] >= xeb.i-pass then do: x = i. leave. end.
   else if mstd.{&fld} = 23 then
      if {&fil}.col-value[i] >= xef.n-cuts then do: x = i. leave. end.
   else if mstd.{&fld} = 24 then
      if {&fil}.col-value[i] >= xef.blank-qty then do: x = i. leave. end.
   else if mstd.{&fld} = 25 then
      if {&fil}.col-value[i] >= xef.n-out then do: x = i. leave. end.
end.

/* --------------------------------- r o w   ----- */
if mstd.{&fld2} = 1 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= maxco then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 2 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.len then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 3 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.wid then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 4 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.t-len then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 5 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.t-wid then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 6 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.gluelap then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 7 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.t-sqin then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 8 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.cal then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 9 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.weight then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 10 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.roll-wid then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 11 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.trim-l then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 12 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.trim-w then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 13 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.num-up then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 14 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.leaf-l[1] then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 15 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.leaf-w[1] then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 16 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.nsh-len then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 17 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.nsh-wid then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 18 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= qty then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 19 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.die-in then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 20 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= qty / z
   then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 21 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= (xef.trim-l * xef.trim-w) then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 22 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xeb.i-pass then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 23 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.n-cuts then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 24 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.blank-qty then do:
      y = i. leave.
   end.
end.
else
if mstd.{&fld2} = 25 then
do i = 1 to 15:
   if {&fil}.row-value[i] >= xef.n-out then do:
      y = i. leave.
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
