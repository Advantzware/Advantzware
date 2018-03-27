/*** sys/inc/maxnumon.i ***/
if available(mach) then
assign 
  {&blank}.num-wid = if  (mach.num-wid gt 0 and 
                          mach.num-wid lt {&blank}.num-wid)
                     then mach.num-wid
                     else {&blank}.num-wid
  {&blank}.num-len = if  (mach.num-len gt 0 and
                          mach.num-len lt {&blank}.num-len)
                     then mach.num-len
                     else {&blank}.num-len.
else
if not(available(mach)) and available(ce-ctrl) then
assign
  {&blank}.num-wid = if  (ce-ctrl.num-wid gt 0 and 
                          ce-ctrl.num-wid lt {&blank}.num-wid)
                     then ce-ctrl.num-wid
                     else {&blank}.num-wid
  {&blank}.num-len = if  (ce-ctrl.num-len gt 0 and
                          ce-ctrl.num-len lt {&blank}.num-len)
                     then ce-ctrl.num-len
                     else {&blank}.num-len.
