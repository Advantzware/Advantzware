
IF xef.m-code EQ "" THEN xef.m-code = ceroute-chr.

find first mach
    {sys/look/machW.i}
      and mach.m-code eq xef.m-code
    use-index m-code no-lock no-error.
if avail mach then do:
  assign
   xef.m-dscr   = mach.m-dscr
   xef.roll     = mach.p-type eq "R"
   xef.lam-dscr = "S"
   xef.lsh-wid  = mach.max-len
   xef.lsh-len  = mach.max-wid.

  find first item
      {sys/look/itemadW.i}
        and item.i-no eq xef.board
      use-index i-no no-lock no-error.
  if not avail item or item.i-code eq "E" then
    assign
     xef.gsh-wid = trunc(mach.max-{3}{4} / xeb.t-wid,0) *
                         xeb.t-wid + (mach.min-trim{3} * 2)
     xef.gsh-len = trunc(mach.max-{1}{2} / xeb.t-len,0) *
                         xeb.t-len + (mach.min-trim{1} * 2).
  else
    assign
     xef.gsh-wid = item.s-wid
     xef.gsh-len = item.s-len.

  assign
   xef.nsh-wid = xef.gsh-{1}{2}
   xef.nsh-len = xef.gsh-{3}{4}
   xef.n-out   = 1
   xef.n-out-l = 1
   xef.trim-w  = xef.nsh-wid - (mach.min-trim{3} * 2)
   xef.trim-l  = xef.nsh-len - (mach.min-trim{1} * 2)
   xeb.num-wid = trunc(xef.trim-w / xeb.t-{3}{4},0)
   xeb.num-len = trunc(xef.trim-l / xeb.t-{1}{2},0)
   xeb.num-up  = xeb.num-wid * xeb.num-len.
end.
