/* ------------------------------------------------- cec/mach-seq.i 08/96 JLF */
/* create machine routing sequence - mach parameters                          */
/* -------------------------------------------------------------------------- */

if avail mach then do:
  create tt-mach-exc.
  assign
   tt-mach-exc.form-no  = xef.form-no
   tt-mach-exc.blank-no = if xest.est-type eq 5 then 1 else
                          if avail xeb and mach.p-type eq "B" then xeb.blank-no else 0
   tt-mach-exc.m-code   = mach.m-code
   tt-mach-exc.dept     = mach.dept[1]
   tt-mach-exc.defr     = "{&defr}" ne "".

  RUN cec/mach-qty.p (ROWID(mach), ROWID(xeb), v-on-f, {1}, qty, OUTPUT v-run).

  RUN cec/mach-dim.p (ROWID(tt-mach-exc), ROWID(xeb), {1}, {2}, {3}, v-run,
                      OUTPUT tt-mach-exc.reason).
  
  if tt-mach-exc.reason eq "" then do:
    {cec/machslot.i}
     
    {cec/mach-pan.i}

    if avail mach then do:
      if avail tt-mach-exc then delete tt-mach-exc.
      leave.
    end.
  end.
  
  else release mach.

  if avail mach and avail tt-mach-exc then delete tt-mach-exc.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
