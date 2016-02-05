
for each asinotes
    where asinotes.company   eq cocode                 
      and asinotes.type      eq "DEPARTMENT"           
      and asinotes.key-2     eq eb.cust-no:screen-value
      and (asinotes.notes[1] ne "" or
           asinotes.notes[2] ne "" or
           asinotes.notes[3] ne "")
    no-lock:

  find first est-inst
      where est-inst.est-no eq est.est-no
        and est-inst.dept   eq asinotes.key
      exclusive-lock no-error.

  if not avail est-inst then do:
    find first dept
        where dept.company eq cocode
          and dept.code    eq asinotes.key
        no-lock no-error.
        
    create est-inst.
    assign
     est-inst.company = cocode
     est-inst.loc     = locode
     est-inst.est-no  = est.est-no
     est-inst.dept    = asinotes.key
     est-inst.dscr    = if avail dept then dept.dscr else "".
  end.

  if est-inst.inst[1] eq "" and
     est-inst.inst[2] eq "" and
     est-inst.inst[3] eq "" then
    assign 
     est-inst.inst[1] = asinotes.notes[1]
     est-inst.inst[2] = asinotes.notes[2]
     est-inst.inst[3] = asinotes.notes[3].
end.
