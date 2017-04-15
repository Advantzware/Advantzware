                                       def buffer bf-fg-rdtlh for fg-rdtlh.
def buffer bf-fg-rcpth for fg-rcpth.

for each fg-bin where fg-bin.company eq '001' and
qty gt 0 and fg-bin.std-tot-cost eq ? no-lock break by fg-bin.i-no.
 
 if first-of(fg-bin.i-no) then do:
 find itemfg where itemfg.company eq fg-bin.company
 and itemfg.i-no eq fg-bin.i-no no-lock.
    disp fg-bin.i-no format "x(30)"
         fg-bin.loc fg-bin.loc-bin fg-bin.qty fg-bin.std-tot-cost
         with width 180 20 down .
         
   /* filter zero quantity records to purge */
   for each fg-rcpth where fg-rcpth.company eq '001' 
      and fg-rcpth.i-no eq fg-bin.i-no
      /* and fg-rcpth.trans-date gt today - 60 */ no-lock,
       first fg-rdtlh where fg-rdtlh.r-no = fg-rcpth.r-no 
            and fg-rdtlh.qty = 0 no-lock
       break by fg-rdtlh.r-no.
       
       find first bf-fg-rdtlh where bf-fg-rdtlh.r-no = fg-rcpth.r-no exclusive-lock.
       find bf-fg-rcpth where rowid(bf-fg-rcpth) eq rowid(fg-rcpth) exclusive-lock.
       /* if first-of(fg-rdtlh.i-no) then  */
       disp fg-rdtlh.r-no fg-rcpth.trans-date fg-rdtlh.i-no fg-rdtlh.cost fg-rdtlh.qty.  
       delete bf-fg-rdtlh.
       delete bf-fg-rcpth.
       
       
    end. /* each history */
    
    run fg/fg-calcbcst.p (input rowid(itemfg)).
    message "done with item" fg-bin.i-no view-as alert-box.
  end. /* first of i-no */
  
end.
