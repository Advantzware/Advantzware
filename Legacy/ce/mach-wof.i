
def buffer xstyle for style.

def var v-dept like dept.code no-undo.

window-foil:
do on error undo:
  do i = 1 to 4:
    if xef.leaf[i] eq "" then next.
    
    release mach.
    
    find first item
        {sys/look/itemlwW.i}
          and item.i-no eq xef.leaf[i]
        no-lock no-error.
                                                           
    if avail item then do:
      v-dept = if item.mat-type eq "W" then
                 if xef.leaf-bnum[i] eq 0 then "WS" else "WN"
               else
               if item.mat-type eq "F" then
                 if xef.leaf-bnum[i] eq 0 then "FS" else "FB"
               else "".
               
      if v-dept eq "" then next.         
      
      find first eb
          where eb.e-num    eq xef.e-num
            and eb.form-no  eq xef.leaf-snum[i]
            and eb.blank-no eq xef.leaf-bnum[i]
          no-lock no-error.
          
      release xstyle.
    
      if avail eb then
      find first xstyle
          where xstyle.company eq cocode
            and xstyle.style   eq eb.style
          no-lock no-error.
        
      if avail eb and avail xstyle then
      do j = 1 to 7:
        if xstyle.m-code[i] eq "" then next.
    
        find first mach
            {&where-machine}
              and mach.m-code eq xstyle.m-code[j]
            no-lock no-error.
        if avail mach and
           not (mach.dept[1] eq v-dept or mach.dept[2] eq v-dept  or
                mach.dept[3] eq v-dept or mach.dept[4] eq v-dept) then do:
          release mach.
        end.
        if avail mach then do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
            release mach.
        end.
        if avail mach then leave.
      end.

      if not avail mach then
      for each mach
          {&where-machine}
            and mach.dept[1] eq v-dept
            and ((xef.leaf-bnum[i] gt 0     and
                  mach.min-len le xeb.t-len and
                  mach.min-wid le xeb.t-wid and
                  mach.max-len ge xeb.t-len and
                  mach.max-wid ge xeb.t-wid) or
                 (xef.leaf-bnum[i] eq 0     and
                  mach.min-len le sh-len  and
                  mach.min-wid le sh-wid  and
                  mach.max-len ge sh-len  and
                  mach.max-wid ge sh-wid))
            and mach.min-cal le xcal
            and mach.max-cal ge xcal
          no-lock:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
      
      if avail mach then do:
        create m-lst.
        assign
         m-lst.f-no   = xef.form-no
         m-lst.b-no   = xef.leaf-bnum[i]
         m-lst.seq    = (10 * mach.d-seq) + i
         m-lst.dept   = v-dept
         m-lst.m-code = mach.m-code
         m-lst.dscr   = mach.m-dscr
         m-lst.bl     = xef.leaf-bnum[i] gt 0.
      end.
    end.
  end. /* 1 to 4 */
end.

