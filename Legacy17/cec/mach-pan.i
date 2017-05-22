
if v-cepanel-log and style.type eq "B" then do:
  assign
   v-int   = 1
   v-panel = 1
   v-prev  = ""
   v-next  = ""
   v-dec   = 0.

  if v-cepanel-cha eq "WminLmin" then
  do while avail mach and v-panel le 20 and v-int le length(style.formula[2]):
    assign
     v-prev = if v-int eq 1 then "+"
              else substr(style.formula[2],v-int - 1,1)
    
     v-next = if v-int eq length(style.formula[2]) then "+"
              else substr(style.formula[2],v-int + 1,1).
              
    if v-prev eq "" then v-prev = "+".
    if v-next eq "" then v-next = "+".
      
    if substr(style.formula[2],v-int,1) eq "+" then v-panel = v-panel + 1.
  
    else
    if xeb.k-len-array2[v-panel] ne 0                     and
       v-prev eq "+"                                      and
       v-next eq "+"                                      then do:
    
      if substr(style.formula[2],v-int,1) eq "l"    and
         xeb.k-len-array2[v-panel] lt mach.min-pan-l then tt-mach-exc.reason = "Minimum Score Length". 
        
      else
      if substr(style.formula[2],v-int,1) eq "w"    and
         xeb.k-len-array2[v-panel] lt mach.min-pan-w then tt-mach-exc.reason = "Minimum Score Width".
    end.
    
    v-int = v-int + 1.

    if tt-mach-exc.reason ne "" then release mach.
  end.

  else do:
    do while avail mach and v-panel le 20 and v-int le length(style.formula[2]):
      assign
       v-prev = if v-int eq 1 then "+"
                else substr(style.formula[2],v-int - 1,1)
    
       v-next = if v-int eq length(style.formula[2]) then "+"
                else substr(style.formula[2],v-int + 1,1).
              
      if v-prev eq "" then v-prev = "+".
      if v-next eq "" then v-next = "+".
 
      if substr(style.formula[2],v-int,1) eq "+" then v-panel = v-panel + 1.
  
      else
      if substr(style.formula[2],v-int,1) ne "J" then do:
        v-dec = xeb.k-len-array2[v-panel].

        if v-dec ne 0 and v-prev eq "+" and v-next eq "+" then
          if v-dec lt mach.min-pan-w then
            tt-mach-exc.reason = "Minimum Panel (Head-Head)".

          else
          if v-dec gt mach.max-pan-w then
            tt-mach-exc.reason = "Maximum Panel (Head-Head)".
      end.

      v-int = v-int + 1.
       
      if tt-mach-exc.reason ne "" then release mach.
    end.

    assign
     v-int   = 1
     v-panel = 1
     v-prev  = ""
     v-next  = ""
     v-dec   = xeb.k-wid-array2[1].

    do while avail mach and v-panel le 2 and v-int le length(style.formula[1]):
      assign
       v-prev = if v-int eq 1 then "+"
                else substr(style.formula[1],v-int - 1,1)
    
       v-next = if v-int eq length(style.formula[1]) then "+"
                else substr(style.formula[1],v-int + 1,1).
              
      if v-prev eq "" then v-prev = "+".
      if v-next eq "" then v-next = "+".

      if substr(style.formula[1],v-int,1) eq "+" then
        assign
         v-panel = v-panel + 1
         v-dec   = xeb.k-wid-array2[v-panel] + v-dec.

      if v-dec ne 0 and v-panel eq 2 and v-prev eq "+" and v-next eq "+" then
        if v-dec lt mach.min-pan-l then                  
          tt-mach-exc.reason = "Minimum Slot/Score Panel".

        else
        if v-dec gt mach.max-pan-l then                  
          tt-mach-exc.reason = "Maximum Slot/Score Panel".

      v-int = v-int + 1.

      if tt-mach-exc.reason ne "" then release mach.
    end.
  end.
end.
