/* cec/board.i */

do:
   {1}.test = item.reg-no.
   /*display item.ect
           caps(item.i-no)            @ {1}.board
           item.est-dscr              @ {1}.brd-dscr
           item.i-code                @ {1}.i-code
        /* "" when {2}.style ne "999" @ {1}.roll-wid*/
           item.flute                 @ {1}.flute
           item.reg-no                @ {1}.test
           item.basis-w               @ {1}.weight
           "" when {2}.style ne "999" @ {1}.gsh-len
           "" when {2}.style ne "999" @ {1}.gsh-wid
           "" when {2}.style ne "999" @ {1}.nsh-len
           "" when {2}.style ne "999" @ {1}.nsh-wid
           "" when {2}.style ne "999" @ {1}.trim-w
           "" when {2}.style ne "999" @ {1}.trim-l.
   */
   
   assign {1}.board = caps(item.i-no)
    {1}.flute    = item.flute
    {1}.brd-dscr = item.i-name.

   if item.i-code eq "R" then do:
      if item.r-wid ne 0
      then assign {1}.gsh-wid  = item.r-wid
                  {1}.lsh-len  = item.r-wid
                  {1}.roll-wid = item.r-wid
                  {1}.roll     = true.
      else assign {1}.gsh-wid  = item.s-wid
                  {1}.gsh-len  = item.s-len
                  {1}.lsh-len  = item.s-len
                  {1}.lsh-wid  = item.s-wid
                  {1}.roll-wid = 0
                  {1}.roll     = no.
                  
      {1}.gsh-dep = item.s-dep.
  
   end.
   else {1}.roll-wid = 0.
   /*
   display /*{1}.roll*/
           {sys/inc/k16.i {1}.lsh-wid}
           {sys/inc/k16.i {1}.lsh-len}
           {sys/inc/k16.i {1}.gsh-wid}
           {sys/inc/k16.i {1}.gsh-len}
           {sys/inc/k16.i {1}.nsh-wid}
           {sys/inc/k16.i {1}.nsh-len}
           {sys/inc/k16.i {1}.trim-w}
           {sys/inc/k16.i {1}.trim-l}
           {1}.cost-uom.
   */        
end.
