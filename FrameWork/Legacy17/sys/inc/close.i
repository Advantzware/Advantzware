/* sys/inc/close.i */

if lookup(printer.pr-port,"TERMINAL,FAX_MODEM,FILE") gt 0 then do:
  output {2} close.
  hide all no-pause.

  if printer.pr-port eq "TERMINAL" then run sys/inc/screen.p (input v-filname).
  else
  if printer.pr-port eq "FAX" then run sys/inc/faxprint.p (input v-filname).
end.

else do:
  put {2} control v-end-compress. /* restore printer pitch */
  output {2} close.

  run sys/inc/print.p (printer.pr-port).
end.

prt-copies = 1.
