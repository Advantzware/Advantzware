/* ------------------------------------------------- cec/foammach.i 08/00 JLF */
/* -------------------------------------------------------------------------- */

if avail mach and not v-mach then do:
  v-run = xest.est-qty[1] / v-out[4].
  
  {sys/inc/roundup.i v-run}
  
  if mach.min-len le {1}   and
     mach.max-len ge {1}   and

     mach.min-wid le {2}   and
     mach.max-wid ge {2}   and

     mach.min-cal le {3}   and
     mach.max-cal ge {3}   and

     mach.min-run le v-run and
     mach.max-run ge v-run then v-mach = yes.
end.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
