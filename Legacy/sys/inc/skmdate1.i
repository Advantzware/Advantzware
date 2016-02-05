/* --------------------------------------------- sys/inc/skmdate1.i 06/97 JLF */
/*  skim through dates                                                        */
/* -------------------------------------------------------------------------- */

if lookup(keyfunction(lastkey),"+,page-down") ne 0 then do:
  {&date} = {&date} + 1.
  if {&date} eq ? then {&date} = today.
  display {&date}.
  next.
end.

if lookup(keyfunction(lastkey),"-,page-up") ne 0 then do:
  {&date} = {&date} - 1.
  if {&date} eq ? then {&date} = today.
  display {&date}.
  next.
end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
