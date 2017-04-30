if v-msg eq ""                                                      and
   can-find(first {&fil} where {&fil}.company eq itemfg.company
                           and {&fil}.{&fld}  eq itemfg.i-no)       then
                           
  v-msg = "FG still exists in " + trim("{&msg}") + " file, cannot delete...".
                           
   
   
