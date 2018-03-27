
/*if can-find(first vend where vend.company eq cocode
                         and vend.vend-no eq po-ord.vend-no
                         and not vend.an-edi-vend) or
   not v-poexport-log                              or
   not v-corrugator                                or
   v-poexport-dir eq ""                            then*/ po-ord.printed = yes.
