
def var v-set           like itemfg.i-no.
def var v-comp          like itemfg.i-no.
def var v-part-onh      as   int.
def var v-part-alloc    as   int.
def var v-part-set      as   int.

def {1} shared frame checkset.

form v-comp                                 label "Component"
     v-part-alloc    format "->,>>>,>>9"    label "Allocated"
     v-part-onh      format "->,>>>,>>9"    label "On Hand"
     v-part-set      format "->,>>>,>>9"    label "Sets"
     
    with frame checkset no-attr-space 10 down row 2 column 31 overlay
         title "  Set: " + trim(caps(v-set)) + " / Components  ".
