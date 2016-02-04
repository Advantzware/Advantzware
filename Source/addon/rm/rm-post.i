/* --------------------------------------------------- rm/rm-post.i 10/97 JLF */
/* Rm Posting - Calculate new average cost for the bin                        */
/* -------------------------------------------------------------------------- */

    assign
     v-r-qty = {2}
     v-i-qty = {1}.qty.

    if (v-r-qty + v-i-qty lt 0) and v-i-qty gt 0 then v-r-qty = v-r-qty * -1.
    if (v-r-qty + v-i-qty gt 0) and v-i-qty lt 0 then v-i-qty = v-i-qty * -1.

    assign
     v-t-qty  = v-i-qty + v-r-qty
     {1}.cost = ((v-i-qty * {1}.cost) + (v-r-qty * {3})) / v-t-qty.

    if {1}.cost lt 0 then {1}.cost = {1}.cost * -1.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
