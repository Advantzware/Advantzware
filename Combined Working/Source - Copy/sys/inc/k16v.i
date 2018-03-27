/* -------------------------------------------------- sys/inc/k16v.i 12/93 cd */
/* Formula to display fractions as 16ths of an inch (without frame def)       */
/* -------------------------------------------------------------------------- */

trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC)

/* end ---------------------------------- copr. 1993  advanced software, inc. */
