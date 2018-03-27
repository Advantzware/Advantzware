/* -------------------------------------------------- sys/inc/k16b.i 12/93 cd */
/* Formula to convert fractions in 16ths into decimal values                  */
/* -------------------------------------------------------------------------- */

{1} = trunc({1},0) + (({1} - trunc({1},0)) * K_FRAC).

/* end ---------------------------------- copr. 1993  advanced software, inc. */

