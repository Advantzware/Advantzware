/* ----------------------------------------------- sys/inc/k16bv.i 11/95 BSM  */
/* Formula to convert fractions in 16ths into decimal values                  */
/* -------------------------------------------------------------------------- */

trunc({1},0) + (({1} - trunc({1},0)) * K_FRAC)

/* end ---------------------------------- copr. 1995  advanced software, inc. */
