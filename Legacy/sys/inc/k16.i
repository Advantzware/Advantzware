/* --------------------------------------------------- sys/inc/k16.i 12/93 cd */
/* Formula to display fractions as 16ths of an inch                           */
/* -------------------------------------------------------------------------- */

round(trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC), IF v-cecscrn-char NE "Decimal" THEN 2 ELSE 6)   /* @ {1} */

/* end ---------------------------------- copr. 1993  advanced software, inc. */

