/* -------------------------------------------------- cec/descalc.i 11/95 BSM */
/* Box Design Calculation shared variables                                    */
/* -------------------------------------------------------------------------- */

def {1} shared var v-lscore-c     like box-design-hdr.lscore     no-undo.
def {1} shared var v-lcum-score-c like box-design-hdr.lcum-score no-undo.
def {1} shared var v-num-lines    as   int no-undo.

def {1} shared workfile w-box-design-line no-undo
  field line-no         like box-design-line.line-no
  field wscore-c        as   char format "x(06)"
  field wscore-d        as   dec decimals 2 format "999.99"
  field wcum-score-c    as   char format "x(06)"
  field wcum-score-d    as   dec decimals 2 format "999.99"
  field wscore-fld-id   as   int.

/* end ---------------------------------- copr. 1995  advanced software, inc. */

