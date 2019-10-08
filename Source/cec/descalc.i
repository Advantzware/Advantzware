/* -------------------------------------------------- cec/descalc.i 11/95 BSM */
/* Box Design Calculation shared variables                                    */
/* -------------------------------------------------------------------------- */

def {1} shared var v-lscore-c     like box-design-hdr.lscore     no-undo.
def {1} shared var v-lcum-score-c like box-design-hdr.lcum-score no-undo.
def {1} shared var v-num-lines    as   int no-undo.


def {1} shared TEMP-TABLE w-box-design-line no-undo
  field line-no         like box-design-line.line-no
  field wscore-c        as   char format "x(06)"
  field wscore-d        as   dec decimals 6 format "999.99"
  field wcum-score-c    as   char format "x(06)"
  field wcum-score-d    as   dec decimals 6 format "999.99"
  field wscore-fld-id   as   int.

DEF {1} SHARED VAR lv-k-wid-scr-type LIKE eb.k-wid-scr-type2 NO-UNDO.
DEF {1} SHARED VAR lv-k-len-scr-type LIKE eb.k-len-scr-type2 NO-UNDO.

/*Hack just to try to pass variables outside of box design strings for est.metric flag
Extents set to match nonsensical variables in descalc.p
This whole thing needs to be overhauled - BV*/
DEFINE {1} SHARED VARIABLE dPanelsLength AS DECIMAL EXTENT 30 NO-UNDO.  
DEFINE {1} SHARED VARIABLE dPanelsWidth AS DECIMAL EXTENT 50 NO-UNDO.
/* end ---------------------------------- copr. 1995  advanced software, inc. */
