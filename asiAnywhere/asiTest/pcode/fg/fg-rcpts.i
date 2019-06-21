/* --------------------------------------------------- fg/fg-rcpts.i 11/93 rd */
/* Finished Goods - Warehouse Transactions Posting                            */
/*   Create Transaction History Records.                                      */
/* -------------------------------------------------------------------------- */

ASSIGN
  {1}.r-no       = {2}.r-no
  {1}.company    = {2}.company
  {1}.loc        = {2}.loc
  {1}.trans-date = {2}.rct-date
  {1}.po-no      = {2}.po-no
  {1}.i-no       = {2}.i-no
  {1}.i-name     = {2}.i-name
  {1}.job-no     = {2}.job-no
  {1}.job-no2    = {2}.job-no2
  {1}.pur-uom    = {2}.pur-uom
  {1}.rita-code  = {2}.rita-code
  {1}.post-date  = v-post-date.

RELEASE oe-bolh.

IF {2}.bol-no NE 0 THEN
FIND FIRST oe-bolh NO-LOCK
    WHERE oe-bolh.company EQ {2}.company
      AND oe-bolh.bol-no  EQ {2}.bol-no
    NO-ERROR.

IF AVAIL oe-bolh THEN {1}.b-no = oe-bolh.b-no.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

