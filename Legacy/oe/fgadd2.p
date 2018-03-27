/* ---------------------------------------------------- oe/fgadd2.p 07/93 cd  */
/* Order entry lines - 2Pc. Box Add fgitem                                    */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest    for est.
def shared buffer xeb     for eb.
def shared buffer xef     for ef.
def shared buffer xoe-ord for oe-ord.

def var v-item-no    like itemfg.i-no NO-UNDO.
def var v-item-part  like eb.part-no NO-UNDO.
def var v-item-name  like itemfg.i-name NO-UNDO.
def var v-item-dscr1 like itemfg.part-dscr1 NO-UNDO.
def var v-item-dscr2 like itemfg.part-dscr2 NO-UNDO.
def var v-part-qty   as dec NO-UNDO.
def var v-tot-qty    like oe-ordl.qty NO-UNDO.
def var v-item-price like oe-ordl.price NO-UNDO.
def var v-eb-part    as ch format "x(20)" NO-UNDO.
DEF VAR ll-one-part  AS LOG NO-UNDO.
DEF VAR lv-set-part  LIKE eb.part-no NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

DEF SHARED VAR fil_id AS RECID NO-UNDO.
DEF SHARED VAR s-est-no AS cha NO-UNDO.
def shared var v-i-item like oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
def shared var v-i-qty like oe-ordl.qty NO-UNDO.   /* INPUT QUANTITY */

DEF BUFFER bf-eb FOR eb.
DEF BUFFER x-eb FOR eb.
/*
form "Est.#" to 12 oe-ordl.est-no
     "Cust. Part #" to 45 v-item-part skip
     "Item #" to 12 v-item-no
     "Item Name" to 45 v-item-name  skip
     "Qty" to 12 v-tot-qty format ">>,>>>,>>9"
     "Item Dscr" to 45 v-item-dscr1 skip
     "Price" to 12 v-item-price format ">>,>>>,>>9.99"
     "UOM" oe-ordl.pr-uom  format "X(3)"
     "     Dscr" to 45 v-item-dscr2  skip(3)
    with title color value(col-warn) v-eb-part frame eb-part
         row 13 overlay no-labels width 80 no-attr-space
         color value(col-bg) prompt value(col-input).

*/

{oe/oe-sysct1.i NEW}

{ce/msfcalc.i}
    
{sys/inc/f16to32.i}
{oe/fgfreight.i}

DO WITH TRANSACTION:
   {sys/inc/graphic.i}
END.

RUN oe/oe-sysct.p.

find oe-ordl where recid(oe-ordl) eq fil_id.

outers:
do on error undo:
  FIND FIRST bf-eb
      WHERE bf-eb.company EQ oe-ordl.company
        AND bf-eb.est-no  EQ s-est-no
        AND bf-eb.form-no EQ 0
      NO-LOCK NO-ERROR.
  lv-set-part = IF AVAIL bf-eb THEN bf-eb.part-no ELSE "".

  for each bf-eb where bf-eb.company = oe-ordl.company
                AND bf-eb.est-no eq s-est-no /*oe-ordl.est-no */
                AND bf-eb.part-no ne ""
                AND bf-eb.part-no NE lv-set-part,
      first xef where xef.company = bf-eb.company
                  AND xef.est-no   eq bf-eb.est-no
                  and xef.form-no eq bf-eb.form-no no-lock
      break by bf-eb.form-no
            by bf-eb.blank-no:

    release itemfg.
    if bf-eb.stock-no eq "" then do:
      v-item-no = bf-eb.part-no.
      IF v-est-fg1 EQ "Hughes" THEN DO:
        RUN fg/hughesfg.p (ROWID(bf-eb), OUTPUT v-item-no).
        SUBSTR(v-item-no,1,12) = SUBSTR(oe-ordl.i-no,1,12).
      END.
      ELSE
      IF v-est-fg1 EQ "Fibre" THEN DO:
        RUN fg/fibre-fg.p (ROWID(bf-eb), OUTPUT v-item-no).
        SUBSTR(v-item-no,1,LENGTH(TRIM(oe-ordl.i-no)) - 1) =
            SUBSTR(oe-ordl.i-no,1,LENGTH(TRIM(oe-ordl.i-no)) - 1).
      END.
      ELSE IF can-do("Manual,None,Hold",v-est-fg1)  THEN.
      ELSE do:
              
              RUN fg/autofg.p ( ROWID(bf-eb),
                                  v-est-fg1, 
                                  bf-eb.procat,
                                  IF xest.est-type LE 4 THEN "F" ELSE "C",
                                  bf-eb.cust-no,
                                  OUTPUT v-item-no).
              SUBSTR(v-item-no,1,12) = SUBSTR(oe-ordl.i-no,1,12).
      END.
    END.
    else do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq bf-eb.stock-no
          no-lock no-error.
      v-item-no = bf-eb.stock-no.
    end.

    if bf-eb.form-no gt 1 or bf-eb.blank-no gt 1 then
      v-eb-part = "  " + v-item-no + " - Lid  ".
    else
      v-eb-part = "  " + v-item-no + " - Bottom  ".

    if not avail itemfg then        /** NEW FINISHED GOOD ITEM ADDED **/
    do with frame eb-part:
      hide frame eb-part no-pause.
      assign
       v-item-part  = bf-eb.part-no
       v-item-name  = bf-eb.part-dscr1
       v-item-dscr1 = bf-eb.part-dscr2
       v-item-dscr2 = ""
       v-item-price = 0
       v-part-qty   = if bf-eb.cust-% ne 0 then bf-eb.cust-% else bf-eb.quantityPerSet.

      if v-part-qty lt 0 then v-part-qty = -1 / v-part-qty.

      v-tot-qty = oe-ordl.qty * v-part-qty.

      if bf-eb.form-no gt 0 then
        assign
         bf-eb.part-no    = v-item-part
         bf-eb.stock-no   = v-item-no
         bf-eb.part-dscr1 = v-item-name
         bf-eb.part-dscr2 = v-item-dscr1.
      else
        assign
         bf-eb.part-no    = oe-ordl.part-no
         bf-eb.stock-no   = oe-ordl.i-no
         bf-eb.part-dscr1 = oe-ordl.i-name
         bf-eb.part-dscr2 = oe-ordl.part-dscr1.

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq v-item-no
          exclusive-lock no-error.
      if not avail itemfg then
      DO:
         CREATE itemfg.

         IF v-graphic-char NE "" THEN 
         DO:
            IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
               v-graphic-char = v-graphic-char + "\".
         
            IF SEARCH(v-graphic-char + v-item-no + ".jpg") NE ? THEN
               itemfg.box-image = v-graphic-char + v-item-no + ".jpg".
         END.
      END.

      assign
       itemfg.company     = cocode
       itemfg.loc         = locode
       itemfg.job-date    = today
       itemfg.i-no        = v-item-no
       itemfg.q-ord       = v-tot-qty
       itemfg.est-no      = s-est-no /*oe-ordl.est-no*/
       itemfg.i-name      = v-item-name
       itemfg.part-dscr1  = v-item-dscr1
       itemfg.part-dscr2  = v-item-dscr2
       itemfg.sell-uom    = oe-ordl.pr-uom
       itemfg.part-no     = v-item-part
       itemfg.cust-po-no  = oe-ordl.po-no
       itemfg.request-date = oe-ordl.req-date
       itemfg.cust-no     = xoe-ord.cust-no
       itemfg.cust-name   = xoe-ord.cust-name
       itemfg.cust-job-no = string(xoe-ord.job-no) + "-" +
                            string(xoe-ord.job-no2)
       itemfg.pur-uom     = IF bf-eb.pur-man AND bf-eb.form-no GT 0 THEN "EA" ELSE "M"
       itemfg.prod-uom    = IF bf-eb.pur-man AND bf-eb.form-no GT 0 THEN "EA" ELSE "M"
       itemfg.stocked     = yes
       itemfg.die-no      = bf-eb.die-no
       itemfg.procat      = bf-eb.procat
       itemfg.plate-no    = bf-eb.plate-no
       itemfg.style       = bf-eb.style
       itemfg.cad-no      = bf-eb.cad-no
       itemfg.upc-no      = bf-eb.upc-no
       itemfg.spc-no      = bf-eb.spc-no
       itemfg.isaset      = no 
       itemfg.pur-man     = bf-eb.form-no GT 0 AND bf-eb.pur-man
       itemfg.alloc       = bf-eb.set-is-assembled
       itemfg.setupDate   = TODAY.

      /* Create an itemfg-loc for the default warehouse */
      RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

      {oe/fgfreighta.i bf-eb}


      IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

      {fg/set-inks1.i itemfg bf-eb}
       
      {sys/inc/fgcascnt.i itemfg bf-eb} 

      {sys/inc/updfgdim.i "bf-eb"}

      find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
      itemfg.i-code = if avail oe-ctrl and oe-ctrl.i-code then "S" else "C".

      RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT bf-eb.loc).

    end. /* not avail itemfg */
  end. /* each eb */
                                 /** CREATE FINISHED GOODS SET PARTS **/
  {fg/addset.i oe-ordl.i-no}
/* Wade Kaldawi   3/9/16
   Ticket 13466, ll-on-part should not change itemfg.alloc */
/*   IF ll-one-part THEN DO: */
/*     FIND FIRST itemfg */
/*         WHERE itemfg.company EQ cocode */
/*           AND itemfg.i-no    EQ oe-ordl.i-no */
/*         EXCLUSIVE NO-ERROR. */
/*     IF AVAIL itemfg THEN itemfg.alloc = YES. */
/*     FIND CURRENT itemfg NO-LOCK. */
/*     RELEASE itemfg. */
/*   END. */

  hide frame eb-part no-pause.
end.


/* end ---------------------------------- copr. 1993  advanced software, inc. */
