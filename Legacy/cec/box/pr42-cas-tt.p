/* ---------------------------------------------------- cec/box/pr42-cas-tt.p*/

DEFINE INPUT PARAMETER save-qty AS DEC NO-UNDO. 
DEFINE INPUT PARAMETER v-form-no AS INT NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var qty as int NO-UNDO.
def var tmpstore as cha no-undo.
def var v-t-qty as DEC NO-UNDO.
DEF VAR c-qty AS DEC NO-UNDO.
DEF VAR p-qty AS DEC NO-UNDO.

def shared buffer xest for est.
def buffer xef for ef.
def buffer xeb for eb.

DEF BUFFER b-ef FOR ef.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF VAR vmclean2 AS LOG NO-UNDO.
DEF VAR b-wt AS DEC NO-UNDO.
DEF VAR b-uom AS CHAR NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "SETPRINT"
      no-lock no-error.
  
vmclean2 = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld eq "McLean" ELSE NO.

def SHARED TEMP-TABLE cas-2 NO-UNDO
        field    typ        as int
        field    id         as char
        field    snum       as int
        field    bnum       as int
        field    ino        like item.i-no
        field    dscr       like item.est-dscr
        field    t-qty      as int
        field    qty        as int
        field    cosm       as dec format ">>>9.99"
        field    cost       as dec format ">>>,>>9.99".

def buffer xcas-2 for cas-2.

def var v-pallets as log init yes no-undo.
def var v-pck-code as char format "x(12)" initial "Pack Code/Unit" no-undo.
def var v-yld as dec no-undo.
DEF VAR v-setup LIKE e-item-vend.setup NO-UNDO.
DEF VAR b-wt-tot AS DEC NO-UNDO.
DEF VAR ll-unitize AS LOG NO-UNDO.
DEF VAR ld-rm-rate AS DEC NO-UNDO.

/*save-qty = qty.*/

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

ll-unitize = CAN-FIND(FIRST b-eb                /* Set is Unitized */
                      WHERE b-eb.company  EQ xest.company
                        AND b-eb.est-no   EQ xest.est-no
                        AND b-eb.form-no  EQ 0
                        AND b-eb.blank-no EQ 0
                        AND b-eb.tr-no    NE ""
                        AND b-eb.pur-man  EQ YES).

/******************************** C A S E S ****************************/
FOR EACH xef
    WHERE xef.company  EQ xest.company
      AND xef.est-no   EQ xest.est-no
      AND (xef.form-no EQ v-form-no OR (NOT vmclean2))
    NO-LOCK:

  FIND FIRST item
      {sys/look/itemW.i}
        AND item.i-no EQ xef.board
      NO-LOCK NO-ERROR.
  b-wt = IF AVAIL item THEN item.basis-w ELSE 0.

  IF NOT ll-unitize THEN
  FOR EACH xeb
      WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no
      NO-LOCK:

    ASSIGN
     v-yld    = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
     qty      = save-qty * v-yld
     b-wt-tot = (xeb.t-sqin - xeb.t-win) * qty / 144000 * b-wt.

    RUN do-cas-no.
  END.

  IF xef.form-no EQ xest.form-qty THEN
  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.cas-no  NE ""
        AND xeb.form-no EQ 0
      NO-LOCK:

    RUN get-wt.

    ASSIGN
     qty   = save-qty
     v-yld = 1.

    RUN do-cas-no.

    LEAVE.
  END.
END.

for each cas-2 where cas-2.typ = 1 by cas-2.snum by cas-2.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas-2.snum   and
                        xeb.blank-no = cas-2.bnum   no-lock no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas-2.ino
   no-lock no-error.

   cas-2.t-qty = 0.
   FOR EACH xcas-2 WHERE xcas-2.typ EQ 1 AND xcas-2.ino EQ cas-2.ino:
     cas-2.t-qty = cas-2.t-qty + xcas-2.qty.
   END.

   v-setup = 0.

   IF xeb.cas-cost GT 0 THEN cas-2.cost = xeb.cas-cost * cas-2.qty.
      
   ELSE DO:
     {est/matcost.i cas-2.t-qty cas-2.cost 1}

     ASSIGN
      cas-2.cost = (cas-2.cost * cas-2.qty) + lv-setup-1
      v-setup  = lv-setup-1.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   ASSIGN
    cas-2.cosm = cas-2.cost / (cas-2.cosm / 1000).
    
end.

/*************************** T R A Y S **********************************/
for each xeb where xeb.company = xest.company
               and xeb.est-no    eq xest.est-no
               and (xeb.form-no eq v-form-no or (not vmclean2))
          NO-LOCK
          break by xeb.part-no:
   v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty.

   if not first-of(xeb.part-no) then next.
   if xeb.tr-no ne "" then DO:
   
      find first cas-2 where cas-2.typ = 2 and cas-2.id = xeb.part-no no-error.
      if not available cas-2 then do:
         find first item {sys/look/itemW.i} and item.i-no = xeb.tr-no
         no-lock no-error.

         if item.mat-type eq "Z" then v-pallets = no.
         leave.
         find first e-item of item no-lock no-error.
         create cas-2.
         assign
            cas-2.typ  = 2
            cas-2.id   = xeb.part-no
            cas-2.snum = xeb.form-no
            cas-2.bnum = xeb.blank-no
            cas-2.ino  = item.i-no
            cas-2.dscr = item.est-dscr.
      end.
      cas-2.cosm = cas-2.cosm + (save-qty * v-yld).
      find first xcas-2 where xcas-2.typ = 1 and xcas-2.id = xeb.part-no no-error.
      if available xcas-2 then do:
         v-t-qty = xeb.tr-cas * xcas-2.qty.
         {sys/inc/roundup.i v-t-qty}
         cas-2.qty = cas-2.qty + v-t-qty.
      end.
   end.
end.

for each cas-2 where cas-2.typ = 2 by cas-2.snum by cas-2.bnum:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas-2.snum   and
                        xeb.blank-no = cas-2.bnum   no-lock no-error.
   find first item {sys/look/itemW.i}
                     and item.i-no = cas-2.ino no-lock no-error.

   cas-2.t-qty = 0.
   FOR EACH xcas-2 WHERE xcas-2.typ EQ 2 AND xcas-2.ino EQ cas-2.ino:
       cas-2.t-qty = cas-2.t-qty + xcas-2.qty.
   END.

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "M" THEN
     ASSIGN
      cas-2.qty   = cas-2.qty / 1000
      cas-2.t-qty = cas-2.t-qty / 1000.

   v-setup = 0.

   IF xeb.cas-cost GT 0 THEN cas-2.cost = xeb.cas-cost * cas-2.qty.
      
   ELSE DO:
     {est/matcost.i cas-2.t-qty cas-2.cost 2}

     ASSIGN
      cas-2.cost = (cas-2.cost * cas-2.qty) + lv-setup-2
      v-setup  = lv-setup-2.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   cas-2.cosm = cas-2.cost / (cas-2.cosm / 1000).
end.

/**************** P A L L E T S *************************/
IF v-pallets THEN
FOR EACH xef
    WHERE xef.company  EQ xest.company
      AND xef.est-no   EQ xest.est-no
      AND (xef.form-no EQ v-form-no OR (NOT vmclean2))
    NO-LOCK:

  FIND FIRST item
      {sys/look/itemW.i}
        AND item.i-no EQ xef.board
      NO-LOCK NO-ERROR.
  b-wt = IF AVAIL item THEN item.basis-w ELSE 0.

  IF NOT ll-unitize THEN
  FOR EACH xeb
      WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no
        AND xeb.tr-no   NE ""
      NO-LOCK:

    ASSIGN
     v-yld    = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
     qty      = save-qty * v-yld
     b-wt-tot = (xeb.t-sqin - xeb.t-win) * qty / 144000 * b-wt.

    RUN do-tr-no.
  END.

  IF xef.form-no EQ xest.form-qty THEN
  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.tr-no   NE ""
        AND xeb.form-no EQ 0
      NO-LOCK:

    RUN get-wt.
.
    ASSIGN
     qty   = save-qty
     v-yld = 1.

    RUN do-tr-no.

    LEAVE.
  END.
END.

for each cas-2 where cas-2.typ = 3 by cas-2.snum by cas-2.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas-2.snum   and
                        xeb.blank-no = cas-2.bnum   no-lock no-error.
   find first item {sys/look/itemW.i}
                     and item.i-no = cas-2.ino no-lock no-error.

   cas-2.t-qty = 0.
   FOR EACH xcas-2 WHERE xcas-2.typ EQ 3 AND xcas-2.ino EQ cas-2.ino:
      cas-2.t-qty = cas-2.t-qty + xcas-2.qty.
   END.

   v-setup = 0.

   IF xeb.tr-cost GT 0 THEN cas-2.cost = xeb.tr-cost * cas-2.qty.
      
   ELSE DO:
     {est/matcost.i cas-2.t-qty cas-2.cost 3}

     ASSIGN
      cas-2.cost = (cas-2.cost * cas-2.qty) + lv-setup-3
      v-setup  = lv-setup-3.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   cas-2.cosm = cas-2.cost / (cas-2.cosm / 1000).

   if xeb.cas-no ne "" then
     v-pck-code = caps(substr(xeb.cas-no,1,1)) +
                  lc(substr(xeb.cas-no,2,9)) +
                  "s/Unit".

   find first xcas-2
        where xcas-2.typ  = 1 
          and xcas-2.id   = xeb.part-no
          and xcas-2.snum = xeb.form-no
        no-error.

   if cas-2.qty ne 0 then do:
      
      {cec/pr4-str.i}

      if strap-qty ne 0 then do:
         strap-qty = strap-qty * cas-2.qty.

         find first item
             where item.company eq cocode
               and item.i-no    eq strap.code2
             no-lock no-error.

         find first xcas-2 where xcas-2.typ = 4 and xcas-2.id = cas-2.id no-error.
         if not available xcas-2 then do:
            create xcas-2.
            assign
             xcas-2.typ  = 4
             xcas-2.id   = cas-2.id
             xcas-2.snum = cas-2.snum
             xcas-2.bnum = cas-2.bnum
             xcas-2.ino  = item.i-no
             xcas-2.dscr = item.est-dscr.
         end.
         xcas-2.qty = xcas-2.qty + strap-qty.
      end.
   end.
end.

for each cas-2 where cas-2.typ = 4 by cas-2.snum by cas-2.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas-2.snum   and
                        xeb.blank-no = cas-2.bnum   no-lock no-error.

   find first item
       where item.company eq cocode
         and item.i-no    eq cas-2.ino
       no-lock no-error.

   if not avail item then next.

   cas-2.t-qty = 0.
   FOR EACH xcas-2 WHERE xcas-2.typ EQ 4 AND xcas-2.ino EQ cas-2.ino:
      cas-2.t-qty = cas-2.t-qty + xcas-2.qty.
   END.

   strap-qty = cas-2.qty / 1000.

   {est/matcost.i strap-qty cas-2.cost strap}

   cas-2.cost = (cas-2.cost * strap-qty) + lv-setup-strap.
end.

IF CAN-FIND(FIRST cas-2 WHERE cas-2.typ EQ 1 AND cas-2.snum EQ 0) THEN
FOR EACH cas-2 WHERE cas-2.snum NE 0:
  DELETE cas-2.
END.

RETURN.

PROCEDURE do-cas-no:

  /* case */
  if xeb.cas-no ne "" then DO:
    find first item {sys/look/itemW.i} and item.i-no = xeb.cas-no
    no-lock no-error.
    find first e-item of item no-lock no-error.
    find first cas-2
        where cas-2.typ  eq 1
          and cas-2.snum eq xeb.form-no
          and cas-2.bnum eq xeb.blank-no
        no-error.
    if not available cas-2 then do:
      create cas-2.
      assign
       cas-2.typ  = 1
       cas-2.id   = xeb.part-no
       cas-2.snum = xeb.form-no
       cas-2.bnum = xeb.blank-no
       cas-2.ino  = item.i-no
       cas-2.dscr = item.est-dscr.
    end.
    if xeb.cas-cnt ne 0 then c-qty = qty / xeb.cas-cnt.
      /* following if .. do .. end added 9508 by CAH */
    else
    if xeb.cas-cnt = 0 and xeb.cas-wt > 0 and xeb.weight-m > 0 then do:
      def var ws_cas-2-cnt as int no-undo.
      ws_cas-2-cnt = xeb.cas-wt / xeb.weight-m * 1000.
      c-qty = qty / ws_cas-2-cnt. /* number of cas-2es required, by weight */
    end.
    else c-qty = b-wt-tot / item.avg-w.
    {sys/inc/roundup.i c-qty} /* CTS end */
    ASSIGN
    cas-2.qty = cas-2.qty + c-qty
    cas-2.cosm = cas-2.cosm + qty.
  end.

END PROCEDURE.

PROCEDURE do-tr-no:

  /* Pallets */
  find first item {sys/look/itemW.i} and item.i-no = xeb.tr-no
  no-lock no-error.
  find first e-item of item no-lock no-error.

  if xeb.cas-no ne "" then DO:
    find first xcas-2
        where xcas-2.typ  eq 1
          and xcas-2.snum eq xeb.form-no
          and xcas-2.bnum eq xeb.blank-no
        no-error.
    find first cas-2
        where cas-2.typ  eq 3
          and cas-2.snum eq xeb.form-no
          and cas-2.bnum eq xeb.blank-no
        no-error.
    if not available cas-2 then do:
      create cas-2.
      assign
       cas-2.typ  = 3
       cas-2.id   = xeb.part-no
       cas-2.snum = xeb.form-no
       cas-2.bnum = xeb.blank-no
       cas-2.ino  = item.i-no
       cas-2.dscr = item.est-dscr.
    end.
    cas-2.cosm = cas-2.cosm + qty.
    if xeb.cas-pal ne 0 then p-qty = xcas-2.qty / xeb.cas-pal.
    else p-qty = (b-wt-tot + xcas-2.qty) / item.avg-w.
    {sys/inc/roundup.i p-qty}
    cas-2.qty = cas-2.qty + p-qty.
  end.

END PROCEDURE.

PROCEDURE get-wt:

  b-wt-tot = 0.

  FOR EACH b-ef
      WHERE b-ef.company EQ xef.company
        AND b-ef.est-no  EQ xef.est-no
      NO-LOCK:

    FIND FIRST item
        WHERE item.company EQ b-ef.company
          AND item.i-no    EQ b-ef.board
        NO-LOCK NO-ERROR.
    b-wt = IF AVAIL item THEN item.basis-w ELSE 0.

    FOR EACH b-eb FIELDS(yld-qty t-sqin t-win)
        WHERE b-eb.company EQ b-ef.company
          AND b-eb.est-no  EQ b-ef.est-no
          AND b-eb.form-no EQ b-ef.form-no
        NO-LOCK:
      ASSIGN
       v-yld    = IF b-eb.yld-qty LT 0 THEN -1 / b-eb.yld-qty ELSE b-eb.yld-qty
       qty      = save-qty * v-yld
       b-wt-tot = b-wt-tot +
                  ((b-eb.t-sqin - b-eb.t-win) * qty / 144000 * b-wt).
    END.
  END.

END PROCEDURE.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
