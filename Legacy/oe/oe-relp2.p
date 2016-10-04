/* -------------------------------------------------- oe/oe-relp2.p 06/98 JLF */
/* ORDER ENTRY MODULE - O/E RELEASE POST                                      */
/* -------------------------------------------------------------------------- */

def input parameter v-term  as char.
def input parameter v-royal as log.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def new shared buffer xoe-relh for oe-relh.

DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER bf-rell FOR oe-rell.

{oe/oe-relp1.i}

def var v-ship-lu as ch initial ["I,S,B"] no-undo.
def var v-ship-no as INT no-undo.
def var save_id as RECID no-undo.
def var time_stamp as ch no-undo.
def var v-s-code as CHAR no-undo.
def var v-first-release as LOG no-undo.
def var v-r-no like inv-head.r-no no-undo.
def var v-ext-price like inv-line.t-price no-undo.
def var v-nxt-r-no as INT no-undo.
def var v-po-no like oe-rel.po-no no-undo.
def var v-n-bol like oe-ctrl.n-bol no-undo.
def var v-bol-qty like oe-boll.qty no-undo.
def var temp-tax as dec init 0 no-undo.
def var v-relpost-hld as char no-undo.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR ll-exception AS LOG NO-UNDO.
DEF VAR lv-cust-x LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR vfrt-pay AS CHAR NO-UNDO.
DEF VAR vfob-code AS CHAR NO-UNDO.
DEF VAR vfrt-list AS CHAR NO-UNDO.
DEF VAR vfob-list AS CHAR NO-UNDO.
DEF VAR rell-ctr AS INTE NO-UNDO.

DEF VAR  v-chkflg AS LOG NO-UNDO.

DO TRANSACTION:
  {sys/ref/relpost.i}
  {sys/inc/boldate.i}

   /* gdm - 03110902 */
   {sys/inc/relcrhold.i}
   /* gdm - 03110902 */

END.

ASSIGN
   v-relpost-hld = relpost-chr
   lv-bol-no     = 0
   lv-cust-x = "".

FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.

headblok:
for each oe-relh no-lock
    where oe-relh.company  eq cocode
      and oe-relh.posted   eq no
      and oe-relh.printed  eq yes
      and oe-relh.release# ge v-frel
      and oe-relh.release# le v-trel
      and oe-relh.rel-date ge v-fdat
      and oe-relh.rel-date le v-tdat
      and oe-relh.cust-no  ge v-fcus
      and oe-relh.cust-no  le v-tcus
    /* gdm - 03110907 */
      AND (NOT v-chkflg OR 
           (v-chkflg AND 
            oe-relh.w-ord EQ NO))
   /* gdm - 03110907 end */
      and not can-find(first oe-rell
                       where oe-rell.company eq oe-relh.company
                         and oe-rell.r-no    eq oe-relh.r-no
                         and (oe-rell.ord-no lt v-ford or
                              oe-rell.ord-no gt v-tord)
                       USE-INDEX r-no)
    use-index post

  {oe/oe-relp2.i}
  
  FOR EACH oe-rell where oe-rell.company eq oe-relh.company
                     and oe-rell.r-no    eq oe-relh.r-no
                       USE-INDEX r-no
                   NO-LOCK,
    EACH oe-ordl WHERE oe-ordl.company EQ oe-relh.company
                   AND oe-ordl.ord-no EQ oe-rell.ord-no
      EXCLUSIVE-LOCK:

      RUN oe/ordlsqty.p (ROWID(oe-ordl),
                     OUTPUT oe-ordl.inv-qty,
                     OUTPUT oe-ordl.ship-qty).

        if oe-rell.link-no eq 0 then do:
          find first oe-rel
              where oe-rel.company  eq oe-rell.company
                and oe-rel.ord-no   eq oe-rell.ord-no
                and oe-rel.line     eq oe-rell.line
                and oe-rel.i-no     eq oe-rell.i-no
                and oe-rel.ship-id  eq oe-relh.ship-id
                and oe-rel.link-no  eq 0
              no-error.
        
          if not avail oe-rel then
          find first oe-rel
              where oe-rel.company  eq oe-rell.company
                and oe-rel.ord-no   eq oe-rell.ord-no
                and oe-rel.line     eq oe-rell.line
                and oe-rel.i-no     eq oe-rell.i-no
                and oe-rel.link-no  eq 0
              no-error.
        end.
        
        else
        find first oe-rel
            where oe-rel.r-no eq oe-rell.link-no
            use-index seq-no no-error.

        if avail oe-rel THEN DO:
          /*if not avail shipto then*/
            RUN oe/custxship.p (oe-relh.company,
                                      oe-relh.cust-no,
                                      oe-relh.ship-id,
                                      BUFFER shipto).

            if avail shipto and avail oe-rel then
              assign
               oe-rel.ship-addr[1] = shipto.ship-addr[1]
               oe-rel.ship-addr[2] = shipto.ship-addr[2]
               oe-rel.ship-city    = shipto.ship-city
               oe-rel.ship-state   = shipto.ship-state
               oe-rel.ship-zip     = shipto.ship-zip.
        END.
  END.
  
  STATUS DEFAULT "Posting Release# " +
                 TRIM(STRING(oe-relh.release#,">>>>>>>>>>")).
end.

STATUS DEFAULT "".

/* end --------------------------------- copyright 1993 Advanced Software Inc.*/
