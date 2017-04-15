/* --------------------------------------------------- oe/surchrg.p 06/01 JLF */
/* ORDER ENTRY MODULE - Create surcharges for invoice                         */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}


def var v-surchrg as dec.

find inv-head WHERE ROWID(inv-head) EQ ip-rowid NO-LOCK.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVPRINT"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "INVPRINT"
   sys-ctrl.descrip = "Print Invoice Headers on Invoice Form".
  message "Invoice Format:" update sys-ctrl.char-fld.
end.

if sys-ctrl.char-fld eq "TRIAD" then do transaction:  /* Fuel Surcharge */
  v-surchrg = 0.

  for each inv-line
      where inv-line.r-no eq inv-head.r-no
        and not can-find(first itemfg where itemfg.company eq cocode
                                        and itemfg.i-no    eq inv-line.i-no
                                        and itemfg.procat  eq "FS")
      no-lock,
      
      first oe-bolh where oe-bolh.b-no eq inv-line.b-no no-lock,
    
      first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq oe-bolh.cust-no
        and shipto.ship-id eq oe-bolh.ship-id
      no-lock:
    
    if shipto.del-chg ne 0 then
      v-surchrg = v-surchrg + (inv-line.t-price * shipto.del-chg / 100). 
  end.
     
  find first inv-line
      where inv-line.r-no eq inv-head.r-no
        and can-find(first itemfg where itemfg.company eq cocode
                                    and itemfg.i-no    eq inv-line.i-no
                                    and itemfg.procat  eq "FS")
      no-error.
      
  if v-surchrg ne 0 then do:
    if not avail inv-line then do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.procat  eq "FS"
          no-lock no-error.
        
      if avail itemfg then do:
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq inv-head.bill-to
            no-lock no-error.
    
        create inv-line.
      
        assign
         inv-line.company    = cocode
         inv-line.r-no       = inv-head.r-no
         inv-line.i-no       = itemfg.i-no
         inv-line.cas-cnt    = 1
         inv-line.stat       = "B"
         inv-line.cust-no    = inv-head.cust-no
         inv-line.ord-date   = today
         inv-line.part-no    = itemfg.part-no
         inv-line.i-name     = itemfg.i-name
         inv-line.i-dscr     = itemfg.i-dscr
         inv-line.pr-uom     = "EA"
         inv-line.part-dscr1 = itemfg.part-dscr1
         inv-line.part-dscr2 = itemfg.part-dscr2
         inv-line.tax        = avail cust and cust.sort eq "Y" and
                               inv-head.tax-gr ne "" and
                               itemfg.taxable
         inv-line.qty        = 1
         inv-line.inv-qty    = 1
         inv-line.p-c        = yes.
      end.
    end.
  
    if avail inv-line then
      assign
       inv-line.t-price = v-surchrg
       inv-line.price   = inv-line.t-price.
  end.
    
  else
  if avail inv-line then delete inv-line.
end.

/* end --------------------------------- copyright 2001 Advanced Software Inc.*/
