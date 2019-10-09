/* -------------------------------------------------- po/po-brick.p 03/00 DAV */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Brick Container Corporation    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{po/po-print.i}

def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var pol-counter as int no-undo. /* CTS */
def var save_id as recid.
def var time_stamp as char.
def var v-inst-lines as int.
def var v-lastpage-num as int.
def var v-sname like shipto.ship-name.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-job as char format "x(9)".
def var v-po-tot like po-ord.t-cost.
def var v-sqft as dec.
def var v-tot-sqft as dec.
def var v-ratio as dec.
def var xg-flag as log initial no no-undo.
def var ctr as int.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as char no-undo.
def var v-test-scr as log no-undo.
def var v-change-dscr as char format "x(7)" no-undo.
def var v-change-ord as char format "x(35)" no-undo.

def var len-score as char.
def var v-space as log initial yes.
def var v-adder like item.i-no extent 2 no-undo.
def var v-num-add as int initial 0 no-undo.
def var v-counter as int initial 0 no-undo.

{ce/msfcalc.i}

def var v-city-state-zip as char format "x(28)".
def var p-city-state-zip as char format "x(27)".
def var v-net-days       as char format "x(12)".
def var v-date-ordered   as char format "x(10)".
def var v-date-required  as char format "x(10)".
def var v-cost           like po-ordl.cost.
def var v-print-lines    as int.
def var v-lines          as int.
def var v-ord-qty        as integer format ">>>,>>9".

def var v-months as char init "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

form
  skip(3)
  /*skip(4)*/
  po-ord.po-no          at 65

  skip(7)
  /*skip(6)*/

  vend.name             at 11          po-ord.ship-name         at 50
  skip(1)
  vend.add1             at 11          po-ord.ship-addr[1]      at 50 skip
  vend.add2             at 11          po-ord.ship-addr[2]      at 50 skip
  v-city-state-zip      at 11          p-city-state-zip         at 50
  
  skip(4)
  v-date-ordered        at 3     /*10 chars*/       /*DATE ORDERED*/
  v-date-required       at 14    /*10 chars*/       /*DATE REQUIRED*/
  oe-rel.po-no          at 27    /*15 chars*/       /*CUST PO*/
  po-ord.ship-id        at 42    /*8 chars*/        /*FOR*/
  v-net-days            at 57    /*12 chars*/       /*TERMS*/
  po-ord.carrier        at 70    /*5 chars*/        /*SHIP VIA*/
  skip(5)
  
with frame hdr no-box stream-io width 80 no-labels.

form
  v-ord-qty         to 8  /*11*/           
  po-ordl.i-name    at 14           /*30 chars*/
  v-cost            to 64           /*15 places*/
  po-ordl.pr-uom    to 69           /*4 chars*/                     skip
  po-ordl.dscr[1]   at 14           /*30 chars, array of [2]*/      skip
  
with frame polines down no-box stream-io width 80 no-labels.


find first po-ctrl where po-ctrl.company eq cocode no-lock no-error.

print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
    break by po-ord.po-no:

  {po/exportpo.i}

  find first vend
      where vend.company eq cocode
        and vend.vend-no eq po-ord.vend-no
      no-lock no-error.

  find first terms
      where terms.company eq cocode
        and terms.t-code  eq po-ord.terms
      no-lock no-error.

  find first oe-rel
      where oe-rel.company eq cocode
        and oe-rel.po-no   eq string(po-ord.po-no)
      no-lock no-error.

  run print-header.
        
  for each po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no EQ po-ord.po-no no-lock with frame polines:
    assign
     v-cost    = trunc(po-ordl.cost,2)
     v-ord-qty = int(po-ordl.ord-qty)
     v-lines   = 2.
     
    if v-print-sn then do:
      ctr = 0.
      for each item-spec
          where item-spec.company   eq cocode
            and item-spec.i-no      eq po-ordl.i-no
            and item-spec.item-type eq po-ordl.item-type
          no-lock:
        do i = 1 to 4:
          if item-spec.notes[i] ne "" then ctr = ctr + 1.
        end.
      end.
      if ctr gt 0 then ctr = ctr + 1.
      
      if ctr gt 5 then ctr = 5.
    
      v-lines = v-lines + ctr.
    end.
         
    ctr = 0.
    do i = 1 to 4:
      if po-ordl.spec-i[i] ne "" then ctr = ctr + 1.
    end.
    if ctr gt 0 then ctr = ctr + 1.
    
    v-lines = v-lines + ctr.
    
    if v-print-lines + v-lines gt 26 then do:
      page.
      run print-header.
      v-print-lines = 0.
    end.
    
    v-print-lines = v-print-lines + v-lines.
      
    display v-ord-qty
            po-ordl.i-name
            po-ordl.dscr[1]
            v-cost
            po-ordl.pr-uom.
    down.
    
    if v-print-sn then do:
      ctr = 0.
      for each item-spec
          where item-spec.company   eq cocode
            and item-spec.i-no      eq po-ordl.i-no
            and item-spec.item-type eq po-ordl.item-type
          no-lock:
        do i = 1 to 4:
          if item-spec.notes[i] ne "" then do:
            ctr = ctr + 1.
            if ctr le 4 then put item-spec.notes[i] at 10 skip.
          end.
        end.
      end.
      if ctr gt 0 then put skip(1).
    end.
    
    ctr = 0.
    do i = 1 to 4:
      if po-ordl.spec-i[i] ne "" then do:
        ctr = ctr + 1.
        put po-ordl.spec-i[i] at 10 skip.
      end.
    end.
    if ctr gt 0 then put skip(1).
  end. /*purchase order lines loop*/
  
  ctr = 0.
  do i = 1 to 4:
    if po-ord.spec-i[i] ne "" then ctr = ctr + 1.
  end.
  if ctr gt 0 then ctr = ctr + 1.
  
  if v-print-lines + ctr gt 26 then do:
    page.
    run print-header.
    v-print-lines = 0.
  end.
  
  v-print-lines = v-print-lines + ctr.
  
  ctr = 0.
  do i = 1 to 4:
    if po-ord.spec-i[i] ne "" then do:
      ctr = ctr + 1.
      put po-ord.spec-i[i] at 10 skip.
    end.
  end.
  if ctr gt 0 then put skip(1).
  
  page.
end. /*header loop*/


procedure print-header:

   display po-ord.po-no with frame hdr.
   
   if avail vend then do:
   
     if (vend.city <> "") and (vend.state <> "") and (vend.zip <> "") then 
       v-city-state-zip = trim(vend.city) + ", " + trim(vend.state) + " " + trim(vend.zip).

     display
       vend.name
       vend.add1
       vend.add2
       v-city-state-zip
     with frame hdr.
   end.
     
   if po-ord.type eq "D" then do:

     if (po-ord.ship-city <> "") and (po-ord.ship-state <> "") and (po-ord.ship-zip <> "") then 
       p-city-state-zip = trim(po-ord.ship-city) + ", " + trim(po-ord.ship-state) + " " +
                          trim(po-ord.ship-zip).

     display
       po-ord.ship-name
       po-ord.ship-addr[1]
       po-ord.ship-addr[2]
       p-city-state-zip
     with frame hdr.
   end.
   else
   do:
     find first company where company.company eq cocode no-lock no-error.
     if avail company then do:
   
       if (company.city <> "") and (company.state <> "") and (company.zip <> "") then 
         p-city-state-zip = trim(company.city) + ", " + trim(company.state) + " " +
                            trim(company.zip).

       display
         company.name @ po-ord.ship-name
         company.addr[1] @ po-ord.ship-addr[1]
         company.addr[2] @ po-ord.ship-addr[2]
         p-city-state-zip
        with frame hdr.
     end.
   end.
       
   v-date-ordered = string(entry(int(month(po-ord.po-date)),v-months)) + " " +
                    string(day(po-ord.po-date)) + " " +
                    substr(string(year(po-ord.po-date),"9999"),3,2).
   display v-date-ordered with frame hdr.
   
   v-date-required = string(entry(int(month(po-ord.due-date)),v-months)) + " " +
                    string(day(po-ord.due-date)) + " " +
                    substr(string(year(po-ord.due-date),"9999"),3,2).
   display v-date-required with frame hdr.
   
   if po-ord.type eq "D" then do:
     if avail oe-rel then 
       display oe-rel.po-no with frame hdr.
   end.
   
   if po-ord.type eq "D" then do:
     display po-ord.ship-id with frame hdr.
   end.
   
   if avail terms then do:
     v-net-days = "Net " + string(terms.net-days) + " days".
     display v-net-days with frame hdr.
   end.
   
   display po-ord.carrier with frame hdr.

end procedure.


/* END ----------------------------------- Copr. 1997  Advanced Software Inc. */


