/* ----------------------------------------------- oe/rep/relprem.p 04/01 JLF */
/* Print OE Release/Picking tickets (Fibre Container)                         */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def var v-zone like shipto.dest-code no-undo.
def var v-frt-pay-dscr as char format "x(11)" no-undo.
def var v-bin as char no-undo.
def var v-print as log no-undo.
def var v-part-info like itemfg.i-name no-undo.
def var v-qty like oe-rell.qty no-undo.
def var v-rel-qty like v-qty.

def workfile w-bin 
   field w-loc like fg-bin.loc
   field w-bin like fg-bin.loc-bin
   field w-qty like fg-bin.qty extent 2
   field w-c-c like fg-bin.case-count
   field w-cas as   dec
   field w-par like oe-ordl.part-dscr1.

def buffer b-cust  for cust.
def buffer b-ship  for shipto.
def buffer b-w-bin for w-bin.

find first oe-relh no-lock no-error.
find first shipto no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(1)
         
       rel-pack                                     to 75
         
       "Date      Number"                           at 62                          
       v-ticket-date                                at 60
       trim(string(oe-relh.release#,">>>>>>>>"))    at 70
         
       skip(1)
         
       v-comp-name                                                      skip
         
       v-add-line[1]                                                    skip
         
       v-add-line[2]                                                    skip
         
       v-city
       v-state
       v-zip
           
       skip(2)
         
       "SOLD To:"                       at 11
       "SHIP To:"                       at 50
         
       cust.name                        at 11
       shipto.ship-name                 at 50
         
       cust.addr[1]                     at 11
       shipto.ship-addr[1]              at 50
         
       cust.addr[2]                     at 11
       shipto.ship-addr[2]              at 50
          
       cust.city                        at 11
       cust.state
       cust.zip
       shipto.ship-city                 at 50
       shipto.ship-state
       shipto.ship-zip
         
       skip(1)
         
       "Notes"
       "Ship Date"                      at 72
       oe-relh.ship-i[1]                at 2
       oe-relh.rel-date                 at 73
       oe-relh.ship-i[2]                at 2
       oe-relh.ship-i[3]                at 2
       oe-relh.ship-i[4]                at 2
       
       fill("-",80)                             format "x(80)"
         
       "Ship Via"
       "DelZone"                        at 27
       "F.O.B."                         at 40
       "Weight"                         to 57
       "Freight Terms"                  at 65
         
       v-carrier          
       v-zone                           at 27
       oe-ord.fob-code                  at 40
       v-weight                         to 57
       v-frt-pay-dscr                   at 65
         
       fill("-",80)                             format "x(80)"
         
       skip(1)
         
       "Order#"                         to 6
       "Item / Descrip"                 at 8
       "Whs  / Bin"                     at 39
       "Units"                          to 59
       "Unit Count"                     to 70
       "Rel Qty"                        to 80
       
       "------"                         to 6
       "--------------"                 at 8
       "----------"                     at 39
       "-----"                          to 59
       "----------"                     to 70
       "-------"                        to 80
       
    with frame rel-top no-box no-labels STREAM-IO width 85 page-top.
        
format oe-rell.ord-no                   to 6
       w-par                            at 8    format "x(30)"
       v-bin                            at 39   format "x(14)"
       w-cas                            to 59   format "->>>>"
       w-c-c                            to 70   format "->>>>>>>>"
       v-rel-qty                        to 80   format "->>>>>>>>"
    
    with down frame rel-mid no-box no-label STREAM-IO width 85.
   

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

    view frame rel-top.

    {oe/rep/foreachr.i},

        first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-relh.cust-no
        no-lock

        break by oe-relh.release#:

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      assign
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-zone    = shipto.dest-code.
       
      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.

      assign
       v-carrier      = if avail carrier then carrier.dscr else ""
       v-frt-pay-dscr = "".

      FOR EACH oe-rell
          where oe-rell.company eq oe-relh.company
            and oe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
          FIRST oe-ord
          where oe-ord.company eq oe-rell.company
            and oe-ord.ord-no  eq oe-rell.ord-no
          no-lock:

        case oe-ord.frt-pay:
             when "P" THEN v-frt-pay-dscr = "Prepaid".
             when "C" THEN v-frt-pay-dscr = "Collect".
             when "B" THEN v-frt-pay-dscr = "Bill".
             when "T" THEN v-frt-pay-dscr = "Third Party".
        end case.

        LEAVE.
      END.

      if oe-ctrl.pr-broker and avail cust and shipto.broker then
        assign
         v-comp-name   = cust.name
         v-add-line[1] = cust.addr[1]
         v-add-line[2] = cust.addr[2]
         v-city        = cust.city
         v-state       = cust.state
         v-zip         = cust.zip.
         
      else
        assign
         v-comp-name   = company.name
         v-add-line[1] = company.addr[1]
         v-add-line[2] = company.addr[2]
         v-city        = company.city
         v-state       = company.state
         v-zip         = company.zip.
         
      for each oe-rell
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no,

          first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rell.ord-no
            and oe-ordl.i-no    eq oe-rell.i-no
            and oe-ordl.line    eq oe-rell.line
          no-lock,
          
          first itemfg of oe-rell no-lock
          
          break by oe-rell.i-no
                by oe-rell.ord-no
                by oe-rell.po-no:
          
        v-rel-qty = v-rel-qty + oe-rell.qty.
        
        if last-of(oe-rell.po-no) then do:
          for each w-bin:
            delete w-bin.
          end.

          i = 0.

          for each fg-bin
              where fg-bin.company  eq cocode
                and fg-bin.i-no     eq oe-rell.i-no
                and (fg-bin.job-no  eq oe-ordl.job-no  or oe-ordl.job-no eq "")
                and (fg-bin.job-no2 eq oe-ordl.job-no2 or oe-ordl.job-no eq "")
                and fg-bin.qty      gt 0
              no-lock:
          
            create w-bin.
            assign
             w-loc    = fg-bin.loc
             w-bin    = fg-bin.loc-bin
             w-qty[1] = fg-bin.qty
             w-qty[2] = fg-bin.qty 
             w-c-c    = fg-bin.case-count
             i        = i + 1.
          end.
          
          for each w-bin break by w-loc by w-bin:
            v-qty = v-qty + w-qty[1].
            
            if last-of(w-bin) then do:
              for each b-w-bin
                  where b-w-bin.w-loc eq w-bin.w-loc
                    and b-w-bin.w-bin eq w-bin.w-bin:
                b-w-bin.w-qty[2] = v-qty.
              end.
              
              v-qty = 0.
            end.
          end.
          
          if i eq 0 then do:
            find first b-cust
                where b-cust.company eq cocode
                  and b-cust.active  eq "X" 
                no-lock no-error.
            if avail b-cust then do:
              find first b-ship
                  where b-ship.company eq cocode
                    and b-ship.cust-no eq b-cust.cust-no
                  no-lock no-error.
              if avail b-ship then do:
                create w-bin.
                assign   
                 w-loc = b-ship.loc
                 w-bin = b-ship.loc-bin
                 i     = i + 1.
              end.   
            end.
          end.
         
          do i = i to 6:
            create w-bin.
          end.

          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = oe-ordl.part-no.
            leave.
          end.  
        
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = oe-ordl.i-name.
            leave.
          end.
        
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = oe-ordl.part-dscr1.
            leave.
          end.
          
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = oe-ordl.part-dscr2.
            leave.
          end.
          
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = oe-rell.i-no.
            leave.
          end.
         
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            if oe-rell.po-no ne "" then w-par = "PO#: " + oe-rell.po-no.
            leave.
          end.
        
          j = 3.
          for each w-bin:
            if w-par eq "" and w-loc eq "" and w-bin eq "" then delete w-bin.
            else j = j + 1.
          end.
          
          v-print = yes.
          if avail oe-rel then do i = 1 to 4:
            if oe-rel.ship-i[i] ne "" then do:
              if v-print then j = j + 1.
              assign
               j = j + 1
               v-print = no.
            end.
          end.

          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            assign
             w-cas = w-qty[1] / w-c-c
             v-bin = string(w-loc,"x(5)") + "/" + w-bin.
            
            if w-cas eq ? then w-cas = 0.
           
            if trim(v-bin) eq "/" then v-bin = "".
          
            {sys/inc/roundup.i w-cas} 
        
            display oe-rell.ord-no when first(w-qty[2])
                    w-par
                    v-bin
                    w-cas         
                    w-c-c
                    v-rel-qty      when first(w-qty[2])

                with frame rel-mid.
              
            down with frame rel-mid.    
          end.
        
          v-print = yes.
          if avail oe-rel then do i = 1 to 4:
            if oe-rel.ship-i[i] ne "" then do:
              if v-print then put skip(1).
              put oe-rel.ship-i[i] format "x(60)" at 11 skip.
              v-print = no.
            end.
          end.
          
          put fill("-",80) format "x(80)" skip.
          
          v-rel-qty = 0.
        end.

        oe-rell.printed = yes.
      end. /* for each oe-rell */
      
      put skip(1)
          "Pallets_______ "
          "Pulled by_______ "
          "Shipped by_______ "
          "Trailer#________ "
          "Dock#________"
          skip(1).

      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
  
      page.
    end. /* for each oe-relh */
