/* ---------------------------------------------- oe/rep/relcntbx.i 12/03 JLF */
/* Print OE Release/Picking tickets (Century Box)                             */
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
DEF VAR lv-key-list AS CHAR NO-UNDO.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

def workfile w-oe-rell like oe-rell
   field seq    as   int
   field set-no like fg-set.set-no.

def workfile w-bin 
   field w-loc like fg-bin.loc
   field w-bin like fg-bin.loc-bin
   field w-tag like fg-bin.tag
   field w-qty like fg-bin.qty extent 2
   field w-c-c like fg-bin.case-count
   field w-cas as   dec
   field w-pal as   dec
   field w-par like oe-ordl.part-dscr1
   field w-x   as   log.

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
       fill(" ",int((8 - length(trim(string(oe-relh.release#,">>>>>>>>")))) / 2)) +
         trim(string(oe-relh.release#,">>>>>>>>"))  at 70 format "x(8)"
         
       skip(1)
       
       v-comp-name
       "PAGE"                                       at 62 
       page-number - v-last-page                          format ">>"
       "OF"                                  
       v-page-tot                                         format ">>"   skip
         
       v-add-line[1] TODAY AT 62 FORMAT "99/99/99"
       STRING(TIME,"HH:MM AM")    skip
         
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
       oe-relh.rel-date                 at 73   FORMAT "99/99/99"
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
       "Tag/Whs/Bin"                    at 29
       "X"                              at 50
       "#Pal"                           to 55
       "#Cas"                           to 60
       "Count"                          to 70
       "Bin Qty"                        to 80
       
       "------"                         to 6
       "--------------"                 at 8
       "----------"                     at 29
       "-"                              at 50
       "----"                           to 55
       "----"                           to 60
       "-----"                          to 70
       "-------"                        to 80
       
    with frame rel-top no-box no-labels STREAM-IO width 85 page-top.
        
format w-oe-rell.ord-no                 to 6
       w-par                            at 8    format "x(20)"
       v-bin                            at 29   format "x(20)"
       w-x                              at 50   format "X/"
       w-pal                            to 55   format "->>>"
       w-cas                            to 60   format "->>>"
       w-c-c                            to 70   format "->>>>>>>>"
       w-qty[1]                         to 80   format "->>>>>>>>"
    
    with down frame rel-mid no-box no-label STREAM-IO width 85.

def stream last-page.   

output stream last-page to value(tmp-dir + "relcntbx.txt") page-size VALUE(v-lines-per-page).

view frame rel-top.
view stream last-page frame rel-top.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

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

      FOR EACH w-oe-rell:
        DELETE w-oe-rell.
      END.
         
      for each oe-rell
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no,

          first itemfg of oe-rell no-lock:
          
        create w-oe-rell.
        buffer-copy oe-rell to w-oe-rell.
        
        assign
         i                = 0 
         w-oe-rell.seq    = i
         w-oe-rell.set-no = oe-rell.i-no
         oe-rell.printed  = yes.
        
        if v-print-components AND itemfg.isaset and itemfg.alloc EQ YES then
        for each fg-set
            where fg-set.company eq cocode
              and fg-set.set-no  eq oe-rell.i-no
            no-lock:
            
          {sys/inc/part-qty.i v-part-qty fg-set}
          
          create w-oe-rell.
          buffer-copy oe-rell to w-oe-rell.
          
          assign
           i                = i + 1
           w-oe-rell.seq    = i
           w-oe-rell.set-no = oe-rell.i-no
           w-oe-rell.i-no   = fg-set.part-no
           w-oe-rell.qty    = w-oe-rell.qty * v-part-qty.
        end.

        v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).
      end.

      page.
      page stream last-page.

      {oe/rep/relcntbx.i "stream last-page" "(last-page)"}

      v-page-tot = page-number (last-page) - v-last-page.

      {oe/rep/relcntbx.i}

      v-last-page = page-number.
          
      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    end. /* for each oe-relh */
