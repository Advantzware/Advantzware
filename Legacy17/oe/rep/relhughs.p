/* ---------------------------------------------- oe/rep/relfibre.i 09/00 JLF */
/* Print OE Release/Picking tickets (Fibre Container)                         */
/* -------------------------------------------------------------------------- */
{oe/rep/oe-pick1.i}

{sys/FORM/r-top.i}

def var v-zone like shipto.dest-code no-undo.
def var v-frt-pay-dscr as char format "x(11)" no-undo.
def var v-rel-type as char format "x(13)" no-undo.
def var v-bin as char no-undo.
def var v-print as log no-undo.
def var v-part-info like itemfg.i-name no-undo.
def var v-qty like oe-rell.qty no-undo.
def var v-rel-qty like v-qty.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR v-exc-bin AS LOGICAL NO-UNDO.
/* gdm - 10240805 */
DEF VAR v_blanksz AS CHAR NO-UNDO.
DEF VAR v_styledc AS CHAR NO-UNDO.

def TEMP-TABLE w-oe-rell like oe-rell
   field seq    as   int
   field set-no like fg-set.set-no.
     
def TEMP-TABLE w-bin 
   field w-loc like fg-bin.loc
   field w-bin like fg-bin.loc-bin
   field w-qty like fg-bin.qty extent 2
   field w-c-c like fg-bin.case-count
   field w-cas as   dec
   field w-par AS CHAR FORMAT "X(47)".

def buffer b-cust  for cust.
def buffer b-ship  for shipto.
def buffer b-w-bin for w-bin.

{fg/fullset.i NEW}

find first oe-relh no-lock no-error.
find first shipto no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(1)         
       rel-pack                                     to 75         
       "Type         Date      Number"              at 49
       v-rel-type                                   at 45
       v-ticket-date                                at 60
       fill(" ",int((8 - length(trim(string(oe-relh.release#,">>>>>>>>")))) / 2)) +
         trim(string(oe-relh.release#,">>>>>>>>"))  at 70 format "x(8)"
         
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
       oe-relh.rel-date                 at 73   FORMAT "99/99/99"
       oe-relh.ship-i[2]                at 2
       oe-relh.ship-i[3]                at 2
       oe-relh.ship-i[4]                at 2
       
       fill("-",80)   format "x(80)"
         
       "Ship Via"
       "DelZone"                        at 27
       "F.O.B."                         at 40
       "Weight"                         to 57
       "Freight Terms"                  at 65
         
       v-carrier          
       v-zone                           at 27
       oe-ord.fob-code                  at 40
       v-weight    FORM ">,>>>,>>9"     to 57
       v-frt-pay-dscr                   at 65
         
       fill("-",80)      format "x(80)"
         
       skip(1)
         
       "Order#"                         to 6
       "Item / Descrip"                 at 8

       /* gdm - 10240805 */      
       "Blank Size/Style"               AT 39
       "Whs  / Bin"                     AT 57
       
       "Rel Qty"                        to 80
       
       "------"                         to 6
       "--------------"                 at 8
       
       /* gdm - 10240805 */      
       "----------------"               AT 39
       "------------"                   AT 57

       "---------"                      to 80
       
    with frame rel-top no-box no-labels STREAM-IO width 85 page-top.
        
format w-oe-rell.ord-no                 to 6
       w-par                            at 8    format "x(47)"
       v-bin                            at 57   format "x(14)"

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

      FOR EACH oe-rell NO-LOCK
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no,
          FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ oe-rell.company
            AND oe-ord.ord-no  EQ oe-rell.ord-no:

        CASE oe-rell.s-code:
          WHEN "S" THEN v-rel-type = "  SHIP ONLY".
          WHEN "I" THEN v-rel-type = "  BILL ONLY".
          WHEN "B" THEN v-rel-type = "BILL AND SHIP".
          WHEN "T" THEN v-rel-type = "  TRANSFER".
        END CASE.

        CASE oe-ord.frt-pay:
          WHEN "P" THEN v-frt-pay-dscr = "Prepaid".
          WHEN "C" THEN v-frt-pay-dscr = "Collect".
          WHEN "B" THEN v-frt-pay-dscr = "Bill".
          WHEN "T" THEN v-frt-pay-dscr = "Third Party".
        END CASE.

        IF oe-ord.type EQ "T" THEN v-rel-type = "TRANSFER".

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
         
      FOR EACH oe-rell
          WHERE oe-rell.company EQ cocode
            AND oe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no,

          FIRST itemfg of oe-rell NO-LOCK:

        ASSIGN
         i                = 0 
         oe-rell.printed  = YES.

        FOR EACH tt-fg-set:
          DELETE tt-fg-set.
        END.

        IF v-print-components AND itemfg.isaset /*AND itemfg.alloc NE ?*/  THEN
          RUN fg/fullset.p (ROWID(itemfg)).

        CREATE w-oe-rell.
        BUFFER-COPY oe-rell TO w-oe-rell.
        
        ASSIGN
         i                = i + 1
         w-oe-rell.seq    = i
         w-oe-rell.set-no = oe-rell.i-no.

        FOR EACH tt-fg-set:
          CREATE w-oe-rell.
          BUFFER-COPY oe-rell TO w-oe-rell.
          
          ASSIGN
           i                = i + 1
           w-oe-rell.seq    = i
           w-oe-rell.set-no = oe-rell.i-no
           w-oe-rell.i-no   = tt-fg-set.part-no
           w-oe-rell.qty    = w-oe-rell.qty * tt-fg-set.part-qty-dec.

        END.
        v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).        
        
      END.
          
      for each w-oe-rell,
      
          first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq w-oe-rell.ord-no
            and oe-ordl.i-no    eq w-oe-rell.set-no
            and oe-ordl.line    eq w-oe-rell.line
          no-lock,
      
          first itemfg of w-oe-rell no-lock
                
          break by w-oe-rell.ord-no
                by w-oe-rell.set-no
                by w-oe-rell.seq
                by w-oe-rell.i-no
                by w-oe-rell.po-no:
          
        v-rel-qty = v-rel-qty + w-oe-rell.qty.
        
        if last-of(w-oe-rell.po-no) then do:
          for each w-bin:
            delete w-bin.
          end.


          ASSIGN v_blanksz = STRING(itemfg.t-len) + " x " + 
                               STRING(itemfg.t-wid)
                   v_blanksz = IF TRIM(v_blanksz) EQ "x"  
                                 THEN " " ELSE v_blanksz.

          ASSIGN v_styledc = SUBSTRING(itemfg.style,1,14)

          i = 0.
         IF v-exc-bin THEN do:
          for each fg-bin
              where fg-bin.company  eq cocode
                and fg-bin.i-no     eq w-oe-rell.i-no
                and (fg-bin.job-no  eq oe-ordl.job-no  or oe-ordl.job-no eq "")
                and (fg-bin.job-no2 eq oe-ordl.job-no2 or oe-ordl.job-no eq "")
                and fg-bin.qty      gt 0
              NO-LOCK BREAK BY fg-bin.loc BY fg-bin.loc-bin:
            IF FIRST-OF(fg-bin.loc-bin) THEN do:
            create w-bin.
            assign
             w-loc    = fg-bin.loc
             w-bin    = fg-bin.loc-bin
             w-qty[1] = fg-bin.qty
             w-qty[2] = fg-bin.qty 
             w-c-c    = fg-bin.case-count
             i        = i + 1.
            END.
          end.
         END.
         IF NOT v-exc-bin THEN DO:
            for each fg-bin
              where fg-bin.company  eq cocode
                and fg-bin.i-no     eq w-oe-rell.i-no
                and (fg-bin.job-no  eq oe-ordl.job-no  or oe-ordl.job-no eq "")
                and (fg-bin.job-no2 eq oe-ordl.job-no2 or oe-ordl.job-no eq "")
                and fg-bin.qty      gt 0
              NO-LOCK:
           
            create w-bin.
            assign
             w-loc    = fg-bin.loc
             w-bin    = fg-bin.loc-bin
             w-qty[1] = fg-bin.qty
             w-qty[2] = fg-bin.qty 
             w-c-c    = fg-bin.case-count
             i        = i + 1.
          end.
         END.
          
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
         
          do i = i to 5:
            create w-bin.
          end.

          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              ASSIGN
                  w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
                                          else itemfg.part-no
                  w-par = TRIM(w-par) + 
                          FILL(" ",31 - LENGTH(w-par)) + TRIM(v_blanksz).
            leave.
          end.  
        
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            ASSIGN
                  w-par = if w-oe-rell.seq eq 0 then oe-ordl.i-name
                                          else itemfg.i-name
                  w-par = TRIM(w-par) + 
                          FILL(" ",31 - LENGTH(w-par)) + TRIM(v_styledc).
            leave.
          end.
        
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-dscr1
                                          else itemfg.part-dscr1.
            leave.
          end.
          
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = w-oe-rell.i-no.
            leave.
          end.
         
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            if w-oe-rell.po-no ne "" then w-par = "PO#: " + w-oe-rell.po-no.
            leave.
          end.
        
          j = 5.
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
          
          if line-counter + j gt page-size then page.
        
          for each w-bin break by w-qty[2] desc by w-qty[1] DESC BY w-bin :
            assign
             w-cas = w-qty[1] / w-c-c
             v-bin = string(w-loc,"x(5)") + "/" + w-bin.
            
            if w-cas eq ? then w-cas = 0.
           
            if trim(v-bin) eq "/" then
               v-bin = "".
          
            {sys/inc/roundup.i w-cas} 
            /*IF  FIRST-of(w-bin) THEN do:*/
            display w-oe-rell.ord-no    when first(w-qty[2])
                    w-par            
                    v-bin /*WHEN FIRST-of(w-bin)*/
                    STRING(v-rel-qty,'>>>>9.999') when first(w-qty[2]) @ v-rel-qty
                with frame rel-mid.

            down with frame rel-mid.                
           /* END.*/
          end.
          
          put skip(1)
              "Pallets____________ "
              "Pulled by__________ "
              "Shipped by_________ "
              "Total Qty___________" 
              skip.

          v-print = yes.
          if avail oe-rel then do i = 1 to 4:
            if oe-rel.ship-i[i] ne "" then do:
              if v-print then put skip(1).
              if line-counter GE page-size then page.
              put oe-rel.ship-i[i] format "x(60)" at 11 skip.
              v-print = no.
            end.
          end.

          put fill("-",80) format "x(80)"  skip.
        
          v-rel-qty = 0.
        end.
        
        delete w-oe-rell.
      end. /* for each w-oe-rell */

      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
  
      page.
    end. /* for each oe-relh */
