      
      for each w-oe-rell,
      
          first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq w-oe-rell.ord-no
            and oe-ordl.i-no    eq w-oe-rell.set-no
            and oe-ordl.line    eq w-oe-rell.line
          no-lock,
      
          first itemfg of w-oe-rell no-lock
                
          break by w-oe-rell.set-no
                by w-oe-rell.seq
                by w-oe-rell.i-no
                by w-oe-rell.po-no:

        assign
         v-rel-qty   = v-rel-qty + w-oe-rell.qty
         lv-key-list = lv-key-list + "," + w-oe-rell.rec_key.
        
        if last-of(w-oe-rell.po-no) then do:
          for each w-bin:
            delete w-bin.
          end.

          i = 0.

          for each fg-bin
              where fg-bin.company  eq cocode
                and fg-bin.i-no     eq w-oe-rell.i-no
                and (fg-bin.job-no  eq oe-ordl.job-no  or oe-ordl.job-no eq "")
                and (fg-bin.job-no2 eq oe-ordl.job-no2 or oe-ordl.job-no eq "")
                and fg-bin.qty      gt 0
              no-lock:
          
            create w-bin.
            assign
             w-tag    = fg-bin.tag
             w-loc    = fg-bin.loc
             w-bin    = fg-bin.loc-bin
             w-qty[1] = fg-bin.qty
             w-qty[2] = fg-bin.qty 
             w-c-c    = fg-bin.case-count
             w-x      = NO
             i        = i + 1.

            assign
             w-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                     (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                     (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
             w-pal = w-qty[1] / w-pal.

            {sys/inc/roundup.i w-pal}
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
         
          do i = i to 5:
            create w-bin.
          end.

          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
                                          else itemfg.part-no.
            leave.
          end.  
        
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = if w-oe-rell.seq eq 0 then oe-ordl.i-name
                                          else itemfg.i-name.
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
          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            if w-par eq "" and w-loc eq "" and w-bin eq "" then delete w-bin.
            else j = j + 1.
          end.

          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            if last(w-qty[2]) and (w-loc ne "" or w-bin ne "") then j = j + 1. 
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
          
          if line-counter {2} + j ge page-size {2} then page {1}.
        
          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            assign
             w-cas = w-qty[1] / w-c-c
             v-bin = trim(substr(w-tag,16,5)) + "/" +
                     trim(w-loc)              + "/" +
                     trim(w-bin).
            
            if w-cas eq ? then w-cas = 0.
           
            if trim(v-bin) eq "//" then v-bin = "".
          
            {sys/inc/roundup.i w-cas}

            display {1}
                    w-oe-rell.ord-no    when first(w-qty[2])
                    w-par
                    v-bin
                    w-x
                    w-pal
                    w-cas
                    w-c-c
                    w-qty[1]

                with frame rel-mid. 

            if last(w-qty[2]) then do:
              if w-loc ne "" or w-bin ne "" then down {1} with frame rel-mid.

              display {1}
                      "  Rel Qty"       @ w-c-c
                      v-rel-qty         @ w-qty[1]

                  with frame rel-mid. 
            end.

            down {1} with frame rel-mid.
          end.

          put {1}
              skip(1)
              "Pallets____________ "
              "Pulled by__________ "
              "Shipped by_________ "
              "Total Qty___________" 
              skip.

          v-print = yes.
          if avail oe-rel then do i = 1 to 4:
            if oe-rel.ship-i[i] ne "" then do:
              if v-print then put {1} skip(1).
              if line-counter {2} ge page-size {2} then page {1}.
              put {1} oe-rel.ship-i[i] format "x(60)" at 11 skip.
              v-print = no.
            end.
          end.

          put {1} fill("-",80) format "x(80)"  skip.
        
          assign
           v-rel-qty   = 0
           lv-key-list = "".
        end.
      end. /* for each w-oe-rell */
