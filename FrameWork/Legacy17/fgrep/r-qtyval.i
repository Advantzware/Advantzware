    for each cust
        where cust.company eq cocode
          and cust.cust-no ge fcst
          and cust.cust-no le tcst
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
          AND ttCustList.log-fld no-lock) else true)
          and cust.sman    ge fslm
          and cust.sman    le tslm
        no-lock
        break by cust.cust-no:

         {custom/statusMsg.i "'Processing Customer # ' + cust.cust-no"}

      if first-of(cust.cust-no) then
        assign
         v-frst     = yes
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0
         v-print    = no.

      if not v-rec-dat then do:
        if line-counter ge 56 then page.
           for each oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.cust-no eq cust.cust-no
              and oe-ordl.po-no   ge fpo#
              and oe-ordl.po-no   le tpo#
            no-lock,

            first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq oe-ordl.i-no
              and itemfg.cust-no eq cust.cust-no
              and (itemfg.i-code eq type or type eq "A")
            no-lock

            break by oe-ordl.i-no
                  by oe-ordl.job-no
                  by oe-ordl.job-no2:

          v-frst-ord = yes.

          v-sales-rep = "" .
                  IF AVAIL cust AND cust.ACTIVE NE "X" THEN do:
                        FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                             AND cust-part.i-no = itemfg.i-no
                             AND cust-part.cust-no EQ cust.cust-no
                             NO-LOCK, 
                             FIRST reftable WHERE reftable.reftable = "cp-lab-p" 
                             AND reftable.company = cust-part.company  
                             AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK:
                             
                             IF cust-part.spare-char-1 NE "" THEN do:
                                 v-sales-rep = cust-part.spare-char-1.
                                 LEAVE .
                             END.
                         END. /* end of cust-part */

                         IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
                             v-sales-rep = cust.sman.
                        END.
                  END.
                  ELSE DO:
                      v-sales-rep = cust.sman.
                  END.


          for each fg-bin
              where fg-bin.company      eq cocode
                and fg-bin.i-no         eq itemfg.i-no
                and fg-bin.job-no       eq oe-ordl.job-no
                and fg-bin.job-no2      eq oe-ordl.job-no2
                and (v-custown or (fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq ""))

              use-index co-ino no-lock

              break by fg-bin.loc:

            v-qty-onh = v-qty-onh + fg-bin.qty.

            if last-of(fg-bin.loc)      and
               (v-qty-onh ne 0 or zbal) then do:

              if itemfg.sell-uom   eq "CS" and
                 itemfg.case-count ne 0    then
                v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.

              else
              if itemfg.sell-uom eq "L" then v-ext = oe-ordl.price.

              else do:
                find first uom
                    where uom.uom  eq itemfg.sell-uom
                      and uom.mult ne 0
                    no-lock no-error.

                v-ext = v-qty-onh * oe-ordl.price /
                        (if avail uom then uom.mult else 1000).
              end.
  
        IF tb_excel THEN  
         EXPORT STREAM excel DELIMITER ","
               (IF v-frst     THEN cust.cust-no    ELSE "")
               (IF v-frst-ord THEN oe-ordl.po-no   ELSE "")
               (IF v-frst     THEN /*cust.sman*/ v-sales-rep      ELSE "") 
               (IF v-frst-ord THEN oe-ordl.i-no    ELSE "")
               (IF v-frst-ord THEN oe-ordl.part-no ELSE "") WHEN v-prt-cpn /* btr */
               (IF v-frst-ord THEN oe-ordl.i-name  ELSE "")
               fg-bin.loc
               (IF v-frst-ord THEN oe-ordl.qty     ELSE 0)
               (IF v-frst-ord THEN oe-ordl.ship-qty ELSE 0)
               (IF first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) THEN v-qty-onh ELSE 0)
               oe-ordl.price
               (IF first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) then v-ext else 0)
               SKIP.

              if v-prt-cpn then do:
                display cust.cust-no        when v-frst
                        oe-ordl.po-no       when v-frst-ord
                        /*cust.sman*/ v-sales-rep      when v-frst
                        oe-ordl.i-no        when v-frst-ord
                        oe-ordl.part-no     when v-frst-ord
                        oe-ordl.i-name      when v-frst-ord
                        fg-bin.loc
                        oe-ordl.qty         when v-frst-ord
                        oe-ordl.ship-qty    when v-frst-ord
                        v-qty-onh           when first-of(oe-ordl.job-no) or
                                                 first-of(oe-ordl.job-no2)
                        oe-ordl.price
                        v-ext               when first-of(oe-ordl.job-no) or
                                                 first-of(oe-ordl.job-no2)
                     with frame itemx1.

                down with frame itemx1.
              end.

              else do:
                display cust.cust-no        when v-frst
                        oe-ordl.po-no       when v-frst-ord
                        /*cust.sman*/ v-sales-rep when v-frst
                        oe-ordl.i-no        when v-frst-ord
                        oe-ordl.i-name      when v-frst-ord
                        fg-bin.loc
                        oe-ordl.qty         when v-frst-ord
                        oe-ordl.ship-qty    when v-frst-ord
                        v-qty-onh           when first-of(oe-ordl.job-no) or
                                                 first-of(oe-ordl.job-no2)
                        oe-ordl.price
                        v-ext               when first-of(oe-ordl.job-no) or
                                                 first-of(oe-ordl.job-no2)
                     with frame itemx3.

                down with frame itemx3.
              end.

              if first-of(oe-ordl.job-no) or first-of(oe-ordl.job-no2) then
                assign
                 v-tot-onh        = v-tot-onh       + v-qty-onh
                 v-tot-ext        = v-tot-ext       + v-ext

                 v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                 v-grand-tot-ext  = v-grand-tot-ext + v-ext.

              if v-frst-ord then
                assign
                 v-tot-ord        = v-tot-ord        + oe-ordl.qty
                 v-tot-ship       = v-tot-ship       + oe-ordl.ship-qty

                 v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                 v-grand-tot-ship = v-grand-tot-ship + oe-ordl.ship-qty.

              assign
               v-qty-onh   = 0
               v-frst      = no
               v-frst-ord  = no
               v-print     = yes.
            end.  /* last of fg bin */
          end.  /* for each fg bin */
        end.  /* for each oe-ordl */
      end.  /* not rec date */

      else do:
        for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
                           and itemfg.cust-po-no >= fpo#
                           and itemfg.cust-po-no <= tpo#
               no-lock break by itemfg.cust-no
               by itemfg.i-no:

            v-sales-rep = "" .
                  IF AVAIL cust AND cust.ACTIVE NE "X" THEN do:
                        FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                             AND cust-part.i-no = itemfg.i-no
                             AND cust-part.cust-no EQ cust.cust-no
                             NO-LOCK, 
                             FIRST reftable WHERE reftable.reftable = "cp-lab-p" 
                             AND reftable.company = cust-part.company  
                             AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK:
                             
                             IF cust-part.spare-char-1 NE "" THEN do:
                                 v-sales-rep = cust-part.spare-char-1.
                                 LEAVE .
                             END.
                         END. /* end of cust-part */

                        IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
                             v-sales-rep = cust.sman.
                        END.
                  END.
                  ELSE DO:
                      v-sales-rep = cust.sman.
                  END.

        if first-of(itemfg.i-no) then
          assign v-frst-i-no = yes.

          if ((type ne "A") and (type ne itemfg.i-code)) or (type = "A") then
          do:
            for each fg-bin no-lock where fg-bin.company = cocode and
                                        fg-bin.i-no = itemfg.i-no
                                        use-index co-ino
                              break by fg-bin.loc
                                    by fg-bin.job-no
                                    by fg-bin.job-no2:

              if (fg-bin.loc eq "CUST" or trim(fg-bin.cust-no) gt "") and not v-custown then
                next.
              else
                v-qty-onh = v-qty-onh + fg-bin.qty.

              if last-of(fg-bin.loc) then
              do:
                if (v-qty-onh ne 0 ) or (v-qty-onh eq 0 and zbal) then
                do:
                  if itemfg.sell-uom = "CS" and itemfg.case-count ne 0 then
                    v-ext = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                  else
                    find first uom where uom.uom = itemfg.sell-uom and
                                        uom.mult ne 0 no-lock no-error.
                  if available uom then
                    v-ext = (v-qty-onh * itemfg.sell-price / uom.mult).
                  else
                    v-ext = (v-qty-onh * itemfg.sell-price) / 1000.

                  if itemfg.sell-uom = "L" then
                    v-ext = itemfg.sell-price.

                  for each xbin where xbin.company = cocode and
                                    xbin.i-no    = itemfg.i-no and
                                    xbin.loc     = fg-bin.loc
                                    no-lock break by xbin.job-no
                                                  by xbin.job-no2:
                   if first-of(xbin.job-no) or first-of(xbin.job-no2) then
                      assign
                        v-qty-job = 0
                        v-ext-job = 0.

                   v-qty-job = v-qty-job + xbin.qty.

                   if last-of(xbin.job-no) or last-of(xbin.job-no2) then
                   do:
                      find first xbin2 where xbin2.company = cocode and
                                             xbin2.i-no = itemfg.i-no and
                                             xbin2.loc = fg-bin.loc and
                                            (xbin2.job-no  <> xbin.job-no or
                                             xbin2.job-no2 <> xbin.job-no2) and
                                             xbin2.qty <> 0
                                             no-lock no-error.
                      if available xbin2 and v-qty-job = 0 then
                         next.
                      if itemfg.sell-uom = "CS" and
                         itemfg.case-count ne 0 then
                         v-ext-job = (v-qty-job * itemfg.sell-price) /
                                      itemfg.case-count.
                      else
                         find first uom where uom.uom = itemfg.sell-uom and
                                              uom.mult ne 0 no-lock no-error.
                      if available uom then
                         v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                      else
                         v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                      if itemfg.sell-uom = "L" then
                         v-ext-job = itemfg.sell-price.
                      
                      trans-date = ?.
/*                    for each fg-rcpth where fg-rcpth.company = cocode and      */
/*                                            fg-rcpth.i-no    = itemfg.i-no and */
/*                                            fg-rcpth.rita-code  = "R" and      */
/*                                            fg-rcpth.job-no  = xbin.job-no and */
/*                                            fg-rcpth.job-no2 = xbin.job-no2    */
/*                                            no-lock                            */
/*                                            break by fg-rcpth.trans-date:      */
/* /*                        if last-of(fg-rcpth.trans-date) then */             */
/*                       if FIRST-OF(fg-rcpth.trans-date) then                   */
/*                           trans-date = fg-rcpth.trans-date.                   */
/*                     end.                                                      */
                      IF AVAIL xbin THEN
                         IF TRIM(xbin.tag) NE "" THEN
                            FOR EACH b-rdtlh NO-LOCK WHERE b-rdtlh.company   EQ xbin.company
                                                       AND b-rdtlh.tag       EQ xbin.tag
                                                       AND b-rdtlh.rita-code EQ "R"
                                                       USE-INDEX tag,
                               EACH b-rcpth NO-LOCK WHERE b-rcpth.r-no      EQ b-rdtlh.r-no
                                                      AND b-rcpth.i-no      EQ xbin.i-no
                                                      AND b-rcpth.rita-code EQ b-rdtlh.rita-code 
                                                USE-INDEX r-no
                                                       BY b-rcpth.trans-date
                                                       BY b-rcpth.r-no:
                                  LEAVE.
                            END.
                            ELSE
                               IF TRIM(xbin.job-no) NE "" THEN
                                  FOR EACH b-rcpth NO-LOCK WHERE b-rcpth.company   EQ xbin.company
                                                             AND b-rcpth.job-no    EQ xbin.job-no
                                                             AND b-rcpth.job-no2   EQ xbin.job-no2
                                                             AND b-rcpth.i-no      EQ xbin.i-no
                                                             AND b-rcpth.rita-code EQ "R"
                                                       USE-INDEX job,
                                     EACH b-rdtlh NO-LOCK WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
                                                            AND b-rdtlh.rita-code EQ b-rcpth.rita-code
                                                             BY b-rcpth.trans-date
                                                             BY b-rcpth.r-no:
                                        LEAVE.
                                  END.

                         IF AVAIL b-rcpth THEN trans-date = b-rcpth.trans-date.

                    if line-counter >= 56 then page.
                    if xbin.job-no = "" and xbin.job-no2 = 0 then
                          v-job = "".
                       else
                          v-job = xbin.job-no + "-" + string(xbin.job-no2).

                     find first oe-ordl
                         where oe-ordl.company eq cocode
                           and oe-ordl.job-no  eq xbin.job-no
                           and oe-ordl.job-no2 eq xbin.job-no2
                           and oe-ordl.i-no    eq xbin.i-no
                         use-index job no-lock no-error.

                     if avail oe-ordl then
                       v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.


        IF tb_excel THEN  
           EXPORT STREAM excel DELIMITER ","
                 (IF v-frst THEN cust.cust-no ELSE "")                 
                 (IF avail oe-ordl THEN oe-ordl.po-no ELSE itemfg.cust-po-no)
                 (IF v-frst     THEN v-sales-rep       ELSE "")
 					  itemfg.i-no
                 itemfg.part-no WHEN v-prt-cpn
                 itemfg.i-name
                 v-job
                 v-qty-job
                 (IF not(zbal and v-qty-onh = 0) AND not(v-qty-job = 0) THEN STRING(trans-date) ELSE "")
                 (IF avail oe-ordl THEN (oe-ordl.t-price / oe-ordl.qty * 1000) ELSE itemfg.sell-price)
                  v-ext-job
               SKIP.

                     if v-prt-cpn then do:
                       display cust.cust-no when v-frst
                               itemfg.cust-po-no
                               oe-ordl.po-no
                                 when avail oe-ordl @ itemfg.cust-po-no
                               /*cust.sman*/ v-sales-rep when v-frst
                               itemfg.i-no
                               itemfg.part-no
                               itemfg.i-name
                               v-job
                               v-qty-job
                               trans-date when not(zbal and v-qty-onh = 0) and
                                               not(v-qty-job = 0)
                               itemfg.sell-price
                                          (oe-ordl.t-price / oe-ordl.qty * 1000)
                                 when avail oe-ordl @ itemfg.sell-price
                               v-ext-job
                           with frame itemx2.

                       down with frame itemx2.
                     end.

                     else do:
                       display cust.cust-no when v-frst
                               itemfg.cust-po-no
                               oe-ordl.po-no
                                 when avail oe-ordl @ itemfg.cust-po-no
                               /*cust.sman*/ v-sales-rep when v-frst
                               itemfg.i-no
                                /* itemfg.part-no btr */
                               itemfg.i-name
                               v-job
                               v-qty-job
                               trans-date when not(zbal and v-qty-onh = 0) and
                                               not(v-qty-job = 0)
                               itemfg.sell-price
                                          (oe-ordl.t-price / oe-ordl.qty * 1000)
                                 when avail oe-ordl @ itemfg.sell-price
                               v-ext-job
                           with frame itemx4.

                       down with frame itemx4.
                     end.

                     assign
                      v-tot-ext = v-tot-ext + v-ext-job
                      v-grand-tot-ext = v-grand-tot-ext + v-ext-job.
                   end. /* if last-of(... */
                end. /* for each xbin */

                if v-frst-i-no then
                assign v-tot-ord = v-tot-ord + v-qty-ord
                       v-grand-tot-ord = v-grand-tot-ord + v-qty-ord
                       v-tot-ship = v-tot-ship + v-qty-ship
                       v-grand-tot-ship = v-grand-tot-ship + v-qty-ship
                       v-frst-i-no = no.

                assign v-tot-onh = v-tot-onh + v-qty-onh
                       v-frst = no
                       v-grand-tot-onh = v-grand-tot-onh + v-qty-onh
                       v-qty-onh = 0
                       v-qty-ship = 0
                       v-print = yes.


              end. /* qty onh */
              end. /* last of bin */
            end. /* for each bin */
          end. /* item type */
        end.  /* for each item */
      end.  /* rec date */

      if v-print                      and
         fcst ne tcst                 and
         (v-tot-onh ne 0 or zbal)     then
        IF NOT v-rec-dat THEN
          IF v-prt-cpn THEN
            PUT "------------"      TO 94
                "----------"        TO 105
                "------------"      TO 118
                "--------------"    TO 148 SKIP
                "CUSTOMER TOTALS:"  TO 77
                v-tot-ord           TO 94
                v-tot-ship          TO 105
                v-tot-onh           TO 118
                v-tot-ext           TO 148 SKIP(1).

          ELSE
            PUT "------------"      TO 78
                "----------"        TO 89
                "------------"      TO 102
                "--------------"    TO 132 SKIP
                "CUSTOMER TOTALS:"  TO 61
                v-tot-ord           TO 78
                v-tot-ship          TO 89
                v-tot-onh           TO 102
                v-tot-ext           TO 132 SKIP(1).

        ELSE
          IF v-prt-cpn THEN
            PUT "-----------"       TO 94
                "--------------"    TO 132 SKIP
                "CUSTOMER TOTALS:"  TO 77
                v-tot-onh           TO 94
                v-tot-ext           TO 132 SKIP(1).

          ELSE
            PUT "-----------"       TO 78
                "--------------"    TO 116 SKIP
                "CUSTOMER TOTALS:"  TO 61
                v-tot-onh           TO 78
                v-tot-ext           TO 116 SKIP(1).
    end.  /* for each cust */

    IF NOT v-rec-dat THEN
      IF v-prt-cpn THEN
        PUT "------------"      TO 94
            "----------"        TO 105
            "------------"      TO 118
            "--------------"    TO 148 SKIP
            "GRAND TOTALS:"     TO 77
            v-grand-tot-ord     TO 94
            v-grand-tot-ship    TO 105
            v-grand-tot-onh     TO 118
            v-grand-tot-ext     TO 148 SKIP(1).

      ELSE
        PUT "------------"      TO 78
            "----------"        TO 89
            "------------"      TO 102
            "--------------"    TO 132 SKIP
            "GRAND TOTALS:"     TO 61
            v-grand-tot-ord     TO 78
            v-grand-tot-ship    TO 89
            v-grand-tot-onh     TO 102
            v-grand-tot-ext     TO 132 SKIP(1).

    ELSE
      IF v-prt-cpn THEN
        PUT "-----------"       TO 94
            "--------------"    TO 132 SKIP
            "GRAND TOTALS:"     TO 76
            v-grand-tot-onh     TO 94
            v-grand-tot-ext     TO 132 SKIP(1).

      ELSE
        PUT "-----------"       TO 78
            "--------------"    TO 116 SKIP
            "GRAND TOTALS:"     TO 61
            v-grand-tot-onh     TO 78
            v-grand-tot-ext     TO 116 SKIP(1).
