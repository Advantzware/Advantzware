/* --------------------------------------------------- po/po-cent.p 06/99 FWK */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Century                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

      page {1}. 

      assign v-bottom = int("{1}" eq "") + 1
             v-tot-sqft[v-bottom] = 0.

      hide {1} frame po-tots.
      view {1} frame po-cont.

      assign v-po-tot = 0
             v-tot-sqft[v-bottom] = 0.

      for each po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no AND
          (v-printde-po or
           (not po-ordl.deleted))
          exclusive break by i-no
          with frame po-line:

        assign
         v-print-lines = IF v-itemDescription AND NOT po-ordl.item-type THEN 6 ELSE 5
         v-job         = fill(" ",6 - length(trim(po-ordl.job-no))) +
                         trim(po-ordl.job-no)
         v-adder[1]    = ""
         v-adder[2]    = ""
         xg-flag       = no.

       assign v-change-dscr = "".
       
        find first item
            where item.company eq po-ordl.company
              and item.i-no    eq po-ordl.i-no
              and po-ordl.item-type
            no-lock no-error.

        if v-job ne "" then
          find last oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.job-no  eq v-job
                and oe-ordl.job-no2 eq po-ordl.job-no2
              use-index job no-lock no-error.

        v-inst-lines = 0.
        
        if (LINE-COUNTER + v-print-lines) gt page-size then page {1}.

        v-job = trim(v-job) + "-" + trim(string(po-ordl.job-no2,"99")).
        if trim(v-job) begins "-" then v-job = "".

        v-po-tot = v-po-tot + po-ordl.t-cost.

        if po-ordl.s-len gt 0 and po-ordl.s-wid gt 0 then do:
          assign
           v-sqft = (round(
                     (((if v-corr then (po-ordl.s-len * po-ordl.s-wid) * .007
                           else (po-ordl.s-len * po-ordl.s-wid) / 144) *
                           po-ordl.ord-qty) / 1000),1)).
           v-tot-sqft[v-bottom] = v-tot-sqft[v-bottom] + v-sqft.
        end.

        if avail item then do:

          if item.mat-type eq "B" then do:
            assign
             v-wid = po-ordl.s-wid - trunc(po-ordl.s-wid,0)
             v-wid = ( v-wid * 16 ) / 100
             v-wid = trunc(po-ordl.s-wid,0) + v-wid
             v-len = po-ordl.s-len - trunc(po-ordl.s-len,0)
             v-len = ( v-len * 16 ) / 100
             v-len = trunc(po-ordl.s-len,0) + v-len.

            FIND FIRST job WHERE job.company = cocode AND
                                      job.job-no = STRING(FILL(" ",6 - LENGTH(
                                                  TRIM(po-ordl.job-no)))) +
                                                  TRIM(po-ordl.job-no) AND
                                      job.job-no2 = po-ordl.job-no2
                                 NO-LOCK NO-ERROR.
            IF AVAIL job THEN
            do:
              for each job-mat
                  where job-mat.company  eq cocode
                    and job-mat.job      eq job.job
                    and job-mat.job-no   eq job.job-no
                    and job-mat.job-no2  eq job.job-no2
                    and job-mat.i-no     eq po-ordl.i-no
                    and job-mat.frm      eq po-ordl.s-num
                  use-index job no-lock
                  break by job-mat.blank-no desc:
                if last(job-mat.blank-no)            or
                   job-mat.blank-no eq po-ordl.b-num then leave.
              end.

              if avail job-mat then 
              do:
                assign v-num-add = 0.
                for each xjob-mat where xjob-mat.company  eq cocode
                                        and xjob-mat.job      eq job-mat.job
                                        and xjob-mat.job-no   eq job-mat.job-no
                                        and xjob-mat.job-no2  eq job-mat.job-no2
                                        and xjob-mat.frm             eq job-mat.frm
                                        and xjob-mat.blank-no eq job-mat.blank-no
                                        and xjob-mat.i-no            ne job-mat.i-no
                                    no-lock:
                    find first xitem where xitem.company         eq cocode 
                                         and xitem.i-no         eq xjob-mat.i-no
                                         and xitem.mat-type        eq "A" no-lock no-error.
                  if avail xitem then
                  do:
                    assign v-num-add = v-num-add + 1.
                    if v-num-add eq 1 then
                      assign v-adder[1] = xitem.i-name.
                    else if v-num-add eq 2 then
                      assign v-adder[2] = xitem.i-name.
                  end.
                end.

                display {1} po-ordl.ord-qty
                        po-ordl.i-name
                        v-adder[1]
                        /* po-ordl.vend-i-no */
                        v-job
                        po-ordl.cost
                        po-ordl.pr-uom
                        v-sqft
                        po-ordl.dscr[1]
                        v-adder[2]
                        po-ordl.due-date
                        v-change-dscr.
                
                if po-ordl.dscr[2] ne "" then
                  put {1} po-ordl.dscr[2] format "x(50)" at 8 skip.

                /* Adder i-no and i-name to po of exist */
                assign v-counter = 0.
                for each xjob-mat where xjob-mat.company  eq cocode
                                        and xjob-mat.job      eq job-mat.job
                                        and xjob-mat.job-no   eq job-mat.job-no
                                        and xjob-mat.job-no2  eq job-mat.job-no2
                                        and xjob-mat.frm             eq job-mat.frm
                                        and xjob-mat.blank-no eq job-mat.blank-no
                                        and xjob-mat.i-no            ne job-mat.i-no
                                    no-lock:
                    find first xitem where xitem.company         eq cocode 
                                         and xitem.i-no         eq xjob-mat.i-no
                                         and xitem.mat-type        eq "A" no-lock no-error.

                  if avail xitem then
                  do:
                    assign v-counter = v-counter + 1.
                    if v-counter gt 2 then
                      put {1} xitem.i-no at 31. 
                  end.
                end.

                    FIND FIRST ef WHERE ef.e-num = job.e-num AND
                                      ef.form-no = job-mat.frm NO-LOCK NO-ERROR.
                if avail ef and (ef.xgrain = "S" or ef.xgrain = "B") then
                    assign xg-flag = yes.

              end. /* avail job-mat */
              else do:

                display {1} po-ordl.ord-qty
                        po-ordl.i-name
                        /* po-ordl.vend-i-no */
                        v-job
                        po-ordl.cost
                        po-ordl.pr-uom
                        v-sqft
                        po-ordl.dscr[1]
                        po-ordl.due-date
                        v-change-dscr.
          
                if po-ordl.dscr[2] ne "" then
                  put {1} po-ordl.dscr[2] format "x(50)" at 8 skip.

              end.

            end. /* avail job */
            else do:

              display {1} po-ordl.ord-qty
                      po-ordl.i-name
                      /* po-ordl.vend-i-no */
                      v-job
                      po-ordl.cost
                      po-ordl.pr-uom
                      v-sqft
                      po-ordl.dscr[1]
                      po-ordl.due-date        
                      v-change-dscr.
          
              if po-ordl.dscr[2] ne "" then
                put {1} po-ordl.dscr[2] format "x(50)" at 8 skip.
            end.

            put {1} "W: " at 15 v-wid space(2) "L: " v-len
                space(2) "Flute: " item.flute space(2) item.reg-no.

            run po/po-ordls.p (recid(po-ordl)).
            
            {po/poprints.i}
            
                if not v-test-scr then
                  put {1}
                      skip
                      space(14)
                      "Score: "
                      len-score format "x(50)".
          
                else
                if dec(trim(len-score)) ne v-wid then
                  put {1}
                      skip
                      space(14)
                      "Score: "
                      len-score format "x(50)".
              end.
              END.
            end.
          end. /* if item-type = B */
          
          else do:

            display {1} po-ordl.ord-qty
                    po-ordl.i-name
                    /* po-ordl.vend-i-no */
                    v-job
                    po-ordl.cost
                    po-ordl.pr-uom
                    v-sqft
                    po-ordl.dscr[1]
                    po-ordl.due-date        
                    v-change-dscr.

            if po-ordl.dscr[2] ne "" then
              put {1} po-ordl.dscr[2] format "x(50)" at 8 skip.
              end.

        end. /* if avail item */

        else do:
          display {1} po-ordl.ord-qty
                  po-ordl.i-name
                  v-job
                  po-ordl.cost 
                  po-ordl.pr-uom
                  v-sqft
                  po-ordl.dscr[1]
                  po-ordl.due-date
                  v-change-dscr.

          if po-ordl.dscr[2] ne "" then
             put {1} po-ordl.dscr[2] format "x(50)" at 8 skip.

          IF v-itemDescription THEN
          DO:
              FIND FIRST itemfg WHERE
                  itemfg.company = po-ordl.company AND
                  itemfg.i-no = po-ordl.i-no
                  NO-LOCK NO-ERROR.

              IF AVAIL itemfg AND itemfg.part-dscr3 NE '' THEN
                 PUT {1} itemfg.part-dscr3 format "x(50)" at 8 skip.
          END. /* avail itemfg */

        end.
       
        {custom/notesprt.i po-ordl v-inst 4}
        PUT {1} SKIP.
        DO i = 1 TO 4:
           IF v-inst[i] <> "" THEN DO:
             IF LINE-COUNTER gt page-size then DO:
                page {1}.                
             END.   
             PUT {1} v-inst[i] SKIP.             
           END.
       END.
       IF LINE-COUNTER  gt page-size then DO:
          page {1}.
       END.  

        put {1} skip fill("-",80) format "x(80)" skip.

      end. /* for each po-ordl record */


 {custom/notesprt.i po-ord v-inst 4}
 DO i = 1 TO 4:
           IF v-inst[i] <> "" THEN DO:
             IF LINE-COUNTER   gt page-size then DO:
                page {1}.
                
             END.
             PUT {1} v-inst[i] SKIP.
           END.
 END.
 IF LINE-COUNTER  gt page-size then DO:
          page {1}.
 END.  

      v-lastpage-num = page-number.

      hide {1} frame po-cont.
      view {1} frame po-tots.

/* END ----------------------------------- Copr. 1997  Advanced Software Inc. */





