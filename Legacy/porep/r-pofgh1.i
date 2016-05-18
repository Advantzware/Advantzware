EMPTY TEMP-TABLE tt-report.
DEF VAR v-i-name AS CHAR NO-UNDO.
DEF VAR v-vend-name AS CHAR NO-UNDO.

   if tb_excel then
   do:
      output stream s-temp TO VALUE(fi_file).
      /*str_buffa = "".
      {sys/inc/outstrPL.i v-hdr 1 218}.*/
      PUT STREAM s-temp UNFORMATTED v-hdr SKIP.
   end.

   for each po-ord
       where po-ord.company eq cocode
         and po-ord.po-no   ge v-s-pono
         and po-ord.po-no   le v-e-pono
         and po-ord.po-date ge v-s-date
         and po-ord.po-date le v-e-date
         and po-ord.due-date ge v-sdue-date
         and po-ord.due-date le v-edue-date
         and po-ord.vend-no ge v-s-vend
         and po-ord.vend-no le v-e-vend
         and ((po-ord.opened eq no and v-stat eq "C") or
              (po-ord.opened eq yes and v-stat eq "O") or v-stat eq "A")
       no-lock,

       each po-ordl WHERE 
            po-ordl.company EQ po-ord.company
         AND po-ordl.po-no EQ po-ord.po-no
         AND po-ordl.i-no      ge v-s-item
         and po-ordl.i-no      le v-e-item
         and po-ordl.vend-i-no ge v-s-vitm
         and po-ordl.vend-i-no le v-e-vitm
         and ((po-ordl.stat eq "C" and v-stat eq "C") or
              (po-ordl.stat ne "C" and v-stat eq "O") or v-stat eq "A")
         and ((po-ordl.item-type eq yes and v-type eq "R") or
              (po-ordl.item-type eq no  and v-type eq "F") or v-type eq "B")
         AND (tb_po                                  OR
              (po-ordl.item-type AND
               CAN-FIND(FIRST rm-rcpth {&where-rm})) OR
              (NOT po-ordl.item-type AND
               CAN-FIND(FIRST fg-rcpth {&where-fg})) OR
              (begin_r-date EQ 01/01/0001 AND end_r-date EQ 12/31/9999))
       no-lock:
       
       {custom/statusMsg.i " 'Processing PO#  '  + string(po-ord.po-no) "}

     release item.
     release itemfg.
     release job-hdr.
     release oe-ordl.
     release oe-ord.
     
     IF po-ordl.item-type THEN DO:
        FIND FIRST item
            WHERE item.company EQ cocode
              AND item.i-no    EQ po-ordl.i-no
            NO-ERROR.
        IF NOT AVAIL item THEN NEXT.
     END.

     ELSE DO:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ cocode
               AND itemfg.i-no    EQ po-ordl.i-no
             NO-ERROR.
        IF NOT AVAIL itemfg THEN NEXT.
     END.
     
     if po-ordl.job-no ne "" then
        for each job-hdr
            where job-hdr.company eq cocode
              and job-hdr.job-no  eq po-ordl.job-no
              and job-hdr.job-no2 eq po-ordl.job-no2
            no-lock
            break by job-hdr.frm desc:
            
          if job-hdr.frm eq po-ordl.s-num or
             last(job-hdr.frm)            then leave.
        end.
     
     if avail job-hdr and job-hdr.ord-no ne 0 then do:
        find first oe-ordl
            where oe-ordl.company  eq cocode
              and oe-ordl.ord-no   eq job-hdr.ord-no
              and oe-ordl.i-no     eq job-hdr.i-no
            no-lock no-error.
            
        if not avail oe-ordl then
           find first oe-ordl
                where oe-ordl.company  eq cocode
                  and oe-ordl.ord-no   eq job-hdr.ord-no
                no-lock no-error.
     end.
     
     else
     if not po-ordl.item-type then
        find first oe-ordl
             where oe-ordl.company  eq cocode
               and oe-ordl.po-no-po eq po-ordl.po-no
               and oe-ordl.i-no     eq po-ordl.i-no
             no-lock no-error.
         
     if avail oe-ordl then
        find first oe-ord
             where oe-ord.company  eq cocode
               and oe-ord.ord-no   eq oe-ordl.ord-no
             no-lock no-error.
            
     v-cust-no = if avail job-hdr then job-hdr.cust-no else
                 if avail oe-ord  then oe-ord.cust-no  else "xxxxxxxxxxxxx".
                 
     find first cust
         where cust.company eq cocode
           and cust.cust-no eq v-cust-no
         no-lock no-error.
     
     if index(v-mattype-list,"A") ne 0 and
        avail job-hdr                  and 
        avail item                     and
        item.mat-type eq "B"           then
     for each job-mat
         where job-mat.company  eq cocode
           and job-mat.job      eq job-hdr.job
           and job-mat.frm      eq job-hdr.frm
           and job-mat.job-no   eq job-hdr.job-no
           and job-mat.job-no2  eq job-hdr.job-no2
           and job-mat.rm-i-no  eq po-ordl.i-no
         no-lock,
         
         each xjob-mat
         where xjob-mat.company  eq cocode
           and xjob-mat.job      eq job-mat.job
           and xjob-mat.frm      eq job-mat.frm
           and xjob-mat.job-no   eq job-mat.job-no
           and xjob-mat.job-no2  eq job-mat.job-no2
           and can-find(first item where item.company  eq cocode
                                     and item.i-no     eq xjob-mat.i-no
                                     and item.mat-type eq "A")
         no-lock:
         
       run create-report (xjob-mat.i-no, xjob-mat.qty-uom).
     end.
     
     if avail item and index(v-mattype-list,item.mat-type) eq 0 then next.

     run create-report (po-ordl.i-no, "").
   end.

   RELEASE tt-report.

   for each tt-report where tt-report.term-id eq "",

       first po-ordl no-lock where recid(po-ordl) eq tt-report.rec-id,

       first po-ord WHERE
             po-ord.company eq po-ordl.company and
             po-ord.po-no eq po-ordl.po-no no-lock

       break by tt-report.key-01
             by tt-report.key-02
             by tt-report.key-03
             by tt-report.key-04

       transaction:

       {custom/statusMsg.i " 'Processing PO#  '  + string(po-ord.po-no) "}

     if first-of(tt-report.key-02) then v-first[2] = tt-report.key-02.
     if first-of(tt-report.key-03) then v-first[3] = tt-report.key-03.
     if first-of(tt-report.key-04) then v-first[4] = tt-report.key-04.

     release item.
     release itemfg.

     v-cons-qty = po-ordl.cons-qty.
     
     if po-ordl.item-type then do:
       find first item
           where item.company eq cocode
             and item.i-no    eq tt-report.key-03
           no-lock no-error.
           
       if po-ordl.i-no ne tt-report.key-03 and
          tt-report.key-07 ne ""           then do:
          
         find first item
             where item.company eq cocode
               and item.i-no    eq po-ordl.i-no
             no-lock no-error. 
          
         if po-ordl.cons-uom ne "EA" then
           run sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                                   po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                   v-cons-qty, output v-cons-qty).
       
         find first item
             where item.company eq cocode
               and item.i-no    eq tt-report.key-03
             no-lock no-error.
           
         if "EA" ne tt-report.key-07 then
           run sys/ref/convquom.p ("EA", tt-report.key-07, item.basis-w,
                                   po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                   v-cons-qty, output v-cons-qty).
       end.
     end.
     
     else do:
       find first itemfg
           where itemfg.company eq cocode
             and itemfg.i-no    eq po-ordl.i-no
           no-lock no-error.
       if po-ordl.cons-uom ne "EA" then
         run sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0, 
                                v-cons-qty, output v-cons-qty).
     end.      

     assign
      v-ord[4]  = v-ord[4] + v-cons-qty
      v-balance = v-cons-qty.
      
     find first cust
         where cust.company eq cocode
           and cust.cust-no eq tt-report.key-09
         no-lock no-error.
         
     find first vend
         where vend.company eq cocode
           and vend.vend-no eq po-ord.vend-no
         no-lock no-error.

     IF TB_separate-dates = NO THEN DO:

        IF rd_show-2 = "FG Item#" THEN
           display tt-report.key-09     when first-of(tt-report.key-01)
                                     @ cust.cust-no
                   po-ord.po-no
                   po-ord.vend-no    @ vend.name
                     vend.name       when avail vend and vend.name ne ""
                   tt-report.key-04  COLUMN-LABEL "FG Item#"
                   po-ordl.i-name
                    /*item.i-name       when avail item
                     itemfg.i-name   when avail itemfg 
                                     @ item.i-name*/
                   po-ordl.s-wid
                   po-ordl.s-len FORMAT ">>>>>9.9<<<"
                   po-ordl.cost
                   po-ordl.pr-uom
                   po-ordl.due-date  FORMAT "99/99/99" @ rm-rcpth.trans-date
          
               with frame main NO-ERROR.
         ELSE
            display tt-report.key-09     when first-of(tt-report.key-01)
                                     @ cust.cust-no
                   po-ord.po-no
                   po-ord.vend-no    @ vend.name
                     vend.name       when avail vend and vend.name ne ""
                   tt-report.key-04  COLUMN-LABEL "Cust Part#"
                   po-ordl.i-name
                    /*item.i-name       when avail item
                     itemfg.i-name   when avail itemfg 
                                     @ item.i-name*/
                   po-ordl.s-wid
                   po-ordl.s-len FORMAT ">>>>>9.9<<<"
                   po-ordl.cost
                   po-ordl.pr-uom
                   po-ordl.due-date  FORMAT "99/99/99" @ rm-rcpth.trans-date
          
               with frame main-2 NO-ERROR.
     END.
     ELSE DO:
        IF rd_show-2 = "FG Item#" THEN
           display tt-report.key-09     when first-of(tt-report.key-01)
                                     @ cust.cust-no
                   po-ord.po-no
                   po-ord.vend-no    @ vend.name
                     vend.name       when avail vend and vend.name ne ""
                   tt-report.key-04  COLUMN-LABEL "FG Item#"
                   po-ordl.i-name
                    /*item.i-name       when avail item
                     itemfg.i-name   when avail itemfg 
                                     @ item.i-name*/
                   po-ordl.s-wid
                   po-ordl.s-len FORMAT ">>>>>9.9<<<"
                   po-ordl.cost
                   po-ordl.pr-uom
                   po-ordl.due-date  FORMAT "99/99/99"
               with frame main2 NO-ERROR.
        ELSE
           display tt-report.key-09     when first-of(tt-report.key-01)
                                     @ cust.cust-no
                   po-ord.po-no
                   po-ord.vend-no    @ vend.name
                     vend.name       when avail vend and vend.name ne ""
                   tt-report.key-04  COLUMN-LABEL "Cust Part#"
                   po-ordl.i-name
                    /*item.i-name       when avail item
                     itemfg.i-name   when avail itemfg 
                                     @ item.i-name*/
                   po-ordl.s-wid
                   po-ordl.s-len FORMAT ">>>>>9.9<<<"
                   po-ordl.cost
                   po-ordl.pr-uom
                   po-ordl.due-date  FORMAT "99/99/99"
               with frame main2-2 NO-ERROR.
     END.

     if tb_excel THEN
     DO:
        
        IF AVAIL vend AND vend.NAME <> "" THEN
           v-vend-name = vend.NAME.
        ELSE
           v-vend-name = po-ord.vend-no.

        /*if avail itemfg THEN*/
         ASSIGN  v-i-name = po-ordl.i-name.
       /* ELSE
           v-i-name = ITEM.i-name.*/

        ASSIGN
           v-i-name = REPLACE(REPLACE(v-i-name,'"', " "),","," ")
           v-vend-name = REPLACE(REPLACE(v-vend-name,'"', " "),","," ")
           str_buffa = tt-report.key-09 + v-comma +
                       string(po-ord.po-no,">>>>>9") + v-comma +
                       v-vend-name + v-comma +
                       tt-report.key-04 + v-comma +
                       v-i-name + v-comma +
                       string(po-ordl.s-wid,">>9.9999") + v-comma +
                       string(po-ordl.s-len,">>9.9999") + v-comma +
                       string(po-ordl.cost,"->>>9.99")  + v-comma +
                       po-ordl.pr-uom .
       
        PUT STREAM s-temp UNFORMATTED '"' REPLACE(str_buffa,',','","') '"' SKIP.
     END.
 
     if po-ordl.item-type then
     for each rm-rcpth no-lock {&where-rm},

         each rm-rdtlh
         where rm-rdtlh.r-no eq rm-rcpth.r-no AND
               rm-rdtlh.rita-code EQ rm-rcpth.rita-code AND
               rm-rdtlh.s-num     EQ po-ordl.s-num
         no-lock

         break by rm-rcpth.trans-date by rm-rcpth.r-no:

       if not first(rm-rcpth.trans-date) then do:
          v-first[4] = "".
          IF TB_separate-dates = NO THEN DO:
         
            IF rd_show-2 = "FG Item#" THEN
            DO:
               down with frame main.
               clear frame main.
            END.
            ELSE
            DO:
               down with frame main-2.
               clear frame main-2.
            END.
         
          END.
          ELSE DO:
         
            IF rd_show-2 = "FG Item#" THEN
            DO:
               down with frame main2.
               clear frame main2.
            END.
            ELSE
            DO:
               down with frame main2-2.
               clear frame main2-2.
            END.
          END.
       end.
       
       v-cons-qty = rm-rdtlh.qty.
       
       if po-ordl.i-no ne tt-report.key-03 and
          tt-report.key-07 ne ""           then do:
          
          find first item
              where item.company eq cocode
                and item.i-no    eq po-ordl.i-no
              no-lock no-error. 
           
          if po-ordl.cons-uom ne "EA" then
             run sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                                     po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                     v-cons-qty, output v-cons-qty).
         
          find first item
              where item.company eq cocode
                and item.i-no    eq tt-report.key-03
              no-lock no-error.
          
          if "EA" ne tt-report.key-07 then
             run sys/ref/convquom.p ("EA", tt-report.key-07, item.basis-w,
                                     po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                     v-cons-qty, output v-cons-qty).
       end.
       
       assign
          v-qty[4]  = v-qty[4] + v-cons-qty
          v-balance = v-balance - v-cons-qty.
     
       IF TB_separate-dates = NO THEN DO:

          IF rd_show-2 = "FG Item#" THEN
             display rm-rcpth.trans-date FORMAT "99/99/99"
                     v-cons-qty @ v-fg-rdtlhqty
                with frame main NO-ERROR.
          ELSE
             display rm-rcpth.trans-date FORMAT "99/99/99"
                     v-cons-qty @ v-fg-rdtlhqty
                with frame main-2 NO-ERROR.
            
          if tb_excel then
          DO:
             ASSIGN
             str_buffb = str_buffa + v-comma +
                         (if rm-rcpth.trans-date <> ? 
                         then string(rm-rcpth.trans-date,"99/99/9999")
                         else string(po-ordl.due-date,"99/99/9999")) + v-comma +
                         string( v-cons-qty,"->>>>>>9.999").
            PUT STREAM s-temp UNFORMATTED '"' REPLACE(str_buffb,',','","') '"' SKIP.
            /*PUT STREAM s-temp UNFORMATTED str_buffb SKIP.*/
             
          END.
       END.
       ELSE DO:
          IF rd_show-2 = "FG Item#" THEN
             display 
                po-ordl.due-date FORMAT "99/99/99" 
                rm-rcpth.trans-date FORMAT "99/99/99"
                v-cons-qty @ v-fg-rdtlhqty
                with frame main2 NO-ERROR.
          ELSE
             display 
                po-ordl.due-date FORMAT "99/99/99" 
                rm-rcpth.trans-date FORMAT "99/99/99"
                v-cons-qty @ v-fg-rdtlhqty
                with frame main2-2 NO-ERROR.
                  
          if tb_excel then
          DO:
              
             ASSIGN
             str_buffb = str_buffa + v-comma
                       + STRING(po-ordl.due-date,"99/99/9999")
                       + v-comma
                       + STRING(rm-rcpth.trans-date,"99/99/9999")
                       + v-comma 
                       + STRING(v-cons-qty,"->>>>>>9.999").
            
             PUT STREAM s-temp UNFORMATTED '"' REPLACE(str_buffb,',','","') '"' SKIP.
             /*PUT STREAM s-temp UNFORMATTED str_buffb SKIP.*/
          END.
       END.
     end.                                

     else
     for each fg-rcpth NO-LOCK {&where-fg},

         each fg-rdtlh
         where fg-rdtlh.r-no eq fg-rcpth.r-no
         no-lock

         break by fg-rcpth.trans-date BY fg-rdtlh.trans-time by fg-rcpth.r-no:

         if not first(fg-rcpth.trans-date) then do:
            v-first[4] = "".
         
            IF rd_show-2 = "FG Item#" THEN
            DO:
               down with frame main.
               clear frame main.
            END.
            ELSE
            DO:
               down with frame main-2.
               clear frame main-2.
            END.
          end.
         
          assign
           v-qty[4]  = v-qty[4] + fg-rdtlh.qty
           v-balance = v-balance - fg-rdtlh.qty
           v-fg-rdtlhqty = fg-rdtlh.qty.
         
          IF TB_separate-dates = NO THEN DO:
         
             IF rd_show-2 = "FG Item#" THEN
                display fg-rcpth.trans-date FORMAT "99/99/99" @ rm-rcpth.trans-date
                        v-fg-rdtlhqty  
                with frame main NO-ERROR.
             ELSE
                display fg-rcpth.trans-date FORMAT "99/99/99" @ rm-rcpth.trans-date
                        v-fg-rdtlhqty  
                with frame main-2 NO-ERROR.
         
             if tb_excel then
             DO:
                ASSIGN
                str_buffb = str_buffa + v-comma +
                            (if fg-rcpth.trans-date <> ? 
                            then string(fg-rcpth.trans-date,"99/99/9999")
                            else string(po-ordl.due-date,"99/99/9999")) + v-comma +
                            string(fg-rdtlh.qty,"->>>>>>9.999").
                PUT STREAM s-temp UNFORMATTED '"' REPLACE(str_buffb,',','","') '"' SKIP.
                /*PUT STREAM s-temp UNFORMATTED str_buffb SKIP.                           */
             END.
          END.
          ELSE DO:
             IF rd_show-2 = "FG Item#" THEN
                display 
                   po-ordl.due-date
                   fg-rcpth.trans-date FORMAT "99/99/99" @ rm-rcpth.trans-date
                   v-fg-rdtlhqty  
                   with frame main2 NO-ERROR. 
             ELSE
                display 
                   po-ordl.due-date
                   fg-rcpth.trans-date FORMAT "99/99/99" @ rm-rcpth.trans-date
                   v-fg-rdtlhqty  
                   with frame main2-2 NO-ERROR. 
              
             if tb_excel then
             DO:
                ASSIGN
                str_buffb = str_buffa + v-comma
                          + STRING(po-ordl.due-date,"99/99/9999") 
                          + v-comma
                          + STRING(fg-rcpth.trans-date,"99/99/9999")
                          + v-comma 
                          + STRING(fg-rdtlh.qty,"->>>>>>9.999").

                PUT STREAM s-temp UNFORMATTED '"' REPLACE(str_buffb,',','","') '"' SKIP.
                /*PUT STREAM s-temp UNFORMATTED str_buffb SKIP.                           */
             END.
          END.
     end. /*end for each fg-rcpth*/
     
     IF TB_separate-dates = NO THEN DO:
        IF rd_show-2 = "FG Item#" THEN
        DO:
           down with frame main.
           clear frame main.
        END.
        ELSE
        DO:
           down with frame main-2.
           clear frame main-2.
        END.
        put fill("-",135) format "x(135)" skip.
     END.
     ELSE DO:
        IF rd_show-2 = "FG Item#" THEN
        DO:
           down with frame main2.
           clear frame main2.
        END.
        ELSE
        DO:
           down with frame main2-2.
           clear frame main2-2. 
        END.
        put fill("-",143) format "x(143)" skip.
     END.
     
     v-bal[4] = v-bal[4] + v-balance.

     if last-of(tt-report.key-01) then put skip(1).

     v-first = "".
   end.   
 
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
  OUTPUT CLOSE.
