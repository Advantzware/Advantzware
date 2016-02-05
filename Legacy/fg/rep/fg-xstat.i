/*******************************************************************
  Program: fg-xstat.i
 
   Author: Advanced Software
  Written:
  Updated: 01/18/07
  
Called by: r-stajob.w

 *******************************************************************/
DEFINE VARIABLE chrCust   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrPoNo   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrINo    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrPart   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrName   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrJob    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrOrdQty AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrShpQty AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrQtyOH  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrPrice  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrTotV   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrSman   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrTransDte AS CHARACTER  NO-UNDO.


DEFINE VARIABLE decPrice    AS DECIMAL    NO-UNDO.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    each cust
   where cust.company eq cocode
     and cust.cust-no EQ ttCustList.cust-no /*fcst*/
    /* and cust.cust-no le tcst*/
     and cust.sman    ge fslm
     and cust.sman    le tslm
   no-lock
   by cust.cust-no:

      assign
         v-frst     = yes
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0
         v-print    = no.

    if not v-rec-dat then
    do:
        if line-counter ge 56 then page.

        for each oe-ordl
            no-lock
           where oe-ordl.company eq cocode
             and oe-ordl.cust-no eq cust.cust-no
             and oe-ordl.po-no   ge fpo#
             and oe-ordl.po-no   le tpo#,

            first oe-ord where oe-ord.company eq oe-ordl.company 
                           and oe-ord.ord-no eq oe-ordl.ord-no
                           and ((oe-ord.opened EQ YES AND v-ocb NE "C") OR
                                (oe-ord.opened EQ NO  AND v-ocb NE "O"))
                         no-lock,

            first itemfg where itemfg.company eq cocode
                           and itemfg.i-no    eq oe-ordl.i-no
                           and itemfg.cust-no eq cust.cust-no
                           and (itemfg.i-code eq typex or typex eq "A")
                        no-lock
            break 
                  by if v-sortby then oe-ordl.part-no ELSE oe-ordl.job-no
                  by IF NOT v-sortby then oe-ordl.job-no2 else ""
                  by oe-ordl.i-no
                  by oe-ordl.job-no
                  by oe-ordl.job-no2 :

                  {custom/statusMsg.i " 'Processing Order#  '  + string(oe-ordl.ord-no) "}

            FOR EACH tt-oe-rel NO-LOCK:
                DELETE tt-oe-rel.
            END.
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
                         FIND FIRST sman WHERE sman.company = itemfg.company
                             AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                         IF AVAIL sman THEN v-sales-rep = sman.sman.
                         LEAVE .
                     END.
                  END. /* end of cust-part */
         
                  IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
                      FIND FIRST sman WHERE sman.company = cust.company
                          AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                      IF AVAIL sman THEN v-sales-rep = sman.sman.
                  END.
            END.
            ELSE DO:
                FIND FIRST sman WHERE sman.company = cust.company
                    AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                IF AVAIL sman THEN v-sales-rep = sman.sman.
            END.
               

           FOR EACH oe-rel
           where oe-rel.company EQ oe-ordl.company
             AND oe-rel.ord-no  EQ oe-ordl.ord-no
             AND oe-rel.i-no    EQ oe-ordl.i-no
             AND oe-rel.line    EQ oe-ordl.line
           NO-LOCK:
               FIND FIRST oe-rell NO-LOCK
                   WHERE oe-rell.company  EQ cocode
                   AND oe-rell.ord-no   EQ oe-rel.ord-no
                   /*AND oe-rell.r-no     EQ oe-rel.link-no*/
                   AND oe-rell.rel-no   EQ oe-rel.rel-no
                   AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                   AND oe-rell.i-no     EQ oe-rel.i-no
                   AND oe-rell.line     EQ oe-rel.LINE NO-ERROR .
               FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
               
               CREATE tt-oe-rel.
               ASSIGN
                   tt-oe-rel.rel-date = string(oe-rel.rel-date)
                   tt-oe-rel.tot-qty  = oe-rel.tot-qty .
               IF AVAIL oe-relh  THEN 
                   ASSIGN  tt-oe-rel.rel-no   = oe-relh.release#.
             END.

             RUN oe/ordlsqty.p (ROWID(oe-ordl), 
                              OUTPUT li-inv-qty, 
                              OUTPUT li-ship-qty).

            v-frst-ord = yes.

            IF CAN-FIND(first fg-bin where fg-bin.company        eq cocode
                                       and fg-bin.i-no           eq itemfg.i-no
                                       and fg-bin.job-no         eq oe-ordl.job-no
                                       and fg-bin.job-no2        eq oe-ordl.job-no2
                                       and (v-custown or 
                                            (fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "")))
            then do:
                for each fg-bin where fg-bin.company        eq cocode
                                  and fg-bin.i-no           eq itemfg.i-no
                                  and fg-bin.job-no         eq oe-ordl.job-no
                                  and fg-bin.job-no2        eq oe-ordl.job-no2
                                  and (v-custown or 
                                       (fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq ""))
                                use-index co-ino no-lock
                    break by fg-bin.loc:

                    v-qty-onh = v-qty-onh + fg-bin.qty.

                    if last-of(fg-bin.loc)      and
                       (v-qty-onh ne 0 or zbal) then 
                    do:
                        if itemfg.sell-uom   eq "CS" and
                           itemfg.case-count ne 0    then
                          v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                        else if itemfg.sell-uom eq "L" then 
                          v-ext = oe-ordl.price.
                        else
                        do:
                            find first uom where uom.uom  eq itemfg.sell-uom
                                             and uom.mult ne 0
                                 no-lock no-error.

                            v-ext = v-qty-onh * oe-ordl.price /
                            (if avail uom then uom.mult else 1000).
                        end.

                        if v-prt-cpn then 
                        do:
                            v-job-no = "".
                            if oe-ordl.job-no ne "" then
                              v-job-no = STRING(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99").

/*XXX*/
                            v-disp-item = yes.
/*                          if (v-qty-onh eq 0) and (itemfg.q-ship gt oe-ordl.qty) then do: */
                            if (v-qty-onh eq 0) and (li-ship-qty ge oe-ordl.qty) then 
                            do:
                               v-disp-item = no.                                    
                            end.

                            if v-disp-item THEN do:
                               IF rd_smry-dtl = "S" THEN DO:
                                display cust.cust-no        when v-frst
                                        oe-ordl.po-no       when v-frst-ord
             /*                         cust.sman           when v-frst*/
                                        oe-ordl.i-no        when v-frst-ord
                                        oe-ordl.part-no     when v-frst-ord
                                        oe-ordl.i-name      when v-frst-ord
                                        v-job-no            when v-frst-ord
             /*                         fg-bin.loc*/
                                        oe-ordl.qty         when v-frst-ord
                                        li-ship-qty         when v-frst-ord
                                        v-qty-onh           when 
                                                            first-of(oe-ordl.job-no) or
                                                            first-of(oe-ordl.job-no2)
                                        oe-ordl.price
                                        v-ext               when 
                                                            first-of(oe-ordl.job-no) or
                                                            first-of(oe-ordl.job-no2)
                                   with frame itemx1.
                                down with frame itemx1.
                               END.
                                ELSE DO:
                                display cust.cust-no        when v-frst
                                        oe-ordl.po-no       when v-frst-ord
                                        oe-ordl.i-no        when v-frst-ord
                                        oe-ordl.part-no     when v-frst-ord
                                        oe-ordl.i-name      when v-frst-ord
                                        v-job-no            when v-frst-ord
                                        oe-ordl.qty         when v-frst-ord
                                        li-ship-qty         when v-frst-ord
                                        v-qty-onh           when 
                                                            first-of(oe-ordl.job-no) or
                                                            first-of(oe-ordl.job-no2)
                                        oe-ordl.price
                                        v-ext               when 
                                                            first-of(oe-ordl.job-no) or
                                                            first-of(oe-ordl.job-no2) SKIP
                                   with frame itemx5.
                                down with frame itemx5.
                               
                                FOR EACH tt-oe-rel NO-LOCK:
                                PUT SPACE(72)
                                    tt-oe-rel.rel-no SPACE(4)  
                                    tt-oe-rel.rel-date SPACE(4)
                                    tt-oe-rel.tot-qty SKIP.
                                END.
                                END.
                                IF tb_excel THEN 
                                DO: 
                                    ASSIGN
                                        chrCust = IF v-frst 
                                                  THEN cust.cust-no
                                                  ELSE ""
                                        chrPoNo = IF v-frst-ord
                                                  THEN oe-ordl.po-no
                                                  ELSE ""
                                        chrINo =  IF v-frst-ord
                                                  THEN oe-ordl.i-no 
                                                  ELSE ""
                                        chrPart = IF v-frst-ord
                                                  THEN oe-ordl.part-no
                                                  ELSE ""
                                        chrName = IF v-frst-ord
                                                  THEN oe-ordl.i-name
                                                  ELSE ""
                                        chrJob =  IF v-frst-ord
                                                  THEN v-job-no 
                                                  ELSE ""
                                        chrOrdQty = IF v-frst-ord
                                                    THEN STRING(oe-ordl.qty)
                                                    ELSE ""
                                        chrShpQty = IF v-frst-ord
                                                    THEN STRING(li-ship-qty)
                                                    ELSE ""

                                        chrQtyOH = IF first-of(oe-ordl.job-no) or
                                                      first-of(oe-ordl.job-no2)
                                                   THEN STRING(v-qty-onh)
                                                   ELSE ""

                                        chrTotV =  IF first-of(oe-ordl.job-no) or
                                                      first-of(oe-ordl.job-no2)
                                                   THEN STRING(v-ext)
                                                   ELSE "".

                                    EXPORT STREAM excel DELIMITER "," 
                                        chrCust chrPoNo chrINo chrPart
                                        chrName chrJob chrOrdQty chrShpQty
                                        chrQtyOH oe-ordl.price chrTotV.
                                    IF rd_smry-dtl = "D" THEN
                                        FOR EACH tt-oe-rel NO-LOCK:
                                        EXPORT STREAM excel DELIMITER ","
                                            tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                            tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                            tt-oe-rel.rel-no tt-oe-rel.rel-date tt-oe-rel.tot-qty.
                                        END.  
                                        
                                END.

                                ASSIGN
                                 v-frst  = NO
                                 v-print = YES.
                            END. /* v-disp-item */
                        end. /* v-prt-cpn */
                        else
                        do:
                           IF rd_smry-dtl = "S" THEN DO:
                            display cust.cust-no        when v-frst
                                    oe-ordl.po-no       when v-frst-ord
                                    v-sales-rep         when v-frst
                                    oe-ordl.i-no        when v-frst-ord
                                    oe-ordl.i-name      when v-frst-ord
                                    fg-bin.loc
                                    oe-ordl.qty         when v-frst-ord
                                    li-ship-qty         when v-frst-ord
                                    v-qty-onh           when 
                                                        first-of(oe-ordl.job-no) or
                                                        first-of(oe-ordl.job-no2)
                                    oe-ordl.price
                                    v-ext               when 
                                                        first-of(oe-ordl.job-no) or
                                                        first-of(oe-ordl.job-no2)
                               with frame itemx3.
                            down with frame itemx3.
                           END.
                           ELSE DO:
                            display cust.cust-no        when v-frst
                                    oe-ordl.po-no       when v-frst-ord
                                    v-sales-rep         when v-frst
                                    oe-ordl.i-no        when v-frst-ord
                                    oe-ordl.i-name      when v-frst-ord
                                    fg-bin.loc
                                    oe-ordl.qty         when v-frst-ord
                                    li-ship-qty         when v-frst-ord
                                    v-qty-onh           when 
                                                        first-of(oe-ordl.job-no) or
                                                        first-of(oe-ordl.job-no2)
                                    oe-ordl.price
                                    v-ext               when 
                                                        first-of(oe-ordl.job-no) or
                                                        first-of(oe-ordl.job-no2) SKIP
                               with frame itemx7.
                            down with frame itemx7.
                            FOR EACH tt-oe-rel NO-LOCK: 
                            PUT SPACE(57)
                                tt-oe-rel.rel-no SPACE(4)  
                                tt-oe-rel.rel-date SPACE(4)
                                tt-oe-rel.tot-qty SKIP.
                           END.
                           END.

                            IF tb_excel THEN 
                            DO:
                                ASSIGN
                                  chrCust = IF v-frst 
                                            THEN cust.cust-no
                                            ELSE ""
                                  chrPoNo = IF v-frst-ord
                                            THEN oe-ordl.po-no
                                            ELSE ""
                                  chrSman = IF v-frst 
                                            THEN v-sales-rep
                                            ELSE "" 
                                  chrINo =  IF v-frst-ord
                                            THEN oe-ordl.i-no 
                                            ELSE ""
                                  chrName = IF v-frst-ord
                                            THEN oe-ordl.i-name
                                            ELSE ""
                                  chrOrdQty = IF v-frst-ord
                                              THEN STRING(oe-ordl.qty)
                                              ELSE ""
                                  chrShpQty = IF v-frst-ord
                                              THEN STRING(li-ship-qty)
                                              ELSE ""
                                  chrQtyOH = IF first-of(oe-ordl.job-no) or
                                                first-of(oe-ordl.job-no2)
                                             THEN STRING(v-qty-onh)
                                             ELSE ""
                                  chrTotV =  IF first-of(oe-ordl.job-no) or
                                                first-of(oe-ordl.job-no2)
                                             THEN STRING(v-ext)
                                             ELSE "".

                                EXPORT STREAM excel DELIMITER "," 
                                    chrCust chrPoNo chrSman chrINo 
                                    chrName fg-bin.loc chrOrdQty chrShpQty
                                       chrQtyOH oe-ordl.price chrTotV.
                                IF rd_smry-dtl = "D" THEN
                                    FOR EACH tt-oe-rel NO-LOCK:
                                    EXPORT STREAM excel DELIMITER ","
                                        tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                        tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                        tt-oe-rel.rel-no tt-oe-rel.rel-date tt-oe-rel.tot-qty.
                                    END.  

                            END.

                            ASSIGN
                             v-frst  = NO
                             v-print = YES.
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
                            v-tot-ship       = v-tot-ship       + li-ship-qty
                            v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                            v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.

                        assign
                          v-qty-onh   = 0
                          v-frst-ord  = no.
                    end.  /* last of fg bin */
                end.  /* for each fg bin */
            end. /*if avail fg bin*/
            else /* not avail fg bin */
            do:
                if itemfg.sell-uom   eq "CS" and
                   itemfg.case-count ne 0    then
                  v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                else if itemfg.sell-uom eq "L" then 
                  v-ext = oe-ordl.price.
                else
                do:
                    find first uom where uom.uom  eq itemfg.sell-uom
                                     and uom.mult ne 0
                                   no-lock no-error.

                    v-ext = 
             v-qty-onh * oe-ordl.price / (if avail uom then uom.mult else 1000).
                end.

                v-job-no = "".
                if oe-ordl.job-no ne "" then
                  v-job-no = STRING(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99").
/*XXX*/
                v-disp-item = yes.
/*              if (v-qty-onh eq 0) and (itemfg.q-ship gt oe-ordl.qty) then do: */
                if (v-qty-onh eq 0) and (li-ship-qty ge oe-ordl.qty) then 
                do:
                    v-disp-item = no.                                    
                end.

/****************************************************                
                if (zbal eq no) and (v-qty-onh eq 0) then do:
                    v-disp-item = no.
                end.
********************************************************/

                if v-prt-cpn then 
                do:
                    if v-disp-item THEN do:
                       IF rd_smry-dtl = "S" THEN DO:
                         display cust.cust-no        when v-frst
                                oe-ordl.po-no       when v-frst-ord
/*                              cust.sman           when v-frst */
                                oe-ordl.i-no        when v-frst-ord
                                oe-ordl.part-no     when v-frst-ord
                                oe-ordl.i-name      when v-frst-ord
                                v-job-no            when v-frst-ord
/*                              fg-bin.loc*/
                                oe-ordl.qty         when v-frst-ord
                                li-ship-qty         when v-frst-ord
                                v-qty-onh           when 
                                                    first-of(oe-ordl.job-no) or
                                                    first-of(oe-ordl.job-no2)
                                oe-ordl.price
                                v-ext               when 
                                                    first-of(oe-ordl.job-no) or
                                                    first-of(oe-ordl.job-no2)
                          with frame itemx1.
                        down with frame itemx1.
                       END.
                       ELSE DO:
                        display cust.cust-no        when v-frst
                                oe-ordl.po-no       when v-frst-ord
/*                              cust.sman           when v-frst */
                                oe-ordl.i-no        when v-frst-ord
                                oe-ordl.part-no     when v-frst-ord
                                oe-ordl.i-name      when v-frst-ord
                                v-job-no            when v-frst-ord
/*                              fg-bin.loc*/
                                oe-ordl.qty         when v-frst-ord
                                li-ship-qty         when v-frst-ord
                                v-qty-onh           when 
                                                    first-of(oe-ordl.job-no) or
                                                    first-of(oe-ordl.job-no2)
                                oe-ordl.price
                                v-ext               when 
                                                    first-of(oe-ordl.job-no) or
                                                    first-of(oe-ordl.job-no2) SKIP
                           
                          with frame itemx5.
                        down with frame itemx5.
                       FOR EACH tt-oe-rel NO-LOCK:   
                        PUT SPACE(72)
                            tt-oe-rel.rel-no SPACE(4)  
                            tt-oe-rel.rel-date SPACE(4)
                            tt-oe-rel.tot-qty SKIP.
                       END.
                       END. 

                        IF tb_excel THEN 
                        DO:
                            ASSIGN
                              chrCust = IF v-frst 
                                        THEN cust.cust-no
                                        ELSE ""
                              chrPoNo = IF v-frst-ord
                                        THEN oe-ordl.po-no
                                        ELSE ""
                              chrINo =  IF v-frst-ord
                                        THEN oe-ordl.i-no 
                                        ELSE ""
                              chrPart = IF v-frst-ord
                                        THEN oe-ordl.part-no
                                        ELSE ""
                              chrName = IF v-frst-ord
                                        THEN oe-ordl.i-name
                                        ELSE ""
                              chrJob =  IF v-frst-ord
                                        THEN v-job-no 
                                        ELSE ""
                              chrOrdQty = IF v-frst-ord
                                          THEN STRING(oe-ordl.qty)
                                          ELSE ""
                              chrShpQty = IF v-frst-ord
                                          THEN STRING(li-ship-qty)
                                          ELSE ""
                              chrQtyOH = IF first-of(oe-ordl.job-no) or
                                            first-of(oe-ordl.job-no2)
                                         THEN STRING(v-qty-onh)
                                         ELSE ""
                              chrTotV =  IF first-of(oe-ordl.job-no) or
                                            first-of(oe-ordl.job-no2)
                                         THEN STRING(v-ext)
                                         ELSE "".

                            EXPORT STREAM excel DELIMITER "," 
                                chrCust chrPoNo chrINo chrPart
                                chrName chrJob chrOrdQty chrShpQty
                                chrQtyOH oe-ordl.price chrTotV.
                            IF rd_smry-dtl = "D" THEN
                                FOR EACH tt-oe-rel NO-LOCK:
                                EXPORT STREAM excel DELIMITER ","
                                    tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                    tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                    tt-oe-rel.rel-no tt-oe-rel.rel-date tt-oe-rel.tot-qty.
                                END.  
                                
                        END.

                        ASSIGN
                         v-frst  = NO
                         v-print = YES.
                    end.
                end.
                else
                do:
                    IF rd_smry-dtl = "S" THEN DO:
                    display cust.cust-no        when v-frst
                            oe-ordl.po-no       when v-frst-ord
                            v-sales-rep         when v-frst
                            oe-ordl.i-no        when v-frst-ord
                            oe-ordl.i-name      when v-frst-ord
                            fg-bin.loc
                            oe-ordl.qty         when v-frst-ord
                            li-ship-qty         when v-frst-ord
                            v-qty-onh           when 
                                                first-of(oe-ordl.job-no) or
                                                first-of(oe-ordl.job-no2)
                            oe-ordl.price
                            v-ext               when 
                                                first-of(oe-ordl.job-no) or
                                                first-of(oe-ordl.job-no2) 
                      with frame itemx3.
                    down with frame itemx3.
                    END.
                    ELSE DO:
                    display cust.cust-no        when v-frst
                            oe-ordl.po-no       when v-frst-ord
                            v-sales-rep         when v-frst
                            oe-ordl.i-no        when v-frst-ord
                            oe-ordl.i-name      when v-frst-ord
                            fg-bin.loc
                            oe-ordl.qty         when v-frst-ord
                            li-ship-qty         when v-frst-ord
                            v-qty-onh           when 
                                                first-of(oe-ordl.job-no) or
                                                first-of(oe-ordl.job-no2)
                            oe-ordl.price
                            v-ext               when 
                                                first-of(oe-ordl.job-no) or
                                                first-of(oe-ordl.job-no2) SKIP
                      with frame itemx7.
                    down with frame itemx7.
                    FOR EACH tt-oe-rel NO-LOCK:
                    PUT SPACE(57)
                        tt-oe-rel.rel-no SPACE(4)    
                        tt-oe-rel.rel-date SPACE(4)
                        tt-oe-rel.tot-qty SKIP.
                     END.
                    END.

                    IF tb_excel THEN 
                    DO:
                        ASSIGN
                          chrCust = IF v-frst 
                                    THEN cust.cust-no
                                    ELSE ""
                          chrPoNo = IF v-frst-ord
                                    THEN oe-ordl.po-no
                                    ELSE ""
                          chrSman = IF v-frst 
                                    THEN v-sales-rep
                                    ELSE "" 
                          chrINo =  IF v-frst-ord
                                    THEN oe-ordl.i-no 
                                    ELSE ""
                          chrName = IF v-frst-ord
                                    THEN oe-ordl.i-name
                                    ELSE ""
                          chrOrdQty = IF v-frst-ord
                                      THEN STRING(oe-ordl.qty)
                                      ELSE ""
                          chrShpQty = IF v-frst-ord
                                      THEN STRING(li-ship-qty)
                                      ELSE ""
                          chrQtyOH = IF first-of(oe-ordl.job-no) or
                                        first-of(oe-ordl.job-no2)
                                     THEN STRING(v-qty-onh)
                                     ELSE ""
                          chrTotV =  IF first-of(oe-ordl.job-no) or
                                        first-of(oe-ordl.job-no2)
                                     THEN STRING(v-ext)
                                     ELSE "".

                        EXPORT STREAM excel DELIMITER "," 
                            chrCust chrPoNo chrSman chrINo 
                            chrName fg-bin.loc chrOrdQty chrShpQty
                            chrQtyOH oe-ordl.price chrTotV.
                       IF rd_smry-dtl = "D" THEN
                           FOR EACH tt-oe-rel NO-LOCK:
                           EXPORT STREAM excel DELIMITER ","
                               tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                               tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                               tt-oe-rel.rel-no tt-oe-rel.rel-date tt-oe-rel.tot-qty.
                           END. 
                           
                    END.

                    ASSIGN
                     v-frst  = NO
                     v-print = YES.
                end.
/*XXX*/
                if first-of(oe-ordl.job-no) or 
                   first-of(oe-ordl.job-no2) then 
                do:
                    if v-disp-item then
                      assign
                        v-tot-onh        = v-tot-onh       + v-qty-onh
                        v-tot-ext        = v-tot-ext       + v-ext
                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                        v-grand-tot-ext  = v-grand-tot-ext + v-ext.
                end.
/*XXX*/
                if v-frst-ord then 
                do:
                    if v-disp-item then
                      assign
                       v-tot-ord        = v-tot-ord        + oe-ordl.qty
                       v-tot-ship       = v-tot-ship       + li-ship-qty
                       v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                       v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
                end.

                assign
                  v-qty-onh  = 0
                  v-frst-ord = no.
            end. /*if not avail fg bin*/
        end.  /* for each oe-ordl */
    end.  /* not rec dat */
    else
    do:
        for each oe-ordl
            no-lock
            where oe-ordl.company eq cocode
              and oe-ordl.cust-no eq cust.cust-no
              and oe-ordl.po-no   ge fpo#
              and oe-ordl.po-no   le tpo#,

           first oe-ord 
                where oe-ord.company eq oe-ordl.company 
                  and oe-ord.ord-no eq oe-ordl.ord-no
                  and ((oe-ord.opened EQ YES AND v-ocb NE "C") OR
                       (oe-ord.opened EQ NO  AND v-ocb NE "O"))
                no-lock,
                   
           first itemfg where itemfg.company eq cocode
                          and itemfg.i-no    eq oe-ordl.i-no
                          and itemfg.cust-no eq cust.cust-no
                          and (itemfg.i-code eq typex or typex eq "A")
                        no-lock

          /* for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
                           and itemfg.cust-po-no >= fpo#
                           and itemfg.cust-po-no <= tpo#
               no-lock*/ 
              break by itemfg.cust-no
                    by itemfg.i-no:
        
               FOR EACH tt-oe-rel NO-LOCK:
                   DELETE tt-oe-rel.
               END.

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
                         FIND FIRST sman WHERE sman.company = itemfg.company
                             AND sman.sman = cust-part.spare-char-1 NO-LOCK NO-ERROR.
                         IF AVAIL sman THEN v-sales-rep = sman.sman.
                         LEAVE .
                     END.
                  END. /* end of cust-part */
         
                  IF AVAIL cust AND v-sales-rep EQ "" THEN DO:
                      FIND FIRST sman WHERE sman.company = cust.company
                          AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                      IF AVAIL sman THEN v-sales-rep = sman.sman.
                  END.
            END.
            ELSE DO:
                FIND FIRST sman WHERE sman.company = cust.company
                    AND sman.sman = cust.sman NO-LOCK NO-ERROR.
                IF AVAIL sman THEN v-sales-rep = sman.sman.
            END.
               

           FOR EACH oe-rel
               where oe-rel.company EQ oe-ordl.company
               AND oe-rel.ord-no  EQ oe-ordl.ord-no
               AND oe-rel.i-no    EQ oe-ordl.i-no
               AND oe-rel.line    EQ oe-ordl.line
               NO-LOCK:
               
               FIND FIRST oe-rell NO-LOCK
                   WHERE oe-rell.company  EQ cocode
                   AND oe-rell.ord-no   EQ oe-rel.ord-no
                   AND oe-rell.rel-no   EQ oe-rel.rel-no
                   AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                   AND oe-rell.i-no     EQ oe-rel.i-no
                   AND oe-rell.line     EQ oe-rel.LINE NO-ERROR .
                  FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
                  
                  CREATE tt-oe-rel.
                  ASSIGN
                      tt-oe-rel.rel-date = string(oe-rel.rel-date)
                      tt-oe-rel.tot-qty  = oe-rel.tot-qty .
                  IF AVAIL oe-relh  THEN 
                      ASSIGN  tt-oe-rel.rel-no   = oe-relh.release#.
            END.

           if first-of(itemfg.i-no) then
             assign v-frst-i-no = yes.

           if ((typex ne "A") and (typex ne itemfg.i-code)) or (typex = "A") then
           do:
               for each fg-bin no-lock where fg-bin.company = cocode 
                                         AND fg-bin.i-no = itemfg.i-no
                                        use-index co-ino
                                       break by fg-bin.loc
                                             by fg-bin.job-no
                                             by fg-bin.job-no2:

                   if (fg-bin.loc eq "CUST" or trim(fg-bin.cust-no) gt "") and
                       not v-custown then
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
                             find first uom where uom.uom = itemfg.sell-uom 
                                              AND uom.mult ne 0 
                                   no-lock no-error.

                           if available uom then
                             v-ext = (v-qty-onh * itemfg.sell-price / uom.mult).
                           else
                             v-ext = (v-qty-onh * itemfg.sell-price) / 1000.

                           if itemfg.sell-uom = "L" then
                             v-ext = itemfg.sell-price.

                           for each xbin where xbin.company = cocode 
                                           AND xbin.i-no    = itemfg.i-no 
                                           AND xbin.loc     = fg-bin.loc
                                          no-lock break by xbin.job-no
                                                        by xbin.job-no2:
                               if first-of(xbin.job-no) or 
                                  first-of(xbin.job-no2) then
                                 assign
                                   v-qty-job = 0
                                   v-ext-job = 0.

                               v-qty-job = v-qty-job + xbin.qty.

                               if last-of(xbin.job-no) or 
                                  last-of(xbin.job-no2) then
                               do:
                                   find first xbin2 where xbin2.company = cocode 
                                                      and xbin2.i-no = itemfg.i-no 
                                                      AND xbin2.loc = fg-bin.loc 
                                                      and (xbin2.job-no  <> xbin.job-no 
                                                           OR 
                                                           xbin2.job-no2 <> xbin.job-no2) 
                                                      and xbin2.qty <> 0 
                                                    no-lock no-error.
                                   if available xbin2 and v-qty-job = 0 then
                                     next.

                                   if itemfg.sell-uom = "CS" and
                                      itemfg.case-count ne 0 then
                                     v-ext-job = 
                            (v-qty-job * itemfg.sell-price) / itemfg.case-count.
                                   else
                                     find first uom where uom.uom = itemfg.sell-uom 
                                                      AND uom.mult ne 0 
                                           no-lock no-error.

                                   if available uom then
                                     v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                                   else
                                     v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                                   if itemfg.sell-uom = "L" then
                                     v-ext-job = itemfg.sell-price.

                                   trans-date = ?.

                                   for each fg-rcpth 
                                      where fg-rcpth.company = cocode 
                                        AND fg-rcpth.i-no    = itemfg.i-no 
                                        AND fg-rcpth.rita-code  = "R" 
                                        AND fg-rcpth.job-no  = xbin.job-no 
                                        AND fg-rcpth.job-no2 = xbin.job-no2
                                          no-lock
                                          break by fg-rcpth.trans-date:
                                       if last-of(fg-rcpth.trans-date) then
                                         trans-date = fg-rcpth.trans-date.
                                   end.

                                
                                   if line-counter >= 56 then page.

                                   if xbin.job-no = "" and xbin.job-no2 = 0 then
                                     v-job = "".
                                   else
                                     v-job = xbin.job-no + "-" + string(xbin.job-no2).

                                /* find first oe-ordl
                         where oe-ordl.company eq cocode
                           and oe-ordl.job-no  eq xbin.job-no
                           and oe-ordl.job-no2 eq xbin.job-no2
                           and oe-ordl.i-no    eq xbin.i-no
                         use-index job no-lock no-error.

                                   if avail oe-ordl then */

                                   v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.

                                   if v-prt-cpn THEN do:
                                      IF rd_smry-dtl = "S" THEN DO:
                                      display cust.cust-no when v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no when avail oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep when v-frst
                                              itemfg.i-no
                                              itemfg.part-no
                                              itemfg.i-name
                                              v-job
                                              v-qty-job
                                              trans-date when not(zbal and v-qty-onh = 0) and
                                                              not(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) when avail oe-ordl @ itemfg.sell-price
                                              v-ext-job
                                        with frame itemx2.
                                      down with frame itemx2.
                                      END.
                                      ELSE DO:
                                      display cust.cust-no when v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no when avail oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep when v-frst
                                              itemfg.i-no
                                              itemfg.part-no
                                              itemfg.i-name
                                              v-job
                                              oe-ordl.qty         
                                              li-ship-qty         
                                              v-qty-job
                                              trans-date when not(zbal and v-qty-onh = 0) and
                                                              not(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) when avail oe-ordl @ itemfg.sell-price
                                              v-ext-job SKIP
                                          
                                            with frame itemx6.
                                      down with frame itemx6.
                                      FOR EACH tt-oe-rel NO-LOCK:
                                      PUT SPACE(76)
                                          tt-oe-rel.rel-no SPACE(6)    
                                          tt-oe-rel.rel-date SPACE(1)
                                          tt-oe-rel.tot-qty SKIP.
                                        END.
                                      END.
                                      
                                      IF tb_excel THEN 
                                      DO:
                                          ASSIGN
                                            chrCust = IF v-frst 
                                                      THEN cust.cust-no
                                                      ELSE ""
                                            chrPoNo = IF AVAILABLE(oe-ordl) 
                                                      THEN oe-ordl.po-no
                                                      ELSE itemfg.cust-po-no
                                            chrSman = IF v-frst
                                                      THEN v-sales-rep
                                                      ELSE "" 
                                            chrOrdQty = IF v-frst-ord
                                                        THEN STRING(oe-ordl.qty)
                                                        ELSE ""
                                            chrShpQty = IF v-frst-ord
                                                        THEN STRING(li-ship-qty)
                                                        ELSE ""
                                            chrTransDte = IF not(zbal and v-qty-onh = 0) and 
                                                             not(v-qty-job = 0)
                                                          THEN STRING(trans-date, "99/99/9999")
                                                          ELSE ""
                                            decPrice = IF AVAILABLE(oe-ordl) 
                                                       THEN (oe-ordl.t-price / oe-ordl.qty * 1000)
                                                       ELSE itemfg.sell-price.

                                          EXPORT STREAM excel DELIMITER "," 
                                              chrCust chrPoNo chrSman itemFg.i-no
                                              itemFg.Part-no itemFg.i-name v-job oe-ordl.qty
                                              li-ship-qty v-qty-job chrTransDte decPrice v-ext-job.
                                          IF rd_smry-dtl = "D" THEN
                                              FOR EACH tt-oe-rel NO-LOCK:
                                              EXPORT STREAM excel DELIMITER ","
                                                  tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                                  tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank
                                                  tt-oe-rel.rel-no tt-oe-rel.rel-date tt-oe-rel.tot-qty.
                                              END.   
                                      END.
                                   end.
                                   else
                                   do:
                                      IF rd_smry-dtl = "S" THEN DO:
                                      display cust.cust-no when v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no when avail oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep when v-frst
                                              itemfg.i-no
                                              itemfg.i-name
                                              v-job
                                              v-qty-job
                                              trans-date when not(zbal and v-qty-onh = 0) and
                                                              not(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) when avail oe-ordl @ itemfg.sell-price
                                              v-ext-job
                                        with frame itemx4.
                                      down with frame itemx4.
                                      END.
                                      ELSE DO:
                                      display cust.cust-no when v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no when avail oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep when v-frst
                                              itemfg.i-no
                                              itemfg.i-name
                                              v-job
                                              oe-ordl.qty         
                                              li-ship-qty
                                              v-qty-job
                                              trans-date when not(zbal and v-qty-onh = 0) and
                                                              not(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) when avail oe-ordl @ itemfg.sell-price
                                              v-ext-job SKIP
                                          
                                        with frame itemx8.
                                      down with frame itemx8.
                                      FOR EACH tt-oe-rel NO-LOCK:
                                      PUT SPACE(54)
                                          tt-oe-rel.rel-no SPACE(5)    
                                          tt-oe-rel.rel-date SPACE(4)
                                          tt-oe-rel.tot-qty SKIP.
                                      END.
                                      END.

                                      IF tb_excel THEN 
                                      DO: 
                                          ASSIGN
                                            chrCust = IF v-frst 
                                                      THEN cust.cust-no
                                                      ELSE ""
                                            chrPoNo = IF AVAILABLE(oe-ordl) 
                                                      THEN oe-ordl.po-no
                                                      ELSE itemfg.cust-po-no
                                            chrSman = IF v-frst
                                                      THEN v-sales-rep
                                                      ELSE "" 
                                            chrOrdQty = IF v-frst-ord
                                                        THEN STRING(oe-ordl.qty)
                                                        ELSE ""
                                            chrShpQty = IF v-frst-ord
                                                        THEN STRING(li-ship-qty)
                                                        ELSE ""
                                            chrTransDte = IF not(zbal and v-qty-onh = 0) and 
                                                             not(v-qty-job = 0)
                                                          THEN STRING(trans-date, "99/99/9999")
                                                          ELSE ""
                                            decPrice = IF AVAILABLE(oe-ordl) 
                                                       THEN (oe-ordl.t-price / oe-ordl.qty * 1000)
                                                       ELSE itemfg.sell-price.

                                          EXPORT STREAM excel DELIMITER "," 
                                              chrCust chrPoNo chrSman itemFg.i-no
                                              itemFg.i-name v-job oe-ordl.qty li-ship-qty 
                                              v-qty-job chrTransDte decPrice v-ext-job.
                                          IF rd_smry-dtl = "D" THEN
                                              FOR EACH tt-oe-rel NO-LOCK:
                                              EXPORT STREAM excel DELIMITER "," 
                                                  tt-oe-rel.bl-ank tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                                  tt-oe-rel.bl-ank tt-oe-rel.bl-ank 
                                                  tt-oe-rel.rel-no tt-oe-rel.rel-date tt-oe-rel.tot-qty.
                                              END.   
                                              
                                      END.

                                   end.

                                   v-frst = NO.
                                   assign
                                     v-tot-ext = v-tot-ext + v-ext-job
                                     v-grand-tot-ext = v-grand-tot-ext + v-ext-job.
                               end. /* if last-of(... */
                           end. /* for each xbin */
/*XXX*/
                           if v-frst-i-no then 
                           do:
                               /*if v-disp-item then*/
                                 assign 
                                   v-tot-ord = v-tot-ord + v-qty-ord
                                   v-grand-tot-ord = v-grand-tot-ord + v-qty-ord
                                   v-tot-ship = v-tot-ship + v-qty-ship
                                   v-grand-tot-ship = v-grand-tot-ship + v-qty-ship
                                   v-frst-i-no = no.
                           end.
/*XXX*/
                           /*if v-disp-item then*/
                             assign v-tot-onh = v-tot-onh + v-qty-onh
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
      if v-rec-dat then
        if v-prt-cpn then
          put "------------"        to 126
              "--------------"      to 166 skip
              "CUSTOMER TOTALS:"    to 106
              v-tot-onh             to 126
              v-tot-ext             to 166 skip(1).
        else
          put "------------"        to 104
              "--------------"      to 144 skip
              "CUSTOMER TOTALS:"    to 91
              v-tot-onh             to 104
              v-tot-ext             to 144 skip(1).
      else
        if v-prt-cpn then
          put "-----------"         to 94
              "----------"          to 108
              "------------"        to 121
              "--------------"      to 151 skip
              "CUSTOMER TOTALS:"    to 77
              v-tot-ord             to 94
              v-tot-ship            to 108
              v-tot-onh             to 121
              v-tot-ext             to 151
              skip(1).
        else
          put "-----------"         to 79
              "----------"          to 93
              "------------"        to 106
              "--------------"      to 136 skip
              "CUSTOMER TOTALS:"    to 62
              v-tot-ord             to 79
              v-tot-ship            to 93
              v-tot-onh             to 106
              v-tot-ext             to 136
              skip(1).
end.  /* for each cust */

      if v-rec-dat then
        if v-prt-cpn then
          put "------------"        to 126
              "--------------"      to 166 skip
              "GRAND TOTALS:"       to 106
              v-grand-tot-onh       to 126
              v-grand-tot-ext       to 166 skip(1).
        else
          put "------------"        to 104
              "--------------"      to 144 skip
              "GRAND TOTALS:"       to 91
              v-grand-tot-onh       to 104
              v-grand-tot-ext       to 144 skip(1).
      else
        if v-prt-cpn then
          put "------------"        to 94
              "----------"          to 108
              "------------"        to 121
              "--------------"      to 151 skip
              "GRAND TOTALS:"       to 77
              v-grand-tot-ord       to 94
              v-grand-tot-ship      to 108
              v-grand-tot-onh       to 121
              v-grand-tot-ext       to 144
              skip(1).
        else
          put "------------"        to 79
              "----------"          to 93
              "------------"        to 106
              "--------------"      to 136 skip
              "GRAND TOTALS:"       to 62
              v-grand-tot-ord       to 79
              v-grand-tot-ship      to 93
              v-grand-tot-onh       to 106
              v-grand-tot-ext       to 136
              skip(1).

