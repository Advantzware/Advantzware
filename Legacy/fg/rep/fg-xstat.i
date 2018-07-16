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
    EACH cust
   WHERE cust.company EQ cocode
     AND cust.cust-no EQ ttCustList.cust-no /*fcst*/
    /* and cust.cust-no le tcst*/
     AND cust.sman    GE fslm
     AND cust.sman    LE tslm
   NO-LOCK
   BY cust.cust-no:

      ASSIGN
         v-frst     = YES
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0
         v-print    = NO.

    IF NOT v-rec-dat THEN
    DO:
        IF LINE-COUNTER GE 56 THEN PAGE.

        FOR EACH oe-ordl
            NO-LOCK
           WHERE oe-ordl.company EQ cocode
             AND oe-ordl.cust-no EQ cust.cust-no
             AND oe-ordl.po-no   GE fpo#
             AND oe-ordl.po-no   LE tpo#,

            FIRST oe-ord WHERE oe-ord.company EQ oe-ordl.company 
                           AND oe-ord.ord-no EQ oe-ordl.ord-no
                           AND ((oe-ord.opened EQ YES AND v-ocb NE "C") OR
                                (oe-ord.opened EQ NO  AND v-ocb NE "O"))
                         NO-LOCK,

            FIRST itemfg WHERE itemfg.company EQ cocode
                           AND itemfg.i-no    EQ oe-ordl.i-no
                           AND itemfg.cust-no EQ cust.cust-no
                           AND (itemfg.i-code EQ typex OR typex EQ "A")
                        NO-LOCK
            BREAK 
                  BY IF v-sortby THEN oe-ordl.part-no ELSE oe-ordl.job-no
                  BY IF NOT v-sortby THEN oe-ordl.job-no2 ELSE 0
                  BY oe-ordl.i-no
                  BY oe-ordl.job-no
                  BY oe-ordl.job-no2 :

                  {custom/statusMsg.i " 'Processing Order#  '  + string(oe-ordl.ord-no) "}

            FOR EACH tt-oe-rel NO-LOCK:
                DELETE tt-oe-rel.
            END.
             v-sales-rep = "" .
            IF AVAIL cust AND cust.ACTIVE NE "X" THEN DO:
                 FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                     AND cust-part.i-no = itemfg.i-no
                     AND cust-part.cust-no EQ cust.cust-no
                     AND (cust-part.labelCase or cust-part.labelPallet)
                     NO-LOCK:
         
                     IF cust-part.spare-char-1 NE "" THEN DO:
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
           WHERE oe-rel.company EQ oe-ordl.company
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
                   tt-oe-rel.rel-date = STRING(oe-rel.rel-date)
                   tt-oe-rel.tot-qty  = oe-rel.tot-qty .
               IF AVAIL oe-relh  THEN 
                   ASSIGN  tt-oe-rel.rel-no   = oe-relh.release#.
             END.

             RUN oe/ordlsqty.p (ROWID(oe-ordl), 
                              OUTPUT li-inv-qty, 
                              OUTPUT li-ship-qty).

            v-frst-ord = YES.

            IF CAN-FIND(FIRST fg-bin WHERE fg-bin.company        EQ cocode
                                       AND fg-bin.i-no           EQ itemfg.i-no
                                       AND fg-bin.job-no         EQ oe-ordl.job-no
                                       AND fg-bin.job-no2        EQ oe-ordl.job-no2
                                       AND (v-custown OR 
                                            (fg-bin.loc NE "CUST" AND trim(fg-bin.cust-no) EQ "")))
            THEN DO:
                FOR EACH fg-bin WHERE fg-bin.company        EQ cocode
                                  AND fg-bin.i-no           EQ itemfg.i-no
                                  AND fg-bin.job-no         EQ oe-ordl.job-no
                                  AND fg-bin.job-no2        EQ oe-ordl.job-no2
                                  AND (v-custown OR 
                                       (fg-bin.loc NE "CUST" AND trim(fg-bin.cust-no) EQ ""))
                                USE-INDEX co-ino NO-LOCK
                    BREAK BY fg-bin.loc:

                    v-qty-onh = v-qty-onh + fg-bin.qty.

                    IF LAST-OF(fg-bin.loc)      AND
                       (v-qty-onh NE 0 OR zbal) THEN 
                    DO:
                        IF itemfg.sell-uom   EQ "CS" AND
                           itemfg.case-count NE 0    THEN
                          v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                        ELSE IF itemfg.sell-uom EQ "L" THEN 
                          v-ext = oe-ordl.price.
                        ELSE
                        DO:
                            FIND FIRST uom WHERE uom.uom  EQ itemfg.sell-uom
                                             AND uom.mult NE 0
                                 NO-LOCK NO-ERROR.

                            v-ext = v-qty-onh * oe-ordl.price /
                            (IF AVAIL uom THEN uom.mult ELSE 1000).
                        END.

                        IF v-prt-cpn THEN 
                        DO:
                            v-job-no = "".
                            IF oe-ordl.job-no NE "" THEN
                              v-job-no = STRING(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99").

/*XXX*/
                            v-disp-item = YES.
/*                          if (v-qty-onh eq 0) and (itemfg.q-ship gt oe-ordl.qty) then do: */
                            IF (v-qty-onh EQ 0) AND (li-ship-qty GE oe-ordl.qty) THEN 
                            DO:
                               v-disp-item = NO.                                    
                            END.

                            IF v-disp-item THEN DO:
                               IF rd_smry-dtl = "S" THEN DO:
                                DISPLAY cust.cust-no        WHEN v-frst
                                        oe-ordl.po-no       WHEN v-frst-ord
             /*                         cust.sman           when v-frst*/
                                        oe-ordl.i-no        WHEN v-frst-ord
                                        oe-ordl.part-no     WHEN v-frst-ord
                                        oe-ordl.i-name      WHEN v-frst-ord
                                        v-job-no            WHEN v-frst-ord
             /*                         fg-bin.loc*/
                                        oe-ordl.qty         WHEN v-frst-ord
                                        li-ship-qty         WHEN v-frst-ord
                                        v-qty-onh           WHEN 
                                                            FIRST-OF(oe-ordl.job-no) OR
                                                            first-of(oe-ordl.job-no2)
                                        oe-ordl.price
                                        v-ext               WHEN 
                                                            FIRST-OF(oe-ordl.job-no) OR
                                                            first-of(oe-ordl.job-no2)
                                   WITH FRAME itemx1.
                                DOWN WITH FRAME itemx1.
                               END.
                                ELSE DO:
                                DISPLAY cust.cust-no        WHEN v-frst
                                        oe-ordl.po-no       WHEN v-frst-ord
                                        oe-ordl.i-no        WHEN v-frst-ord
                                        oe-ordl.part-no     WHEN v-frst-ord
                                        oe-ordl.i-name      WHEN v-frst-ord
                                        v-job-no            WHEN v-frst-ord
                                        oe-ordl.qty         WHEN v-frst-ord
                                        li-ship-qty         WHEN v-frst-ord
                                        v-qty-onh           WHEN 
                                                            FIRST-OF(oe-ordl.job-no) OR
                                                            first-of(oe-ordl.job-no2)
                                        oe-ordl.price
                                        v-ext               WHEN 
                                                            FIRST-OF(oe-ordl.job-no) OR
                                                            first-of(oe-ordl.job-no2) SKIP
                                   WITH FRAME itemx5.
                                DOWN WITH FRAME itemx5.
                               
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

                                        chrQtyOH = IF FIRST-OF(oe-ordl.job-no) OR
                                                      first-of(oe-ordl.job-no2)
                                                   THEN STRING(v-qty-onh)
                                                   ELSE ""

                                        chrTotV =  IF FIRST-OF(oe-ordl.job-no) OR
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
                        END. /* v-prt-cpn */
                        ELSE
                        DO:
                           IF rd_smry-dtl = "S" THEN DO:
                            DISPLAY cust.cust-no        WHEN v-frst
                                    oe-ordl.po-no       WHEN v-frst-ord
                                    v-sales-rep         WHEN v-frst
                                    oe-ordl.i-no        WHEN v-frst-ord
                                    oe-ordl.i-name      WHEN v-frst-ord
                                    fg-bin.loc
                                    oe-ordl.qty         WHEN v-frst-ord
                                    li-ship-qty         WHEN v-frst-ord
                                    v-qty-onh           WHEN 
                                                        FIRST-OF(oe-ordl.job-no) OR
                                                        first-of(oe-ordl.job-no2)
                                    oe-ordl.price
                                    v-ext               WHEN 
                                                        FIRST-OF(oe-ordl.job-no) OR
                                                        first-of(oe-ordl.job-no2)
                               WITH FRAME itemx3.
                            DOWN WITH FRAME itemx3.
                           END.
                           ELSE DO:
                            DISPLAY cust.cust-no        WHEN v-frst
                                    oe-ordl.po-no       WHEN v-frst-ord
                                    v-sales-rep         WHEN v-frst
                                    oe-ordl.i-no        WHEN v-frst-ord
                                    oe-ordl.i-name      WHEN v-frst-ord
                                    fg-bin.loc
                                    oe-ordl.qty         WHEN v-frst-ord
                                    li-ship-qty         WHEN v-frst-ord
                                    v-qty-onh           WHEN 
                                                        FIRST-OF(oe-ordl.job-no) OR
                                                        first-of(oe-ordl.job-no2)
                                    oe-ordl.price
                                    v-ext               WHEN 
                                                        FIRST-OF(oe-ordl.job-no) OR
                                                        first-of(oe-ordl.job-no2) SKIP
                               WITH FRAME itemx7.
                            DOWN WITH FRAME itemx7.
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
                                  chrQtyOH = IF FIRST-OF(oe-ordl.job-no) OR
                                                first-of(oe-ordl.job-no2)
                                             THEN STRING(v-qty-onh)
                                             ELSE ""
                                  chrTotV =  IF FIRST-OF(oe-ordl.job-no) OR
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
                        END.

                        IF FIRST-OF(oe-ordl.job-no) OR first-of(oe-ordl.job-no2) THEN
                          ASSIGN
                            v-tot-onh        = v-tot-onh       + v-qty-onh
                            v-tot-ext        = v-tot-ext       + v-ext

                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                        v-grand-tot-ext  = v-grand-tot-ext + v-ext.

                        IF v-frst-ord THEN
                          ASSIGN
                            v-tot-ord        = v-tot-ord        + oe-ordl.qty
                            v-tot-ship       = v-tot-ship       + li-ship-qty
                            v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                            v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.

                        ASSIGN
                          v-qty-onh   = 0
                          v-frst-ord  = NO.
                    END.  /* last of fg bin */
                END.  /* for each fg bin */
            END. /*if avail fg bin*/
            ELSE /* not avail fg bin */
            DO:
                IF itemfg.sell-uom   EQ "CS" AND
                   itemfg.case-count NE 0    THEN
                  v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
                ELSE IF itemfg.sell-uom EQ "L" THEN 
                  v-ext = oe-ordl.price.
                ELSE
                DO:
                    FIND FIRST uom WHERE uom.uom  EQ itemfg.sell-uom
                                     AND uom.mult NE 0
                                   NO-LOCK NO-ERROR.

                    v-ext = 
             v-qty-onh * oe-ordl.price / (IF AVAIL uom THEN uom.mult ELSE 1000).
                END.

                v-job-no = "".
                IF oe-ordl.job-no NE "" THEN
                  v-job-no = STRING(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99").
/*XXX*/
                v-disp-item = YES.
/*              if (v-qty-onh eq 0) and (itemfg.q-ship gt oe-ordl.qty) then do: */
                IF (v-qty-onh EQ 0) AND (li-ship-qty GE oe-ordl.qty) THEN 
                DO:
                    v-disp-item = NO.                                    
                END.

/****************************************************                
                if (zbal eq no) and (v-qty-onh eq 0) then do:
                    v-disp-item = no.
                end.
********************************************************/

                IF v-prt-cpn THEN 
                DO:
                    IF v-disp-item THEN DO:
                       IF rd_smry-dtl = "S" THEN DO:
                         DISPLAY cust.cust-no        WHEN v-frst
                                oe-ordl.po-no       WHEN v-frst-ord
/*                              cust.sman           when v-frst */
                                oe-ordl.i-no        WHEN v-frst-ord
                                oe-ordl.part-no     WHEN v-frst-ord
                                oe-ordl.i-name      WHEN v-frst-ord
                                v-job-no            WHEN v-frst-ord
/*                              fg-bin.loc*/
                                oe-ordl.qty         WHEN v-frst-ord
                                li-ship-qty         WHEN v-frst-ord
                                v-qty-onh           WHEN 
                                                    FIRST-OF(oe-ordl.job-no) OR
                                                    first-of(oe-ordl.job-no2)
                                oe-ordl.price
                                v-ext               WHEN 
                                                    FIRST-OF(oe-ordl.job-no) OR
                                                    first-of(oe-ordl.job-no2)
                          WITH FRAME itemx1.
                        DOWN WITH FRAME itemx1.
                       END.
                       ELSE DO:
                        DISPLAY cust.cust-no        WHEN v-frst
                                oe-ordl.po-no       WHEN v-frst-ord
/*                              cust.sman           when v-frst */
                                oe-ordl.i-no        WHEN v-frst-ord
                                oe-ordl.part-no     WHEN v-frst-ord
                                oe-ordl.i-name      WHEN v-frst-ord
                                v-job-no            WHEN v-frst-ord
/*                              fg-bin.loc*/
                                oe-ordl.qty         WHEN v-frst-ord
                                li-ship-qty         WHEN v-frst-ord
                                v-qty-onh           WHEN 
                                                    FIRST-OF(oe-ordl.job-no) OR
                                                    first-of(oe-ordl.job-no2)
                                oe-ordl.price
                                v-ext               WHEN 
                                                    FIRST-OF(oe-ordl.job-no) OR
                                                    first-of(oe-ordl.job-no2) SKIP
                           
                          WITH FRAME itemx5.
                        DOWN WITH FRAME itemx5.
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
                              chrQtyOH = IF FIRST-OF(oe-ordl.job-no) OR
                                            first-of(oe-ordl.job-no2)
                                         THEN STRING(v-qty-onh)
                                         ELSE ""
                              chrTotV =  IF FIRST-OF(oe-ordl.job-no) OR
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
                    END.
                END.
                ELSE
                DO:
                    IF rd_smry-dtl = "S" THEN DO:
                    DISPLAY cust.cust-no        WHEN v-frst
                            oe-ordl.po-no       WHEN v-frst-ord
                            v-sales-rep         WHEN v-frst
                            oe-ordl.i-no        WHEN v-frst-ord
                            oe-ordl.i-name      WHEN v-frst-ord
                            fg-bin.loc
                            oe-ordl.qty         WHEN v-frst-ord
                            li-ship-qty         WHEN v-frst-ord
                            v-qty-onh           WHEN 
                                                FIRST-OF(oe-ordl.job-no) OR
                                                first-of(oe-ordl.job-no2)
                            oe-ordl.price
                            v-ext               WHEN 
                                                FIRST-OF(oe-ordl.job-no) OR
                                                first-of(oe-ordl.job-no2) 
                      WITH FRAME itemx3.
                    DOWN WITH FRAME itemx3.
                    END.
                    ELSE DO:
                    DISPLAY cust.cust-no        WHEN v-frst
                            oe-ordl.po-no       WHEN v-frst-ord
                            v-sales-rep         WHEN v-frst
                            oe-ordl.i-no        WHEN v-frst-ord
                            oe-ordl.i-name      WHEN v-frst-ord
                            fg-bin.loc
                            oe-ordl.qty         WHEN v-frst-ord
                            li-ship-qty         WHEN v-frst-ord
                            v-qty-onh           WHEN 
                                                FIRST-OF(oe-ordl.job-no) OR
                                                first-of(oe-ordl.job-no2)
                            oe-ordl.price
                            v-ext               WHEN 
                                                FIRST-OF(oe-ordl.job-no) OR
                                                first-of(oe-ordl.job-no2) SKIP
                      WITH FRAME itemx7.
                    DOWN WITH FRAME itemx7.
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
                          chrQtyOH = IF FIRST-OF(oe-ordl.job-no) OR
                                        first-of(oe-ordl.job-no2)
                                     THEN STRING(v-qty-onh)
                                     ELSE ""
                          chrTotV =  IF FIRST-OF(oe-ordl.job-no) OR
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
                END.
/*XXX*/
                IF FIRST-OF(oe-ordl.job-no) OR 
                   first-of(oe-ordl.job-no2) THEN 
                DO:
                    IF v-disp-item THEN
                      ASSIGN
                        v-tot-onh        = v-tot-onh       + v-qty-onh
                        v-tot-ext        = v-tot-ext       + v-ext
                        v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
                        v-grand-tot-ext  = v-grand-tot-ext + v-ext.
                END.
/*XXX*/
                IF v-frst-ord THEN 
                DO:
                    IF v-disp-item THEN
                      ASSIGN
                       v-tot-ord        = v-tot-ord        + oe-ordl.qty
                       v-tot-ship       = v-tot-ship       + li-ship-qty
                       v-grand-tot-ord  = v-grand-tot-ord  + oe-ordl.qty
                       v-grand-tot-ship = v-grand-tot-ship + li-ship-qty.
                END.

                ASSIGN
                  v-qty-onh  = 0
                  v-frst-ord = NO.
            END. /*if not avail fg bin*/
        END.  /* for each oe-ordl */
    END.  /* not rec dat */
    ELSE
    DO:
        FOR EACH oe-ordl
            NO-LOCK
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.cust-no EQ cust.cust-no
              AND oe-ordl.po-no   GE fpo#
              AND oe-ordl.po-no   LE tpo#,

           FIRST oe-ord 
                WHERE oe-ord.company EQ oe-ordl.company 
                  AND oe-ord.ord-no EQ oe-ordl.ord-no
                  AND ((oe-ord.opened EQ YES AND v-ocb NE "C") OR
                       (oe-ord.opened EQ NO  AND v-ocb NE "O"))
                NO-LOCK,
                   
           FIRST itemfg WHERE itemfg.company EQ cocode
                          AND itemfg.i-no    EQ oe-ordl.i-no
                          AND itemfg.cust-no EQ cust.cust-no
                          AND (itemfg.i-code EQ typex OR typex EQ "A")
                        NO-LOCK

          /* for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
                           and itemfg.cust-po-no >= fpo#
                           and itemfg.cust-po-no <= tpo#
               no-lock*/ 
              BREAK BY itemfg.cust-no
                    BY itemfg.i-no:
        
               FOR EACH tt-oe-rel NO-LOCK:
                   DELETE tt-oe-rel.
               END.

            v-sales-rep = "" .
            IF AVAIL cust AND cust.ACTIVE NE "X" THEN DO:
                 FOR EACH cust-part WHERE cust-part.company = itemfg.company   
                     AND cust-part.i-no = itemfg.i-no
                     AND cust-part.cust-no EQ cust.cust-no
                     AND (cust-part.labelCase or cust-part.labelPallet)
                       NO-LOCK:
         
                     IF cust-part.spare-char-1 NE "" THEN DO:
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
               WHERE oe-rel.company EQ oe-ordl.company
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
                      tt-oe-rel.rel-date = STRING(oe-rel.rel-date)
                      tt-oe-rel.tot-qty  = oe-rel.tot-qty .
                  IF AVAIL oe-relh  THEN 
                      ASSIGN  tt-oe-rel.rel-no   = oe-relh.release#.
            END.

           IF FIRST-OF(itemfg.i-no) THEN
             ASSIGN v-frst-i-no = YES.

           IF ((typex NE "A") AND (typex NE itemfg.i-code)) OR (typex = "A") THEN
           DO:
               FOR EACH fg-bin NO-LOCK WHERE fg-bin.company = cocode 
                                         AND fg-bin.i-no = itemfg.i-no
                                        USE-INDEX co-ino
                                       BREAK BY fg-bin.loc
                                             BY fg-bin.job-no
                                             BY fg-bin.job-no2:

                   IF (fg-bin.loc EQ "CUST" OR trim(fg-bin.cust-no) GT "") AND
                       NOT v-custown THEN
                     NEXT.
                   ELSE
                     v-qty-onh = v-qty-onh + fg-bin.qty.

                   IF LAST-OF(fg-bin.loc) THEN
                   DO:
                       IF (v-qty-onh NE 0 ) OR (v-qty-onh EQ 0 AND zbal) THEN
                       DO:
                           IF itemfg.sell-uom = "CS" AND itemfg.case-count NE 0 THEN
                             v-ext = (v-qty-onh * itemfg.sell-price) / itemfg.case-count.
                           ELSE
                             FIND FIRST uom WHERE uom.uom = itemfg.sell-uom 
                                              AND uom.mult NE 0 
                                   NO-LOCK NO-ERROR.

                           IF AVAILABLE uom THEN
                             v-ext = (v-qty-onh * itemfg.sell-price / uom.mult).
                           ELSE
                             v-ext = (v-qty-onh * itemfg.sell-price) / 1000.

                           IF itemfg.sell-uom = "L" THEN
                             v-ext = itemfg.sell-price.

                           FOR EACH xbin WHERE xbin.company = cocode 
                                           AND xbin.i-no    = itemfg.i-no 
                                           AND xbin.loc     = fg-bin.loc
                                          NO-LOCK BREAK BY xbin.job-no
                                                        BY xbin.job-no2:
                               IF FIRST-OF(xbin.job-no) OR 
                                  first-of(xbin.job-no2) THEN
                                 ASSIGN
                                   v-qty-job = 0
                                   v-ext-job = 0.

                               v-qty-job = v-qty-job + xbin.qty.

                               IF LAST-OF(xbin.job-no) OR 
                                  last-of(xbin.job-no2) THEN
                               DO:
                                   FIND FIRST xbin2 WHERE xbin2.company = cocode 
                                                      AND xbin2.i-no = itemfg.i-no 
                                                      AND xbin2.loc = fg-bin.loc 
                                                      AND (xbin2.job-no  <> xbin.job-no 
                                                           OR 
                                                           xbin2.job-no2 <> xbin.job-no2) 
                                                      AND xbin2.qty <> 0 
                                                    NO-LOCK NO-ERROR.
                                   IF AVAILABLE xbin2 AND v-qty-job = 0 THEN
                                     NEXT.

                                   IF itemfg.sell-uom = "CS" AND
                                      itemfg.case-count NE 0 THEN
                                     v-ext-job = 
                            (v-qty-job * itemfg.sell-price) / itemfg.case-count.
                                   ELSE
                                     FIND FIRST uom WHERE uom.uom = itemfg.sell-uom 
                                                      AND uom.mult NE 0 
                                           NO-LOCK NO-ERROR.

                                   IF AVAILABLE uom THEN
                                     v-ext-job = (v-qty-job * itemfg.sell-price / uom.mult).
                                   ELSE
                                     v-ext-job = (v-qty-job * itemfg.sell-price) / 1000.

                                   IF itemfg.sell-uom = "L" THEN
                                     v-ext-job = itemfg.sell-price.

                                   trans-date = ?.

                                   FOR EACH fg-rcpth 
                                      WHERE fg-rcpth.company = cocode 
                                        AND fg-rcpth.i-no    = itemfg.i-no 
                                        AND fg-rcpth.rita-code  = "R" 
                                        AND fg-rcpth.job-no  = xbin.job-no 
                                        AND fg-rcpth.job-no2 = xbin.job-no2
                                          NO-LOCK
                                          BREAK BY fg-rcpth.trans-date:
                                       IF LAST-OF(fg-rcpth.trans-date) THEN
                                         trans-date = fg-rcpth.trans-date.
                                   END.

                                
                                   IF LINE-COUNTER >= 56 THEN PAGE.

                                   IF xbin.job-no = "" AND xbin.job-no2 = 0 THEN
                                     v-job = "".
                                   ELSE
                                     v-job = xbin.job-no + "-" + string(xbin.job-no2).

                                /* find first oe-ordl
                         where oe-ordl.company eq cocode
                           and oe-ordl.job-no  eq xbin.job-no
                           and oe-ordl.job-no2 eq xbin.job-no2
                           and oe-ordl.i-no    eq xbin.i-no
                         use-index job no-lock no-error.

                                   if avail oe-ordl then */

                                   v-ext-job = (oe-ordl.t-price / oe-ordl.qty) * v-qty-job.

                                   IF v-prt-cpn THEN DO:
                                      IF rd_smry-dtl = "S" THEN DO:
                                      DISPLAY cust.cust-no WHEN v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no WHEN AVAIL oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep WHEN v-frst
                                              itemfg.i-no
                                              itemfg.part-no
                                              itemfg.i-name
                                              v-job
                                              v-qty-job
                                              trans-date WHEN NOT(zbal AND v-qty-onh = 0) AND
                                                              NOT(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) WHEN AVAIL oe-ordl @ itemfg.sell-price
                                              v-ext-job
                                        WITH FRAME itemx2.
                                      DOWN WITH FRAME itemx2.
                                      END.
                                      ELSE DO:
                                      DISPLAY cust.cust-no WHEN v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no WHEN AVAIL oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep WHEN v-frst
                                              itemfg.i-no
                                              itemfg.part-no
                                              itemfg.i-name
                                              v-job
                                              oe-ordl.qty         
                                              li-ship-qty         
                                              v-qty-job
                                              trans-date WHEN NOT(zbal AND v-qty-onh = 0) AND
                                                              NOT(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) WHEN AVAIL oe-ordl @ itemfg.sell-price
                                              v-ext-job SKIP
                                          
                                            WITH FRAME itemx6.
                                      DOWN WITH FRAME itemx6.
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
                                            chrTransDte = IF NOT(zbal AND v-qty-onh = 0) AND 
                                                             NOT(v-qty-job = 0)
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
                                   END.
                                   ELSE
                                   DO:
                                      IF rd_smry-dtl = "S" THEN DO:
                                      DISPLAY cust.cust-no WHEN v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no WHEN AVAIL oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep WHEN v-frst
                                              itemfg.i-no
                                              itemfg.i-name
                                              v-job
                                              v-qty-job
                                              trans-date WHEN NOT(zbal AND v-qty-onh = 0) AND
                                                              NOT(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) WHEN AVAIL oe-ordl @ itemfg.sell-price
                                              v-ext-job
                                        WITH FRAME itemx4.
                                      DOWN WITH FRAME itemx4.
                                      END.
                                      ELSE DO:
                                      DISPLAY cust.cust-no WHEN v-frst
                                              itemfg.cust-po-no
                                              oe-ordl.po-no WHEN AVAIL oe-ordl @ itemfg.cust-po-no
                                              v-sales-rep WHEN v-frst
                                              itemfg.i-no
                                              itemfg.i-name
                                              v-job
                                              oe-ordl.qty         
                                              li-ship-qty
                                              v-qty-job
                                              trans-date WHEN NOT(zbal AND v-qty-onh = 0) AND
                                                              NOT(v-qty-job = 0)
                                              itemfg.sell-price
                                              (oe-ordl.t-price / oe-ordl.qty * 1000) WHEN AVAIL oe-ordl @ itemfg.sell-price
                                              v-ext-job SKIP
                                          
                                        WITH FRAME itemx8.
                                      DOWN WITH FRAME itemx8.
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
                                            chrTransDte = IF NOT(zbal AND v-qty-onh = 0) AND 
                                                             NOT(v-qty-job = 0)
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

                                   END.

                                   v-frst = NO.
                                   ASSIGN
                                     v-tot-ext = v-tot-ext + v-ext-job
                                     v-grand-tot-ext = v-grand-tot-ext + v-ext-job.
                               END. /* if last-of(... */
                           END. /* for each xbin */
/*XXX*/
                           IF v-frst-i-no THEN 
                           DO:
                               /*if v-disp-item then*/
                                 ASSIGN 
                                   v-tot-ord = v-tot-ord + v-qty-ord
                                   v-grand-tot-ord = v-grand-tot-ord + v-qty-ord
                                   v-tot-ship = v-tot-ship + v-qty-ship
                                   v-grand-tot-ship = v-grand-tot-ship + v-qty-ship
                                   v-frst-i-no = NO.
                           END.
/*XXX*/
                           /*if v-disp-item then*/
                             ASSIGN v-tot-onh = v-tot-onh + v-qty-onh
                                    v-grand-tot-onh = v-grand-tot-onh + v-qty-onh
                                    v-qty-onh = 0
                                    v-qty-ship = 0
                                    v-print = YES.

                       END. /* qty onh */
                   END. /* last of bin */
               END. /* for each bin */
           END. /* item type */
        END.  /* for each item */
    END.  /* rec date */

    IF v-print                      AND
       fcst NE tcst                 AND
       (v-tot-onh NE 0 OR zbal)     THEN
      IF v-rec-dat THEN
        IF v-prt-cpn THEN
          PUT "------------"        TO 126
              "--------------"      TO 166 SKIP
              "CUSTOMER TOTALS:"    TO 106
              v-tot-onh             TO 126
              v-tot-ext             TO 166 SKIP(1).
        ELSE
          PUT "------------"        TO 104
              "--------------"      TO 144 SKIP
              "CUSTOMER TOTALS:"    TO 91
              v-tot-onh             TO 104
              v-tot-ext             TO 144 SKIP(1).
      ELSE
        IF v-prt-cpn THEN
          PUT "-----------"         TO 94
              "----------"          TO 108
              "------------"        TO 121
              "--------------"      TO 151 SKIP
              "CUSTOMER TOTALS:"    TO 77
              v-tot-ord             TO 94
              v-tot-ship            TO 108
              v-tot-onh             TO 121
              v-tot-ext             TO 151
              SKIP(1).
        ELSE
          PUT "-----------"         TO 79
              "----------"          TO 93
              "------------"        TO 106
              "--------------"      TO 136 SKIP
              "CUSTOMER TOTALS:"    TO 62
              v-tot-ord             TO 79
              v-tot-ship            TO 93
              v-tot-onh             TO 106
              v-tot-ext             TO 136
              SKIP(1).
END.  /* for each cust */

      IF v-rec-dat THEN
        IF v-prt-cpn THEN
          PUT "------------"        TO 126
              "--------------"      TO 166 SKIP
              "GRAND TOTALS:"       TO 106
              v-grand-tot-onh       TO 126
              v-grand-tot-ext       TO 166 SKIP(1).
        ELSE
          PUT "------------"        TO 104
              "--------------"      TO 144 SKIP
              "GRAND TOTALS:"       TO 91
              v-grand-tot-onh       TO 104
              v-grand-tot-ext       TO 144 SKIP(1).
      ELSE
        IF v-prt-cpn THEN
          PUT "------------"        TO 94
              "----------"          TO 108
              "------------"        TO 121
              "--------------"      TO 151 SKIP
              "GRAND TOTALS:"       TO 77
              v-grand-tot-ord       TO 94
              v-grand-tot-ship      TO 108
              v-grand-tot-onh       TO 121
              v-grand-tot-ext       TO 144
              SKIP(1).
        ELSE
          PUT "------------"        TO 79
              "----------"          TO 93
              "------------"        TO 106
              "--------------"      TO 136 SKIP
              "GRAND TOTALS:"       TO 62
              v-grand-tot-ord       TO 79
              v-grand-tot-ship      TO 93
              v-grand-tot-onh       TO 106
              v-grand-tot-ext       TO 136
              SKIP(1).

