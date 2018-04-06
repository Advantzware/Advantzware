/* ---------------------------------------------- oe/rep/bolfibrex2.i         */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */
ASSIGN v-tot-cases = 0.

FOR EACH tt-boll, 
  FIRST itemfg NO-LOCK
   WHERE itemfg.company EQ cocode
     AND itemfg.i-no    EQ tt-boll.i-no
    BREAK BY tt-boll.i-no
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:    

  RUN get_lot_no.      

  IF ll-consol-bolls THEN DO:

    IF FIRST-OF(tt-boll.LINE) THEN DO:

      EMPTY TEMP-TABLE w2.

      ASSIGN
        i = 0
        v-tot-case-qty = 0.

      FOR EACH bf-ttboll 
        WHERE bf-ttboll.i-no = tt-boll.i-no
          AND bf-ttboll.po-no = tt-boll.po-no
          AND bf-ttboll.ord-no = tt-boll.ord-no
          AND bf-ttboll.LINE = tt-boll.LINE
         BREAK BY bf-ttboll.cases DESC:

         FIND FIRST oe-ordl 
           WHERE oe-ordl.company EQ cocode 
             AND oe-ordl.ord-no  EQ tt-boll.ord-no 
             AND oe-ordl.i-no    EQ tt-boll.i-no 
             AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

         ASSIGN v-tot-case-qty = v-tot-case-qty + bf-ttboll.qty.
         IF bf-ttboll.partial GT 0 THEN
            v-tot-case-qty = v-tot-case-qty +   bf-ttboll.partial .
         FIND FIRST oe-ord 
           WHERE oe-ord.company EQ cocode 
             AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.
         i = i + 1.
         FIND FIRST w2 WHERE (w2.cas-cnt * w2.cases) EQ (bf-ttboll.qty-case * bf-ttboll.cases) NO-ERROR.
          IF NOT AVAILABLE w2 THEN CREATE w2.

         ASSIGN 
           w2.job-po = ""
           w2.i-no = ""
           w2.cas-cnt = bf-ttboll.qty-case
           w2.cases   = w2.cases + bf-ttboll.cases
           w2.rec-id = RECID(bf-ttboll)
           w2.partial = w2.partial + bf-ttboll.partial
           w2.unitCount = bf-ttboll.unitCount
           w2.qty-sum   = bf-ttboll.qty-sum .

         IF i = 1 
           THEN ASSIGN w2.job-po = bf-ttboll.po-no
                       w2.dscr = oe-ordl.part-no
                       w2.qty = oe-ordl.qty.
           ELSE 
            IF i = 2 
              THEN ASSIGN w2.job-po = IF oe-ordl.job-no EQ "" 
                                        THEN "" 
                                        ELSE (TRIM(oe-ordl.job-no) + "-" + 
                                              STRING(oe-ordl.job-no2,"99"))
                          w2.dscr = oe-ordl.i-name
                          w2.i-no = oe-ordl.i-no.
              ELSE 
               IF i EQ 3 THEN
                   IF oe-ordl.part-dscr1 NE "" THEN 
                    ASSIGN w2.dscr = oe-ordl.part-dscr1 /*
                             w2.job-po = v-lot#   gdm 04160923 */.
                 ELSE 
                  IF i EQ 4 
                    THEN ASSIGN w2.dscr = oe-ordl.part-dscr2.
                    ELSE 
                     IF i EQ 5 THEN DO:
                       /*FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "oe-boll.lot-no" 
                           AND reftable.rec_key  EQ bf-ttboll.rec_id NO-ERROR.
                       IF AVAIL reftable THEN DO:
                         IF reftable.code NE "" 
                           THEN ASSIGN w2.dscr = reftable.code.
                         RELEASE reftable.
                       END.*/

                       ASSIGN w2.dscr = v-lot#  /* gdm 06120902 */.
                     END. /* ELSE IF i EQ 5*/
      END. /* FOR EACH bf-ttboll  */   

      IF i < 5 THEN DO i = i TO 5:
         CREATE w2.
      END.

      ASSIGN i = 0.

      RUN get_lot_no. 

      FOR EACH w2 BREAK BY w2.cases DESC:

       FIND FIRST bf-ttboll 
         WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.

       ASSIGN i = i + 1.

       IF w2.rec-id = ? THEN DO:

         FIND FIRST oe-ordl 
           WHERE oe-ordl.company EQ cocode
             AND oe-ordl.ord-no  EQ tt-boll.ord-no
             AND oe-ordl.i-no    EQ tt-boll.i-no
             AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

         ASSIGN w2.i-no = "".

         IF i = 2 
          THEN ASSIGN w2.job-po = IF oe-ordl.job-no EQ "" 
                                    THEN "" 
                                    ELSE (TRIM(oe-ordl.job-no) + "-" + 
                                          STRING(oe-ordl.job-no2,"99"))
                      w2.dscr = oe-ordl.i-name
                      w2.i-no = oe-ordl.i-no.
          ELSE 
           IF i EQ 3 
             THEN ASSIGN w2.dscr = IF TRIM(oe-ordl.part-dscr1) NE ""
                                     THEN oe-ordl.part-dscr1
                                     ELSE v-lot#
                         v-lot#  = IF TRIM(oe-ordl.part-dscr1) EQ "" 
                                     THEN ""
                                     ELSE v-lot#           /*
                         w2.job-po = v-lot#   gdm 04160923 */.
             ELSE 
              IF i EQ 4 
                THEN ASSIGN w2.dscr = IF TRIM(oe-ordl.part-dscr2) NE ""
                                        THEN oe-ordl.part-dscr2
                                        ELSE v-lot#
                            v-lot#  = IF TRIM(oe-ordl.part-dscr2) EQ "" 
                                        THEN ""
                                        ELSE v-lot#.                
                ELSE 
                 IF i EQ 5 THEN DO:
                   /*FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "oe-boll.lot-no" 
                           AND reftable.rec_key  EQ bf-ttboll.rec_id NO-ERROR.
                       IF AVAIL reftable THEN DO:
                         IF reftable.code NE "" 
                           THEN ASSIGN w2.dscr = reftable.code.
                         RELEASE reftable.
                       END.*/

                       ASSIGN w2.dscr = v-lot#  /* gdm 06120902 */.
                 END. /* ELSE IF i EQ 5 */

       END. /* IF w2.rec-id = ? */      

       IF w2.qty     EQ 0  AND 
          trim(w2.i-no)    EQ "" AND 
          trim(w2.dscr)    EQ "" AND 
          w2.cas-cnt EQ 0 
         THEN DELETE w2.

      END. /* FOR EACH w2 */

      ASSIGN i = 0.  

      RUN get_lot_no. 

      FOR EACH w2 BREAK BY w2.cases DESC:

        FIND FIRST bf-ttboll 
          WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.

        ASSIGN i = i + 1.

        IF w2.rec-id = ? THEN DO:

            FIND FIRST oe-ordl 
              WHERE oe-ordl.company EQ cocode
                and oe-ordl.ord-no  EQ tt-boll.ord-no
                and oe-ordl.i-no    EQ tt-boll.i-no
                and oe-ordl.line    EQ tt-boll.LINE NO-LOCK no-error.
             
            ASSIGN w2.i-no = "".

            IF i = 2 
              THEN ASSIGN w2.job-po = IF oe-ordl.job-no EQ "" 
                                       THEN "" 
                                       ELSE (TRIM(oe-ordl.job-no) + "-" + 
                                             STRING(oe-ordl.job-no2,"99"))
                          w2.dscr = oe-ordl.i-name
                          w2.i-no = oe-ordl.i-no.
              ELSE 
               IF i EQ 3 
                 THEN  ASSIGN w2.dscr = IF TRIM(oe-ordl.part-dscr1) NE ""
                                          THEN oe-ordl.part-dscr1
                                          ELSE v-lot#
                              v-lot#  = IF TRIM(oe-ordl.part-dscr1) EQ "" 
                                          THEN ""
                                          ELSE v-lot#           /*
                              w2.job-po = v-lot#   gdm 04160923 */.                 
                 ELSE 
                  IF i EQ 4 
                   THEN ASSIGN w2.dscr = IF TRIM(oe-ordl.part-dscr2) NE ""
                                           THEN oe-ordl.part-dscr2
                                           ELSE v-lot#
                               v-lot#  = IF TRIM(oe-ordl.part-dscr2) EQ "" 
                                           THEN ""
                                           ELSE v-lot#.                        
                   ELSE 
                    IF i EQ 5 THEN DO:
                      /*FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable EQ "oe-boll.lot-no" 
                           AND reftable.rec_key  EQ bf-ttboll.rec_id NO-ERROR.
                       IF AVAIL reftable THEN DO:
                         IF reftable.code NE "" 
                           THEN ASSIGN w2.dscr = reftable.code.
                         RELEASE reftable.
                       END.*/

                       ASSIGN w2.dscr = v-lot#  /* gdm 06120902 */.
                    END. /* ELSE IF i EQ 5 */
        END. /* IF w2.rec-id = ? */                        

        IF w2.qty  EQ 0  AND 
           w2.i-no EQ "" AND 
           w2.dscr EQ "" AND w2.cases EQ 0
            /*NOT last(w2.cases) */
          THEN .
          ELSE DO:
              ASSIGN icountpallet  = w2.cas-cnt * w2.cases .
               DISPLAY 
                 w2.i-no                       
                 TRIM(STRING(w2.qty,"->>,>>>,>>>")) WHEN i = 1 @ w2.i-no
                 w2.job-po
                 w2.dscr
                 w2.unitcount @ w2.cases
                 w2.qty-sum @ icountpallet
                 icountpallet + w2.partial /*v-tot-case-qty + w2.partial WHEN FIRST (w2.cases)*/ @ tt-boll.qty
                 bf-ttboll.p-c  WHEN AVAIL bf-ttboll AND FIRST(w2.cases) @ bf-ttboll.p-c
               WITH FRAME bol-mid.
               DOWN WITH FRAME bol-mid.       

               ASSIGN v-printline = v-printline + 1.
          END. /* ELSE DO */

        IF v-printline >= 34 THEN DO:

          ASSIGN v-printline = 0.
          PAGE {1}.
          {oe/rep/bolfibrex.i}
        END.

        ASSIGN v-tot-cases = v-tot-cases + w2.cases.

      END. /* FOR EACH w2 */

      PUT {1} SKIP(1).

      ASSIGN v-printline = v-printline + 1
             tt-boll.printed = yes.


      IF v-print-components AND 
         itemfg.alloc NE YES 
        THEN
         FOR EACH fg-set NO-LOCK
           WHERE fg-set.company EQ cocode
             AND fg-set.set-no  EQ tt-boll.i-no ,
          FIRST b-itemfg NO-LOCK
           WHERE b-itemfg.company EQ cocode
             AND b-itemfg.i-no    EQ fg-set.part-no 
            BREAK BY fg-set.set-no:

            {sys/inc/part-qty.i v-part-qty fg-set}

            IF v-printline >= 34 THEN DO: 
              ASSIGN v-printline = 0.
              PAGE {1}.
              {oe/rep/bolfibrex.i}
            END.

            DISPLAY {1}
              TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")
                   )                   @ w2.i-no
              b-itemfg.part-no         @ w2.dscr
              tt-boll.qty * v-part-qty @ tt-boll.qty
             WITH FRAME bol-mid.
             DOWN {1} WITH FRAME bol-mid.

            ASSIGN v-printline = v-printline + 1.

            DISPLAY {1}
              fg-set.part-no    @ w2.i-no
              v-job-po          @ w2.job-po
              b-itemfg.i-name   @ w2.dscr
             WITH FRAME bol-mid.
             DOWN {1} WITH FRAME bol-mid.

            DISPLAY {1}
               v-lot#            @ w2.job-po
             WITH FRAME bol-mid.
             DOWN {1} WITH FRAME bol-mid.

            PUT {1} SKIP(1).

            ASSIGN v-printline = v-printline + 2.
       
         END. /* FOR EACH fg-set */
    END. /* IF FIRST-OF(tt-boll.LINE) */
  END. /* IF ll-consol-bolls */
  ELSE DO:

   RUN get_lot_no.   

   FIND FIRST oe-ordl NO-LOCK 
     WHERE oe-ordl.company EQ cocode
       AND oe-ordl.ord-no  EQ tt-boll.ord-no
       AND oe-ordl.i-no    EQ tt-boll.i-no
       AND oe-ordl.line    EQ tt-boll.LINE NO-ERROR.

   FIND FIRST oe-ord NO-LOCK 
     WHERE oe-ord.company EQ cocode
       AND oe-ord.ord-no  EQ tt-boll.ord-no NO-ERROR.

   IF v-printline >= 34 THEN DO:
      ASSIGN v-printline = 0.
      PAGE {1}.
      {oe/rep/bolfibrex.i}
   END.

   IF tt-boll.qty-case NE 0 AND 
      tt-boll.cases NE 0 
     THEN DO:

      FIND FIRST w2 
        WHERE w2.cas-cnt EQ tt-boll.qty-case NO-ERROR.
      IF NOT AVAIL w2 
        THEN CREATE w2.

      ASSIGN w2.cas-cnt = tt-boll.qty-case
             w2.cases   = w2.cases + tt-boll.cases.

   END. /* IF tt-boll.qty-case NE 0 */

   ASSIGN v-lines = 0.

   FOR EACH w2 BREAK BY w2.cases:
     ASSIGN v-lines = v-lines + 1.
   END.

   DO i = v-lines + 1 TO 5:

     ASSIGN v-part-dscr = ""
            v-job-po    = "".

     IF i EQ 1 
      THEN ASSIGN v-part-dscr = oe-ordl.part-no
                  v-job-po    = tt-boll.po-no.
      ELSE 
       IF i EQ 2 
         THEN ASSIGN v-part-dscr = oe-ordl.i-name
                     v-job-po    = IF oe-ordl.job-no EQ "" 
                                     THEN "" 
                                     ELSE (TRIM(oe-ordl.job-no) + "-" + 
                                           STRING(oe-ordl.job-no2,"99")).
         ELSE 
          IF i EQ 3 
           THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1 /*
                       v-job-po    = v-lot#   gdm 04160923 */.
           ELSE 
            IF i EQ 4 
              THEN v-part-dscr = oe-ordl.part-dscr2.
              ELSE
               IF i EQ 5 THEN DO:
                 /*FIND FIRST reftable NO-LOCK
                   WHERE reftable.reftable EQ "oe-boll.lot-no" 
                     AND reftable.rec_key  EQ tt-boll.rec_id NO-ERROR.
                 IF AVAIL reftable THEN DO:
                   IF reftable.code NE "" 
                     THEN ASSIGN v-part-dscr = reftable.code.
                   RELEASE reftable.
                 END.*/

                 ASSIGN v-part-dscr = v-lot#  /* gdm 06120902 */.
               END.  /* IF i EQ 5 */

     IF v-part-dscr NE "" OR 
        v-job-po    NE "" OR 
        i LE 3
       THEN ASSIGN v-lines = v-lines + 1.

   END. /* DO i = v-lines + 1 */
   
   ASSIGN v-lines = v-lines + 1
          i = 0.

   FOR EACH w2 BREAK BY w2.cases:
       
     ASSIGN i = i + 1
            v-part-dscr = ""
            v-job-po    = "".    

     IF i EQ 1 
      THEN ASSIGN v-part-dscr = oe-ordl.part-no
                  v-job-po    = tt-boll.po-no.
      ELSE
       IF i EQ 2 
         THEN ASSIGN v-part-dscr = oe-ordl.i-name
                     v-job-po    = IF oe-ordl.job-no EQ "" 
                                     THEN "" 
                                     ELSE (TRIM(oe-ordl.job-no) + "-" + 
                                            STRING(oe-ordl.job-no2,"99")).
         ELSE 
          IF i EQ 3 
            THEN ASSIGN v-part-dscr = IF TRIM(oe-ordl.part-dscr1) NE ""
                                        THEN oe-ordl.part-dscr1
                                        ELSE v-lot#
                        v-lot#      = IF TRIM(oe-ordl.part-dscr1) EQ "" 
                                        THEN ""
                                        ELSE v-lot#
                /*
                        v-job-po = v-lot#   gdm 04160923 */.
            ELSE 
             IF i EQ 4
               THEN ASSIGN v-part-dscr =  IF TRIM(oe-ordl.part-dscr2) NE ""
                                            THEN oe-ordl.part-dscr2 
                                            ELSE v-lot#
                           v-lot#      = IF TRIM(oe-ordl.part-dscr2) EQ "" 
                                           THEN ""
                                           ELSE v-lot#.
               ELSE 
                IF i EQ 5 THEN DO:
                  /*FIND FIRST reftable NO-LOCK
                   WHERE reftable.reftable EQ "oe-boll.lot-no" 
                     AND reftable.rec_key  EQ tt-boll.rec_id NO-ERROR.
                 IF AVAIL reftable THEN DO:
                   IF reftable.code NE "" 
                     THEN ASSIGN v-part-dscr = reftable.code.
                   RELEASE reftable.
                 END.*/

                 ASSIGN v-part-dscr = v-lot#  /* gdm 06120902 */.
                END. /* IF i EQ 5 */
     ASSIGN icountpallet = w2.cas-cnt *  w2.cases .
     DISPLAY TRIM(STRING(oe-ordl.qty,"->>,>>>,>>>")
                  )                        WHEN i EQ 1          @ oe-ordl.i-no
             oe-ordl.i-no                  WHEN i EQ 2
             v-job-po
             v-part-dscr
             1 @ w2.cases
             icountpallet 
             tt-boll.qty /*+ tt-boll.partial*/ WHEN LAST(w2.cases)  @ tt-boll.qty
             tt-boll.p-c                   WHEN LAST(w2.cases)                
             1  WHEN i = 2 AND tt-boll.partial > 0  @ w2.cases
             tt-boll.partial WHEN i = 2 AND tt-boll.partial > 0 @ w2.cas-cnt
      WITH FRAME bol-mid2.
      DOWN  WITH FRAME bol-mid2.

     ASSIGN v-printline = v-printline + 1.

     IF v-printline >= 34 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolfibrex.i}
     END.

     ASSIGN v-tot-cases = v-tot-cases + w2.cases.

     DELETE w2.
   END. /* each w2 */

   /* gdm 06120902 */
   RUN get_lot_no.

   DO i = i + 1 TO 5:

     CLEAR FRAME bol-mid2 NO-PAUSE.

     ASSIGN v-part-dscr = "" 
            v-job-po    = "". 

     IF i EQ 1 
      THEN ASSIGN v-part-dscr = oe-ordl.part-no
                  v-job-po    = tt-boll.po-no.
      ELSE 
       IF i EQ 2 
         THEN ASSIGN v-part-dscr = oe-ordl.i-name
                     v-job-po    = IF oe-ordl.job-no EQ "" 
                                    THEN "" 
                                    ELSE (TRIM(oe-ordl.job-no) + "-" + 
                                          STRING(oe-ordl.job-no2,"99")).
         ELSE 
          IF i EQ 3 
            THEN ASSIGN v-part-dscr = IF TRIM(oe-ordl.part-dscr1) NE ""
                                        THEN oe-ordl.part-dscr1 
                                        ELSE v-lot#
                        v-lot#      = IF TRIM(oe-ordl.part-dscr1) EQ ""
                                        THEN ""
                                        ELSE v-lot# 
                /*
                        v-job-po = v-lot#   gdm 04160923 */.
            ELSE 
             IF i EQ 4
               THEN ASSIGN v-part-dscr = IF TRIM(oe-ordl.part-dscr2) NE ""
                                           THEN oe-ordl.part-dscr2 
                                           ELSE v-lot#
                           v-lot#      = IF TRIM(oe-ordl.part-dscr2) EQ ""
                                           THEN ""
                                           ELSE v-lot# .
               ELSE 
                IF i EQ 5 THEN DO:
                  /*FIND FIRST reftable NO-LOCK
                   WHERE reftable.reftable EQ "oe-boll.lot-no" 
                     AND reftable.rec_key  EQ tt-boll.rec_id NO-ERROR.
                 IF AVAIL reftable THEN DO:
                   IF reftable.code NE "" 
                     THEN ASSIGN v-part-dscr = reftable.code.
                   RELEASE reftable.
                 END.*/

                 ASSIGN v-part-dscr = v-lot#  /* gdm 06120902 */.
                END. /* IF i EQ 5 */

     IF i = 2 AND 
        v-job-po = "" 
       THEN ASSIGN v-job-po = IF tt-boll.job-no EQ "" 
                                THEN "" 
                                ELSE (TRIM(tt-boll.job-no) + "-" + 
                                      STRING(tt-boll.job-no2,"99")).

     IF v-part-dscr NE "" OR 
        v-job-po    NE "" OR 
        i le 2
       THEN do:

        DISPLAY {1}
                oe-ordl.i-no    WHEN i EQ 2
                v-job-po
                v-part-dscr                          
                1               WHEN tt-boll.partial > 0 AND 
                                i = 2                    @ w2.cases
                tt-boll.partial WHEN tt-boll.partial > 0 AND 
                                i = 2                    @ w2.cas-cnt
         WITH FRAME bol-mid2.
         DOWN {1} WITH FRAME bol-mid2.

        ASSIGN v-printline = v-printline + 1.

     END. /* IF v-part-dscr NE "" */

   END. /* DO i = i */

   PUT {1} SKIP(1).

   ASSIGN v-printline = v-printline + 1
          tt-boll.printed = yes.

   IF v-print-components AND 
      itemfg.alloc NE YES 
     THEN
      FOR EACH fg-set NO-LOCK
        WHERE fg-set.company EQ cocode
          AND fg-set.set-no  EQ tt-boll.i-no,
       FIRST b-itemfg NO-LOCK
        WHERE b-itemfg.company EQ cocode
          AND b-itemfg.i-no    EQ fg-set.part-no 
         BREAK BY fg-set.set-no:

         {sys/inc/part-qty.i v-part-qty fg-set}

         IF v-printline >= 34 THEN DO:
           v-printline = 0.
           PAGE {1}.
           {oe/rep/bolfibrex.i}
         END.

         DISPLAY {1}
                 TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                  @ oe-ordl.i-no
                 b-itemfg.part-no                 @ v-part-dscr
                 tt-boll.qty * v-part-qty         @ tt-boll.qty        
          WITH FRAME bol-mid2.
          DOWN {1} WITH FRAME bol-mid2.

         ASSIGN v-printline = v-printline + 1.
            
         DISPLAY {1}
                 fg-set.part-no                          @ oe-ordl.i-no
                 v-job-po
                 b-itemfg.i-name                         @ v-part-dscr
          WITH FRAME bol-mid2.
          DOWN {1} WITH FRAME bol-mid2.

         PUT {1} SKIP(1).

         ASSIGN v-printline = v-printline + 2.
    END. /* FOR EACH fg-set */

  END. /* ELSE DO - non consol-bol */

END. /* FOR EACH tt-boll */

ASSIGN v-tot-wt = oe-bolh.tot-wt.


IF oe-bolh.ship-i[1] NE "" OR
   oe-bolh.ship-i[2] NE "" OR
   oe-bolh.ship-i[3] NE "" OR
   oe-bolh.ship-i[4] NE "" 
  THEN DO:

   IF v-printline >= 30 THEN DO:
     ASSIGN v-printline = 0.
     PAGE {1}.
     {oe/rep/bolfibrex.i}
   END.

   PUT "<FArial><P12><B>Notes:</P12><P10></B>" SKIP.

   DO v-i = 1 TO 4:
     IF v-printline >= 34 THEN DO:
       ASSIGN v-printline = 0.
       PAGE {1}.
       {oe/rep/bolfibrex.i}
     END.

     IF oe-bolh.ship-i[v-i] NE "" THEN DO:
        PUT oe-bolh.ship-i[v-i] SKIP.
        ASSIGN v-printline = v-printline + 1.
     END.
   END.

END.

IF AVAIL tt-bolx THEN DO:

  IF tt-bolx.note-1 NE "" THEN DO:
    IF v-printline >= 34 THEN DO:
      ASSIGN v-printline = 0.
      PAGE {1}.
      {oe/rep/bolfibrex.i}
    END.

    PUT tt-bolx.note-1 FORMAT "X(60)" SKIP.

    ASSIGN v-printline = v-printline + 1.
  END.

  IF tt-bolx.note-2 NE "" THEN DO:
    IF v-printline >= 34 THEN DO:
      ASSIGN v-printline = 0.
      PAGE {1}.
      {oe/rep/bolfibrex.i}
    END.

    PUT tt-bolx.note-2 FORMAT "X(60)" SKIP.

    ASSIGN v-printline = v-printline + 1.

  END.

END.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
