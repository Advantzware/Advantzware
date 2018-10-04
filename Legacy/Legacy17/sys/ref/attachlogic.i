/*sys/ref/attachlogic.i*/

DEF BUFFER bf2-est FOR est.
DEF BUFFER bf2-eb FOR eb.
DEF BUFFER bf2-oe-ord FOR oe-ord.
DEF BUFFER bf2-oe-ordl FOR oe-ordl.
DEF BUFFER bf2-itemfg FOR itemfg.
DEF BUFFER bf3-oe-ordl FOR oe-ordl.

FIND FIRST bf2-est WHERE bf2-est.rec_key = ip-rec_key NO-LOCK NO-ERROR.
  IF AVAIL bf2-est THEN DO:
     ASSIGN
        v-est-no = bf2-est.est-no
        v-rec-key-list = bf2-est.rec_key.

     FOR EACH bf2-eb fields(stock-no) WHERE
         bf2-eb.company = bf2-est.company AND
         bf2-eb.est-no = bf2-est.est-no NO-LOCK :
         IF bf2-eb.stock-no <> "" THEN
         DO:
            v-i-no = v-i-no + bf2-eb.stock-no + ",".
            FIND FIRST bf2-itemfg WHERE
                 bf2-itemfg.company EQ bf2-est.company AND
                 bf2-itemfg.i-no    EQ bf2-eb.stock-no
                 NO-LOCK NO-ERROR.

            IF AVAIL bf2-itemfg AND LOOKUP(bf2-itemfg.rec_key,v-rec-key-list) EQ 0 THEN
               v-rec-key-list = v-rec-key-list + "," + bf2-itemfg.rec_key.
         END.
     END.
  END.
  ELSE DO:

      FIND FIRST bf2-oe-ord WHERE
           bf2-oe-ord.rec_key EQ ip-rec_key
           NO-LOCK NO-ERROR.

      IF AVAIL bf2-oe-ord THEN
      DO:
          FIND FIRST bf2-est WHERE
               bf2-est.company EQ bf2-oe-ord.company AND
               bf2-est.est-no EQ bf2-oe-ord.est-no
               NO-LOCK NO-ERROR.

          v-rec-key-list = bf2-oe-ord.rec_key.

          IF AVAIL bf2-est THEN DO:
             ASSIGN
             v-est-no = bf2-est.est-no
             v-rec-key-list = v-rec-key-list + "," + bf2-est.rec_key.

             FOR EACH bf2-eb FIELDS(stock-no) WHERE
                 bf2-eb.company = bf2-est.company AND
                 bf2-eb.est-no = bf2-est.est-no AND
                 bf2-eb.stock-no NE ""
                 NO-LOCK:
                 v-i-no = v-i-no + bf2-eb.stock-no + ",". 

                 FIND FIRST bf2-itemfg WHERE
                      bf2-itemfg.company EQ bf2-est.company AND
                      bf2-itemfg.i-no    EQ bf2-eb.stock-no
                      NO-LOCK NO-ERROR.

                 IF AVAIL bf2-itemfg AND LOOKUP(bf2-itemfg.rec_key,v-rec-key-list) EQ 0 THEN
                    v-rec-key-list = v-rec-key-list + "," + bf2-itemfg.rec_key.
             END.
          END.
      END.
      ELSE DO: 
          FIND FIRST bf2-oe-ordl WHERE
               bf2-oe-ordl.rec_key EQ ip-rec_key
               NO-LOCK NO-ERROR.
               
          IF AVAIL bf2-oe-ordl THEN DO:

             FOR EACH bf3-oe-ordl FIELDS(i-no) WHERE
                 bf3-oe-ordl.company EQ bf2-oe-ordl.company AND
                 bf3-oe-ordl.ord-no EQ bf2-oe-ordl.ord-no
                 NO-LOCK:
             
                 v-i-no = v-i-no + bf3-oe-ordl.i-no + ",".

                 FIND FIRST bf2-itemfg WHERE
                      bf2-itemfg.company EQ bf2-est.company AND
                      bf2-itemfg.i-no    EQ bf2-eb.stock-no
                      NO-LOCK NO-ERROR.

                 IF AVAIL bf2-itemfg AND LOOKUP(bf2-itemfg.rec_key,v-rec-key-list) EQ 0 THEN
                    v-rec-key-list = v-rec-key-list + (IF v-rec-key-list NE "" THEN "," ELSE "") + bf2-itemfg.rec_key.
             END.
          END.
          ELSE
          DO:
             FIND FIRST bf2-itemfg WHERE bf2-itemfg.rec_key = ip-rec_key NO-LOCK NO-ERROR.
             IF AVAIL bf2-itemfg THEN
                ASSIGN
                   v-i-no = bf2-itemfg.i-no
                   v-rec-key-list = bf2-itemfg.rec_key.

             ELSE IF CONNECTED("emptrack") THEN 
                RUN browsers/addonatt.p(INPUT ip-rec_key,
                                        OUTPUT v-est-no,
                                        OUTPUT v-i-no,
                                        OUTPUT v-rec-key-list).
          END.
      END.

  END.
