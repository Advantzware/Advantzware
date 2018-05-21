DEF PARAM BUFFER io-po-ordl FOR po-ordl.
DEF BUFFER io-e-itemfg-vend FOR e-itemfg-vend.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-job-hdr FOR job-hdr.

DEF VAR lv-ord-no LIKE po-ordl.ord-no NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-ordl.

IF AVAIL io-po-ordl THEN
FIND FIRST po-ord
    WHERE po-ord.company EQ io-po-ordl.company
      AND po-ord.po-no   EQ io-po-ordl.po-no
    NO-ERROR.

IF AVAIL po-ord THEN DO:
  ASSIGN
   cocode = po-ord.company
   locode = po-ord.loc.  

  /*raw material*/
  IF io-po-ordl.item-type THEN DO:          
    FIND FIRST item NO-LOCK
        WHERE item.company EQ io-po-ordl.company
          AND item.i-no    EQ io-po-ordl.i-no
        USE-INDEX i-no NO-ERROR.
    IF AVAIL item AND item.mat-type EQ "B" AND io-po-ordl.job-no NE "" THEN DO:
      IF io-po-ordl.ord-no EQ 0 THEN
      FOR EACH job-hdr NO-LOCK
          WHERE job-hdr.company   EQ io-po-ordl.company
            AND job-hdr.job-no    EQ io-po-ordl.job-no
            AND job-hdr.job-no2   EQ io-po-ordl.job-no2
            AND (job-hdr.frm      EQ io-po-ordl.s-num OR
                 io-po-ordl.s-num EQ ?                OR
                 CAN-FIND(b-job-hdr
                          WHERE b-job-hdr.company EQ io-po-ordl.company
                            AND b-job-hdr.job-no  EQ io-po-ordl.job-no
                            AND b-job-hdr.job-no2 EQ io-po-ordl.job-no2))
            AND job-hdr.ord-no    NE 0:

        lv-ord-no = job-hdr.ord-no.
        LEAVE.
      END.
      ELSE lv-ord-no = io-po-ordl.ord-no.
      
      FIND FIRST job WHERE job.company = io-po-ordl.company
                       AND job.job-no = io-po-ordl.job-no 
                       AND job.job-no2 = io-po-ordl.job-no2 
                     NO-LOCK NO-ERROR.
      IF AVAIL job AND job.est-no > ""  THEN DO:
        FOR EACH ef WHERE ef.company = job.company
                      AND ef.est-no = job.est-no
                      AND ef.board = io-po-ordl.i-no 
                      AND ef.FORM-no = io-po-ordl.s-num
                   NO-LOCK,
            EACH eb WHERE eb.company = ef.company
                     AND eb.est-no   = ef.est-no
                     AND eb.form-no  = ef.form-no.
           FIND FIRST oe-ordl WHERE oe-ordl.company = io-po-ordl.company
                                AND oe-ordl.job-no  = io-po-ordl.job-no
                                AND oe-ordl.job-no2 = io-po-ordl.job-no2
                                AND oe-ordl.i-no    = eb.stock-no
                              NO-ERROR.
           IF AVAIL oe-ordl THEN
             ASSIGN
               oe-ordl.vend-no  = po-ord.vend-no
               oe-ordl.po-no-po = po-ord.po-no.           
        END.
      END.
      IF lv-ord-no NE 0 THEN
      FOR EACH oe-ordl
          WHERE oe-ordl.company  EQ io-po-ordl.company
            AND oe-ordl.ord-no   EQ lv-ord-no
            AND oe-ordl.job-no   EQ io-po-ordl.job-no
            AND oe-ordl.job-no2  EQ io-po-ordl.job-no2
            AND (oe-ordl.form-no EQ io-po-ordl.s-num OR io-po-ordl.s-num EQ ?):
        ASSIGN
         oe-ordl.vend-no  = po-ord.vend-no
         oe-ordl.po-no-po = po-ord.po-no.
      END.
    END. /*if avail item and mat-type eq "B"*/

    IF AVAIL item THEN
    DO:
       /*order misc charges*/
       /* gdm - 01270906 */
       IF io-po-ordl.job-no NE "" AND 
          io-po-ordl.ord-no NE 0 THEN DO:

           FOR FIRST prep NO-LOCK
               WHERE prep.company EQ io-po-ordl.company 
                 AND prep.i-no EQ io-po-ordl.i-no,
               EACH oe-ordm EXCLUSIVE-LOCK
               WHERE oe-ordm.company   EQ io-po-ordl.company 
                 AND oe-ordm.ord-no    EQ io-po-ordl.ord-no 
                 AND oe-ordm.ord-i-no  EQ io-po-ordl.job-no 
                 AND oe-ordm.ord-line  EQ io-po-ordl.job-no2 
                 AND oe-ordm.charge    EQ prep.CODE:

               IF io-po-ordl.cons-uom EQ "EA" 
                 THEN 
                  oe-ordm.cost = (io-po-ordl.ord-qty * io-po-ordl.cons-cost).
                 ELSE
                  RUN sys/ref/convcuom.p (io-po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                                          io-po-ordl.cons-cost, OUTPUT oe-ordm.cost).             

               ASSIGN 
                   oe-ordm.po-no-po = io-po-ordl.po-no.                   

           END. /* for each */
       END. /* both job-no & ord-no avail */
       ELSE
       IF io-po-ordl.job-no NE "" AND 
          (io-po-ordl.ord-no EQ 0 OR io-po-ordl.ord-no EQ ?) THEN DO:

           FOR FIRST prep NO-LOCK
               WHERE prep.company EQ io-po-ordl.company 
                 AND prep.i-no EQ io-po-ordl.i-no,
               EACH oe-ordm EXCLUSIVE-LOCK
               WHERE oe-ordm.company   EQ io-po-ordl.company 
                 AND oe-ordm.ord-i-no  EQ io-po-ordl.job-no 
                 AND oe-ordm.ord-line  EQ io-po-ordl.job-no2 
                 AND oe-ordm.charge    EQ prep.CODE:

               IF io-po-ordl.cons-uom EQ "EA" 
                 THEN 
                  oe-ordm.cost = (io-po-ordl.ord-qty * io-po-ordl.cons-cost).
                 ELSE
                  RUN sys/ref/convcuom.p (io-po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                                          io-po-ordl.cons-cost, OUTPUT oe-ordm.cost)).             

               ASSIGN 
                   oe-ordm.po-no-po = io-po-ordl.po-no.                   

           END. /* for each */
       END. /* job-no avail & ord-no not avail */
       ELSE
       IF io-po-ordl.job-no EQ "" AND 
          io-po-ordl.ord-no NE 0 THEN DO:

           FOR FIRST prep NO-LOCK
               WHERE prep.company EQ io-po-ordl.company 
                 AND prep.i-no EQ io-po-ordl.i-no,
               EACH oe-ordm EXCLUSIVE-LOCK
               WHERE oe-ordm.company   EQ io-po-ordl.company 
                 AND oe-ordm.ord-no    EQ io-po-ordl.ord-no                  
                 AND oe-ordm.charge    EQ prep.CODE:

               IF io-po-ordl.cons-uom EQ "EA" 
                 THEN 
                  oe-ordm.cost = (io-po-ordl.ord-qty * io-po-ordl.cons-cost).                 ELSE
                  RUN sys/ref/convcuom.p (io-po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                                          io-po-ordl.cons-cost, OUTPUT oe-ordm.cost)).             

               ASSIGN 
                   oe-ordm.po-no-po = io-po-ordl.po-no.
                   
           END. /* for each */
       END. /* job-no not avail & ord-no avail */

    END. /* IF AVAIL item */
  END. /*if raw material*/
          
  /*finished good*/
  ELSE DO:
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ io-po-ordl.company 
          AND oe-ordl.i-no    EQ io-po-ordl.i-no
          AND oe-ordl.ord-no  EQ io-po-ordl.ord-no
        NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
      ASSIGN
       oe-ordl.po-no-po = po-ord.po-no
       oe-ordl.vend-no  = po-ord.vend-no.
               
      IF io-po-ordl.cons-uom EQ "M" THEN
        oe-ordl.cost = io-po-ordl.cons-cost.
      ELSE
        RUN sys/ref/convcuom.p (io-po-ordl.cons-uom, "M", 0, 0, 0, 0,
                                io-po-ordl.cons-cost, OUTPUT oe-ordl.cost).

      FIND FIRST io-e-itemfg-vend WHERE
                   io-e-itemfg-vend.company EQ io-po-ordl.company AND
                   io-e-itemfg-vend.i-no EQ io-po-ordl.i-no AND
                   io-e-itemfg-vend.vend-no EQ po-ord.vend-no AND
                   io-e-itemfg-vend.est-no EQ ""
                   NO-LOCK NO-ERROR.

              IF AVAIL io-e-itemfg-vend THEN
      DO:
         oe-ordl.cost = oe-ordl.cost * (1 + (io-e-itemfg-vend.markup / 100.0 )).
         END.
    END.
  END.
  /* Add scores data to ref table if required per NK1 value */
  RUN po/poaddscores.p (INPUT ROWID(io-po-ordl)).
END.
