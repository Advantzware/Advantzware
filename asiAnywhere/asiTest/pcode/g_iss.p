/*------------------------------------------------------------------------
    File        : g-iss.p
    Purpose     : PO#
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt_g_iss_Lookup NO-UNDO 
    FIELD vfrm            AS INT
    FIELD vblk            AS INT
    FIELD vout            AS INT
    FIELD rm-no           AS CHAR
    FIELD fgitem          AS CHAR
    FIELD rec_key          AS CHAR 
    .

DEFINE DATASET ds_g_iss_Lookup FOR tt_g_iss_Lookup .

DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsno             AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmbno             AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmjob             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmjob2            AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmitem            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmqty             AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmqtyuom          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmpruom           AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcost            AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmpo              AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmvout            AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmline            AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey          AS CHAR NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR ds_g_iss_Lookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-s-num NO-UNDO FIELD s-num LIKE po-ordl.s-num
                                FIELD row-id AS ROWID.

DEF TEMP-TABLE tt-job-mat NO-UNDO LIKE job-mat
    FIELD orig-lot-cost-upd AS LOG
    FIELD orig-lot-cost AS DEC DECIMALS 6
    FIELD row-id AS ROWID.

DEF NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no LIKE job-mat.i-no
    FIELD rec-id AS RECID.

def buffer xpo-ordl for po-ordl.

{windows/l-jobmt1.i NEW}
{sys/inc/var.i new shared}
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.


IF prmAction           = ? THEN ASSIGN prmAction          = "".
IF prmUser             = ? THEN ASSIGN prmUser            = "".
IF prmsno              = ? THEN ASSIGN prmsno             = 0.  
IF prmbno              = ? THEN ASSIGN prmbno             = 0.  
IF prmjob              = ? THEN ASSIGN prmjob             = "". 
IF prmjob2             = ? THEN ASSIGN prmjob2            = 0.  
IF prmitem             = ? THEN ASSIGN prmitem            = "". 
IF prmqty              = ? THEN ASSIGN prmqty             = 0.  
IF prmqtyuom           = ? THEN ASSIGN prmqtyuom          = "". 
IF prmpruom            = ? THEN ASSIGN prmpruom           = "".
IF prmcost             = ? THEN ASSIGN prmcost            = 0.
IF prmpo               = ? THEN ASSIGN prmpo              = "".
IF prmvout             = ? THEN ASSIGN prmvout            = 0.
IF prmReckey           = ? THEN ASSIGN prmReckey          = "".


  DEF VAR ld-job-up AS DEC NO-UNDO.
  DEF VAR ld-job-qty LIKE job-hdr.qty NO-UNDO.
  DEF VAR count-mat AS INT NO-UNDO.
  DEF VAR v-out AS INT NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR lv-sheet LIKE po-ordl.s-num NO-UNDO.
  DEF VAR lv-blank LIKE po-ordl.b-num NO-UNDO.
  DEF VAR ll-layout AS LOG NO-UNDO.
  DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.

  DEF BUFFER xitem FOR item.
  DEF VAR ll AS LOG NO-UNDO.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
 cocode = prmComp
 g_company = cocode  .


FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpo)  and
           po-ordl.LINE EQ INT(prmline) NO-LOCK NO-ERROR.

 FIND FIRST job-hdr
        WHERE job-hdr.company   EQ g_company
          AND job-hdr.job-no    EQ FILL(" ",6 - LENGTH(TRIM(prmjob))) + TRIM(prmjob)
          AND job-hdr.job-no2   EQ INT(prmjob2)
          AND (job-hdr.frm      EQ INT(prmsno) OR
               prmsno EQ ? OR INT(prmsno) EQ 0) NO-LOCK NO-ERROR.
 ASSIGN
     prmsno = job-hdr.frm .
 RELEASE job-hdr .



IF prmAction = "submit" then do:
 

 FOR EACH tt-job-mat WHERE tt-job-mat.frm NE ?:
    DELETE tt-job-mat.
 END.

  EMPTY TEMP-TABLE tt-s-num.

  
    
      CREATE tt-s-num.
      tt-s-num.s-num = INT(prmsno).
    

    FOR EACH tt-s-num BREAK BY tt-s-num.s-num:
      FIND FIRST job
          WHERE job.company EQ g_company
            AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(prmjob))) + TRIM(prmjob)
            AND job.job-no2 EQ INT(prmjob2)
          NO-LOCK NO-ERROR.

      FIND FIRST item
          WHERE item.company EQ cocode
            AND item.i-no    EQ prmitem
          NO-LOCK NO-ERROR.

      ASSIGN
       fil_id    = ?
       ll-layout = CAN-DO("1,2,3,4,B,P,R",item.mat-type).

      FIND job-mat WHERE ROWID(job-mat) EQ tt-s-num.row-id NO-LOCK NO-ERROR.

      IF NOT AVAIL job-mat THEN DO:
        EMPTY TEMP-TABLE item-chg.

        FOR EACH job-mat
            WHERE job-mat.company  EQ job.company
              AND job-mat.job      EQ job.job
              AND job-mat.job-no   EQ job.job-no
              AND job-mat.job-no2  EQ job.job-no2
              AND job-mat.frm      EQ tt-s-num.s-num  
              AND (job-mat.rec_key  EQ prmReckey OR prmReckey = "")
              AND (job-mat.blank-no EQ INT(prmbno) OR
                   INT(prmbno) = 0 OR
                   job-mat.blank-no EQ 0)        
              AND NOT CAN-FIND(FIRST xpo-ordl
                               WHERE xpo-ordl.company EQ job-mat.company
                                 AND xpo-ordl.po-no   EQ INT(prmpo)
                                 AND xpo-ordl.job-no  EQ job-mat.job-no
                                 AND xpo-ordl.job-no2 EQ job-mat.job-no2
                                 AND xpo-ordl.i-no    EQ job-mat.rm-i-no
                                 AND (xpo-ordl.s-num  EQ job-mat.frm OR
                                      xpo-ordl.s-num  EQ ?)
                                 AND xpo-ordl.b-num   EQ job-mat.blank-no
                                 AND RECID(xpo-ordl)  NE RECID(po-ordl))                                       
            NO-LOCK,
 
            FIRST xitem
            WHERE xitem.company  EQ job-mat.company
              AND xitem.i-no     EQ job-mat.rm-i-no
              AND xitem.mat-type EQ item.mat-type
            NO-LOCK:
 
          count-mat = count-mat + 1.
          CREATE item-chg.
          ASSIGN
           /*item-chg.i-no   = xitem.i-no*/
           item-chg.rec-id = RECID(job-mat)
           fil_id          = RECID(item-chg).    
        END. /*job-mat*/

        /*IF count-mat GT 1 THEN RUN rm/g-itmchg.w.*/

        FIND FIRST item-chg WHERE RECID(item-chg) EQ fil_id NO-LOCK NO-ERROR.

        fil_id = ?.

        RELEASE job-mat.

        IF AVAIL item-chg THEN
        FIND job-mat WHERE RECID(job-mat) EQ item-chg.rec-id NO-ERROR.
      END. /*if job-mat*/

      CREATE tt-job-mat.

      ASSIGN
       ld-job-up  = 0
       ld-job-qty = 0.

      FIND FIRST est
          WHERE est.company EQ job.company
            AND est.est-no  EQ job.est-no
          NO-LOCK NO-ERROR.

      IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
         FOR EACH job-hdr FIELDS(qty)
             WHERE job-hdr.company EQ job.company
               AND job-hdr.job     EQ job.job
               AND job-hdr.job-no  EQ job.job-no
               AND job-hdr.job-no2 EQ job.job-no2
             NO-LOCK:
        
             ld-job-qty = ld-job-qty + job-hdr.qty.
         END.
      ELSE
         FOR EACH job-hdr FIELDS(n-on qty)
             WHERE job-hdr.company EQ job.company
               AND job-hdr.job     EQ job.job
               AND job-hdr.job-no  EQ job.job-no
               AND job-hdr.job-no2 EQ job.job-no2
               AND job-hdr.frm    EQ tt-s-num.s-num
             NO-LOCK:
         
             ASSIGN
                ld-job-qty = ld-job-qty + job-hdr.qty
                ld-job-up  = ld-job-up + job-hdr.n-on.
         END.
      
      IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
         FOR EACH eb FIELDS(num-up)
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               AND eb.form-no EQ tt-s-num.s-num
             NO-LOCK:
             ld-job-up = ld-job-up + eb.num-up.
         END.

      IF ll-layout OR ld-job-up EQ 0 THEN ld-job-up = 1.

      IF FIRST(tt-s-num.s-num) THEN DO:
        lv-sheet = INT(prmsno).

        IF AVAIL job-mat THEN
          ASSIGN
           lv-blank = job-mat.blank-no
           prmvout    = (job-mat.n-up / ld-job-up).
        ELSE
          ASSIGN
           lv-blank = INT(prmbno)
           prmvout    = 1.

       IF ll-layout THEN DO:
          /*RUN rm/g-iss2.w (lv-sheet, lv-blank, INPUT-OUTPUT prmvout). */
                 
          IF AVAIL job-mat THEN DO:
            IF item.i-code EQ "R" THEN DO:
              IF (item.r-wid NE 0 AND item.r-wid LT job-mat.wid) OR
                 (item.r-wid EQ 0 AND (item.s-wid LT job-mat.wid OR
                                       item.s-len LT job-mat.len)) THEN DO:
                choice = NO.

                /*IF item.r-wid NE 0 THEN
                  RUN rm/g-iss21.w (job-mat.len, job-mat.len, item.r-wid, job-mat.wid, job-mat.frm,
                                    OUTPUT choice)  .
                ELSE
                  RUN rm/g-iss21.w (item.s-len, job-mat.len, item.s-wid,job-mat.wid, job-mat.frm,
                                    OUTPUT choice)  .
                
                IF NOT choice THEN DELETE tt-job-mat.*/
              END.
            END.
          END.
       END.
      END.

      IF AVAIL job-mat THEN DO:
        FIND FIRST xitem
            WHERE xitem.company EQ cocode
              AND xitem.i-no    EQ job-mat.rm-i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAIL xitem THEN DELETE tt-job-mat.

        IF AVAIL tt-job-mat THEN
          BUFFER-COPY job-mat TO tt-job-mat
          ASSIGN tt-job-mat.row-id = ROWID(job-mat).
      END.

      ELSE DO:
        ASSIGN
         tt-job-mat.company  = job.company
         tt-job-mat.job      = job.job
         tt-job-mat.job-no   = job.job-no
         tt-job-mat.job-no2  = job.job-no2
         tt-job-mat.frm      = tt-s-num.s-num
         tt-job-mat.blank-no = lv-blank
         tt-job-mat.qty-uom  = item.cons-uom
         tt-job-mat.n-up     = ld-job-up.

        IF prmqtyuom NE ""  AND
           DEC(prmqty) NE 0 THEN
          ASSIGN
           tt-job-mat.qty-uom = prmqtyuom
           tt-job-mat.qty     = DEC(prmqty).

        IF prmpruom NE ""   AND
           DEC(prmcost) NE 0 THEN
           DO:
              ASSIGN
                 tt-job-mat.sc-uom   = prmpruom
                 tt-job-mat.std-cost = DEC(prmcost).

              IF LOOKUP(tt-job-mat.sc-uom,"L,LOT") GT 0 THEN
                 ASSIGN
                    tt-job-mat.orig-lot-cost-upd = YES
                    tt-job-mat.orig-lot-cost = DEC(prmcost).
           END.
      END.

      IF AVAIL tt-job-mat THEN DO:
        IF tt-job-mat.sc-uom EQ tt-job-mat.qty-uom THEN
           v-cost = tt-job-mat.std-cost.
        ELSE
           IF LOOKUP(tt-job-mat.sc-uom,"L,LOT") EQ 0 THEN
              RUN sys/ref/convcuom.p(tt-job-mat.sc-uom,
                                     tt-job-mat.qty-uom,
                                     tt-job-mat.basis-w,
                                     tt-job-mat.len,
                                     tt-job-mat.wid,
                                     item.s-dep,
                                     tt-job-mat.std-cost,
                                     OUTPUT v-cost).
        ELSE
           IF LOOKUP(tt-job-mat.sc-uom,"L,LOT") GT 0 AND
              tt-job-mat.qty NE 0 THEN
              v-cost = tt-job-mat.std-cost / tt-job-mat.qty.

        v-cost = v-cost * tt-job-mat.qty.                       
                    
        IF tt-job-mat.n-up LE 0 THEN tt-job-mat.n-up = 1.
        IF ld-job-up LE 0 THEN ld-job-up = 1.
        IF prmvout LE 0 THEN v-out = 1.

        ASSIGN                 
         tt-job-mat.rm-i-no = item.i-no
         tt-job-mat.i-no    = item.i-no
         tt-job-mat.basis-w = item.basis-w
         tt-job-mat.qty     = tt-job-mat.qty * tt-job-mat.n-up
         tt-job-mat.n-up    = ld-job-up * prmvout
         tt-job-mat.qty     = tt-job-mat.qty / tt-job-mat.n-up
         tt-job-mat.cost-m  = v-cost / (ld-job-qty / 1000).

        IF item.i-code EQ "R" OR NOT AVAIL job-mat THEN
          ASSIGN
           tt-job-mat.sc-uom  = item.cons-uom
           tt-job-mat.wid     = IF item.r-wid NE 0 THEN
                                item.r-wid ELSE item.s-wid
           tt-job-mat.len     = IF item.r-wid NE 0 THEN
                                tt-job-mat.len ELSE item.s-len.
                     
        IF tt-job-mat.qty-uom EQ "EA" THEN DO:
          {sys/inc/roundup.i tt-job-mat.qty}
        END.
        
        v-cost = v-cost / tt-job-mat.qty.
        IF v-cost = ? THEN v-cost = 0.

        IF tt-job-mat.qty-uom EQ tt-job-mat.sc-uom THEN
           tt-job-mat.std-cost = v-cost.
        ELSE
           RUN sys/ref/convcuom.p(tt-job-mat.qty-uom,
                                  tt-job-mat.sc-uom,
                                  tt-job-mat.basis-w,
                                  tt-job-mat.len,
                                  tt-job-mat.wid,
                                  item.s-dep,
                                  v-cost,
                                  OUTPUT tt-job-mat.std-cost).
      END.

      DELETE tt-s-num.
    END.  /* each tt-s-num */

    FIND FIRST tt-job-mat
        WHERE tt-job-mat.frm EQ INT(prmsno)
        NO-ERROR.

    IF AVAIL tt-job-mat THEN fil_id = RECID(tt-job-mat).
  /*END.*/


  FOR EACH tt-job-mat:
    IF tt-job-mat.frm NE ? THEN DO:
      FIND FIRST job-mat
          WHERE ROWID(job-mat)  EQ tt-job-mat.row-id
            AND job-mat.j-no    EQ 0
          NO-ERROR.

      ll = AVAIL job-mat.

      IF ll THEN RUN jc/maydeletejob-mat.p (BUFFER job-mat, OUTPUT ll).

      IF ll NE YES THEN CREATE job-mat.

      BUFFER-COPY tt-job-mat EXCEPT rec_key TO job-mat
      ASSIGN
       job-mat.blank-no = prmbno
       job-mat.j-no     = 1
       job-mat.qty-all  = job-mat.qty.
      IF po-ordl.s-num NE ? THEN job-mat.frm = prmsno.
    END.
    DELETE tt-job-mat.
  END.


END.


IF prmAction = "Select" then do:


    EMPTY TEMP-TABLE tt-s-num.
    
        CREATE tt-s-num.
        tt-s-num.s-num = INT(prmsno).


    FOR EACH tt-s-num BREAK BY tt-s-num.s-num:
      FIND FIRST job
          WHERE job.company EQ g_company
            AND job.job-no  EQ prmjob
            AND job.job-no2 EQ INT(prmjob2)
          NO-LOCK NO-ERROR.

      FIND FIRST item
          WHERE item.company EQ job.company
            AND item.i-no    EQ prmitem
          NO-LOCK NO-ERROR.

      ASSIGN
       fil_id    = ?
       ll-layout = CAN-DO("1,2,3,4,B,P,R",item.mat-type).

      FIND job-mat WHERE ROWID(job-mat) EQ tt-s-num.row-id NO-LOCK NO-ERROR.

      IF NOT AVAIL job-mat THEN DO:
        EMPTY TEMP-TABLE item-chg.

        FOR EACH job-mat
            WHERE job-mat.company  EQ job.company
              AND job-mat.job      EQ job.job
              AND job-mat.job-no   EQ job.job-no
              AND job-mat.job-no2  EQ job.job-no2
              AND job-mat.frm      EQ tt-s-num.s-num                    
              AND (job-mat.blank-no EQ INT(prmbno) OR
                   INT(prmbno) = 0 OR
                   job-mat.blank-no EQ 0)        
              AND NOT CAN-FIND(FIRST xpo-ordl
                               WHERE xpo-ordl.company EQ job-mat.company
                                 AND xpo-ordl.po-no   EQ INT(prmpo)
                                 AND xpo-ordl.job-no  EQ job-mat.job-no
                                 AND xpo-ordl.job-no2 EQ job-mat.job-no2
                                 AND xpo-ordl.i-no    EQ job-mat.rm-i-no
                                 AND (xpo-ordl.s-num  EQ job-mat.frm OR
                                      xpo-ordl.s-num  EQ ?)
                                 AND xpo-ordl.b-num   EQ job-mat.blank-no
                                 AND RECID(xpo-ordl)  NE RECID(po-ordl))                                        
            NO-LOCK,
 
            FIRST xitem
            WHERE xitem.company  EQ job-mat.company
              AND xitem.i-no     EQ job-mat.rm-i-no
              AND xitem.mat-type EQ item.mat-type
            NO-LOCK:


   
          count-mat = count-mat + 1.
          CREATE item-chg.
          ASSIGN
           item-chg.i-no   = xitem.i-no
           item-chg.rec-id = RECID(job-mat)
           fil_id          = RECID(item-chg).    
        END. /*job-mat*/

       /* IF count-mat GT 1 THEN /*RUN rm/g-itmchg.w. */*/

            FOR EACH item-chg,
            FIRST job-mat WHERE RECID(job-mat) EQ item-chg.rec-id NO-LOCK:

            ASSIGN
                ld-job-up  = 0
                ld-job-qty = 0.

            FIND FIRST est
                WHERE est.company EQ job.company
                AND est.est-no  EQ job.est-no
                NO-LOCK NO-ERROR.

            IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                FOR EACH job-hdr FIELDS(qty)
                WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                NO-LOCK:

                ld-job-qty = ld-job-qty + job-hdr.qty.
            END.

            ELSE
                FOR EACH job-hdr FIELDS(n-on qty)
                    WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.frm    EQ tt-s-num.s-num
                    NO-LOCK:

                ASSIGN
                    ld-job-qty = ld-job-qty + job-hdr.qty
                    ld-job-up  = ld-job-up + job-hdr.n-on.
                END.

                IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                    FOR EACH eb FIELDS(num-up)
                    WHERE eb.company EQ est.company
                    AND eb.est-no  EQ est.est-no
                    AND eb.form-no EQ tt-s-num.s-num
                    NO-LOCK:
                    ld-job-up = ld-job-up + eb.num-up.
                    END.

                    IF ll-layout OR ld-job-up EQ 0 THEN ld-job-up = 1.
            /*END.*/


            lv-sheet = INT(prmsno).
        
            IF AVAIL job-mat THEN
                ASSIGN
                lv-blank = job-mat.blank-no
                v-out    = (job-mat.n-up / ld-job-up).
            ELSE
                ASSIGN
                    lv-blank = INT(prmbno)
                    v-out    = 1.

           CREATE tt_g_iss_Lookup.
           ASSIGN
               tt_g_iss_Lookup.rec_key = job-mat.rec_key
               tt_g_iss_Lookup.rm-no   = item-chg.i-no 
               tt_g_iss_Lookup.vfrm     =  lv-sheet
               tt_g_iss_Lookup.vblk     =  lv-blank
               tt_g_iss_Lookup.vout     =  v-out  .


                    
            FIND FIRST reftable
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ job-mat.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(job-mat.job,"999999999")
                AND reftable.val[12]  EQ job-mat.frm
                AND (reftable.val[13] EQ job-mat.blank-no OR job-mat.blank-no EQ 0)
                NO-LOCK NO-ERROR.

            IF AVAIL reftable THEN tt_g_iss_Lookup.fgitem = reftable.code2.
            ELSE
                FOR EACH job-hdr
                    WHERE job-hdr.company EQ job-mat.company
                    AND job-hdr.job     EQ job-mat.job
                    AND job-hdr.job-no  EQ job-mat.job-no
                    AND job-hdr.job-no2 EQ job-mat.job-no2
                    NO-LOCK
                    BREAK BY job-hdr.frm      DESC
                    BY job-hdr.blank-no DESC:

                IF (job-hdr.frm EQ job-mat.frm AND
                    (job-hdr.blank-no EQ job-mat.blank-no OR
                     job-mat.blank-no EQ 0)) OR
                    LAST(job-hdr.frm) THEN DO:
                    tt_g_iss_Lookup.fgitem = job-hdr.i-no.
                    LEAVE.
                END.
          END.
      END.
   END.

     
    END.
      
/* FIND FIRST tt_g_iss_Lookup NO-LOCK NO-ERROR.
 IF NOT AVAIL tt_g_iss_Lookup THEN do:
        lv-sheet = INT(prmsno).
            
            ASSIGN
                lv-blank = INT(prmbno)
                v-out    = 1.

            create tt_g_iss_Lookup.
                    assign
                        tt_g_iss_Lookup.vfrm     =  lv-sheet 
                        tt_g_iss_Lookup.vblk     =  lv-blank
                        tt_g_iss_Lookup.vout     =  v-out  .
 END.*/

            
END. 
  

/***************************** view*******************/


IF prmAction = "View" then do:


    EMPTY TEMP-TABLE tt-s-num.
    
        CREATE tt-s-num.
        tt-s-num.s-num = INT(prmsno).


    FOR EACH tt-s-num BREAK BY tt-s-num.s-num:
      FIND FIRST job
          WHERE job.company EQ g_company
            AND job.job-no  EQ prmjob
            AND job.job-no2 EQ INT(prmjob2)
          NO-LOCK NO-ERROR.

      FIND FIRST item
          WHERE item.company EQ job.company
            AND item.i-no    EQ prmitem
          NO-LOCK NO-ERROR.

      ASSIGN
       fil_id    = ?
       ll-layout = CAN-DO("1,2,3,4,B,P,R",item.mat-type).

      FIND job-mat WHERE job-mat.company EQ g_company
          AND job-mat.rec_key = prmReckey NO-LOCK NO-ERROR.

          /*FIRST job-mat WHERE RECID(job-mat) EQ item-chg.rec-id NO-LOCK:*/

            ASSIGN
                ld-job-up  = 0
                ld-job-qty = 0.

            FIND FIRST est
                WHERE est.company EQ job.company
                AND est.est-no  EQ job.est-no
                NO-LOCK NO-ERROR.

            IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                FOR EACH job-hdr FIELDS(qty)
                WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                NO-LOCK:

                ld-job-qty = ld-job-qty + job-hdr.qty.
            END.

            ELSE
                FOR EACH job-hdr FIELDS(n-on qty)
                    WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.frm    EQ tt-s-num.s-num
                    NO-LOCK:

                ASSIGN
                    ld-job-qty = ld-job-qty + job-hdr.qty
                    ld-job-up  = ld-job-up + job-hdr.n-on.
                END.

                IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
                    FOR EACH eb FIELDS(num-up)
                    WHERE eb.company EQ est.company
                    AND eb.est-no  EQ est.est-no
                    AND eb.form-no EQ tt-s-num.s-num
                    NO-LOCK:
                    ld-job-up = ld-job-up + eb.num-up.
                    END.

                    IF ll-layout OR ld-job-up EQ 0 THEN ld-job-up = 1.
            


            lv-sheet = INT(prmsno).
        
            IF AVAIL job-mat THEN
                ASSIGN
                lv-blank = job-mat.blank-no
                v-out    = (job-mat.n-up / ld-job-up).
            ELSE
                ASSIGN
                    lv-blank = INT(prmbno)
                    v-out    = 1.
           IF AVAIL job-mat THEN do:
           CREATE tt_g_iss_Lookup.
           ASSIGN
               tt_g_iss_Lookup.rec_key = job-mat.rec_key
               tt_g_iss_Lookup.vfrm     =  lv-sheet
               tt_g_iss_Lookup.vblk     =  lv-blank
               tt_g_iss_Lookup.vout     =  v-out  .
           END.

     
    END.
      
 FIND FIRST tt_g_iss_Lookup NO-LOCK NO-ERROR.
 IF NOT AVAIL tt_g_iss_Lookup THEN do:
        lv-sheet = INT(prmsno).
            
            ASSIGN
                lv-blank = INT(prmbno)
                v-out    = 1.

            create tt_g_iss_Lookup.
                    assign
                        tt_g_iss_Lookup.vfrm     =  lv-sheet 
                        tt_g_iss_Lookup.vblk     =  lv-blank
                        tt_g_iss_Lookup.vout     =  v-out  .
 END.

            
END. 

