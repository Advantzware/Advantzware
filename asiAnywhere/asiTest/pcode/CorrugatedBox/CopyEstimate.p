/*------------------------------------------------------------------------
    File        : CopyEstimate.p
    Purpose     :  Corrugated/Folding Estimate

    Syntax      :

    Description : Return a Dataset of Corrugated Estimates

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCopyEstimate NO-UNDO
    FIELD cCopyEst              AS CHAR FORMAT "x(8)"
    FIELD cCopyCust             AS CHAR FORMAT "x(8)"
    FIELD cCustPart         AS CHAR FORMAT "x(15)"
    FIELD cShipTo           AS CHAR FORMAT "x(8)"
      .

DEFINE DATASET dsCopyEstimate FOR ttCopyEstimate.

    DEFINE INPUT PARAMETER prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromComp          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromEstimate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopyroute         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopydie           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopyplate         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopyfgitem        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopycost          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopynote          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmTocompany         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmToestimate        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmNewcust           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmnewcustpart       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopyItemName        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopyItemDesc           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopyItemDesc2       AS CHAR NO-UNDO.

    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError2             AS CHAR NO-UNDO .
    
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCopyEstimate.

    IF   prmAction        = ?      THEN    prmAction         = "".       
    IF   prmUser          = ?      THEN    prmUser           = "".       
    IF   prmFromComp      = ?      THEN    prmFromComp       = "".       
    IF   prmFromEstimate  = ?      THEN    prmFromEstimate   = "".       
    IF   prmCopyroute     = ?      THEN    prmCopyroute      = "".       
    IF   prmCopydie       = ?      THEN    prmCopydie        = "".       
    IF   prmCopyplate     = ?      THEN    prmCopyplate      = "".       
    IF   prmCopyfgitem    = ?      THEN    prmCopyfgitem     = "".       
    IF   prmCopycost      = ?      THEN    prmCopycost       = "".       
    IF   prmTocompany     = ?      THEN    prmTocompany      = "".       
    IF   prmToestimate    = ?      THEN    prmToestimate     = "".       
    IF   prmNewcust       = ?      THEN    prmNewcust        = "".       
    IF   prmnewcustpart   = ?      THEN    prmnewcustpart    = "".       
    
DEF VAR prmComp AS CHAR NO-UNDO.

    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF VAR lv-cust-no LIKE eb.cust-no NO-UNDO.
DEF VAR cMissingTerms AS CHAR NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-msg AS CHAR NO-UNDO.
DEF BUFFER b-eb FOR eb. 
DEF VAR to_est AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
{sys/inc/var.i new shared}

assign
 cocode = prmComp
 locode = "MAIN" .

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


IF prmAction = "CheckValidate" THEN do:

    FIND FIRST company WHERE company.company EQ prmFromComp NO-LOCK NO-ERROR.
    IF NOT AVAIL company THEN
      lv-msg = "Invalid Company, try help".

    IF lv-msg EQ "" AND NOT CAN-FIND(FIRST eb WHERE eb.company EQ prmFromComp) THEN
      lv-msg = "Sorry, no estimates exist for this company".
    
    IF  lv-msg <> "" THEN DO:
        ASSIGN cError = lv-msg .
        RETURN.
    END.

  v-msg = "".
    FIND FIRST eb
        WHERE eb.company EQ prmFromComp
          AND eb.est-no  EQ FILL(" ", 8 - LENGTH(TRIM(prmFromEstimate))) +
                            TRIM(prmFromEstimate)
          AND eb.form-no NE 0
        NO-LOCK NO-ERROR.

    IF v-msg EQ "" THEN
      IF NOT AVAIL eb THEN v-msg = "Invalid Estimate, try help".

    IF v-msg EQ "" THEN
      IF AVAIL eb                                                   AND
         (eb.est-type EQ 2 OR eb.est-type EQ 5 OR eb.est-type EQ 6) THEN
      FOR EACH b-eb
          WHERE b-eb.company EQ eb.company
            AND b-eb.est-no  EQ eb.est-no
            AND ROWID(b-eb)  NE ROWID(eb)
          NO-LOCK BY b-eb.form-no:
        IF b-eb.form-no NE 0 THEN v-msg = "Set estimate has no header".
        LEAVE.
      END.

    IF v-msg NE "" THEN DO:
      cError =  TRIM(v-msg) .
      RETURN .
    END.

    IF AVAIL eb THEN
      IF LOOKUP(eb.cust-no, custcount) = 0 THEN DO:
        cError =  "Estimate Customer Not Assign to Company, try help..." .
      RETURN .
      END.

  FIND FIRST company WHERE company.company EQ prmTocompany NO-LOCK NO-ERROR.
    IF NOT AVAIL company THEN DO:
        cError = "Invalid To Company, try help".
        RETURN .
    END.
   
  
  
  FIND FIRST cust WHERE cust.company EQ prmTocompany
      AND cust.cust-no EQ prmNewcust NO-LOCK NO-ERROR.
  
  IF NOT AVAIL cust THEN DO:
      cError =  "Invalid Customer, try help..." .
      RETURN .
    END.


    IF prmnewcustpart = "" THEN DO:
        cError =  "Invalid Customer Part, try help..." .
      RETURN .
    END.
  
   
   FOR EACH quotehd WHERE quotehd.company EQ prmFromComp
     AND quotehd.loc EQ locode
     AND quotehd.est-no EQ FILL(" ",8 - LENGTH(TRIM(prmFromEstimate))) + TRIM(prmFromEstimate) NO-LOCK:
     IF NOT CAN-FIND(FIRST terms WHERE terms.company EQ prmTocompany
                                   AND terms.t-code EQ quotehd.terms)
                     THEN DO:
        cMissingTerms = quotehd.terms.    
        LEAVE.
     END.
   END.

   IF cMissingTerms GT "" THEN DO:
      cError =  "Terms code " + cMissingTerms + 
        ", used on a related quote but is not defined in company "
        +  prmTocompany .
      RETURN .
    END.
  
                   
 END.  /* end of valdat */

 IF prmAction = "RunProcess" THEN do: 

     DO TRANSACTION:

    REPEAT:
    
    FIND FIRST ce-ctrl
        WHERE ce-ctrl.company EQ prmTocompany
          AND ce-ctrl.loc     EQ locode
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

    IF AVAIL ce-ctrl THEN
    DO:
    ASSIGN
     to_est              = STRING(ce-ctrl.e-num + 1,">>>>>>>>")
     /*to_est:SCREEN-VALUE = to_est*/
     ce-ctrl.e-num       = ce-ctrl.e-num + 1.
    FIND CURRENT ce-ctrl NO-LOCK.
    LEAVE.
    END.
    END.
  END. /* do for ce-ctrl */

  CREATE ttCopyEstimate .
  ASSIGN ttCopyEstimate.cCopyEst = to_est .

  RELEASE ce-ctrl.
  
  /*MESSAGE "Are you sure you want to copy this estimate?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.*/

   RUN run-process.

 END.



    IF prmAction = "Select" THEN DO:

    /*FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp  NO-LOCK ,
     EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom NO-LOCK,
      EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  
                AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank NO-LOCK:
    
         vQty = display-combo-qty ().           
        CREATE ttCorrugateEstimate.
        ASSIGN 
            ttCorrugateEstimate.vEst           = est.est-no
            ttCorrugateEstimate.vCust          = eb.cust-no
            ttCorrugateEstimate.vCustPart      = eb.part-no
            ttCorrugateEstimate.vShipTo        = eb.ship-id
            ttCorrugateEstimate.vItemName      = eb.part-dscr1*/

    END.

PROCEDURE run-process :
/* ---------------------------------------------------- ce/cp-est.p 10/94 gb */
/* copy estimate & standards files                                           */
/* -------------------------------------------------------------------------- */
  DEF BUFFER kest  FOR est.
  DEF BUFFER kqty  FOR est-qty.
  DEF BUFFER kef   FOR ef.
  DEF BUFFER keb   FOR eb.
  DEF BUFFER kprep FOR est-prep.
  DEF BUFFER kop   FOR est-op.
  DEF BUFFER kinst FOR est-inst.
  DEF BUFFER kflm  FOR est-flm.
  DEF BUFFER kref  FOR reftable.
  DEF BUFFER kbdh  FOR box-design-hdr.
  DEF BUFFER kbdl  FOR box-design-line.
  DEF BUFFER knsh  FOR ef-nsh.
  DEF BUFFER kei   FOR e-itemfg.
  DEF BUFFER keiv  FOR e-itemfg-vend.
  DEF BUFFER knot  FOR notes.

  DEF VAR txno AS INT.

  DEF VAR fcom       LIKE company.company.
  DEF VAR fest       LIKE est.est-no.
  DEF VAR fest-mr    AS   LOG INIT NO.
  DEF VAR tcom       LIKE company.company.
  DEF VAR test       LIKE est.est-no.
  DEF VAR ls-key     AS   cha FORMAT "x(20)" NO-UNDO.
  DEF VAR li         AS   INT NO-UNDO.
  DEF VAR lj         AS   INT NO-UNDO.
  DEF VAR lv-part-no LIKE eb.part-no NO-UNDO.
  

  SESSION:SET-WAIT-STATE("general").

  assign
   fcom     = prmFromComp
   fest     = prmFromEstimate
   fest-mr  = IF prmCopyroute = "Yes" THEN TRUE ELSE false
   tcom     = prmTocompany
   test     = to_est .

  find first est
      where est.company eq fcom
        and est.est-no  eq FILL(" ",8 - LENGTH(TRIM(fest))) + TRIM(fest)
      no-lock no-error.

  ls-key = string(today,"99999999") +
                  string(next-value(rec_key_seq,asi),"99999999").
  create rec_key.
  assign rec_key.rec_key = ls-key
         rec_key.table_name = "EST".

  create kest.
  buffer-copy est to kest
  assign
   kest.company  = tcom
   kest.est-no   = FILL(" ",8 - LENGTH(TRIM(test))) + TRIM(test)
   kest.e-num    = INT(est.est-no)
   kest.ord-no   = 0
   kest.ord-date = ?
   kest.est-date = today
   kest.rec_key = ls-key   .

  DISABLE TRIGGERS FOR LOAD OF keb.

  for each eb
      where eb.company eq est.company
        and eb.est-no  eq est.est-no
      NO-LOCK
      BY eb.form-no
      BY eb.blank-no:

    IF eb.form-no EQ 0 THEN lv-part-no = eb.part-no.
                       ELSE li = li + 1.

    create keb.
    buffer-copy eb except rec_key die-no plate-no stock-no ord-no to keb
    assign
     keb.company  = kest.company
     keb.est-no   = kest.est-no
     keb.est-int  = int(kest.est-no)
     keb.cust-no  = IF prmNewcust NE lv-cust-no THEN prmNewcust ELSE eb.cust-no
     keb.part-no  = eb.part-no
     keb.stock-no = IF prmCopyfgitem = "Yes" THEN eb.stock-no ELSE keb.stock-no
     keb.part-dscr1 = IF prmCopyItemName = "Yes"  THEN eb.part-dscr1 ELSE keb.part-dscr1
     keb.part-dscr2 = IF prmCopyItemDesc  = "Yes" THEN eb.part-dscr2 ELSE keb.part-dscr2.

    IF prmnewcustpart NE lv-part-no                  AND
       (eb.est-type LE 2 OR eb.est-type GE 5) AND
       eb.est-type NE 8                       THEN DO:
      keb.part-no = prmnewcustpart.

      IF (eb.est-type EQ 2 OR eb.est-type EQ 6) AND
         eb.form-no NE 0 AND est.form-qty GE 2 THEN
        keb.part-no = TRIM(SUBSTR(keb.part-no,1,12)) + "-" + TRIM(STRING(li,">9")).
    END.
    IF prmNewcust NE lv-cust-no THEN DO:
       find cust where cust.company = cocode and
                       cust.cust-no = keb.cust-no
                 no-lock no-error.
       keb.sman = if avail cust then cust.sman else "".
       find sman where sman.company = cocode
                   AND sman.sman = keb.sman
                 no-lock no-error.
       assign keb.comm = if avail sman then sman.scomm else 0.
    END.
    IF prmCopycost = "Yes" THEN
    FOR EACH e-itemfg-vend NO-LOCK
        WHERE e-itemfg-vend.company  EQ eb.company
          AND e-itemfg-vend.est-no   EQ eb.est-no
          AND e-itemfg-vend.form-no  EQ eb.form-no
          AND e-itemfg-vend.blank-no EQ eb.blank-no
        BREAK BY e-itemfg-vend.vend-no:

      IF FIRST(e-itemfg-vend.vend-no) THEN DO:
        FIND FIRST reftable
            WHERE reftable.reftable EQ "e-itemfg-vend.std-uom"
              AND reftable.company  EQ e-itemfg-vend.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ e-itemfg-vend.est-no
              AND reftable.val[1]   EQ e-itemfg-vend.form-no
              AND reftable.val[2]   EQ e-itemfg-vend.blank-no
            NO-LOCK NO-ERROR.

        IF AVAIL reftable THEN DO:
          CREATE kref.
          BUFFER-COPY reftable EXCEPT rec_key TO kref
          ASSIGN
           kref.company = keb.company
           kref.code    = keb.est-no.
        END.
/*          task 10231204 - Was causing an error message and doesn't make sense */
/*                                                                              */
/*         IF NOT CAN-FIND(FIRST kei                                            */
/*                         WHERE kei.company EQ keb.company                     */
/*                           AND kei.i-no    EQ keb.stock-no) THEN DO:          */
/*           CREATE kei.                                                        */
/*           BUFFER-COPY e-item-vend EXCEPT rec_key TO kei                      */
/*           ASSIGN                                                             */
/*            kei.company = keb.company                                         */
/*            kei.i-no    = keb.stock-no                                        */
/*            kei.std-uom = IF AVAIL reftable THEN reftable.code2 ELSE "EA".    */
/*         END.                                                                 */
/*                                                                              */
      END.

      CREATE keiv.
      BUFFER-COPY e-itemfg-vend EXCEPT rec_key TO keiv
      ASSIGN
       keiv.company = keb.company
       keiv.est-no  = keb.est-no
       keiv.i-no    = keb.stock-no.
    END.

    IF prmNewcust NE eb.cust-no THEN
    FOR EACH shipto
        WHERE shipto.company EQ keb.company
          AND shipto.cust-no EQ keb.cust-no
        NO-LOCK
        BREAK BY shipto.ship-id:

      IF shipto.ship-id EQ shipto.cust-no OR LAST(shipto.ship-id) THEN DO:
        ASSIGN
         keb.ship-id      = shipto.ship-id
         keb.carrier      = shipto.carrier
         keb.ship-name    = shipto.ship-name
         keb.ship-addr[1] = shipto.ship-addr[1]
         keb.ship-addr[2] = shipto.ship-addr[2]
         keb.ship-city    = shipto.ship-city
         keb.ship-state   = shipto.ship-state
         keb.ship-zip     = shipto.ship-zip.
        LEAVE.
      END.
    END.

    if prmCopydie = "Yes"  then keb.die-no   = eb.die-no.
    if prmCopyplate = "yes" then keb.plate-no = eb.plate-no.

    {sys/inc/box-del.i keb}

    IF est.est-type LT 5 THEN
    DO:

    FIND FIRST reftable
          WHERE reftable.reftable EQ "cedepth"
            AND reftable.company  EQ eb.company
            AND reftable.loc      EQ eb.est-no
            AND reftable.code     EQ STRING(eb.form-no,"9999999999")
            AND reftable.code2    EQ STRING(eb.blank-no,"9999999999")
          NO-LOCK NO-ERROR.

    IF AVAIL reftable THEN
    DO:
       FIND FIRST kref
            WHERE kref.reftable EQ reftable.reftable
              AND kref.company  EQ keb.company
              AND kref.loc      EQ keb.est-no
              AND kref.code     EQ STRING(keb.form-no,"9999999999")
              AND kref.code2    EQ STRING(keb.blank-no,"9999999999")
            NO-ERROR.

       IF NOT AVAIL kref THEN
       DO:
           CREATE kref.
           ASSIGN
           kref.reftable = reftable.reftable
           kref.company  = keb.company
           kref.loc      = keb.est-no
           kref.code     = STRING(keb.form-no,"9999999999")
           kref.code2    = STRING(keb.blank-no,"9999999999").
       END.

       ASSIGN
          kref.val[1] = reftable.val[1]
          kref.val[2] = reftable.val[2].

       RELEASE reftable.
    END.
    END.

    DO lj = 1 TO 2:
      FOR EACH reftable
          WHERE reftable.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING(lj - 1,">"))
            AND reftable.company  EQ eb.company
            AND reftable.loc      EQ eb.est-no
            AND reftable.code     EQ STRING(eb.form-no,"9999999999")
            AND reftable.code2    EQ STRING(eb.blank-no,"9999999999")
          NO-LOCK:

        FIND FIRST kref
            WHERE kref.reftable EQ reftable.reftable
              AND kref.company  EQ keb.company
              AND kref.loc      EQ keb.est-no
              AND kref.code     EQ STRING(keb.form-no,"9999999999")
              AND kref.code2    EQ STRING(keb.blank-no,"9999999999")
            NO-ERROR.
        IF NOT AVAIL kref THEN DO:
          CREATE kref.
          ASSIGN
           kref.reftable = reftable.reftable
           kref.company  = keb.company
           kref.loc      = keb.est-no
           kref.code     = STRING(keb.form-no,"9999999999")
           kref.code2    = STRING(keb.blank-no,"9999999999").
        END.

        DO li = 1 TO 12:
          kref.val[li] = reftable.val[li].
        END.

        kref.dscr = reftable.dscr.

        LEAVE.
      END.
    END.
  end.

  for each est-qty
      where est-qty.company eq est.company
        and est-qty.est-no  EQ est.est-no
      NO-LOCK:

    create kqty.
    buffer-copy est-qty except rec_key to kqty
    assign
     kqty.company = kest.company
     kqty.est-no  = kest.est-no.
  end.

  for each ef-nsh
      where ef-nsh.company eq est.company
        and ef-nsh.est-no  EQ est.est-no
      NO-LOCK:
    create knsh.
    buffer-copy ef-nsh except rec_key to knsh
    assign
     knsh.company = kest.company
     knsh.est-no  = kest.est-no.
  end.
    
  for each ef
      where ef.company eq est.company
        and ef.est-no  EQ est.est-no
      NO-LOCK:
    create kef.
    buffer-copy ef EXCEPT rec_key to kef
    assign
     kef.company = kest.company
     kef.est-no  = kest.est-no.
   
    if not fest-mr then kef.op-lock = no.
        
    IF (est.est-type EQ 3 OR
        est.est-type EQ 4 OR
        est.est-type EQ 7 OR
        est.est-type EQ 8)                           AND
       CAN-FIND(FIRST est-flm
                WHERE est-flm.company EQ ef.company
                  AND est-flm.est-no  EQ ef.est-no
                  AND est-flm.snum    EQ ef.form-no) THEN
      ASSIGN
       kef.leaf      = ""
       kef.leaf-dscr = ""
       kef.leaf-bnum = 0
       kef.leaf-w    = 0
       kef.leaf-l    = 0.

    for each reftable /*{ce/est-mrpl.w ef}*/ where reftable.reftable eq "EST-MISC"
  and reftable.company  eq ef.company
  and reftable.loc      eq ef.loc
  and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99") no-lock:
      create kref.
      buffer-copy reftable except rec_key to kref
      assign
       kref.company = kef.company
       kref.code    = trim(kef.est-no) + string(kef.form-no,"/99").
    end.
       
    for each reftable
        where reftable.reftable eq "EST-MISC"
          and reftable.company  eq ef.company
          and reftable.loc      eq ef.loc
          and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
        no-lock:
      create kref.
      buffer-copy reftable except rec_key to kref
      assign
       kref.company = kef.company
       kref.code    = trim(kef.est-no) + string(kef.form-no,"/99").  
    end.
  end.

  for each est-prep
      where est-prep.company eq est.company
        and est-prep.est-no  EQ est.est-no
      NO-LOCK:
    create kprep.
    buffer-copy est-prep except rec_key to kprep
    assign
     kprep.company = kest.company
     kprep.est-no  = kest.est-no.
  end.

  if fest-mr then
  for each est-op
       where est-op.company eq est.company
         and est-op.est-no  eq est.est-no
         and est-op.line    lt 500
      NO-LOCK:
    create kop.
    buffer-copy est-op except rec_key to kop
    assign
     kop.company = kest.company
     kop.est-no  = kest.est-no.
  end.

  for each est-inst
      where est-inst.company eq est.company
        and est-inst.est-no  EQ est.est-no
      NO-LOCK:
    create kinst.
    buffer-copy est-inst except rec_key to kinst
    assign
     kinst.company = kest.company
     kinst.est-no  = kest.est-no.
  end.

  IF est.est-type EQ 3 OR
     est.est-type EQ 4 OR
     est.est-type EQ 7 OR
     est.est-type EQ 8 THEN
  for each est-flm
      where est-flm.company eq est.company
        and est-flm.est-no  EQ est.est-no
        AND NOT CAN-FIND(FIRST kflm
                         WHERE kflm.company EQ kest.company
                           AND kflm.est-no  EQ kest.est-no
                           AND kflm.eqty    EQ est-flm.eqty
                           AND kflm.line    EQ est-flm.line)
      NO-LOCK
      BREAK BY est-flm.line:

    create kflm.
    buffer-copy est-flm except rec_key to kflm
    assign
     kflm.company = kest.company
     kflm.est-no  = kest.est-no.
  end.

  for each box-design-hdr
      where box-design-hdr.design-no eq 0
        and box-design-hdr.company   eq est.company
        and box-design-hdr.est-no    eq est.est-no
      no-lock:

    IF NOT CAN-FIND(FIRST kbdh WHERE
       kbdh.design-no = 0 AND
       kbdh.company EQ kest.company AND
       kbdh.est-no EQ kest.est-no AND
       kbdh.eqty EQ box-design-hdr.eqty AND
       kbdh.form-no EQ box-design-hdr.form-no AND
       kbdh.blank-no EQ box-design-hdr.blank-no) THEN
       DO:
          create kbdh.
          buffer-copy box-design-hdr except rec_key to kbdh
          assign
             kbdh.design-no = 0
             kbdh.company   = kest.company
             kbdh.est-no    = kest.est-no.
       END.

    for each box-design-line of box-design-hdr no-lock:

      IF NOT CAN-FIND(FIRST kbdl WHERE
         kbdl.design-no EQ 0 AND
         kbdl.company EQ kest.company AND
         kbdl.est-no EQ kest.est-no AND
         kbdl.eqty EQ box-design-line.eqty AND
         kbdl.form-no EQ box-design-line.form-no AND
         kbdl.blank-no EQ box-design-line.blank-no AND
         kbdl.line-no  EQ box-design-line.line-no) THEN
         DO:
            create kbdl.
            buffer-copy box-design-line except rec_key to kbdl
            assign
               kbdl.design-no = 0
               kbdl.company   = kest.company
               kbdl.est-no    = kest.est-no.
         END.
    end.
  end.

  IF prmCopynote = "Yes" THEN
  FOR EACH notes where notes.rec_key eq est.rec_key NO-LOCK:
    CREATE knot.
    BUFFER-COPY notes TO knot
    ASSIGN knot.rec_key = ls-key.
  END.

  /*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).*/

  /*session:set-wait-state("").*/

  cError2 =  " Process Is Completed.".
 
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.
