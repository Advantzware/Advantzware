


/*------------------------------------------------------------------------
    File        : GetFoldQtyTandem.p
    Purpose     : Get Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


    DEFINE TEMP-TABLE ttSharedFoldTandem NO-UNDO 
        FIELD seqno             AS INT  
        FIELD qty1              AS INT 
        FIELD getqty            AS INT  
        FIELD roid              AS CHAR
        FIELD uymld             AS CHAR
        FIELD GsaMat            AS DECIMAL
        FIELD GsaLab            AS DECIMAL
        FIELD GsaWar            AS DECIMAL
        FIELD GsaBrd            AS DECIMAL 
        FIELD GsaMonth          AS INT    
       .       

    DEFINE DATASET dsGetFoldQtyTandem FOR ttSharedFoldTandem .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmDoGsa      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoMr       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoSpeed    AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmEstimate   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmForm       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBlank      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmLvoverride   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmVendor     AS CHARACTER NO-UNDO.


    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGetFoldQtyTandem.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.  

    IF prmUser   = ?  THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".    
    IF prmDoGsa = ?    THEN ASSIGN prmDoGsa = "no".
    IF prmDoMr = ?     THEN ASSIGN prmDoMr = "no".
    IF prmDoSpeed = ?  THEN ASSIGN prmDoSpeed = "no".      
    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".
    IF prmForm = ?  THEN ASSIGN prmForm = 0.
    IF prmBlank = ?  THEN ASSIGN prmBlank = 0.
    IF prmLvoverride = ? THEN ASSIGN prmLvoverride = "Yes".                
    IF prmVendor     = ? THEN ASSIGN prmVendor     = "".


    {ce/print4.i "new shared" "new shared"}
    {ce/print42.i "new shared"}

    {sys/inc/var.i "new shared"}



    DEF BUFFER probe-ref FOR reftable.
    DEF BUFFER b-probemk FOR reftable.

    DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
    DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.   

    def new shared var k_frac as dec init "6.25" no-undo.
    def new shared var day_str as cha form "x(10)" no-undo.
    def new shared var tim_str as cha form "x(8)" no-undo.
    def new shared var maxpage as int form ">9" no-undo.
    def new shared var tmp-dir as cha no-undo.
    def new shared var col-norm as cha init "White/Blue" no-undo. 
    def new shared var qty as int NO-UNDO.
    def new shared var v-do-gsa like do-gsa no-undo.
    def new shared buffer xop for est-op.
    DEF BUFFER reftable-fm FOR reftable.
    DEF BUFFER reftable-fold-pct FOR reftable.

    def new shared var v-qtty like qtty no-undo.
    def new shared var v-drop-rc as log no-undo.
    def new shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
    def new shared var v-prep-lab like tprep-lab no-undo.
    

    def new shared workfile w-form
        field form-no like ef.form-no
        field min-msf as   log init no.


    def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.

    DEF NEW shared buffer xest for est.
    DEF NEW shared buffer xef  for ef.
    DEF NEW shared buffer xeb  for eb.
    
    DEF BUFFER bf-est FOR est.
    DEF BUFFER bf-eb FOR eb.
    DEF BUFFER bf-ef FOR ef.
           
    DEF VAR CALL_id AS RECID NO-UNDO.    
    def var v-vend-no   like e-item-vend.vend-no init "" NO-UNDO.
    DEF VAR ld-fg-amt AS DEC NO-UNDO.
    DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
    DEF VAR lv-eqty LIKE est-op.qty NO-UNDO.

    def var lv-brd-l           like eb.len no-undo.
    def var lv-brd-w           like lv-brd-l no-undo.
    def var lv-brd-sq          as dec format ">>>>9.9<<<<" no-undo.
    def var lv-brd-sf          as dec format ">>>>>9.9<<"  no-undo.
    def var lv-brd-wu          like lv-brd-sq no-undo.

    def buffer xcar for car.

    def new shared var v-summ as log init NO NO-UNDO.
    def new shared var fr-tot-pre as dec.

    def var v-layout  as log NO-UNDO.
    def var v-blk-wt  as dec NO-UNDO.
    def var v-avg-com as log NO-UNDO.
    def var v-avg-tan as log NO-UNDO.
    def var v-mat     as dec NO-UNDO.
    def var v-lab     as dec NO-UNDO.
    def var v-foh     as dec NO-UNDO.
    def var v-voh     as dec NO-UNDO.
    def var v-msf     as dec NO-UNDO.
    DEF VAR lv-error AS LOG NO-UNDO.
    DEF VAR ls-outfile AS cha NO-UNDO.
    DEF VAR ls-probetime AS cha NO-UNDO.
    DEF VAR v-line LIKE probe.line no-undo.
    DEF VAR ll-tandem AS LOG NO-UNDO.

    DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
    DEF VAR lv-format AS CHAR INIT ">>>>9.9<<<<" NO-UNDO.
    DEF VAR ld-wid AS DEC NO-UNDO.
    DEF VAR ld-len AS DEC NO-UNDO.
    DEF VAR ld-dep AS DEC NO-UNDO.
    def var v-module as char format "x(60)" no-undo.
    def var v-brd-cost as dec no-undo.
    DEF VAR lv-override AS LOG NO-UNDO.
    DEF VAR ld-fg-rate AS DEC NO-UNDO.

    DEF NEW SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.
    


    DEF VAR prmComp AS CHAR NO-UNDO.
    FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

    prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   
    assign
        cocode = prmComp
        locode = usercomp.loc
        g_company = prmComp
    . 

    ASSIGN 
        locode = "MAIN"
        g_loc = locode
        .


    vprint = YES.

    IF prmAction = "doCalc" THEN DO:
        FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
        FIND FIRST loc WHERE loc.loc EQ locode NO-LOCK NO-ERROR.

        FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
        FIND FIRST ef WHERE ef.est-no = est.est-no AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
        FIND FIRST eb WHERE eb.est-no = est.est-no AND eb.company = prmComp AND eb.form-no = prmForm AND eb.blank-no = prmBlank NO-LOCK NO-ERROR. 


        find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
        find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
        find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

        vprint = yes.       
  
        FOR EACH mclean:
            DELETE mclean.
        END.


        /*{cec/get-vend.i}   */ /* get vendor number */
        ASSIGN
       v-vend-no  = prmVendor .          

        IF v-vend-no EQ "&nbsp;" OR v-vend-no EQ ? THEN DO:
            ASSIGN
                v-vend-no = "".
        END.

        find first xef where xef.company = xest.company 
                 AND xef.est-no = xest.est-no.              
        find first xeb where xeb.company = xest.company 
                 AND xeb.est-no   eq xest.est-no
                 and xeb.form-no eq xef.form-no.
        find first xop where xop.company = xest.company 
                 AND xop.est-no    eq xest.est-no
                 and xop.op-speed eq 0
            no-lock no-error.

        RUN ce/com/istandem.p (ROWID(xest), OUTPUT ll-tandem).     


        save-lock = xef.op-lock.

        DO TRANSACTION:
            {est/recalc-mr.i xest}
            FIND CURRENT recalc-mr NO-LOCK.

            ASSIGN
                do-speed = xest.recalc
                do-mr    = recalc-mr.val[1] EQ 1
                do-gsa   = xest.override.

            {sys/inc/cerun.i F}
            vmclean = LOOKUP(cerunf,"McLean,HOP") GT 0.

            {ce/msfcalc.i}

            FIND FIRST sys-ctrl
                WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ "CEPg2"
                NO-LOCK NO-ERROR.
                /*IF NOT AVAIL sys-ctrl THEN DO:
                    CREATE sys-ctrl.
                    ASSIGN
                        sys-ctrl.company = cocode
                        sys-ctrl.name    = "CEPg2"
                        sys-ctrl.descrip = "Reverse W & L Labels for press, die, & # Up on Estimate".
                        MESSAGE sys-ctrl.descrip
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE sys-ctrl.log-fld.
            END.
            */
                
            v-layout = sys-ctrl.log-fld.

            FIND FIRST sys-ctrl
                WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ "COMBCOST"
                NO-LOCK NO-ERROR.
            /*IF NOT AVAIL sys-ctrl THEN DO:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = cocode
                sys-ctrl.name    = "COMBCOST"
                sys-ctrl.descrip = "Average Cost for Combination Items?" 
                sys-ctrl.log-fld = NO.
                MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
            END.*/
            ASSIGN
                v-avg-com = sys-ctrl.log-fld
                v-avg-tan = sys-ctrl.int-fld EQ 0.
        END.


        EMPTY TEMP-TABLE tt-rel.

        if vprint then do:
            /*RUN ce/com/selwhif.w (INPUT-OUTPUT do-speed, INPUT-OUTPUT do-mr,
                        INPUT-OUTPUT do-gsa, INPUT-OUTPUT v-summ,
                        INPUT NO, OUTPUT lv-error) NO-ERROR.

            if lv-error then return error.
            */
            ASSIGN
                do-speed    = LOGICAL(prmDoSpeed)
                do-mr       = LOGICAL(prmDoMr)
                do-gsa      = LOGICAL(prmDoGsa)
                v-summ      = NO
                lv-override = LOGICAL(prmLvoverride)                             
                .

            FOR EACH eb NO-LOCK
                WHERE eb.company EQ xest.company
                AND eb.est-no  EQ xest.est-no,
                    FIRST reftable NO-LOCK
                    WHERE reftable.reftable EQ "ce/com/selwhif1.w"
                    AND reftable.company  EQ eb.company
                    AND reftable.loc      EQ eb.est-no
                    AND reftable.code     EQ STRING(eb.form-no,"9999999999")
                    AND reftable.code2    EQ STRING(eb.blank-no,"9999999999"):
                CREATE tt-rel. 
                BUFFER-COPY reftable TO tt-rel.
            END.

            IF lv-override THEN
                for each probe where probe.company = xest.company and
                       probe.est-no = xest.est-no:
                delete probe.                 
            end.

        end.
        
        DO TRANSACTION:
            {est/op-lock.i xest}
            FIND bf-est WHERE RECID(bf-est) EQ RECID(xest).
            FIND CURRENT recalc-mr.
            ASSIGN
                bf-est.recalc    = do-speed
                recalc-mr.val[1] = INT(do-mr)
                bf-est.override  = do-gsa
                op-lock.val[1]   = INT(bf-est.recalc)
                op-lock.val[2]   = recalc-mr.val[1].
            FIND CURRENT bf-est NO-LOCK.
            FIND CURRENT recalc-mr NO-LOCK.
            FIND CURRENT op-lock NO-LOCK.
            FIND xest WHERE RECID(xest) EQ RECID(bf-est).                         
        END.

        FORM day_str v-module tim_str to 79
            SKIP(1)
            "Combination Est#" xest.est-no FORMAT "x(8)"
            "UserID:" xest.updated-id
            "Prober:" probe.probe-user
            SKIP(1)
            with frame hdr page-top STREAM-IO width 80 no-labels no-box.

        FORM "Salesman:" kli.sman kli.sname SKIP
        "Cust:" kli.cust-no
             kli.cust-add[1] FORMAT "x(29)" TO 44
        "Ship:" kli.ship-add[1] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[2] FORMAT "x(29)" TO 44
             kli.ship-add[2] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[3] FORMAT "x(29)" TO 44
             kli.ship-add[3] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[4] FORMAT "x(29)" TO 44
             kli.ship-add[4] FORMAT "x(29)" TO 80
             SKIP
        WITH STREAM-IO NO-LABELS NO-BOX DOWN WIDTH 80 FRAME kli.
        if retry then output close.

        qty = 0.
        for each xef
            where xef.company eq xest.company
            and xef.est-no  eq xest.est-no
            no-lock,
                each xeb
                where xeb.company eq xef.company
                and xeb.est-no  eq xef.est-no
                and xeb.form-no eq xef.form-no
                no-lock:
                    qty = qty + if xeb.yrprice /*AND NOT ll-tandem*/ then xeb.yld-qty else xeb.bl-qty.
        end.

        {est/probeset.i qty 0}

        ASSIGN
            outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
            outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
            outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99").

        output to value(outfile1).
        

        find first ce-ctrl where (ce-ctrl.company = cocode and ce-ctrl.loc = locode) no-lock no-error.
        assign
            ctrl[1]  = ce-ctrl.whse-mrkup / 100
            ctrl[2]  = ce-ctrl.hand-pct / 100
            ctrl[3]  = ce-ctrl.rm-rate
            ctrl[4]  = ce-ctrl.spec-%[1]
            ctrl[5]  = int(ce-ctrl.comm-add)
            ctrl[6]  = int(ce-ctrl.shp-add)
            ctrl[7]  = int(ce-ctrl.sho-labor)
            ctrl[8]  = int(ce-ctrl.trunc-99)
            ctrl[11] = ce-ctrl.spec-%[2]
            ctrl[12] = ce-ctrl.spec-%[3]
            ctrl[13] = int(ce-ctrl.spec-add[1])
            ctrl[14] = int(ce-ctrl.spec-add[2])
            ctrl[15] = int(ce-ctrl.spec-add[3])
            ctrl[16] = int(ce-ctrl.spec-add[6])
            ctrl[17] = int(ce-ctrl.spec-add[7])
            ctrl[18] = int(ce-ctrl.spec-add[8]).

            FIND FIRST reftable-fold-pct
                WHERE reftable-fold-pct.reftable EQ "ce-ctrl.fold-pct"
                AND reftable-fold-pct.company  EQ ce-ctrl.company
                AND reftable-fold-pct.loc      EQ ce-ctrl.loc
            NO-LOCK NO-ERROR.

            IF AVAIL reftable-fold-pct THEN
                ctrl[19] = reftable-fold-pct.val[1].

            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "ce-ctrl.fg-rate-farm"
                AND reftable.company  EQ ce-ctrl.company
                AND reftable.loc      EQ ce-ctrl.loc
            NO-ERROR.  
            fg-rate-f = IF AVAIL reftable THEN reftable.val[1] ELSE 0.

            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "ce-ctrl.rm-rate-farm"
                AND reftable.company  EQ ce-ctrl.company
                AND reftable.loc      EQ ce-ctrl.loc
            NO-ERROR.  
            rm-rate-f = IF AVAIL reftable THEN reftable.val[1] ELSE 0.

            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "ce-ctrl.hand-pct-farm"
                AND reftable.company  EQ ce-ctrl.company
                AND reftable.loc      EQ ce-ctrl.loc
            NO-ERROR.  
            hand-pct-f = (IF AVAIL reftable THEN reftable.val[1] ELSE 0) / 100.


            DO TRANSACTION:
                FOR each est-op
                  WHERE est-op.company EQ xest.company 
                        AND est-op.est-no  EQ xest.est-no
                        AND est-op.line    GT 500:
                    DELETE est-op.
                END.
                FOR EACH est-op
                  WHERE est-op.company EQ xest.company
                        AND est-op.est-no  EQ xest.est-no
                        AND est-op.line    LT 500
                        BY est-op.qty:
                    lv-eqty = est-op.qty.
                    LEAVE.
                END.
                FOR EACH est-op
                  WHERE est-op.company EQ xest.company 
                        AND est-op.est-no  EQ xest.est-no
                        AND est-op.qty     EQ lv-eqty
                        AND est-op.line    LT 500:
                    CREATE xop.
                    BUFFER-COPY est-op EXCEPT rec_key TO xop.
                    xop.line = est-op.line + 500.
                END.
            END.

            for each kli:
                delete kli.
            end.

            for each ink:
                delete ink.
            end.

            for each flm:
                delete flm.
            end.

            for each cas:
                delete cas.
            end.

            for each car:
                delete car.
            end.

            for each blk:
                delete blk.
            end.

            for each xjob:
                delete xjob.
            end.


            for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no:

   xxx = 0.
   for each xeb where xeb.company = xest.company
               AND xeb.est-no eq xest.est-no and xeb.form-no = xef.form-no
       BY xeb.blank-no:
      find first kli where kli.cust-no = xeb.cust-no no-error.
      if not avail kli then do:
         find first sman   where   sman.sman    = xeb.sman no-lock no-error.
         find first cust   where   cust.company = cocode and
                                   cust.cust-no = xeb.cust-no no-lock no-error.
         find first shipto where shipto.company = cust.company and
                                 shipto.cust-no = cust.cust-no and
                                 shipto.ship-id = xeb.ship-id no-lock no-error.
         create kli.
         if avail sman then assign kli.sman    = sman.sman
                                     kli.sname   = sman.sname.
         if xeb.cust-no ne "Temp" then assign
         kli.cust-no = xeb.cust-no
         kli.cust-add[1] = cust.name
         kli.cust-add[2] = cust.addr[1]
         kli.cust-add[3] = cust.addr[2]
         kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
         else assign
         kli.cust-no = xeb.cust-no
         kli.cust-add[1] = xeb.ship-name
         kli.cust-add[2] = xeb.ship-addr[1]
         kli.cust-add[3] = xeb.ship-addr[2]
         kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                           xeb.ship-zip.

         if kli.cust-add[3] = "" then assign
            kli.cust-add[3] = kli.cust-add[4] kli.cust-add[4] = "".
         if kli.cust-add[2] = "" then assign
            kli.cust-add[2] = kli.cust-add[3] kli.cust-add[3] = kli.cust-add[4]
            kli.cust-add[4] = "".
         assign
         kli.ship-add[1] = shipto.ship-name
         kli.ship-add[2] = shipto.ship-addr[1]
         kli.ship-add[3] = shipto.ship-addr[2]
         kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                                         " " + shipto.ship-zip.
         if kli.ship-add[3] = "" then
         assign kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
         if kli.ship-add[2] = "" then
         assign kli.ship-add[2] = kli.ship-add[3]
                kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
      end.
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no no-error.
      if not avail blk then do:
         create blk.
         assign
          blk.kli      = kli.cust-no
          blk.id       = xeb.part-no
          blk.snum     = xeb.form-no
          blk.bnum     = xeb.blank-no
          blk.qreq     = xeb.bl-qty
          blk.qyld     = xeb.yld-qty
          blk.yr$      = xeb.yrprice
          blk.stock-no = xeb.stock-no.
      end.
      xxx = xxx + (xeb.t-sqin * xeb.num-up).
   end.
   for each xeb where xeb.company = xest.company
                  AND xeb.est-no eq xest.est-no
                  and xeb.form-no eq xef.form-no no-lock,
       first blk  where blk.snum eq xeb.form-no
                    and blk.bnum eq xeb.blank-no:
       blk.pct = (xeb.t-sqin * xeb.num-up) / xxx.
   end.
end.

/* print header */
ASSIGN
 day_str  = STRING(TODAY,"99/99/9999")
 tim_str  = STRING(TIME,"hh:mm am") 
 v-module = IF cerunf EQ "HOP" THEN "FCD-0101" ELSE ""
 v-module = FILL(" ",59 - LENGTH(TRIM(v-module))) + TRIM(v-module).

display day_str v-module tim_str
        TRIM(xest.est-no) @ xest.est-no
        xest.updated-id
        probe.probe-user
        with frame hdr .

for each kli with frame kli:
   display kli.sman kli.sname
           kli.cust-no
           kli.cust-add[1] kli.ship-add[1] 
           kli.cust-add[2] kli.ship-add[2]
           kli.cust-add[3] kli.ship-add[3]
           kli.cust-add[4] kli.ship-add[4].
   down.
end.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no
with STREAM-IO frame brd no-labels no-box width 80 down:
   ASSIGN
    brd-l  = 0
    brd-w  = 0
    brd-sq = 0
    brd-sf = 0
    brd-wu = 0

    lv-brd-l  = 0
    lv-brd-w  = 0
    lv-brd-sq = 0
    lv-brd-sf = 0
    lv-brd-wu = 0.

   /* calc. sheet dimensions & weight */
   if cerunf eq "HOP" then
     assign
      brd-l[2] = xef.nsh-len
      brd-w[2] = xef.nsh-wid.
   else
     assign
      lv-brd-l = xef.nsh-len
      lv-brd-w = xef.nsh-wid
      brd-l[2] = xef.gsh-len
      brd-w[2] = xef.gsh-wid.

   brd-l[1] = xef.trim-l.
   if xef.roll = true then brd-l[3] = xef.gsh-len.
   brd-w[1] = xef.trim-w.
   if brd-l[2] = 0 and brd-w[2] = 0 then assign brd-l[2] = xef.lsh-len
                                                brd-w[2] = xef.lsh-wid.
   if xef.roll = true then brd-w[3] = xef.roll-wid.
   brd-sq[1] = xef.trim-l * xef.trim-w.
   brd-sq[2] = brd-l[2] * brd-w[2].
   brd-sq[3] = brd-l[3] * brd-w[3].
   lv-brd-sq = lv-brd-l * lv-brd-w.
   brd-sf[1] = if v-corr then (brd-sq[1] * .007) else (brd-sq[1] / 144).
   brd-sf[2] = if v-corr then (brd-sq[2] * .007) else (brd-sq[2] / 144).
   lv-brd-sf = if v-corr then (lv-brd-sq * .007) else (lv-brd-sq / 144).
   find first xop where xop.company = xest.company
                    AND xop.est-no eq xest.est-no
                    and xop.s-num = xef.form-no and
                    xop.line ge 500 no-lock no-error.
   find first item where (item.company = cocode) and item.i-no = xef.board no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   brd-wu[1] = brd-sf[1]  * item.basis-w.
   brd-wu[2] = brd-sf[2]  * item.basis-w.
   lv-brd-wu = lv-brd-sf  * item.basis-w.
   zzz = 0.

   display skip(1)
   "FORM" xef.form-no "OF" space(0) xest.form-qty
   " Width   Length     Sq.Inches   Sq.Feet/Sheet   Weight per Units" skip
   with no-box no-labels width 80 frame aa1 DOWN STREAM-IO.

   for each xeb OF xef BY xeb.blank-no:
      /* set total # of blanks on all forms */
      tt-blk = tt-blk + if xeb.yrprice /*AND NOT ll-tandem*/ then xeb.yld-qty else xeb.bl-qty.
      /* set total # of blanks on this form */
      t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up.
      /* set total qty of all blanks for this form */
      t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] +
                              if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.
      /* find sheet qty needed for this form (without spoil)*/
      if (xeb.yld-qty / xeb.num-up) > zzz then
      assign zzz = (xeb.yld-qty / xeb.num-up).
      {sys/inc/roundup.i zzz}
      t-shtfrm[xeb.form-no] = zzz.
      call_id = recid(xeb).
      vbsf = vbsf + if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144).

      assign
       brd-l[4]  = xeb.t-len
       brd-w[4]  = xeb.t-wid
       brd-sq[4] = brd-l[4] * brd-w[4]
       brd-sf[4] = if v-corr then (brd-sq[4] * .007) else (brd-sq[4] / 144)
       brd-wu[4] = brd-sf[4] * item.basis-w.

      display 
      "Blk" space(0) xeb.blank-no FORMAT "99"
          "Size :" brd-w[4] to 21 brd-l[4] to 30 brd-sq[4] to 42
               brd-sf[4] to 52 "Sf/Sht"  brd-wu[4] to 70 space(0) "/M Shts" skip
      with no-box no-labels width 80 frame aa2 DOWN STREAM-IO.
   end.
   find xeb where recid(xeb) = call_id no-lock no-error. qty = xeb.yld-qty.    

   run ce/com/prokalk.p.

   brd-sf[3] = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                          else (xef.gsh-len * xef.gsh-wid / 144)) *
               xef.gsh-qty / 1000.
   brd-wu[3] = brd-sf[3] * item.basis-w / 2000.

   display 
   "  Die Size :" brd-w[1] to 21 brd-l[1] to 30 brd-sq[1] to 42
            brd-sf[1] to 52 "Sf/Sht"  brd-wu[1] to 70 space(0) "/M Shts" skip
                   /*xef.trim-l when v-layout @ brd-w[1]
                   xef.trim-w when v-layout @ brd-l[1]*/
   with no-box no-labels width 80 frame aa3 DOWN STREAM-IO.

   if cerunf ne "HOP" then display
   " Feed Size :" lv-brd-w to 21 lv-brd-l to 30 lv-brd-sq to 42
            " #out:" xef.n-out FORMAT ">>9" lv-brd-wu to 70 space(0) "/M Shts" skip
   with no-box no-labels width 80 frame aa4 DOWN STREAM-IO.

   display
   "Sheet Size :" brd-w[2] to 21 brd-l[2] to 30 brd-sq[2] to 42
            brd-sf[2] to 52 "Sf/Sht"  brd-wu[2] to 70 space(0) "/M Shts" skip
   "Roll  Size :"                  when brd-w[3] ne 0
                /*brd-l[3]  to 30  when brd-l[3] ne 0*/
                  brd-w[3]  to 21  when brd-w[3] ne 0
                  "Total Board ="
                  brd-sf[3] to 43  "MSF"
                  brd-wu[3] to 68  "Tons"
                  skip(1)
/*"- Cust.- --- Qty --- --- Description ---- -- Size/Color --- --- Style/Part # --" */
"   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---"
with no-box no-labels width 80 frame aa5 DOWN STREAM-IO.

   for each xeb where xeb.company = xest.company
                  AND xeb.est-no eq xest.est-no and xeb.form-no = xef.form-no
       BY xeb.blank-no
   with STREAM-IO frame blk no-box no-labels width 80 down:
      find first style  where  style.company = cocode and
                                 style.style = xeb.style no-lock no-error.

      ASSIGN
       ld-len = xeb.len * ld-metric
       ld-wid = xeb.wid * ld-metric
       ld-dep = xeb.dep * ld-metric.

      IF ld-metric NE 1 THEN DO:
        {sys/inc/roundup.i ld-len}
        {sys/inc/roundup.i ld-wid}
        {sys/inc/roundup.i ld-dep}
      END.

      ASSIGN
       sizcol[1]  = TRIM(STRING(ld-len,lv-format)) + "x" +
                    TRIM(STRING(ld-wid,lv-format)) + "x" +
                    TRIM(STRING(ld-dep,lv-format))
       sizcol[2]  = xeb.i-coldscr
       stypart[1] = style.dscr
       stypart[2] = xeb.part-no
       dsc[1]     = xeb.part-dscr1
       dsc[2]     = xeb.part-dscr2.

      display /*xeb.cust-no*/
              xeb.yld-qty format ">>>,>>>,>>9"
                xeb.bl-qty when not xeb.yrprice @ xeb.yld-qty space(1)
              dsc[1] format "x(22)"  
              sizcol[1] format "x(21)"   
              stypart[1] format "x(23)" skip
              space(3) /* 10*/
              "#UP= " + string(xeb.num-up,">>9")
              dsc[2] format "x(22)"
              sizcol[2] format "x(21)"
              stypart[2] format "x(23)" SKIP WITH STREAM-IO.
      down.
   end.
end.

put skip(1)
   "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M    TOTAL" skip.
dm-tot[3] = 0. dm-tot[4] = 0. dm-tot[5] = 0.

/* b o a r d        */ run ce/com/pr4-brd.p (v-vend-no).
v-brd-cost = v-brd-cost + dm-tot[5].

/* i n k s          */ run ce/com/pr4-ink.p.

/* film             */ run ce/com/pr4-flm.p.

/* case/tray/pallet */ run ce/com/pr4-cas.p.

/* special          */ run ce/com/pr4-spe.p.

for each blk:
   find first xjob
        where xjob.i-no     eq blk.id
          and xjob.form-no  eq blk.snum
          and xjob.blank-no eq blk.bnum
        no-error.

   if not avail xjob then do:
     create xjob.
     assign
      xjob.form-no  = blk.snum
      xjob.blank-no = blk.bnum
      xjob.cust-no  = blk.kli.
   end.

   assign
    xjob.mat      = blk.cost - blk.lab
    xjob.lab      = blk.lab
    xjob.i-no     = blk.id
    xjob.pct      = blk.pct
    xjob.stock-no = blk.stock-no.
end.

display     "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" to 61
            dm-tot[5] / (tt-blk / 1000) format ">>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            skip(1)
    with STREAM-IO frame ac5 no-labels no-box.

/* prep */ run ce/com/pr4-prp.p.

/* misc */ run ce/com/pr4-mis.p.

put skip(1)
   "Machine Description    MR (Hrs) Run  Speed    Rate     MR $    Run $  Total Cost" .

/* machines */
run ce/com/pr4-mch.p.

if ctrl2[2] ne 0 or ctrl2[3] ne 0 then do:
   put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
   op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
end.

assign
 fr-tot     = 0
 fr-tot-pre = 0
 v-msf      = 0.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no,
    each xeb where xeb.company = xef.company
               AND xeb.est-no   eq xest.est-no
               and xeb.form-no eq xef.form-no,
   first carrier where carrier.company eq cocode
                    and carrier.carrier eq xeb.carrier no-lock,
   first carr-mtx where carr-mtx.company  eq cocode
      and carr-mtx.carrier  eq carrier.carrier
      and carr-mtx.del-zone eq xeb.dest-code no-lock:

  find first car where car.id eq xeb.part-no no-error.
  if not avail car then do:
    create car.
    assign
     car.carrier = carrier.carrier
     car.dscr    = carr-mtx.del-zone
     car.id      = xeb.part-no
     car.snum    = xeb.form-no
     car.bnum    = xeb.blank-no.
  end.
   
  find first item
      where (item.company = cocode)
        and item.i-no     eq xef.board
        and item.mat-type eq "B"
        and item.avg-w    gt 0
      no-lock no-error.
    
  assign
   v-msf    = (xeb.t-sqin - xeb.t-win) * xeb.bl-qty / 144000
   v-msf    = v-msf * if avail item then item.avg-w else 1
   v-blk-wt = xef.weight * v-msf
   car.msf  = car.msf + v-msf.

  if xef.medium ne "" then do:
    find first item where (item.company = cocode) and
               item.i-no = xef.medium no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    (item.basis-w * (1 - (item.shrink / 100)) * v-msf).
  end.
  if xef.flute ne "" then do:
    find first item where (item.company = cocode) and
               item.i-no = xef.flute no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    (item.basis-w * v-msf).
  end.
  if xef.lam-code ne "" then do:
    find first item where (item.company = cocode) and
               item.i-no = xef.lam-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.
  if xef.adh-code ne "" then do:
    find first item where (item.company = cocode) and
               item.i-no = xef.adh-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.
  
  car.qty = car.qty + v-blk-wt.

  find first blk
      where blk.snum eq xeb.form-no
        and blk.bnum eq xeb.blank-no
      no-lock no-error.
  if avail blk then blk.fg-wt = blk.fg-wt + v-blk-wt.

  /* add pallet & case for total weight */
  find first cas
      where cas.typ  eq 1
        and cas.snum eq xeb.form-no
        and cas.bnum eq xeb.blank-no
      no-error.
  if avail cas then do:
    find first item
        where (item.company = cocode)
          and item.i-no eq cas.ino
        no-lock no-error.
    if avail item then do:
      car.qty = car.qty + (cas.qty * ce-ctrl.def-cas-w /*item.basis-w*/).
      if avail blk then blk.fg-wt = blk.fg-wt + (ce-ctrl.def-cas-w /*item.basis-w*/).
    end.
    release item.
    find first cas
        where cas.typ  eq 3
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if avail cas then
    find first item
        where (item.company = cocode)
          and item.i-no eq cas.ino
        no-lock no-error.
    if avail item then do:
      car.qty = car.qty + (cas.qty * ce-ctrl.def-pal-w /*item.basis-w*/).
      if avail blk then blk.fg-wt = blk.fg-wt + (cas.qty * ce-ctrl.def-pal-w /*item.basis-w*/).
    end.
  end.

  for each cas where cas.id = xeb.part-no
                 AND CAN-DO("5,6",item.mat-type):
    find first item where item.company = cocode and
                          item.i-no = cas.ino no-lock no-error.
    car.qty = car.qty + (cas.qty * item.weight-100 / 100).
    IF AVAIL blk THEN blk.fg-wt = blk.fg-wt + (cas.qty * item.weight-100 / 100).
  end.
end.
fg-wt = 0.

for each car break by car.id:
  p-qty = 0.
  for each cas
      where cas.typ  eq 3
        and cas.snum eq car.snum
        and cas.bnum eq car.bnum:
        
    p-qty = p-qty + cas.qty.    
  end.
  
  ASSIGN
   z       = 0
   li-rels = 0.

  FOR EACH bf-eb NO-LOCK
      WHERE bf-eb.company EQ xest.company
        AND bf-eb.est-no  EQ xest.est-no
        AND bf-eb.part-no EQ car.id:
    z = z + bf-eb.bl-qty.
    FIND FIRST tt-rel
        WHERE tt-rel.reftable EQ "ce/com/selwhif1.w"
          AND tt-rel.company  EQ bf-eb.company
          AND tt-rel.loc      EQ bf-eb.est-no
          AND tt-rel.code     EQ STRING(bf-eb.form-no,"9999999999")
          AND tt-rel.code2    EQ STRING(bf-eb.blank-no,"9999999999")
        NO-ERROR.
    li-rels = li-rels + (IF AVAIL tt-rel THEN tt-rel.val[1] ELSE 1).
  END.
  
  find first xeb
      where xeb.company = xest.company
        AND xeb.est-no    eq xest.est-no
        and xeb.form-no  eq car.snum
        and xeb.blank-no eq car.bnum
      no-lock no-error.
  find first carrier
      where carrier.company eq cocode
        and carrier.loc     eq locode
        and carrier.carrier eq car.carrier
      no-lock no-error.
  release carr-mtx.
  if avail carrier then
  find first carr-mtx
      where carr-mtx.company  eq cocode
        and carr-mtx.loc      eq locode
        and carr-mtx.carrier  eq carrier.carrier
        and carr-mtx.del-zone eq car.dscr
       no-lock no-error.
  
  assign
   yyy   = 0
   zzz   = 0
   v-msf = 0.
   
  for each xcar
      where xcar.carrier eq car.carrier
        and xcar.dscr    eq car.dscr:  /* Group by zone? */
    assign
     zzz   = zzz + xcar.qty    /* zzz = total wt for price lookup */
     v-msf = v-msf + xcar.msf.  
  end.

  if xeb.fr-out-c ne 0 then
    yyy = xeb.fr-out-c * xxx / 100.
    
  else
  if xeb.fr-out-m ne 0 then
    yyy = xeb.fr-out-m * z / 1000.
    
  else  
  if avail carr-mtx then do:
    if carrier.chg-method eq "P" then
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * p-qty.
      if carr-mtx.weight[i] ge p-qty then leave.
    end.
    
    else
    if carrier.chg-method eq "W" then
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * car.qty / 100.
      if carr-mtx.weight[i] ge zzz then leave.
    end.
    
    else
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * car.msf.
      if carr-mtx.weight[i] ge v-msf then leave.
    end.
       
    if yyy lt carr-mtx.min-rate then yyy = carr-mtx.min-rate.
        
    yyy = yyy + (carr-mtx.min-rate * (li-rels - 1)).
  end.
  
  assign
   fg-wt    = fg-wt + car.qty
   car.cost = car.cost + yyy
   fr-tot   = fr-tot + yyy.
  
  if xeb.chg-method eq "P" then fr-tot-pre = fr-tot-pre + yyy.

  find first blk where blk.id eq car.id no-error.
  blk.sell = blk.sell + yyy . /* use sell for freight costs for now */
  ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.
  blk.lab  = blk.lab  + (car.qty / 100 * ld-fg-rate).
  blk.cost = blk.cost + (car.qty / 100 * ld-fg-rate).
  ld-fg-amt = ld-fg-amt + (car.qty / 100 * ld-fg-rate).
end.

if ld-fg-amt gt 1 then put "Finished Goods Handling" ld-fg-amt to 80 skip.

op-tot[5] = op-tot[5] + ld-fg-amt.

put "TOTAL  OPERATIONS        " op-tot[3] format ">>>>9.99" to 59
    op-tot[4] format ">>>>>9.99" to 69
    op-tot[5] format ">>>>,>>9.99" to 80 skip(1).

IF cerunf EQ "HOP" THEN DO:
  FOR EACH brd
      WHERE CAN-FIND(FIRST item
                     WHERE item.company EQ xest.company
                       AND item.i-no    EQ brd.i-no
                       AND CAN-DO("B,P,R,1,2,3,4",item.mat-type)):
    ACCUM brd.qty (TOTAL).
    ACCUM brd.qty-mr + brd.qty-wst (TOTAL).
  END.
  PUT "Total Waste Percentage"
      (ACCUM TOTAL brd.qty-mr + brd.qty-wst) / (ACCUM TOTAL brd.qty) * 100
                                  FORMAT ">>,>>9.99" TO 80
      SKIP(1).
END.

/* mat */
   do i = 1 to 6:
      ctrl[9] = ce-ctrl.mat-pct[i] / 100.
      if ce-ctrl.mat-cost[i] > dm-tot[5]  then leave.
   end.

/* lab */
   do i = 1 to 6:
      ctrl[10] = ce-ctrl.lab-pct[i] / 100.
      if ce-ctrl.lab-cost[i] > op-tot[5]  then leave.
   end.
   DO TRANSACTION:
     {est/calcpcts.i xest}
     ASSIGN
      calcpcts.val[1] = ctrl[9] * 100
      calcpcts.val[2] = v-brd-cost.
     FIND CURRENT calcpcts NO-LOCK NO-ERROR.
   END.

assign
 gsa-mat = ctrl[9]  * 100
 gsa-lab = ctrl[10] * 100
 gsa-com = ce-ctrl.comm-mrkup
 gsa-war = ce-ctrl.whse-mrkup
 qty     = tt-blk.


FIND FIRST reftable-fm NO-LOCK
     WHERE reftable-fm.reftable EQ "gsa-fm"
       AND reftable-fm.company  EQ xest.company
       AND reftable-fm.loc      EQ ""
       AND reftable-fm.code     EQ xest.est-no
     NO-ERROR.

IF AVAIL reftable-fm THEN
   gsa-fm = reftable-fm.val[1].
ELSE
   gsa-fm = ctrl[19].

output close.

/*hide frame kalk1 no-pause.
hide frame jobstd1 no-pause.
run ce/gsa.p (ROWID(probe), qty, 1).
*/

/*****************************************************************/
    
CREATE ttSharedFoldTandem .
    ASSIGN
        ttSharedFoldTandem.seqno  = probe.LINE
        ttSharedFoldTandem.qty1   = qty 
        ttSharedFoldTandem.getqty = probe.est-qty
        ttSharedFoldTandem.roid   = string(ROWID(probe)) 
        ttSharedFoldTandem.GsaMat     =  gsa-mat
        ttSharedFoldTandem.GsaLab     =  gsa-lab
        ttSharedFoldTandem.GsaWar     =  gsa-war .


/******************************************************************/

IF NOT do-gsa THEN DO:

assign
ctrl[9]  = gsa-mat / 100
ctrl[10] = gsa-lab / 100
ctrl[1]  = gsa-war / 100
ctrl[19] = gsa-fm / 100.

output to value(outfile1) append.
run ce/com/pr4-tots.p.
output close.

run ce/com/pr4-mis2.p.

IF (v-avg-com AND NOT ll-tandem) OR
   (v-avg-tan AND ll-tandem)     THEN DO:
  assign
   v-mat = 0  
   v-lab = 0
   v-foh = 0
   v-voh = 0.

  for each xjob:
    assign
     v-mat = v-mat + xjob.mat
     v-lab = v-lab + xjob.lab
     v-foh = v-foh + xjob.foh
     v-voh = v-voh + xjob.voh.
  end.

  for each blk,    
      first xjob
      where xjob.i-no     eq blk.id
        and xjob.form-no  eq blk.snum
        and xjob.blank-no eq blk.bnum:
    assign
     blk.fact = fac-tot * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     blk.cost = tt-tot  * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.mat = v-mat   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.lab = v-lab   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.foh = v-foh   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.voh = v-voh   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk).
  end.
END.

ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(probe.line,"99"). 

if vprint then do:
  run ce/com/probemk.p (ROWID(probe)).

  RUN ce/probeu3.p (ROWID(probe)).
END.

END.

        
       
    END.
