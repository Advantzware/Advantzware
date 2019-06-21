


/*------------------------------------------------------------------------
    File        : GetQtyTandem.p
    Purpose     : Get Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


    DEFINE TEMP-TABLE ttSharedTand NO-UNDO 
        FIELD seqno             AS INT  
        FIELD qty1              AS INT 
        FIELD getqty            AS INT  
        FIELD roid              AS CHAR
        FIELD abffs             AS CHAR
        FIELD GsaMat            AS DECIMAL
        FIELD GsaLab            AS DECIMAL
        FIELD GsaWar            AS DECIMAL
        FIELD GsaFm             AS DECIMAL
        FIELD GsaBrd            AS DECIMAL 
        FIELD GsaMonth          AS INT.
            

    DEFINE DATASET dsGetQtyTand FOR ttSharedTand .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmDoGsa      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoMr       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoSpeed    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDropRc     AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmEstimate   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmForm       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBlank      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmLvoverride AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmVendor     AS CHARACTER NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGetQtyTand.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.  

    IF prmUser   = ?  THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".    
    IF prmDoGsa = ?    THEN ASSIGN prmDoGsa = "no".
    IF prmDoMr = ?     THEN ASSIGN prmDoMr = "no".
    IF prmDoSpeed = ?  THEN ASSIGN prmDoSpeed = "no".
    IF prmDropRc = ?   THEN ASSIGN prmDropRc = "no".    
    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".
    IF prmForm = ?  THEN ASSIGN prmForm = 0.
    IF prmBlank = ?  THEN ASSIGN prmBlank = 0.
    IF prmLvoverride = ? THEN ASSIGN prmLvoverride = "Yes".                
    IF prmVendor     = ? THEN ASSIGN prmVendor   = "".


    {cec/print4.i "new shared" "new shared"}
    {cec/print42.i "new shared"}

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
    DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
    def new shared var v-do-gsa like do-gsa no-undo.
    def new shared buffer xop for est-op.
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
    DEF BUFFER reftable-fm FOR reftable.
    DEF BUFFER reftable-broker-pct FOR reftable.                        
    /*DEF VAR CALL_id AS RECID NO-UNDO.    */
    DEF NEW SHARED VAR save_id AS RECID NO-UNDO.
    DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO. 
    DEF NEW SHARED VAR chosen AS LOG FORMAT "y/n" NO-UNDO.
    def var v-vend-no like e-item-vend.vend-no init "" NO-UNDO.
    DEF var v-vend-list AS CHAR NO-UNDO.
    DEF VAR ld-fg-amt AS DEC NO-UNDO.
    DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
    DEF VAR lv-eqty LIKE est-op.qty NO-UNDO.
    DEF VAR v-hdr-depth AS CHAR FORMAT "x(5)" NO-UNDO.
    def var v-brd-only AS LOG no-undo.
    def var v-brd-cost as dec no-undo.
    DEF VAR lv-tot-up AS INT NO-UNDO.
    DEF VAR blk-count AS INT NO-UNDO.
    {sys/inc/f16to32.i}

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
    def var vn-out like xef.n-out NO-UNDO.

    DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
    DEF VAR lv-format AS CHAR INIT ">>>>9.99" NO-UNDO.
    DEF VAR ld-wid AS DEC NO-UNDO.
    DEF VAR ld-len AS DEC NO-UNDO.
    DEF VAR ld-dep AS DEC NO-UNDO.
    def var v-module as char format "x(60)" no-undo.
    DEF VAR lv-override AS LOG NO-UNDO.
    DEF VAR ld-fg-rate AS DEC NO-UNDO.
    DEF VAR ll-use-defaults AS LOG NO-UNDO.
    DEF VAR v-rm$ AS DEC NO-UNDO.
    DEF VAR ld-hand-pct AS DEC NO-UNDO.
    DEF VAR v-probe-fmt AS CHAR NO-UNDO.


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

    find first sys-ctrl where
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "CEBROWSE"
        no-lock no-error.

    IF sys-ctrl.char-fld NE "" THEN
        tmp-dir = sys-ctrl.char-fld.
    ELSE
        tmp-dir = "users\".

        IF INDEX(tmp-dir ,'P:',1) > 0 THEN ASSIGN
            tmp-dir  = REPLACE(tmp-dir ,'P:',"D:").
        
        IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
            tmp-dir = tmp-dir + "\".
        tmp-dir = REPLACE(tmp-dir,"/","\").



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
        

        do transaction:
            {cec/msfcalc.i}       
            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "CEDFAULT" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CEDFAULT"
                sys-ctrl.log-fld = no
                sys-ctrl.descrip = "Use CERUN & CEGSA log values on Whatif?  " +
                        "No uses saved est. values!".
                MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
            end. */
  
            ll-use-defaults = sys-ctrl.log-fld.

            {est/recalc-mr.i xest}
            FIND CURRENT recalc-mr NO-LOCK.

            {sys/inc/cerun.i C}
            ASSIGN
            do-speed  = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.recalc
            do-mr     = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE (recalc-mr.val[1] EQ 1)
            vmclean   = sys-ctrl.char-fld NE ""
            vmclean2  = NO
            vsuthrlnd = LOOKUP(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") NE 0
            v-module  = IF AVAIL company THEN company.NAME ELSE cocode
            v-module  = v-module + " - " + IF AVAIL loc THEN loc.dscr ELSE locode.

            IF sys-ctrl.char-fld EQ "Brick" THEN v-module = v-module + " - ISO# CS-03-1-F".

            {sys/inc/ctrtext.i "v-module" 60}.
                        
            

            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "CEGSA" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CEGSA"
                sys-ctrl.descrip = "Default for GS&A override".
                MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE sys-ctrl.log-fld.
            end.
            */
            do-gsa = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.override.


            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "CESLIT" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CESLIT"
                sys-ctrl.descrip = "Ask 'Drop Slitter...' question at OE cost calculation?"
                sys-ctrl.log-fld = no.
                MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE sys-ctrl.log-fld.
            end.
            */
            v-drop-rc = sys-ctrl.log-fld.

            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "COMBCOST" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "COMBCOST"
                sys-ctrl.descrip = "Average Cost for Combination Items?" 
                sys-ctrl.log-fld = no.
                MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
            end.
            */
            
            assign
                v-avg-com = sys-ctrl.log-fld
                v-avg-tan = sys-ctrl.int-fld eq 0.             
        end.
        
        EMPTY TEMP-TABLE tt-rel.


        if vprint then do:  
            ASSIGN
            do-speed    = LOGICAL(prmDoSpeed)
            do-mr       = LOGICAL(prmDoMr)
            do-gsa      = LOGICAL(prmDoGsa)
            v-summ      = NO
            lv-override = LOGICAL(prmLvoverride)
            .
                                                    

            IF do-speed THEN                  
            IF lv-override THEN
            for each probe where probe.company = xest.company AND probe.est-no = xest.est-no:
                delete probe.                 
            end.
        end.

        ELSE DO:
            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "FGCOST" no-lock no-error.
            /*if not avail sys-ctrl then do transaction:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "FGCOST"
                sys-ctrl.log-fld = no
                sys-ctrl.descrip = "Create FG Cost in Job File with only Board Cost?".
                MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE sys-ctrl.log-fld.
            end.
            */
            v-brd-only = sys-ctrl.log-fld.
        END.       
        
        FOR EACH eb fields(company est-no form-no blank-no) NO-LOCK
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

        FORM day_str v-module tim_str TO 79
     SKIP(1)
     "Combination Est#" xest.est-no FORMAT "x(8)"
     "UserID:" xest.updated-id
     "Prober:" probe.probe-user
     SKIP(1)
     with frame hdr page-top STREAM-IO width 80 no-labels no-box.

FORM "Sales Rep:" kli.sman kli.sname SKIP
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

for each xef FIELDS(company est-no form-no)
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    no-lock,
    each xeb FIELDS(yld-qty bl-qty yrprice)
    where xeb.company eq xef.company
      and xeb.est-no  eq xef.est-no
      and xeb.form-no eq xef.form-no
    no-lock:
  qty = qty + if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.
end.

{est/probeset.i qty 0}

v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

ASSIGN
 outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,v-probe-fmt)
 outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,v-probe-fmt)
 outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,v-probe-fmt)
 outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,v-probe-fmt).

output to value(outfile1).

find first ce-ctrl where (ce-ctrl.company = cocode and ce-ctrl.loc     = locode) no-lock no-error.
assign
v-tt-tot     = 0
v-fac-tot    = 0
v-ord-cost   = 0

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

FIND FIRST reftable-broker-pct
     WHERE reftable-broker-pct.reftable EQ "ce-ctrl.broker-pct"
       AND reftable-broker-pct.company  EQ ce-ctrl.company
       AND reftable-broker-pct.loc      EQ ce-ctrl.loc
     NO-LOCK NO-ERROR.

IF AVAIL reftable-broker-pct THEN
   ctrl[19] = reftable-broker-pct.val[1].

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
  FOR EACH est-op FIELDS(qty)
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
          blk.stock-no = xeb.stock-no
          blk.pur-man  = xeb.pur-man.
      end.
      xxx = xxx + (xeb.t-sqin * xeb.num-up).
   end.
   for each xeb fields(form-no blank-no t-sqin num-up) where
       xeb.company = xest.company
       AND xeb.est-no eq xest.est-no
       AND xeb.form-no eq xef.form-no no-lock,
       first blk  where blk.snum eq xeb.form-no
                    and blk.bnum eq xeb.blank-no:
       blk.pct = (xeb.t-sqin * xeb.num-up) / xxx.
   end.
end.

/* print header */
ASSIGN
 day_str  = STRING(TODAY,"99/99/9999")
 tim_str  = STRING(TIME,"hh:mm am").

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

FOR EACH xef
    WHERE xef.company EQ xest.company 
      AND xef.est-no  EQ xest.est-no
    WITH FRAME brd no-labels no-box width 80 down stream-io:

  RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

  ASSIGN
   brd-l[1] = xef.trim-l
   brd-w[1] = xef.trim-w
   brd-l[2] = xef.gsh-len.

  IF xef.roll THEN brd-l[3] = xef.trim-l.
  brd-w[2] = xef.gsh-wid.
  IF brd-l[2] EQ 0 AND brd-w[2] = 0 THEN
    ASSIGN
     brd-l[2] = xef.lsh-len
     brd-w[2] = xef.lsh-wid.
  brd-w[3] = IF xef.roll THEN xef.nsh-len ELSE 0.
  brd-sq[1] = brd-l[1] * brd-w[1].
  brd-sq[2] = brd-l[2] * brd-w[2].
  brd-sq[3] = brd-l[3] * brd-w[3].

  IF v-corr THEN
    ASSIGN
     brd-sf[1] = brd-sq[1] * .007
     brd-sf[2] = brd-sq[2] * .007
     brd-sf[3] = brd-sq[3] * .007.
   ELSE
     ASSIGN
      brd-sf[1] = brd-sq[1] / 144
      brd-sf[2] = brd-sq[2] / 144
      brd-sf[3] = brd-sq[3] / 144.

  FIND FIRST item
      where (item.company = cocode)
        AND item.i-no EQ xef.board
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN
  FIND FIRST e-item OF item NO-LOCK NO-ERROR.
  ASSIGN
  brd-wu[1] = brd-sf[1]  * item.basis-w
  brd-wu[2] = brd-sf[2]  * item.basis-w
  brd-wu[3] = (brd-sf[3] * item.basis-w) / 2000
  zzz = 0
  tmpstore    = "FORM " +
                TRIM(STRING(xef.form-no,">9")) +
                " OF " +
                TRIM(STRING(xest.form-qty,">9"))
  v-hdr-depth = IF xef.nsh-dep EQ 0 AND
                   xef.gsh-dep EQ 0 THEN "" ELSE "Depth".

  DISPLAY SKIP(1)
          tmpstore                           FORMAT "x(13)"
          "  Width  Length  "
          v-hdr-depth
          "#On  Sq.Inches      Sq.Feet     Wgt/Units"
          SKIP.

  FOR EACH xeb
      WHERE xeb.company EQ xest.company 
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
      BREAK BY xeb.blank-no:

    ASSIGN
    /* set total # of blanks on all forms */
    tt-blk = tt-blk + IF xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty
    /* set total # of blanks on this form */
    t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up
    /* set total qty of all blanks for this form */
    t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] +
                            if xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty.
    /* find sheet qty needed for this form (without spoil)*/
    IF (xeb.yld-qty / xeb.num-up) > zzz THEN
       ASSIGN zzz = xeb.yld-qty / (xeb.num-up * xef.n-out * xef.n-out-l * xef.n-out-d).

    {sys/inc/roundup.i zzz}

    ASSIGN
    t-shtfrm[xeb.form-no] = zzz
    call_id = RECID(xeb)
    brd-l[4]  = xeb.t-len
    brd-w[4]  = xeb.t-wid
    brd-sq[4] = brd-l[4] * brd-w[4]
    brd-sf[4] = IF v-corr THEN brd-sq[4] * .007 ELSE brd-sq[4] / 144
    brd-wu[4] = brd-sf[4] * item.basis-w
    vbsf = vbsf + IF v-corr THEN (xeb.t-sqin * .007) ELSE (xeb.t-sqin / 144).

    DISPLAY "  Blk"
            SPACE(0)
            xeb.blank-no                       FORMAT "99"
            "Size:"
            brd-w[4]                           FORMAT ">>>9.99<<<"
            brd-l[4]                           FORMAT ">>>9.99<<<" 
            xeb.t-dep WHEN xeb.t-dep NE 0      FORMAT ">>>9.99<<<"
            1                                  FORMAT ">>>" 
            SPACE(4)
            brd-sq[4]
            brd-sf[4]                              
            "Sf/BL"
            brd-wu[4]
            SPACE(0)
            "/MBL"
            SKIP
        WITH NO-BOX NO-LABELS COLOR VALUE("blu/brown") WIDTH 80 FRAME aa2-1 STREAM-IO.

    IF NOT vsuthrlnd THEN DO WITH FRAME aa2-1:
      ASSIGN
       brd-w[4]:FORMAT  = ">>>9.99"
       brd-l[4]:FORMAT  = ">>>9.99"
       xeb.t-dep:FORMAT = ">>>9.99".

      DISPLAY {sys/inc/k16v.i brd-w[4]} @ brd-w[4]
              {sys/inc/k16v.i brd-l[4]} @ brd-l[4]
              "" @ xeb.t-dep
              {sys/inc/k16v.i xeb.t-dep} WHEN xeb.t-dep NE 0 @ xeb.t-dep.
    END.

    IF LAST(xeb.blank-no) THEN DO:

      lv-tot-up = 0.

      FOR EACH bf-eb FIELDS(num-up)
          WHERE bf-eb.company EQ xef.company
            AND bf-eb.est-no  EQ xef.est-no
            AND bf-eb.form-no EQ xef.form-no
          NO-LOCK:
          lv-tot-up  = lv-tot-up + bf-eb.num-up.
      END.

      DISPLAY " NetSht Size:"
              brd-w[1]                            FORMAT ">>>9.99<<<"
              brd-l[1]                            FORMAT ">>>9.99<<<"
              xef.nsh-dep WHEN xef.nsh-dep NE 0   FORMAT ">>>9.99<<<"
              lv-tot-up                           FORMAT ">>>"  
              SPACE(4)
              brd-sq[1]
              brd-sf[1]
              "Sf/NS"
              brd-wu[1]
              SPACE(0)
              "/MNS"
              SKIP

              " GrsSht Size:"
              brd-w[2]                            FORMAT ">>>9.99<<<"
              brd-l[2]                            FORMAT ">>>9.99<<<"
              xef.gsh-dep WHEN xef.gsh-dep NE 0   FORMAT ">>>9.99<<<"
              vn-out                              FORMAT ">>>" 
              SPACE(4)
              brd-sq[2]
              brd-sf[2]
              "Sf/GS"
              brd-wu[2]
              SPACE(0)
              "/MGS" SKIP
          WITH NO-BOX NO-LABELS COLOR VALUE("blu/brown") WIDTH 80 FRAME aa2-2 STREAM-IO.

      IF NOT vsuthrlnd THEN DO WITH FRAME aa2-2:
        ASSIGN
         brd-w[1]:FORMAT    = ">>>9.99"
         brd-l[1]:FORMAT    = ">>>9.99"
         xef.nsh-dep:FORMAT = ">>>9.99"
         brd-w[2]:FORMAT    = ">>>9.99"
         brd-l[2]:FORMAT    = ">>>9.99"
         xef.gsh-dep:FORMAT = ">>>9.99".

        DISPLAY {sys/inc/k16v.i brd-w[1]} @ brd-w[1]
                {sys/inc/k16v.i brd-l[1]} @ brd-l[1]
                "" @ xef.nsh-dep
                {sys/inc/k16v.i xef.nsh-dep} WHEN xef.nsh-dep NE 0 @ xef.nsh-dep
                {sys/inc/k16v.i brd-w[2]} @ brd-w[2]
                {sys/inc/k16v.i brd-l[2]} @ brd-l[2]
                "" @ xef.gsh-dep
                {sys/inc/k16v.i xef.gsh-dep} WHEN xef.gsh-dep NE 0 @ xef.gsh-dep.
      END.
    END.
  END.
  FIND xeb WHERE RECID(xeb) = call_id NO-LOCK NO-ERROR. qty = xeb.yld-qty.

  IF brd-w[3] NE 0 THEN
    DISPLAY "Roll  Size :" brd-w[3]                FORMAT ">>9.99<<" TO 22
        WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa3 STREAM-IO.

  IF NOT vsuthrlnd THEN
    IF brd-w[3] NE 0 THEN DISPLAY {sys/inc/k16v.i brd-w[3]} @ brd-w[3] WITH FRAME aa3.

  DISPLAY SKIP(1)
"   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---"
      WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa5 DOWN STREAM-IO.

  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
      BY xeb.blank-no
      WITH STREAM-IO FRAME blk NO-BOX NO-LABELS WIDTH 80 DOWN:

    FIND FIRST style
        WHERE style.company EQ cocode
          AND style.style   EQ xeb.style
        NO-LOCK NO-ERROR.

    ASSIGN
     ld-len = xeb.len * ld-metric
     ld-wid = xeb.wid * ld-metric
     ld-dep = xeb.dep * ld-metric.

    IF ld-metric NE 1 THEN DO:
      {sys/inc/roundup.i ld-len}
      {sys/inc/roundup.i ld-wid}
      {sys/inc/roundup.i ld-dep}
    END.

    ELSE
      ASSIGN
       ld-len = {sys/inc/k16v.i ld-len}
       ld-wid = {sys/inc/k16v.i ld-wid}
       ld-dep = {sys/inc/k16v.i ld-dep}.

    ASSIGN
     sizcol[1]  = TRIM(STRING(ld-len,lv-format)) + "x" +
                  TRIM(STRING(ld-wid,lv-format)) + "x" +
                  TRIM(STRING(ld-dep,lv-format))
     sizcol[2]  = xeb.i-coldscr
     stypart[1] = style.dscr
     stypart[2] = xeb.part-no
     dsc[1]     = xeb.part-dscr1
     dsc[2]     = xeb.part-dscr2.

    DISPLAY /*xeb.cust-no*/
            xeb.yld-qty FORMAT ">>>,>>>,>>9"
              xeb.bl-qty WHEN NOT xeb.yrprice @ xeb.yld-qty SPACE(1)
            dsc[1] FORMAT "x(22)"  
            sizcol[1] FORMAT "x(21)"   
            stypart[1] FORMAT "x(23)" SKIP
            SPACE(3) /* 10*/
            "#UP= " + STRING(xeb.num-up,">>9")
            dsc[2] FORMAT "x(22)"
            sizcol[2] FORMAT "x(21)"
            stypart[2] FORMAT "x(23)" SKIP WITH STREAM-IO.
    DOWN.
  END.
END.  /* for each xef */

PUT SKIP(1)
   "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M    TOTAL" skip.
dm-tot[3] = 0. dm-tot[4] = 0. dm-tot[5] = 0.

/* b o a r d        */ RUN cec/box/pr42-brd.p (v-vend-no, OUTPUT v-vend-list).

    v-brd-cost = v-brd-cost + dm-tot[5].
  
    /* adders           */ RUN cec/box/pr42-add.p (v-vend-list).



FIND CURRENT probe-board NO-ERROR.
IF AVAIL probe-board THEN
  probe-board.val[1] = probe-board.val[1] + dm-tot[5].
FIND CURRENT probe-board NO-LOCK NO-ERROR.

/* i n k s          */ RUN cec/com/pr4-ink.p.

/* film             */ RUN cec/com/pr4-flm.p.

/* case/tray/pallet */ RUN cec/com/pr4-cas.p.

/* special          */ RUN cec/com/pr4-spe.p.



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
    xjob.stock-no = blk.stock-no
    xjob.pur-man  = blk.pur-man.
end.


display     "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" to 61
            dm-tot[5] / (tt-blk / 1000) format ">>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            skip(1)
    with STREAM-IO frame ac5 no-labels no-box.

/* prep */ run cec/com/pr4-prp.p.


/* misc */ run cec/com/pr4-mis.p.

put skip(1)
   "Machine Description    MR (Hrs) Run  Speed    Rate     MR $    Run $  Total Cost" .
   

/* machines */
run cec/com/pr4-mch.p.

ctrl2[2] = 0.

FOR EACH blk:
  FIND FIRST xjob
      WHERE xjob.i-no     EQ blk.id
        AND xjob.form-no  EQ blk.snum
        AND xjob.blank-no EQ blk.bnum
      NO-ERROR.

  ld-hand-pct = IF blk.pur-man THEN hand-pct-f ELSE ctrl[2].

  IF ld-hand-pct NE 0 THEN
    ASSIGN
     v-rm$    = xjob.mat * ld-hand-pct
     blk.cost = blk.cost + v-rm$
     blk.lab  = blk.lab  + v-rm$
     ctrl2[2] = ctrl2[2] + v-rm$
     xjob.lab = xjob.lab + v-rm$.
END.

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

  /*
  if xef.lam-code ne "" then do:
    find first item {sys/look/item.w} and
               item.i-no = xef.lam-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.
  if xef.adh-code ne "" then do:
    find first item {sys/look/item.w} and
               item.i-no = xef.adh-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.*/
  
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
end.

ASSIGN
 fg-wt     = 0
 ld-fg-amt = 0.

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

  FOR EACH bf-eb fields(company est-no form-no blank-no bl-qty) NO-LOCK
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
  ASSIGN
  blk.sell = blk.sell + yyy  /* use sell for freight costs for now */
  ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
  blk.lab  = blk.lab  + (car.qty / 100 * ld-fg-rate)
  blk.cost = blk.cost + (car.qty / 100 * ld-fg-rate)
  ld-fg-amt = ld-fg-amt + (car.qty / 100 * ld-fg-rate).
end.

if ld-fg-amt gt 1 then put "Finished Goods Handling" ld-fg-amt to 80 skip.

op-tot[5] = op-tot[5] + ld-fg-amt.

put "TOTAL  OPERATIONS        " op-tot[3] format ">>>>9.99" to 59
    op-tot[4] format ">>>>>9.99" to 69
    op-tot[5] format ">>>>,>>9.99" to 80 skip(1).

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
 
FIND FIRST cust WHERE
         cust.company EQ xeb.company AND
         cust.cust-no EQ xeb.cust-no
         NO-LOCK NO-ERROR.

IF AVAIL reftable-fm THEN
   gsa-fm = reftable-fm.val[1].
ELSE
   IF AVAIL cust AND cust.scomm NE 0 THEN
      gsa-fm = cust.scomm.
ELSE
   gsa-fm = ctrl[19].

output close.

 DEFINE VAR li-qty AS INT NO-UNDO.
       FOR EACH brd
           WHERE (brd.form-no EQ v-form-no OR (NOT vmclean2))
             AND CAN-FIND(FIRST ITEM WHERE item.company  EQ cocode
                                       AND item.i-no     EQ brd.i-no
                                       AND item.mat-type EQ "D"):
      
         li-qty = li-qty + brd.qty.
       END.

/********************/
    CREATE ttSharedTand .
        ASSIGN
            ttSharedTand.seqno  = probe.LINE
            ttSharedTand.qty1   = qty 
            ttSharedTand.getqty = probe.est-qty
            ttSharedTand.roid   = string(ROWID(probe)) 
            
            ttSharedTand.GsaMat        =  gsa-mat
            ttSharedTand.GsaLab        =  gsa-lab
            ttSharedTand.GsaWar        =  gsa-war
            ttSharedTand.GsaFm         =  gsa-fm
            ttSharedTand.GsaMonth      =  li-qty
            .        


IF NOT do-gsa THEN DO:

assign
ctrl[9]  = gsa-mat / 100
ctrl[10] = gsa-lab / 100
ctrl[1]  = gsa-war / 100
ctrl[19] = gsa-fm / 100.

output to value(outfile1) append.
run cec/com/pr4-tots.p.
output close.

run cec/com/pr4-mis2-copy.p.

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
end.

ASSIGN
   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
   ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(probe.line,v-probe-fmt).

if vprint then do:
  run cec/com/probemk-copy.p (ROWID(probe)).

  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else
    dos silent type value(outfile2) >> value(outfile3).

  if search(outfile1) <> ? then 
    dos silent  type value(outfile3) > value(ls-outfile).

  RUN cec/probeu3.p (ROWID(probe)).
END.
END.
       
    END.
