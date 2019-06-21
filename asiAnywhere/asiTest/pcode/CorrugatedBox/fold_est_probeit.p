


/*------------------------------------------------------------------------
    File        : fold_est_probeit.p
    Purpose     : 

    Syntax      :

    Description : Return a Dataset of Request For Estimate Probeit

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttfoldprobeitestimate NO-UNDO               
        FIELD vCustNo     AS CHARACTER
        FIELD vPartNo     AS CHARACTER
        FIELD vBlQty      AS INTEGER
        FIELD vYldQty     AS INTEGER 
        FIELD vFactCost   AS DECIMAL
        FIELD vFullCost   AS DECIMAL
        FIELD vSellPrice  AS DECIMAL
        FIELD vYRprice    AS CHARACTER 
        FIELD vLine       AS INT
        FIELD ahdg        AS CHARACTER
        .


    DEFINE DATASET dsfoldprobeitestimate FOR ttfoldprobeitestimate .
    DEFINE INPUT PARAMETER prmUser          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction        AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmEstimate      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmLine          AS INT NO-UNDO.    
    DEFINE INPUT PARAMETER prmSellPrice     AS DECIMAL NO-UNDO.    
    DEFINE INPUT PARAMETER prmPartNo        AS CHARACTER NO-UNDO. 

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsfoldprobeitestimate.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser          = ?  THEN ASSIGN prmUser = "".
    IF prmAction        = ?  THEN ASSIGN prmAction = "".    
    IF prmEstimate      = ?  THEN ASSIGN prmEstimate = "".
    IF prmLine          = ?  THEN ASSIGN prmLine = 0.   
    IF prmSellPrice     = ?  THEN ASSIGN prmSellPrice = 0.      
    IF prmPartNo        = ?  THEN ASSIGN prmPartNo = "".


    {cec/print4.i "new shared" "new shared"}
    {cec/print42.i "new shared"}
    {sys/inc/VAR.i NEW SHARED}                

    DEF NEW SHARED BUFFER xest FOR est.
    DEF NEW SHARED BUFFER xeb FOR eb.
    DEF NEW SHARED BUFFER xef FOR ef.
    DEF VAR ld-prev-sell-price AS DEC NO-UNDO.
    DEF NEW SHARED VAR partNum AS CHARACTER NO-UNDO.

    DEF BUFFER b-probeit FOR probeit.

    DEF TEMP-TABLE tt-probeit LIKE probeit FIELD row-id AS ROWID.
    DEF VAR ll-use-margin AS LOG NO-UNDO.
    DEF VAR lv-changed AS cha NO-UNDO.
    DEF VAR lv-price AS CHAR NO-UNDO.

    DEF VAR pfullc AS DECIMAL NO-UNDO.
    DEF VAR pfactc AS DECIMAL NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    def new shared var tmp-dir as cha no-undo.
    DEF NEW SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.


    DO TRANSACTION:
        {sys/inc/ceround.i}

        {sys/inc/cerun.i F}
        {sys/inc/cerun.i C}
        vmclean = CAN-DO("McLean,HOP",cerunf).

        {sys/inc/cewhatif.i}
    END.
        

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
        ASSIGN 
            cocode = prmComp
            locode = "Main"
            .

    IF prmAction = "updateitem" THEN DO:       
        
        DEF VAR ld-tot-pric AS DEC NO-UNDO.
        DEF VAR ld-tot-fact AS DEC NO-UNDO.
        DEF VAR ld-tot-full AS DEC NO-UNDO.
        DEF VAR lv-qty LIKE probe.est-qty NO-UNDO.
        DEF VAR ll-price-change AS LOG NO-UNDO.
        def var qm as dec.
        def var v-comm like tt-tot NO-UNDO.
        def var v-prf-s as dec NO-UNDO.
        def var v-pct-s as dec NO-UNDO.
        DEF VAR ld-price LIKE probe.sell-price NO-UNDO.
        DEF VAR v-probe-fmt AS CHAR NO-UNDO.
        DEF VAR v-old-price AS DEC NO-UNDO.
        DEF VAR v-old-full-cost AS DEC NO-UNDO.
        DEF VAR ld-commc AS DEC NO-UNDO.
        DEF VAR ld-fullc AS DEC NO-UNDO.
        DEF VAR ld-marg% AS DEC NO-UNDO.

        DEF BUFFER probe-ref FOR reftable.

        {est/checkuse.i}

        ASSIGN
            partNum = prmPartNo.
        

        FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
            find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
        
   
        FIND FIRST probe WHERE probe.company = est.company and probe.est-no = est.est-no AND probe.LINE = prmLine 
            AND probe.probe-date ne ? EXCLUSIVE-LOCK NO-ERROR.
        

    
        ASSIGN
            pfullc = DEC(probe.full-cost)
            .        
       

        ll-use-margin = NO.        

        IF cerunf EQ "Fibre" THEN
            RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).


        FOR EACH tt-probeit:
            DELETE tt-probeit.
        END.

        FOR EACH probeit
          WHERE probeit.company EQ probe.company
            AND probeit.est-no  EQ probe.est-no
            AND probeit.line    EQ probe.LINE 
            AND probeit.part-no EQ prmPartNo
            USE-INDEX est-no NO-LOCK:
                CREATE tt-probeit.
                BUFFER-COPY probeit TO tt-probeit
                ASSIGN tt-probeit.row-id = ROWID(probeit).
        END.                     

        FIND FIRST probeit WHERE probeit.company EQ prmComp AND probeit.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND probeit.line EQ prmLine
            AND probeit.part-no EQ prmPartNo NO-LOCK NO-ERROR.

        ld-prev-sell-price = probeit.sell-price.

        RUN calc-fields.        

        ll-price-change = NO.
        FOR EACH tt-probeit:
          FIND FIRST probeit WHERE ROWID(probeit) EQ tt-probeit.row-id NO-LOCK NO-ERROR.
                ll-price-change = NOT AVAIL probeit OR probeit.sell-price NE tt-probeit.sell-price.
                IF ll-price-change THEN LEAVE.
        END.
       


   /*IF ll-price-change THEN DO TRANSACTION:*/
        
     DO TRANSACTION:  

        FIND CURRENT probe.

      probe.do-quote = YES.

      IF est.est-type EQ 2 THEN DO:
        ld-price = probe.sell-price.

        RUN ce/uprobit2.p (RECID(probe)). 

        RUN recalc-fields (ld-price).        

        IF vmclean THEN RUN ce/pr4-mcl1.p (ROWID(probe)).
      END.

      ELSE DO: 
          {cec/combasis.i}
        FOR EACH probeit WHERE probeit.company EQ prmComp AND probeit.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND probeit.line EQ prmLine
            NO-LOCK:
         ASSIGN
           lv-qty      = IF probeit.yrprice THEN probeit.yld-qty
                                            ELSE probeit.bl-qty
           ld-tot-pric = ld-tot-pric +
                         (probeit.sell-price * (lv-qty / 1000))
           ld-tot-fact = ld-tot-fact +
                         (probeit.fact-cost  * (lv-qty / 1000))
           ld-tot-full = ld-tot-full +
                         (probeit.full-cost  * (lv-qty / 1000)).
        END.           
      

        FIND CURRENT probe EXCLUSIVE-LOCK.      
            
        ASSIGN
         v-com              = probe.comm
         qm                 = probe.est-qty / 1000
         v-old-price        = probe.sell-price
         v-old-full-cost    = probe.full-cost
         probe.sell-price   = ROUND(ld-tot-pric / qm,2)
         probe.fact-cost    = ROUND(ld-tot-fact / qm,2)
         probe.full-cost    = ROUND(ld-tot-full / qm,2)
         probe.net-profit   = (1 - (probe.full-cost / probe.sell-price)) * 100
         probe.gross-profit = (1 - (probe.fact-cost / probe.sell-price)) * 100
         .

        MESSAGE "ll-use-margin" ll-use-margin.

        IF ll-use-margin THEN
        DO:
           ASSIGN
              ld-commc = (v-old-price - (IF v-basis EQ "G" THEN probe.fact-cost ELSE 0))
                       * (v-com / 100)
              ld-fullc = v-old-full-cost - ld-commc
              ld-marg% = ROUND((probe.sell-price - ld-fullc) / probe.sell-price * 100,2).        

           RUN est/getsmanmtrx.p (ROWID(est), "C",
                                   INPUT-OUTPUT v-com,
                                   INPUT-OUTPUT ld-marg%).

           ASSIGN
              probe.comm = v-com /*v-com has to be updated with new comm*/
              ld-commc = (probe.sell-price - (IF v-basis EQ "G" THEN probe.fact-cost ELSE 0))
                       * (probe.comm / 100)
              ld-fullc = ld-fullc + ld-commc
              probe.full-cost = ld-fullc
              probe.net-profit = (1 - (probe.full-cost / probe.sell-price)) * 100
              probe.gross-profit = (1 - (probe.fact-cost / probe.sell-price)) * 100
              probe.market-price = ld-marg%.
        END.
        ELSE
           probe.market-price = probe.net-profit + probe.comm.
           

         ASSIGN
         v-prf-s = probe.sell-price - probe.fact-cost
         v-pct-s = v-prf-s / probe.fact-cost * 100.

        RUN ce/uprobit3.p (BUFFER probe).          
              
      END.    
    END.
    ASSIGN
    prmAction = "Select".
 END.



 IF prmAction = "Select" THEN DO:
      
        FOR EACH probeit WHERE probeit.company EQ prmComp AND probeit.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND probeit.line EQ prmLine NO-LOCK, 
            FIRST eb WHERE eb.company = probeit.company   AND eb.est-no = probeit.est-no   AND eb.part-no = probeit.part-no NO-LOCK BY eb.form-no     BY eb.blank-no :

        CREATE ttfoldprobeitestimate.
        ASSIGN          
            ttfoldprobeitestimate.vCustNo     = probeit.cust-no
            ttfoldprobeitestimate.vPartNo     = probeit.part-no
            ttfoldprobeitestimate.vBlQty      = probeit.bl-qty 
            ttfoldprobeitestimate.vYldQty     = probeit.yld-qty
            ttfoldprobeitestimate.vFactCost   = probeit.fact-cost
            ttfoldprobeitestimate.vFullCost   = probeit.full-cost
            ttfoldprobeitestimate.vSellPrice  = probeit.sell-price
            ttfoldprobeitestimate.vYRprice    = string(probeit.YRprice)
            ttfoldprobeitestimate.vLine       = probeit.LINE
            .            
        END.        

    END.  


    IF prmAction = "GetProbeitEst" THEN DO:
      
        FOR EACH probeit WHERE probeit.company EQ prmComp AND probeit.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND probeit.line EQ prmLine
             AND probeit.part-no EQ prmPartNo NO-LOCK, 
            FIRST eb WHERE eb.company = probeit.company   AND eb.est-no = probeit.est-no   AND eb.part-no = probeit.part-no NO-LOCK BY eb.form-no     BY eb.blank-no :

        CREATE ttfoldprobeitestimate.
        ASSIGN          
            ttfoldprobeitestimate.vCustNo     = probeit.cust-no
            ttfoldprobeitestimate.vPartNo     = probeit.part-no
            ttfoldprobeitestimate.vBlQty      = probeit.bl-qty 
            ttfoldprobeitestimate.vYldQty     = probeit.yld-qty
            ttfoldprobeitestimate.vFactCost   = probeit.fact-cost
            ttfoldprobeitestimate.vFullCost   = probeit.full-cost
            ttfoldprobeitestimate.vSellPrice  = probeit.sell-price
            ttfoldprobeitestimate.vYRprice    = string(probeit.YRprice)
            ttfoldprobeitestimate.vLine       = probeit.LINE
            .            
        END.        

    END. 


/*-----------------------------------------------calc-fields------------------------------------------------*/
PROCEDURE calc-fields :


  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.  

  DEF BUFFER b-probemk FOR reftable.


  FIND xest WHERE xest.company = probe.company
              AND xest.est-no = probe.est-no NO-LOCK NO-ERROR.

  {cec/combasis.i}     
      
  FIND FIRST b-probemk
      WHERE b-probemk.reftable EQ "ce/com/probemk.p"
        AND b-probemk.company  EQ probeit.company
        AND b-probemk.loc      EQ probeit.est-no
        AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
        AND b-probemk.code2    EQ probeit.part-no
      NO-ERROR.
  IF AVAIL b-probemk THEN
    v-com = b-probemk.val[2] + b-probemk.val[3] +
            b-probemk.val[4] + b-probemk.val[5].

  {sys/inc/ceround.i}
  lv-changed = "S".

  IF lv-changed NE "" THEN DO:
      ASSIGN
     ld-price = ld-prev-sell-price
     ld-factc = DEC(probeit.fact-cost)
     ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                (v-com / 100)   
     ld-fullc = DEC(probeit.full-cost) - ld-commc.


    IF lv-changed EQ "S" THEN
      ASSIGN
       ld-price = DEC(prmSellPrice)
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).


    ld-fullc = ld-fullc + ld-commc.
    FIND CURRENT probeit EXCLUSIVE-LOCK.
    probeit.full-cost =
       ROUND(ld-fullc, 2) NO-ERROR.

    probeit.sell-price = ROUND(ld-price, 2) NO-ERROR.

    ASSIGN 
        probeit.full-cost = DEC(probeit.full-cost)       
        probeit.sell-price = DEC(probeit.sell-price) 
    
        lv-changed = "".     

  END.

END PROCEDURE.



/*-----------------------------------------------recalc-fields-------------------------------------------*/


PROCEDURE recalc-fields :
  DEF INPUT PARAM ip-price LIKE probe.sell-price NO-UNDO.

  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.


  {cec/combasis.i}

    ld-price = 0.
    FOR EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line
        NO-LOCK:
    
      ld-price = ld-price +
                 (probeit.sell-price * (probeit.yld-qty / probe.est-qty)).
    END.

    lv-changed = IF ld-price EQ ip-price THEN "" ELSE "S".

    probe.sell-price = ROUND(ld-price, 2).

    RUN calc-fields1.

END PROCEDURE.


/*---------------------------------------------calc-fields--------------------------------------*/ 

PROCEDURE calc-fields1 :

  DEF VAR ld-marg% AS DEC NO-UNDO.
  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.
  DEF VAR ld-brd-m AS DEC NO-UNDO.
  DEF VAR ld-brd-% AS DEC NO-UNDO.
  DEF VAR ld-brdcm AS DEC NO-UNDO.
  DEF VAR ld-brdc$ AS DEC NO-UNDO.
  DEF VAR lv-changed2 LIKE lv-changed NO-UNDO.

  {cec/combasis.i}

  FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode) NO-LOCK.

  IF lv-changed NE "" THEN
 
    ASSIGN
     lv-changed2 = lv-changed
     ld-price    = DEC(lv-price).

    FIND FIRST probe-ref NO-LOCK
        WHERE probe-ref.reftable EQ "probe-ref"
          AND probe-ref.company  EQ probe.company
          AND probe-ref.loc      EQ ""
          AND probe-ref.code     EQ probe.est-no
          AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
    IF AVAIL probe-ref THEN
      v-com = (probe-ref.val[2] + probe-ref.val[3] +
               probe-ref.val[4] + probe-ref.val[5]) / 100000.

    /*IF probe.comm NE 0 THEN*/ v-com = probe.comm.

    ASSIGN
     ld-marg%    = DEC(probe.market-price)
     ld-factc    = DEC(probe.fact-cost)
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DEC(pfullc) - ld-commc
     .

    IF lv-changed EQ "S" THEN
      ASSIGN
       ld-price = DEC(probe.sell-price)
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).

    ELSE DO:
      IF lv-changed EQ "BC$" THEN
        ld-price = (ld-brdc$ / (probe.est-qty / 1000)) + ld-brd-m.

      ELSE
      IF lv-changed EQ "BCM" THEN
        ld-price = ld-brdcm + ld-brd-m.

      ELSE
      IF lv-changed EQ "B" THEN
        ld-price = ld-brd-m / (ld-brd-% / 100).

      ELSE DO:
        IF lv-changed EQ "G" THEN DO:
          v-pct = DEC(probe.gross-profit).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
        END.
      
        ELSE v-pct = DEC(probe.net-profit).

        IF ll-use-margin THEN
          ASSIGN
           v-com       = 0
           lv-changed2 = "N"
           v-pct       = IF lv-changed EQ "M" THEN ld-marg% ELSE (v-pct + v-com).

        RUN custom/sellpric.p ("",
                               lv-changed2,
                               v-basis,
                               ld-factc,
                               ld-fullc - ld-factc,
                               v-com,
                               v-pct,
                               OUTPUT ld-price,
                               OUTPUT ld-commc).
      END.

      IF v-round NE "PENNY" THEN DO:
        IF v-round EQ "DOLLAR" THEN DO:
          {sys/inc/roundup.i ld-price}
        END.
        lv-changed = "".
      END.
    END.

    ld-marg% = ROUND((ld-price - ld-fullc) / ld-price * 100,2).

    IF ll-use-margin THEN DO:  /* Get Commission% */
      RUN est/getsmanmtrx.p (ROWID(est), "C",
                             INPUT-OUTPUT v-com,
                             INPUT-OUTPUT ld-marg%).

      ld-commc = ld-price * v-com / 100.
    END.

    ASSIGN
     ld-brd-% = ld-brd-m / ld-price * 100
     ld-brdcm = ld-price - ld-brd-m
     ld-brdc$ = ld-brdcm * (probe.est-qty / 1000)
     ld-fullc = ld-fullc + ld-commc.

    /*IF probe.comm THEN */
      probe.comm = ROUND(v-com, 2).

    probe.full-cost = ROUND(ld-fullc, 2) .



    IF lv-changed NE "M"  THEN
      probe.market-price = ROUND(ld-marg%, 2).

    IF lv-changed NE "S"  THEN
      probe.sell-price = ROUND(ld-price, 2) .
        
    IF lv-changed NE "N" THEN
      probe.net-profit = ROUND(((1 - (ld-fullc / ld-price)) * 100),2) .

    IF lv-changed NE "G" THEN
      probe.gross-profit = ROUND(((1 - (ld-factc / ld-price)) * 100), 2) .

    lv-changed = lv-changed2.


END PROCEDURE.





