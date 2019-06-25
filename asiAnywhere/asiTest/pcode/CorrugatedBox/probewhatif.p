            /*------------------------------------------------------------------------
    File        : probewhatif.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all probe

    Author(s)   : 
    Created     : aug 6 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttProbeWhitifCal NO-UNDO 
    FIELD gross      AS DECIMAL  
    FIELD net        AS DECIMAL   
    FIELD sellprice  AS decimal
    FIELD probeq     AS  CHAR
    FIELD board      AS DECIMAL
    FIELD CONTm      AS DECIMAL
    FIELD cont       AS DECIMAL
    FIELD marketprice AS DECIMAL 
    FIELD factcost   AS DECIMAL 
    FIELD fullcost   AS DECIMAL 
    FIELD comm       AS DECIMAL .

DEFINE DATASET dsProbeWhitifCal FOR ttProbeWhitifCal.

DEFINE INPUT PARAMETER prmAction      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmChange      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmGross       AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmNet         AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSellPrice   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmQ           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBoardM       AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBoard       AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBoardContM  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBoardCont   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMarketprice AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFactCost    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFullCost    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLine   AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsProbeWhitifCal .
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmUser       = ? THEN ASSIGN prmUser       = "".
IF prmChange     = ? THEN ASSIGN prmChange     = "".
IF prmGross      = ? THEN ASSIGN prmGross      = 0.
IF prmNet        = ? THEN ASSIGN prmNet        = 0.
IF prmSellPrice  = ? THEN ASSIGN prmSellPrice  = 0.
IF prmQ          = ? THEN ASSIGN prmQ          = "".
IF prmBoard      = ? THEN ASSIGN prmBoard      = 0.
IF prmBoardContM = ? THEN ASSIGN prmBoardContM = 0.
IF prmBoardCont  = ? THEN ASSIGN prmBoardCont  = 0.
               

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

DEF VAR ld-marg% AS DEC NO-UNDO.
  DEF VAR ld-commc AS DEC NO-UNDO.
  DEF VAR ld-factc AS DEC NO-UNDO.
  DEF VAR ld-fullc AS DEC NO-UNDO.
  DEF VAR ld-price AS DEC NO-UNDO.
  DEF VAR ld-brd-m AS DEC NO-UNDO.
  DEF VAR ld-brd-% AS DEC NO-UNDO.
  DEF VAR ld-brdcm AS DEC NO-UNDO.
  DEF VAR ld-brdc$ AS DEC NO-UNDO.
  DEF VAR lv-changed2 LIKE prmChange NO-UNDO.
  DEF VAR v-tmp-set-markup LIKE probe.set-chg NO-UNDO.
  DEF VAR v-tmp-value LIKE probe.set-chg NO-UNDO.
  DEF VAR lv-orig-changed AS CHAR NO-UNDO.  
  DEF VAR lv-price AS CHAR NO-UNDO.
  DEF VAR ll-use-margin AS LOG NO-UNDO.

DEF NEW SHARED BUFFER xeb FOR  eb.
DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR  ef.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEFINE NEW SHARED  VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED  VAR locode AS CHAR NO-UNDO.
ASSIGN cocode = prmComp
       locode = "Main" .

{sys/inc/cerun.i F}
{sys/inc/cerun.i C}
  
 FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
   find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
   
   FIND FIRST probe WHERE probe.company = est.company and probe.est-no = est.est-no AND probe.LINE = prmLine
   AND probe.probe-date ne ? EXCLUSIVE-LOCK NO-ERROR. 

   IF cerunc EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

    FUNCTION display-gp RETURNS DECIMAL ( INPUT ip-type AS DEC ) :
        DEF VAR lv-gp AS DEC NO-UNDO.
        
        FIND FIRST ce-ctrl where ce-ctrl.company = prmComp and 
       ce-ctrl.loc     = "Main" NO-LOCK.
        
        lv-gp = IF ce-ctrl.sell-by EQ "S" THEN
              IF ip-type EQ 1 THEN
                (probe.sell-price - probe.fact-cost) / probe.fact-cost * 100
              ELSE
                (DEC(probe.sell-price) - DEC(probe.fact-cost)) /
                DEC(probe.fact-cost) * 100
            ELSE
              IF ip-type EQ 1 THEN
                probe.gross-profit
              ELSE
                DEC(gross-profit).
                
                RETURN lv-gp.   /* Function return value. */
        END FUNCTION.


        FUNCTION voverall RETURNS DECIMAL ( INPUT ip-type AS DEC )  :
            DEF VAR lv-overall AS DEC NO-UNDO.
            IF AVAIL probe THEN
                lv-overall = ROUND((IF ip-type EQ 1 THEN probe.sell-price
                                        ELSE DEC(probe.sell-price)) / probe.bsf,2).
                 ELSE lv-overall = 0.
                      IF lv-overall = ? then lv-overall = 0.

                      RETURN lv-overall.   /* Function return value. */

         END FUNCTION.

         FUNCTION cvt-time RETURNS CHARACTER ( input ip-time as int ) :
             def var ls-time as cha no-undo.
             ls-time = string(ip-time,"HH:MM:SS").
             RETURN ls-time.   /* Function return value. */
         END FUNCTION.

          {cec/combasis.i} 

if prmAction = "cal" then do:
  
  {sys/inc/ceround.i}

  FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
            ce-ctrl.loc     = locode)  NO-LOCK.

  IF prmChange NE "" THEN DO:

    IF est.est-type EQ 6 AND probe.set-chg NE 0 /*AND vmclean2*/ THEN
       v-tmp-set-markup = probe.set-chg.

    ASSIGN
     lv-orig-changed = prmChange
     v-com = probe.comm
     lv-changed2 = prmChange
     ld-price    = DEC(prmSellPrice)
     ld-marg%    = DEC(prmMarketprice)
     ld-factc    = DEC(prmFactCost)
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DEC(prmFullCost) -
                   ld-commc
     ld-brd-m    = DEC(prmBoardM)
     ld-brd-%    = DEC(prmBoard)
     ld-brdcm    = DEC(prmBoardContM)
     ld-brdc$    = DEC(prmBoardCont).

    IF prmChange EQ "S" THEN
      ASSIGN
       ld-price = DEC(prmSellPrice)
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).

    ELSE DO:
      IF prmChange EQ "BC$" THEN
        ld-price = (ld-brdc$ / (probe.est-qty / 1000)) + ld-brd-m.

      ELSE
      IF prmChange EQ "BCM" THEN
        ld-price = ld-brdcm + ld-brd-m.

      ELSE
      IF prmChange EQ "B" THEN
        ld-price = ld-brd-m / (ld-brd-% / 100).

      ELSE DO:
        IF prmChange EQ "G" THEN DO:
          v-pct = DEC(prmGross).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
        END.
      
        ELSE v-pct = DEC(prmNet).
            
        IF ll-use-margin THEN
          ASSIGN
           v-com       = 0
           lv-changed2 = "N"
           v-pct       = IF prmChange EQ "M" THEN ld-marg% ELSE (v-pct + v-com).
     
        RUN custom/sellpric.p ("",
                               lv-changed2,
                               v-basis,
                               ld-factc,
                               ld-fullc - ld-factc,
                               v-com,
                               v-pct + v-tmp-set-markup,
                               OUTPUT ld-price,
                               OUTPUT ld-commc).
        
      END.

      IF v-round NE "PENNY" THEN DO:
        IF v-round EQ "DOLLAR" THEN DO:
          {sys/inc/roundup.i ld-price}
        END.
        prmChange = "".
      END.
    END.

    ld-marg% = ROUND((ld-price - ld-fullc) / ld-price * 100,2).

    IF est.est-type EQ 6 AND probe.set-chg NE 0 /*AND vmclean2*/ THEN
       ld-marg% = ld-marg% - v-tmp-set-markup.
    
    IF ll-use-margin THEN DO:  /* Get Commission% */
      RUN est/getsmanmtrx.p (ROWID(est), "C",
                             INPUT-OUTPUT v-com,
                             INPUT-OUTPUT ld-marg%).

      ld-commc = ld-price * v-com / 100.
    END.

    IF est.est-type EQ 6 AND probe.set-chg NE 0 /*AND vmclean2*/ AND
       NOT(prmChange NE "M" AND lv-orig-changed NE "M") THEN
       ld-marg% = ld-marg% + v-tmp-set-markup.

    ASSIGN
     ld-brd-% = ld-brd-m / ld-price * 100
     ld-brdcm = ld-price - ld-brd-m
     ld-brdc$ = ld-brdcm * (probe.est-qty / 1000)
     ld-fullc = ld-fullc + ld-commc.

    
    CREATE ttProbeWhitifCal .
    ASSIGN
        ttProbeWhitifCal.fullcost = ROUND(ld-fullc,2) .

    IF prmChange NE "BC$" THEN ASSIGN
        ttProbeWhitifCal.cont  = ROUND(ld-brdc$,2) .

    IF prmChange NE "BCM"  THEN ASSIGN 
        ttProbeWhitifCal.CONTm = ROUND(ld-brdcm,2) .

    IF prmChange NE "B" THEN ASSIGN
        ttProbeWhitifCal.board  = ROUND(ld-brd-%,2) .

    IF prmChange NE "M" AND lv-orig-changed NE "M"  THEN ASSIGN 
        ttProbeWhitifCal.marketprice  = ROUND(ld-marg%,2) .

    IF prmChange NE "S"  THEN ASSIGN
      ttProbeWhitifCal.sellprice = ROUND(ld-price,2) .
        
    IF prmChange NE "N"  THEN 
    DO:
      IF probe.set-chg NE 0 AND est.est-type EQ 6 /*AND vmclean2*/ THEN
         ttProbeWhitifCal.net = ROUND(((1 - (ld-fullc / ld-price)) * 100 - probe.set-chg),2) . 
      ELSE
         ttProbeWhitifCal.net = ROUND(((1 - (ld-fullc / ld-price)) * 100),2) .
    END.

    IF prmChange NE "G" THEN ASSIGN
        ttProbeWhitifCal.gross = ROUND(((1 - (ld-factc / ld-price)) * 100),2) .

    /*ASSIGN
       ttProbeWhitifCal.comm:SCREEN-VALUE IN BROWSE {&browse-name} =
                  STRING(v-com,probe.comm:FORMAT IN BROWSE {&browse-name})*/
       prmChange = lv-changed2.
  
   /* RUN save-fields.*/
  END.

END.  /*if prmAction <> "search" then do*/ 

/******************************** fold cal whatif***************************/

IF prmAction = "FoldingWhatifCal" THEN DO:

 DEF BUFFER probe-ref FOR reftable.    
  FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
            ce-ctrl.loc     = locode)  NO-LOCK.

  IF prmChange NE "" THEN DO :
    ASSIGN
     lv-changed2 = prmChange
     ld-price    = DEC(prmSellPrice).
MESSAGE "hvhghggfhg" prmChange .
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

MESSAGE "value" prmMarketprice prmFactCost prmFullCost prmSellPrice prmGross prmNet .

    ASSIGN
     ld-marg%    = DEC(prmMarketprice)
     ld-factc    = DEC(prmFactCost)
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DEC(prmFullCost) -
                   ld-commc   .
     

    IF prmChange EQ "S" THEN
      ASSIGN
       ld-price = DEC(prmSellPrice)
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).

    ELSE DO:
      IF prmChange EQ "BC$" THEN
        ld-price = (ld-brdc$ / (probe.est-qty / 1000)) + ld-brd-m.

      ELSE
      IF prmChange EQ "BCM" THEN
        ld-price = ld-brdcm + ld-brd-m.

      ELSE
      IF prmChange EQ "B" THEN
        ld-price = ld-brd-m / (ld-brd-% / 100).

      ELSE DO:
        IF prmChange EQ "G" THEN DO:
          v-pct = DEC(prmGross).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
        END.
      
        ELSE v-pct = DEC(prmNet).

        IF ll-use-margin THEN
          ASSIGN
           v-com       = 0
           lv-changed2 = "N"
           v-pct       = IF prmChange EQ "M" THEN ld-marg% ELSE (v-pct + v-com).

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
        prmChange = "".
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

     MESSAGE "ld-marg%" ld-marg% ld-fullc ld-commc .

    CREATE ttProbeWhitifCal .

     ASSIGN 
         ttProbeWhitifCal.comm = v-com .
    ASSIGN 
        ttProbeWhitifCal.fullcost = ROUND(ld-fullc,2) .
    

    IF prmChange NE "M" THEN

      ttProbeWhitifCal.marketprice = ROUND(ld-marg%,2) .

    IF prmChange NE "S"  THEN
        ttProbeWhitifCal.sellprice  = ROUND(ld-price,2) . 

    IF prmChange NE "N" THEN
        ttProbeWhitifCal.net = ROUND( ((1 - (ld-fullc / ld-price)) * 100),2) .

    IF prmChange NE "G"  THEN
        ttProbeWhitifCal.gross = ((1 - (ld-factc / ld-price)) * 100).

    prmChange = lv-changed2.
      
        IF lv-changed2 NE "S" THEN 
            ttProbeWhitifCal.gross = (display-gp (0)).
  
  END.
END.
