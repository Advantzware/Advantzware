

/*------------------------------------------------------------------------
    File        : FoldMiscSub.p
    Purpose     : FoldMiscSub
    Syntax      :

    Description : Return a Dataset of Folding Misc/Sub 
    Author(s)   : 
    Created     : 13 Mar 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorrPrint NO-UNDO
        FIELD vEstimate            AS CHAR FORMAT "x(8)" 
        FIELD vEstDate             AS DATE  
        FIELD vForm                AS INTEGER
        FIELD vFormQty             AS INTEGER
        FIELD vBlk                 AS INTEGER
        FIELD vBlkQty              AS INTEGER
        FIELD vCustPart            AS CHAR    
        FIELD vQty                 AS INTEGER  
        FIELD vTotalFactCost       AS DECIMAL 
        FIELD vFullCost            AS DECIMAL
        FIELD vMargin              AS DECIMAL 
        FIELD vGross               AS DECIMAL 
        FIELD vNet                 AS DECIMAL 
        FIELD vSellPrice           AS DECIMAL   
        FIELD vTotalSheet          AS INTEGER  
        FIELD vQty2                AS CHAR 
        FIELD vPriceBsf            AS DECIMAL 
        FIELD vProbeDate           AS DATE
        FIELD vBoardM              AS DECIMAL 
        FIELD vBoard               AS DECIMAL
        FIELD vBoardContM          AS DECIMAL 
        FIELD vBoardCont           AS DECIMAL 
        FIELD vProbeBy             AS CHAR   
        FIELD vTotalMsf            AS DECIMAL  
        FIELD vTime                AS CHAR 
        FIELD vLine                AS INT
        FIELD vFile                AS CHAR
        FIELD vType                AS INT
        
        .
DEFINE DATASET dsCorrPrint FOR ttCorrPrint .

DEFINE INPUT PARAMETER prmUser                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmType                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstDate                   AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmForm                      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmFormQty                   AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmBlk                       AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmBlkQty                    AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty                       AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmTotalFactCost             AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmFullCost                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmMargin                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmGross                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmNet                       AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSellPrice                 AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmTotalSheet                AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER prmQty2                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPriceBsf                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmProbeDate                 AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmBoardM                    AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBoard                     AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBoardContM                AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmBoardCont                 AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmProbeBy                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTotalMsf                  AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmTime                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLine                      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmBlank                     AS INT         NO-UNDO.
DEFINE OUTPUT PARAMETER cError                      AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrPrint.

IF prmUser             = ?  THEN ASSIGN prmUser               = "".
IF prmAction           = ?  THEN ASSIGN prmAction             = "".
IF prmType             = ?  THEN ASSIGN prmType               = "".
IF prmComp             = ?  THEN ASSIGN prmComp               = "".
IF prmEstimate         = ?  THEN ASSIGN prmEstimate           = "".
IF prmForm             = ?  THEN ASSIGN prmForm               = 0.
IF prmFormQty          = ?  THEN ASSIGN prmFormQty            = 0.
IF prmBlk              = ?  THEN ASSIGN prmBlk                = 0.
IF prmBlkQty           = ?  THEN ASSIGN prmBlkQty             = 0.
IF prmCustPart         = ?  THEN ASSIGN prmCustPart           = "".
IF prmQty              = ?  THEN ASSIGN prmQty                = 0.
IF prmTotalFactCost    = ?  THEN ASSIGN prmTotalFactCost      = 0.
IF prmFullCost         = ?  THEN ASSIGN prmFullCost           = 0.
IF prmMargin           = ?  THEN ASSIGN prmMargin             = 0.
IF prmGross            = ?  THEN ASSIGN prmGross              = 0.
IF prmNet              = ?  THEN ASSIGN prmNet                = 0.
IF prmSellPrice        = ?  THEN ASSIGN prmSellPrice          = 0.
IF prmTotalSheet       = ?  THEN ASSIGN prmTotalSheet         = 0.
IF prmQty2             = ?  THEN ASSIGN prmQty2               = "".
IF prmPriceBsf         = ?  THEN ASSIGN prmPriceBsf           = 0.
IF prmBoardM           = ?  THEN ASSIGN prmBoardM             = 0.
IF prmBoard            = ?  THEN ASSIGN prmBoard              = 0.
IF prmBoardContM       = ?  THEN ASSIGN prmBoardContM         = 0.
IF prmBoardCont        = ?  THEN ASSIGN prmBoardCont          = 0.
IF prmProbeBy          = ?  THEN ASSIGN prmProbeBy            = "".
IF prmTotalMsf         = ?  THEN ASSIGN prmTotalMsf           = 0.
IF prmTime             = ?  THEN ASSIGN prmTime               = "".
IF prmLine             = ?  THEN ASSIGN prmLine               = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}
{sys/inc/var.i "new shared"}

def var voverall as dec form ">>,>>>,>>9.99" no-undo.
DEF VAR ll-no-valid AS LOG NO-UNDO.
DEF VAR lv-valid-profit AS DEC NO-UNDO.
DEF VAR lv-changed AS CHAR NO-UNDO.
DEF VAR lv-changedm AS CHAR NO-UNDO.
DEF VAR lv-changedn AS CHAR NO-UNDO.
DEF VAR lv-changedg AS CHAR NO-UNDO.
DEF VAR lv-changedb AS CHAR NO-UNDO.
DEF VAR lv-changedc AS CHAR NO-UNDO.
DEF VAR lv-changedcm AS CHAR NO-UNDO.
DEF VAR lv-comm AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_company  AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VAR vTxtFile AS CHAR NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

ASSIGN 
    cocode = prmComp
    g_company  = prmComp .
ASSIGN 
    locode = "Main" 
    g_loc  = locode .
DEF NEW SHARED BUFFER xeb FOR  eb.
DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR  ef.
DEF VAR ld AS DEC NO-UNDO.
def new shared var tmp-dir as cha no-undo.

               
 DEF BUFFER b-ef FOR ef.
 DEF BUFFER b-eb FOR eb.
 DEF VAR li AS INT NO-UNDO.
 DEFINE VAR bi AS INT NO-UNDO.

  DEF VAR ld-tot-fact AS DEC NO-UNDO.
  DEF VAR ld-tot-full AS DEC NO-UNDO.
  DEF VAR ld-tot-pric AS DEC NO-UNDO.
  DEF BUFFER b-reftable FOR reftable.
      DEF VAR ll-use-margin AS LOG NO-UNDO.
      DEF VAR ld-price AS DEC NO-UNDO.
      
DEF VAR lv-fullc AS DEC NO-UNDO.
DEF VAR lv-gprof AS DEC NO-UNDO.
DEF VAR lv-nprof AS DEC NO-UNDO.
DEF VAR lv-price AS DEC NO-UNDO.
DEF VAR lv-brd-% AS DEC NO-UNDO.
DEF VAR lv-brdcm AS DEC NO-UNDO.
DEF VAR lv-brdc$ AS DEC NO-UNDO.
DEF VAR lv-changed2 LIKE lv-changed NO-UNDO.
def var lv-tot-msf as dec no-undo.
DEF NEW SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.


DEF VAR cerunc-dec AS DEC NO-UNDO.
DEF VAR v-prt-note AS LOG NO-UNDO.
DEF VAR v-prt-box AS LOG NO-UNDO.
DEF VAR v-from-dept AS cha NO-UNDO.
DEF VAR v-to-dept AS cha NO-UNDO.
def var ls-outfile as cha no-undo.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

DEF TEMP-TABLE w-probeit LIKE probeit
    FIELD mat-cost   LIKE probe.mat-cost
    FIELD lab-cost   LIKE probe.lab-cost
    FIELD vo-cost    LIKE probe.vo-cost
    FIELD fo-cost    LIKE probe.fo-cost
    FIELD probe-date LIKE probe.probe-date.

DEF VAR ceprep-cha AS CHAR NO-UNDO.

/**********************************Update**************************************/
IF prmAction = "Update" THEN DO:


    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
    find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
  find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
  find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

  FIND FIRST probe WHERE probe.company = eb.company and probe.est-no = eb.est-no AND probe.LINE = prmLine
   AND probe.probe-date ne ? EXCLUSIVE-LOCK NO-ERROR. 

  FIND FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
     reftable.company  EQ probe.company AND 
     reftable.loc      EQ ""            AND 
     reftable.code     EQ probe.est-no  AND 
     reftable.code2    EQ STRING(probe.line,"9999999999") EXCLUSIVE-LOCK 
      NO-ERROR.

    IF AVAIL probe AND AVAIL reftable THEN DO:
            
                ASSIGN
                
                probe.gross-profit    = prmGross
                probe.net-profit      = prmNet
                probe.sell-price      = prmSellPrice
                probe.do-quote        = IF prmQty2 = "Y" THEN TRUE ELSE FALSE
                reftable.val[3]       = prmBoard
                reftable.val[4]       = prmBoardContM
                reftable.val[5]       = prmBoardCont
                
                    .
        END.
 /* END.*/
  ASSIGN prmAction = "Select" .
END.

/*********************************End Update**********************************/
/*********************************Delete*************************************/
IF prmAction = "Delete" THEN DO:
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
    FIND FIRST probe WHERE probe.company = eb.company and probe.est-no = eb.est-no AND probe.LINE = prmLine
      AND probe.probe-date ne ? EXCLUSIVE-LOCK 
         NO-ERROR. 
     FIND FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
        reftable.company  EQ probe.company AND 
        reftable.loc      EQ ""            AND 
        reftable.code     EQ probe.est-no  AND 
        reftable.code2    EQ STRING(probe.line,"9999999999") EXCLUSIVE-LOCK 
         NO-ERROR.
     
        
             DELETE probe.
             DELETE reftable.
     
END.
/********************************End Delete**********************************/

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

/********************************Select****************************************/

IF prmAction = "Select" THEN DO:
    
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
    

    FOR EACH probe WHERE probe.company = eb.company and probe.est-no = eb.est-no  
      AND probe.probe-date ne ? NO-LOCK, 
      FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
        reftable.company  EQ probe.company AND 
        reftable.loc      EQ ""            AND 
        reftable.code     EQ probe.est-no  AND 
        reftable.code2    EQ STRING(probe.line,"9999999999") EXCLUSIVE-LOCK 
        BY probe.company BY probe.est-no BY probe.probe-date BY probe.est-qty:
  
  if avail probe then lv-tot-msf = probe.tot-lbs / 1000.
  else lv-tot-msf = 0.

       
         ASSIGN
             reftable.val[2] = reftable.val[1] / (probe.est-qty / 1000)
             reftable.val[3] = reftable.val[2] / probe.sell-price * 100
             reftable.val[4] = probe.sell-price - reftable.val[2]
             reftable.val[5] = reftable.val[4] * (probe.est-qty / 1000).      

        IF AVAIL est AND AVAIL ef AND AVAIL eb THEN DO:
            CREATE ttCorrPrint.
            ASSIGN
                ttCorrPrint.vEstimate             = est.est-no   
                ttCorrPrint.vEstDate              = est.est-date
                ttCorrPrint.vForm                 = ef.form-no  
                ttCorrPrint.vFormQty              = est.form-qty    
                ttCorrPrint.vBlk                  = eb.blank-no
                ttCorrPrint.vBlkQty               = ef.blank-qty
                ttCorrPrint.vCustPart             = eb.part-no
                ttCorrPrint.vQty                  = probe.est-qty
                ttCorrPrint.vTotalFactCost        = probe.fact-cost
                ttCorrPrint.vFullCost             = probe.full-cost
                ttCorrPrint.vMargin               = probe.market-price
                ttCorrPrint.vGross                = probe.gross-profit /* display-gp(probe.gross-profit)*/
                ttCorrPrint.vNet                  = probe.net-profit
                ttCorrPrint.vSellPrice            = probe.sell-price
                ttCorrPrint.vTotalSheet           = probe.gsh-qty
                ttCorrPrint.vQty2                 = IF probe.do-quote = TRUE THEN "Y" ELSE "N"
                ttCorrPrint.vPriceBsf             = voverall(voverall)
                ttCorrPrint.vProbeDate            = probe.probe-date
                ttCorrPrint.vBoardM               = reftable.val[2]
                ttCorrPrint.vBoard                = reftable.val[3]
                ttCorrPrint.vBoardContM           = reftable.val[4]
                ttCorrPrint.vBoardCont            = reftable.val[5]
                ttCorrPrint.vProbeBy              = probe.probe-user
                ttCorrPrint.vTotalMsf             = lv-tot-msf
                ttCorrPrint.vTime                 = cvt-time(probe.probe-time)
                ttCorrPrint.vLine                 = probe.LINE
                  
                    .
          END. 
       END. 
   END.

/*********************************End Select***********************************/

   IF prmAction = "PrintTop" THEN DO:
    FIND FIRST est WHERE est.est-no =   FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ est.est-no
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
   
    /*DISABLE TRIGGERS FOR LOAD OF ef.*/
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmForm
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        bi = bi + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = bi.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
  

  FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmForm   AND eb.blank-no = prmBlank   NO-LOCK NO-ERROR.
      IF AVAIL eb THEN DO:
          CREATE ttCorrPrint.
              ASSIGN
                ttCorrPrint.vEstimate             = est.est-no   
                ttCorrPrint.vEstDate              = est.est-date
                ttCorrPrint.vForm                 = ef.form-no  
                ttCorrPrint.vFormQty              = est.form-qty    
                ttCorrPrint.vBlk                  = eb.blank-no
                ttCorrPrint.vBlkQty               = bi
                ttCorrPrint.vCustPart             = eb.part-no
                ttCorrPrint.vType                 = est.est-type . 
       END.
      
   END.

/*********************************View****************************************/

   IF prmAction = "View" THEN DO:
    
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
    
    FIND FIRST probe WHERE probe.company = eb.company and probe.est-no = eb.est-no AND probe.LINE = prmLine 
      AND probe.probe-date ne ? NO-LOCK NO-ERROR . 
     FIND FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
        reftable.company  EQ probe.company AND 
        reftable.loc      EQ ""            AND 
        reftable.code     EQ probe.est-no  AND 
        reftable.code2    EQ STRING(probe.line,"9999999999") NO-LOCK 
         NO-ERROR.



    
  
  if avail probe then lv-tot-msf = probe.tot-lbs / 1000.
  else lv-tot-msf = 0.

     

        IF AVAIL est AND AVAIL ef AND AVAIL eb THEN DO:
            CREATE ttCorrPrint.
            ASSIGN
                ttCorrPrint.vEstimate             = est.est-no   
                ttCorrPrint.vEstDate              = est.est-date
                ttCorrPrint.vForm                 = ef.form-no  
                ttCorrPrint.vFormQty              = est.form-qty    
                ttCorrPrint.vBlk                  = eb.blank-no
                ttCorrPrint.vBlkQty               = ef.blank-qty
                ttCorrPrint.vCustPart             = eb.part-no
                ttCorrPrint.vQty                  = probe.est-qty
                ttCorrPrint.vTotalFactCost        = probe.fact-cost
                ttCorrPrint.vFullCost             = probe.full-cost
                ttCorrPrint.vMargin               = probe.market-price
                ttCorrPrint.vGross                = probe.gross-profit  /*display-gp(probe.gross-profit)*/
                ttCorrPrint.vNet                  = probe.net-profit
                ttCorrPrint.vSellPrice            = probe.sell-price
                ttCorrPrint.vTotalSheet           = probe.gsh-qty
                ttCorrPrint.vQty2                 = IF probe.do-quote = TRUE THEN "Y" ELSE "N"
                ttCorrPrint.vPriceBsf             = voverall(voverall)
                ttCorrPrint.vProbeDate            = probe.probe-date
                ttCorrPrint.vBoardM               = reftable.val[2]
                ttCorrPrint.vBoard                = reftable.val[3]
                ttCorrPrint.vBoardContM           = reftable.val[4]
                ttCorrPrint.vBoardCont            = reftable.val[5]
                ttCorrPrint.vProbeBy              = probe.probe-user
                ttCorrPrint.vTotalMsf             = lv-tot-msf
                ttCorrPrint.vTime                 = cvt-time(probe.probe-time)
                ttCorrPrint.vLine                 = probe.LINE
                  
                    .
          END. 
       
   END.


/******************************End View **************************************/

/******************************Create Quote **********************************/

IF prmAction = "Quote" THEN DO:
    

def buffer xprobe for probe.
DEF BUFFER bf-qhd FOR quotehd.
DEF BUFFER bf-notes FOR notes.

/* generating quote record - main code from  quote.a with new table */

/*def var j as int no-undo.
def var i as int no-undo.  */

def buffer bf-eb for eb.
DEF buffer bf-ef for ef.
DEF BUFFER b-probemk FOR reftable.

def var li-q-no LIKE quotehd.q-no NO-UNDO.
def var li-line as int no-undo .  /* for quoteitm.line */
def var ll-new-quote as log no-undo.
def var ll-first as log no-undo.
def var li-first-qty as int no-undo.  /* first qty for quoteitm */
DEF VAR li-prep-qty LIKE quotechg.prep-qty NO-UNDO.
def var li-cnt as int no-undo.
def var ld-cost as dec no-undo.
def var li-value as int no-undo.
DEF VAR lv-quo-price-char AS CHAR NO-UNDO.
DEF VAR v-tot-mat AS DEC NO-UNDO.
DEF VAR v-tot-lab AS DEC NO-UNDO.

/*{est/checkuse.i}*/

/*DISABLE TRIGGERS FOR LOAD OF quoteqty.*/

FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
    
    FIND FIRST probe WHERE probe.company = eb.company and probe.est-no = eb.est-no AND probe.LINE = prmLine 
      AND probe.probe-date ne ? NO-LOCK NO-ERROR . 
     FIND FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
        reftable.company  EQ probe.company AND 
        reftable.loc      EQ ""            AND 
        reftable.code     EQ probe.est-no  AND 
        reftable.code2    EQ STRING(probe.line,"9999999999") NO-LOCK 
         NO-ERROR.


IF CAN-FIND(FIRST xprobe
            WHERE xprobe.company EQ probe.company
              AND xprobe.est-no  EQ probe.est-no
              AND xprobe.do-quote) THEN DO:

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "QUOPRICE"
      NO-LOCK NO-ERROR.
  lv-quo-price-char = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "M".

  /*IF est.est-type EQ 8 THEN DO:
    FIND FIRST xjob NO-ERROR.
    IF NOT AVAIL xjob THEN DO:
      cError =  "You must calculate a combo estimate before creating a quote..." .
      RETURN.
    END.
  END.*/

  assign
   cocode = est.company
   locode = est.loc.

  find last quotehd where quotehd.company = est.company and
                                 quotehd.loc = est.loc and
                                 quotehd.est-no = est.est-no no-error.
  IF AVAIL quotehd THEN DO:
    /* def var li-choice as int no-undo.
     run est/d-qtcnfm.w (output li-choice).
     if li-choice = 1 then /*update */ ll-new-quote = no.
     else if li-choice = 2 then /* new */ ll-new-quote = yes.
     else return. /* cancel */  */
   ASSIGN   ll-new-quote = no.
  end.
  else ll-new-quote = yes.
  li-q-no = if avail quotehd then quotehd.q-no else 1.

  FOR EACH w-probeit:
    DELETE w-probeit.
  END.

  i = 0.

  {cec/probeqt.i}  /* size limitation */

  RUN delete-old-part (li-q-no).

  FOR EACH w-probeit
      BREAK BY w-probeit.cust-no
            BY w-probeit.part-no
            BY w-probeit.bl-qty:

    FIND FIRST bf-eb
        WHERE bf-eb.company EQ w-probeit.company
          AND bf-eb.est-no  EQ w-probeit.est-no
          AND bf-eb.cust-no EQ w-probeit.cust-no
          AND bf-eb.part-no EQ w-probeit.part-no
          AND bf-eb.form-no NE 0
        NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-eb THEN DO:
      FIND FIRST bf-eb
          WHERE bf-eb.company EQ w-probeit.company
            AND bf-eb.est-no  EQ w-probeit.est-no
            AND bf-eb.form-no EQ 0
          NO-LOCK NO-ERROR.
      FIND FIRST eb
          WHERE eb.company EQ w-probeit.company
            AND eb.est-no  EQ w-probeit.est-no
            AND eb.form-no NE 0
          NO-LOCK NO-ERROR.
    END.

    FIND FIRST quotehd
        WHERE quotehd.company EQ cocode
          AND quotehd.loc     EQ locode
          AND quotehd.q-no    EQ li-q-no
        NO-ERROR.
    

    IF FIRST-OF(w-probeit.cust-no) THEN DO:
      IF ll-new-quote OR NOT FIRST(w-probeit.cust-no) OR NOT AVAIL quotehd THEN DO:
        /*FIND LAST quotehd
            WHERE quotehd.company EQ cocode
              AND quotehd.loc     EQ locode
            USE-INDEX q-no NO-LOCK NO-ERROR.
        li-q-no = (IF AVAIL quotehd THEN quotehd.q-no ELSE 0) + 1.*/
        CREATE quotehd.
        quotehd.quo-date = TODAY.
        
      END.

      ASSIGN
       li-q-no            = quotehd.q-no  /* from create trigger */
       /*quotehd.company    = cocode
       quotehd.loc        = locode
       quotehd.q-no       = li-q-no*/
       quotehd.e-num      = est.e-num
       quotehd.est-no     = est.est-no
       quotehd.cust-no    = bf-eb.cust-no
       quotehd.ship-no    = bf-eb.ship-no
       quotehd.ship-id    = bf-eb.ship-id
       quotehd.sold-no    = 1
       quotehd.part-dscr1 = bf-eb.part-dscr1
       quotehd.upd-date   = TODAY
       quotehd.quo-date   = TODAY
       quotehd.upd-user   = prmUser .

      {custom/getrfq.i}
      
      FIND FIRST cust
         where (cust.company = cocode)
            AND cust.cust-no EQ quotehd.cust-no
          NO-LOCK NO-ERROR.
      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ quotehd.cust-no
            AND shipto.ship-id EQ quotehd.ship-id
          NO-LOCK NO-ERROR.
      FOR EACH soldto
          WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ quotehd.cust-no
          NO-LOCK
          BREAK BY soldto.sold-no DESC:
        IF soldto.sold-no EQ 1 OR LAST-OF(soldto.sold-no) THEN LEAVE.
      END.

      ASSIGN
       quotehd.sman      = eb.sman  /* bf-eb.sman */
       quotehd.carrier   = eb.carrier  /*bf-eb.carrier */
       quotehd.del-zone  = eb.dest-code  /* bf-eb.dest-code */
       quotehd.terms     = cust.terms
       quotehd.contact   = cust.contact.

      IF cust.cust-no EQ "TEMP" THEN
        ASSIGN
         quotehd.shipto[1] = eb.ship-name
         quotehd.shipto[2] = eb.ship-addr[1]
         quotehd.shipto[3] = eb.ship-addr[2]
         quotehd.shipto[4] = eb.ship-city + ", " + eb.ship-state +
                             " " + eb.ship-zip
         quotehd.billto[1] = quotehd.shipto[1]
         quotehd.billto[2] = quotehd.shipto[2]
         quotehd.billto[3] = quotehd.shipto[3]
         quotehd.billto[4] = quotehd.shipto[4] 
         quotehd.soldto[1] = quotehd.shipto[1]
         quotehd.soldto[2] = quotehd.shipto[2]
         quotehd.soldto[3] = quotehd.shipto[3]
         quotehd.soldto[4] = quotehd.shipto[4].

      ELSE
        ASSIGN
         quotehd.billto[1] = cust.name
         quotehd.billto[2] = cust.addr[1]
         quotehd.billto[3] = cust.addr[2]
         quotehd.billto[4] = cust.city + ", " + cust.state + " " + cust.zip
         quotehd.shipto[1] = shipto.ship-name
         quotehd.shipto[2] = shipto.ship-addr[1]
         quotehd.shipto[3] = shipto.ship-addr[2]
         quotehd.shipto[4] = shipto.ship-city + ", " + shipto.ship-state +
                             " " + shipto.ship-zip
         quotehd.soldto[1] = soldto.sold-name
         quotehd.soldto[2] = soldto.sold-addr[1]
         quotehd.soldto[3] = soldto.sold-addr[2]
         quotehd.soldto[4] = soldto.sold-city + ", " + soldto.sold-state +
                             " " + soldto.sold-zip.

       /* copy notes from old quotehd */
      IF ll-new-quote THEN DO:
          /*
         FIND FIRST bf-qhd WHERE bf-qhd.company = quotehd.company
                          AND bf-qhd.loc = quotehd.loc
                          AND bf-qhd.est-no = quotehd.est-no
                          AND RECID(bf-qhd) <> RECID(quotehd)
                          NO-LOCK NO-ERROR.
         IF AVAIL bf-qhd THEN DO:
            FOR EACH notes WHERE notes.rec_key = bf-qhd.rec_key NO-LOCK:
                CREATE bf-notes.
                BUFFER-COPY notes EXCEPT notes.rec_key TO bf-notes.
                ASSIGN bf-notes.rec_key = quotehd.rec_key.
            END.
         END.
         */
         FIND FIRST bf-qhd NO-LOCK NO-ERROR.
         IF AVAIL bf-qhd THEN 
             ASSIGN  quotehd.comment[1] = bf-qhd.comment[1]
                     quotehd.comment[2] = bf-qhd.comment[2]
                     quotehd.comment[3] = bf-qhd.comment[3]
                     quotehd.comment[4] = bf-qhd.comment[4]
                     quotehd.comment[5] = bf-qhd.comment[5].
      END.

      IF est.est-type GE 7 THEN DO:
        /*FOR EACH quoteitm OF quotehd:
          DELETE quoteitm.
        END.*/

        FOR EACH quotechg OF quotehd:
          DELETE quotechg.
        END.
      END.
    END.

    FIND FIRST quoteitm
        WHERE quoteitm.company EQ quotehd.company
          AND quoteitm.loc     EQ quotehd.loc
          AND quoteitm.q-no    EQ quotehd.q-no
          AND quoteitm.part-no EQ w-probeit.part-no
        NO-ERROR.

    IF FIRST-OF(w-probeit.part-no) THEN DO:
      FIND FIRST bf-ef
          WHERE bf-ef.company  EQ est.company
            AND bf-ef.est-no   EQ est.est-no
            AND (bf-ef.form-no EQ bf-eb.form-no OR est.est-type EQ 6)
          NO-LOCK NO-ERROR.

      IF ll-new-quote OR NOT AVAIL quoteitm THEN DO:
        FOR EACH quoteitm
            WHERE quoteitm.company EQ quotehd.company
              AND quoteitm.loc     EQ quotehd.loc
              AND quoteitm.q-no    EQ quotehd.q-no
            NO-LOCK
            BY quoteitm.line DESC:
          li-line = quoteitm.line + 1.
          LEAVE.
        END.
        CREATE quoteitm.
      END.
      ELSE li-line = quoteitm.line.

      ASSIGN
       quoteitm.company    = quotehd.company
       quoteitm.loc        = quotehd.loc
       quoteitm.q-no       = quotehd.q-no
       quoteitm.est-no     = quotehd.est-no
       quoteitm.line       = li-line
       quoteitm.style      = bf-eb.style
       quoteitm.part-no    = bf-eb.part-no
       quoteitm.part-dscr1 = bf-eb.part-dscr1
       quoteitm.part-dscr2 = bf-eb.part-dscr2
       quoteitm.i-coldscr  = IF est.est-type EQ 6 THEN eb.i-coldscr
                                                  ELSE bf-eb.i-coldscr
       quoteitm.i-dscr     = bf-ef.brd-dscr
       quoteitm.qty        = w-probeit.bl-qty
       quoteitm.uom        = lv-quo-price-char
       quoteitm.price      = w-probeit.sell-price
       quoteitm.upd-date   = TODAY
       quoteitm.upd-user   = prmUser 
       /*RCO400 only */
       quoteitm.i-no       = bf-eb.stock-no.

      RUN sys/inc/calcsize.p (IF est.est-type EQ 6 THEN ROWID(bf-eb) ELSE ROWID(eb),
                              OUTPUT quoteitm.size).

      IF quoteitm.uom NE "M" THEN
        RUN sys/ref/convcuom.p("M",quoteitm.uom, 0, 0, 0, 0,
                               quoteitm.price, OUTPUT quoteitm.price).
   
      IF bf-ef.brd-dscr EQ '' THEN DO:
        FIND FIRST item
            WHERE item.company EQ cocode
              AND item.i-no    EQ bf-ef.board
            NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
          quoteitm.i-dscr = IF item.i-name   GT "" THEN item.i-name   ELSE
                            IF item.est-dscr GT "" THEN item.est-dscr ELSE
                            item.i-dscr.
      END. /* if brd-dscr */
    END.

    FIND FIRST quoteqty
        WHERE quoteqty.company EQ quoteitm.company
          AND quoteqty.loc     EQ quoteitm.loc
          AND quoteqty.q-no    EQ quoteitm.q-no
          AND quoteqty.LINE    EQ quoteitm.line
          AND quoteqty.qty     EQ w-probeit.bl-qty
          AND quoteqty.rels    EQ INT(w-probeit.freight)
        NO-ERROR.

    IF ll-new-quote OR NOT AVAIL quoteqty THEN CREATE quoteqty.

    ASSIGN
     quoteqty.company    = quoteitm.company
     quoteqty.loc        = quoteitm.loc
     quoteqty.q-no       = quoteitm.q-no
     quoteqty.line       = quoteitm.line
     quoteqty.qty        = w-probeit.bl-qty
     quoteqty.uom        = lv-quo-price-char
     quoteqty.price      = w-probeit.sell-price
     quoteqty.rels       = w-probeit.freight
     quoteqty.quote-date = /*IF ll-new-quote THEN TODAY ELSE */ w-probeit.probe-date
     quoteqty.quote-user = prmUser
     quoteqty.prof-on    = w-probeit.prof-on
     quoteqty.mat-cost   = w-probeit.mat-cost
     quoteqty.lab-cost   = w-probeit.lab-cost
     quoteqty.vo-cost    = w-probeit.vo-cost
     quoteqty.fo-cost    = w-probeit.fo-cost
     quoteqty.tot-lbs    = w-probeit.tot-lbs
     quoteqty.profit     = IF w-probeit.prof-on EQ "Net" THEN w-probeit.net-profit
                                                         ELSE w-probeit.gross-profit.

    IF quoteqty.uom NE "M" THEN
      RUN sys/ref/convcuom.p("M",quoteqty.uom, 0, 0, 0, 0,
                             quoteqty.price, OUTPUT quoteqty.price).

    /* update rfqitem qty - start */
    &SCOPED-DEFINE getrfq
    {custom/rfq-qty.i}
    /* update rfqitem qty - end */

    IF LAST-OF(w-probeit.cust-no) OR est.est-type LT 7 THEN DO:
      li-first-qty = IF est.est-type GE 7 THEN 0 ELSE w-probeit.bl-qty.

      FOR EACH quotechg
          WHERE quotechg.company EQ quoteqty.company
            AND quotechg.loc     EQ quoteqty.loc
            AND quotechg.q-no    EQ quoteqty.q-no
            AND ((quotechg.line  EQ quoteqty.line AND li-first-qty NE 0) OR
                 quotechg.line   EQ 0)
            AND quotechg.qty     EQ li-first-qty:
        DELETE quotechg.
      END.

      FOR EACH bf-ef
          WHERE bf-ef.company EQ quotehd.company
            AND bf-ef.est-no  EQ quotehd.est-no
          NO-LOCK:

        li-prep-qty = 0.

        IF est.est-type GE 7 THEN
        FOR EACH bf-eb FIELDS(bl-qty)
            WHERE bf-eb.company EQ bf-ef.company
              AND bf-eb.est-no  EQ bf-ef.est-no
              AND bf-eb.form-no EQ bf-ef.form-no
              AND bf-eb.cust-no EQ w-probeit.cust-no
            NO-LOCK:
          li-prep-qty = li-prep-qty + bf-eb.bl-qty.
        END.

        ELSE li-prep-qty = w-probeit.bl-qty.

        FOR EACH est-prep
            WHERE est-prep.company EQ bf-ef.company
              AND est-prep.est-no  EQ bf-ef.est-no
              AND est-prep.s-num   EQ bf-ef.form-no
              AND est-prep.simon   EQ "S"
            NO-LOCK:

          /*FIND FIRST quotechg
              WHERE quotechg.company EQ quoteqty.company
                AND quotechg.loc     EQ quoteqty.loc
                AND quotechg.q-no    EQ quoteqty.q-no
                AND ((quotechg.line  EQ quoteqty.line AND li-first-qty NE 0) OR
                     quotechg.line   EQ 0)
                AND quotechg.qty     EQ li-first-qty
                AND quotechg.charge  EQ est-prep.dscr
                AND quotechg.s-num   EQ est-prep.s-num
                AND quotechg.b-num   EQ est-prep.b-num
              NO-ERROR.

          IF NOT AVAIL quotechg THEN*/
          CREATE quotechg.
          ASSIGN
           quotechg.company  = quoteqty.company
           quotechg.loc      = quoteqty.loc
           quotechg.q-no     = quoteqty.q-no
           quotechg.line     = IF li-first-qty EQ 0 THEN 0 ELSE quoteqty.line
           quotechg.qty      = li-first-qty
           quotechg.quote-date = quoteqty.quote-date
           quotechg.charge   = est-prep.dscr
           quotechg.bill     = if est-prep.ml then "M" else "L"
           quotechg.amt      = est-prep.qty * est-prep.cost /
                               (1 - (est-prep.mkup / 100)) *
                               (if est-prep.amtz ne 0 then est-prep.amtz / 100 else 1)
           quotechg.mkup     = est-prep.mkup
           quotechg.cost     = est-prep.cost
           quotechg.amtz     = est-prep.amtz
           quotechg.prep-qty = est-prep.qty
           quotechg.s-num    = est-prep.s-num
           quotechg.b-num    = est-prep.b-num
           quotechg.simon    = est-prep.simon. 

          IF ceprep-cha EQ "Dollar" THEN DO:
             {sys/inc/roundup.i quotechg.amt}
          END.
          ELSE IF ceprep-cha EQ "FiveDollar" THEN DO:
             {sys/inc/roundupfive.i quotechg.amt}
          END.
        END.

        DO j = 1 TO 6:
          IF bf-ef.mis-simon[j] EQ "S" and bf-ef.mis-cost[j] NE "" THEN DO:
            /*FIND FIRST quotechg
                WHERE quotechg.company EQ quoteqty.company
                  AND quotechg.loc     EQ quoteqty.loc
                  AND quotechg.q-no    EQ quoteqty.q-no
                  AND ((quotechg.line  EQ quoteqty.line AND li-first-qty NE 0) OR
                       quotechg.line   EQ 0)
                  AND quotechg.qty     EQ li-first-qty
                  AND quotechg.charge  EQ bf-ef.mis-cost[j]
                  AND quotechg.s-num   EQ bf-ef.mis-snum[j]
                  AND quotechg.b-num   EQ bf-ef.mis-bnum[j]
                NO-ERROR.

            IF NOT AVAIL quotechg THEN*/
            CREATE quotechg.

            IF (bf-ef.mis-labf[j] NE 0 OR bf-ef.mis-labm[j] NE 0) AND
               (bf-ef.mis-matf[j] EQ 0 AND bf-ef.mis-matm[j] EQ 0) THEN
              quotechg.bill = "L".
            ELSE
            IF (bf-ef.mis-labf[j] EQ 0 AND bf-ef.mis-labm[j] EQ 0) AND
               (bf-ef.mis-matf[j] NE 0 OR bf-ef.mis-matm[j] NE 0) THEN
              quotechg.bill = "M".
            ELSE
            IF (bf-ef.mis-labf[j] NE 0 OR bf-ef.mis-labm[j] NE 0) OR
               (bf-ef.mis-matf[j] NE 0 OR bf-ef.mis-matm[j] NE 0) THEN
              quotechg.bill = "T".

            ASSIGN
             quotechg.company  = quoteqty.company
             quotechg.loc      = quoteqty.loc
             quotechg.q-no     = quoteqty.q-no
             quotechg.line     = IF li-first-qty EQ 0 THEN 0 ELSE quoteqty.line
             quotechg.qty      = li-first-qty
             quotechg.quote-date = quoteqty.quote-date
             quotechg.prep-qty = li-prep-qty
             quotechg.s-num    = bf-ef.mis-snum[j]
             quotechg.b-num    = bf-ef.mis-bnum[j]
             quotechg.charge   = bf-ef.mis-cost[j]
             quotechg.labf     = bf-ef.mis-labf[j]
             quotechg.matf     = bf-ef.mis-matf[j]
             quotechg.labm     = bf-ef.mis-labm[j]
             quotechg.matm     = bf-ef.mis-matm[j]
             quotechg.mkup     = bf-ef.mis-mkup[j]
             quotechg.simon    = bf-ef.mis-simon[j]
             quotechg.amtz     = 100.
            
            {est/qt-misc.i "MAT" j}
            quotechg.matm = ld-cost.
            {est/qt-misc.i "LAB" j}

            ASSIGN
             quotechg.labm = ld-cost
             v-tot-mat = (quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000))) /
                         (1 - (quotechg.mkup / 100))
             v-tot-lab = (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000))) /
                         (1 - (quotechg.mkup / 100))
             quotechg.amt = v-tot-mat + v-tot-lab.
            
            IF quotechg.prep-qty NE 0 THEN
               quotechg.cost = ((quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000)))
                             + (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000)))) / quotechg.prep-qty.
            

            IF ceprep-cha EQ "Dollar" THEN DO:
               {sys/inc/roundup.i quotechg.amt}
            END.
            ELSE IF ceprep-cha EQ "FiveDollar" THEN DO:
               {sys/inc/roundupfive.i quotechg.amt}
            END.
          END.
        END.
      END.
    END.

    DELETE w-probeit.
  END.  /* each w-probeit */
END.

IF AVAIL quotechg THEN FIND CURRENT quotechg NO-LOCK NO-ERROR.
IF AVAIL quoteqty THEN FIND CURRENT quoteqty NO-LOCK NO-ERROR.
IF AVAIL quoteitm THEN FIND CURRENT quoteitm NO-LOCK NO-ERROR.
IF AVAIL quotehd  THEN DO:
  FIND CURRENT quotehd  NO-LOCK NO-ERROR.

  RELEASE quotechg.
  RELEASE quoteqty.
  RELEASE quoteitm.

  FOR EACH quoteitm OF quotehd NO-LOCK,
      FIRST bf-eb NO-LOCK
      WHERE bf-eb.company EQ quotehd.company
        AND bf-eb.est-no  EQ quotehd.est-no
        AND bf-eb.part-no EQ quoteitm.part-no,
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ bf-eb.company
        AND itemfg.i-no    EQ bf-eb.stock-no,
      EACH quoteqty OF quoteitm NO-LOCK:

    RUN fg/makenote.p (BUFFER oe-ordl,
                       BUFFER quoteqty,
                       BUFFER ar-invl,
                       NO,
                       itemfg.rec_key).
  END.
END.

/*RUN repo-query (ROWID(probe)).*/


END.  /* end of quote*/
/****************************End Create Quote ********************************/
   
/******************************Print Probe***********************************/
   IF prmAction = "printprob" THEN DO:

       FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
       IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

        assign
        init-dir    = v-webrootpath
        vTxtFile   = 'PrintProb' + '.txt' .
         
    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no NO-LOCK NO-ERROR.
    
    FIND FIRST probe WHERE probe.company = eb.company and probe.est-no = eb.est-no AND probe.LINE = prmLine 
      AND probe.probe-date ne ? NO-LOCK NO-ERROR . 
     FIND FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
        reftable.company  EQ probe.company AND 
        reftable.loc      EQ ""            AND 
        reftable.code     EQ probe.est-no  AND 
        reftable.code2    EQ STRING(probe.line,"9999999999") NO-LOCK 
         NO-ERROR.
    IF AVAIL est AND AVAIL ef AND AVAIL eb THEN DO:
        
        find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
        find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
        find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

    run printProbe.
    END.
    CREATE ttCorrPrint.
    ASSIGN ttCorrPrint.vFile = vTxtFile.

  END.
   

/*****************************End Print Probe*******************************/
   
   PROCEDURE calc-fields :
 
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

  {sys/inc/ceround.i}

  FIND FIRST ce-ctrl where ce-ctrl.company = prmComp and 
       ce-ctrl.loc     = "Main" NO-LOCK.

  IF lv-changed NE "" OR lv-changedg NE "" OR lv-changedn NE "" OR lv-changedm NE "" OR lv-changedb NE "" OR lv-changedc NE "" OR lv-changedcm NE "" THEN
  

    ASSIGN
     v-com       = probe.comm
     lv-changed2 = lv-changed
     ld-price    = DEC(lv-price)
     ld-marg%    = DEC(prmMargin)
     ld-factc    = DEC(prmTotalFactCost)
     ld-commc    = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                   (v-com / 100)   
     ld-fullc    = DEC(prmFullCost) - ld-commc
     ld-brd-m    = DEC(prmBoardM)
     ld-brd-%    = DEC(prmBoard)
     ld-brdcm    = DEC(prmBoardContM)
     ld-brdc$    = DEC(prmBoardCont).

    IF lv-changed EQ "S" OR lv-changedn EQ "S" OR lv-changedm EQ "S" OR lv-changedg EQ "S" OR lv-changedb EQ "S" OR lv-changedc EQ "S" OR lv-changedcm EQ "S" THEN
      ASSIGN
       ld-price = DEC(prmSellPrice)
       ld-commc = (ld-price - (IF v-basis EQ "G" THEN ld-factc ELSE 0)) *
                  (v-com / 100).

    ELSE DO:
      IF lv-changedc EQ "BC$" THEN
        ld-price = (ld-brdc$ / (prmQty / 1000)) + ld-brd-m.

      ELSE
      IF lv-changedcm EQ "BCM" THEN
        ld-price = ld-brdcm + ld-brd-m.

      ELSE
      IF lv-changedb EQ "B" THEN
        ld-price = ld-brd-m / (ld-brd-% / 100).

      ELSE DO:
        IF lv-changedg EQ "G" THEN DO:
          v-pct = DEC(prmGross).

          IF ce-ctrl.sell-by EQ "S" THEN lv-changed2 = "S".
        END.
      
        ELSE v-pct = DEC(prmNet).
            
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

   IF ld-price NE 0 THEN
        ld-marg% = ROUND((ld-price - ld-fullc) / ld-price * 100,2).
   ELSE
        ld-marg% = 1.

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

    prmFullCost = ld-fullc NO-ERROR.

    IF lv-changedc NE "BC$" THEN
      prmBoardCont = ld-brdc$ NO-ERROR.

    IF lv-changedcm NE "BCM" THEN
      prmBoardCont = ld-brdcm NO-ERROR.

    IF lv-changedb NE "B" THEN
      prmBoard = ld-brd-% NO-ERROR.

    IF lv-changedm NE "M" THEN
      prmMargin = ld-marg% NO-ERROR.

    /*IF lv-changed NE "S" THEN
      prmSellPrice = ld-price NO-ERROR.*/
        
    IF lv-changedn NE "N" THEN
      prmNet = ((1 - (ld-fullc / ld-price)) * 100) NO-ERROR.

    IF lv-changedg NE "G" THEN
      prmGross = ((1 - (ld-factc / ld-price)) * 100) NO-ERROR.

    /* IF probe.comm:VISIBLE IN BROWSE {&browse-name} THEN
       probe.comm:SCREEN-VALUE IN BROWSE {&browse-name} =
                 STRING(v-com,probe.comm:FORMAT IN BROWSE {&browse-name}).

    lv-changed = lv-changed2.*/

   /* IF 
       TRIM(prmFullCost)    EQ "?" OR
       TRIM(prmGross) EQ "?" OR
       TRIM(prmNet)   EQ "?" OR
       TRIM(prmSellPrice)   EQ "?" OR
       TRIM(prmBoard)    EQ "?" OR
       TRIM(prmBoardContM)    EQ "?" OR
       TRIM(prmBoardCont)    EQ "?" OR
       ld-price GT 9999999.99            THEN DO:

      MESSAGE "Value(s) invalid, please try again" VIEW-AS ALERT-BOX ERROR.*/

      /*ASSIGN
       prmFullCost    = lv-fullc
       prmNet   = lv-nprof
       prmGross = lv-gprof
       prmSellPrice   = lv-price
       prmBoard    = lv-brd-%
       prmBoardContM    = lv-brdcm
       prmBoardCont    = lv-brdc$.*/

     /* IF probe.comm:VISIBLE IN BROWSE {&browse-name} THEN
         probe.comm:SCREEN-VALUE IN BROWSE {&browse-name} = lv-comm.

      IF lv-changed EQ "BC$" THEN
        APPLY "entry" TO reftable.val[5] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "BCM" THEN
        APPLY "entry" TO reftable.val[4] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "B" THEN
        APPLY "entry" TO reftable.val[3] IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "M" THEN
        APPLY "entry" TO probe.market-price IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "S" THEN
        APPLY "entry" TO probe.sell-price IN BROWSE {&browse-name}.
      ELSE
      IF lv-changed EQ "N" THEN
        APPLY "entry" TO probe.net-profit IN BROWSE {&browse-name}.
      ELSE
        APPLY "entry" TO probe.gross-profit IN BROWSE {&browse-name}.

      lv-changed = "".

      RETURN ERROR.*/
   /* END.*/

    
     /* voverall = (voverall (0)).
      IF lv-changed2 NE "S" THEN 
        prmGross = (display-gp (0)).*/
   

    RUN save-fields.
  

END PROCEDURE.


PROCEDURE save-fields :
  
    ASSIGN
     lv-changed = ""
     lv-fullc   = prmFullCost
     lv-nprof   = prmNet
     lv-gprof   = prmGross
     lv-price   = prmSellPrice
     lv-brd-%   = prmBoard
     lv-brdcm   = prmBoardContM
     lv-brdc$   = prmBoardCont.

   /* IF probe.comm:VISIBLE IN BROWSE {&browse-name} THEN
       lv-comm = probe.comm:SCREEN-VALUE IN BROWSE {&browse-name}.*/
 

END PROCEDURE.


PROCEDURE per-1000 :
 
    DEF BUFFER b-probe FOR probe.

  FOR EACH b-probe
      WHERE b-probe.company EQ probe.company
        AND b-probe.est-no  EQ probe.est-no
      NO-LOCK
      BY b-probe.probe-date
      BY b-probe.est-qty:

    IF b-probe.LINE LT 100 THEN
    DO:
       RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(b-probe.line,"99"),
                     OUTPUT tmp-dir).

       IF INDEX(tmp-dir ,'P',1) > 0 THEN ASSIGN
           tmp-dir  = REPLACE(tmp-dir ,'P',"D").

       if opsys eq "unix" then 
         unix silent cp  value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"99"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"99")).
       else
         dos silent copy value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"99"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"99")).

       RUN get-dir-proc(INPUT trim(est.est-no) + ".a" + string(b-probe.line,"99"),
                        OUTPUT tmp-dir).

       if opsys = "unix" then
          unix silent cat value(tmp-dir + trim(est.est-no) + ".a"
                                                    + string(b-probe.line,"99")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                                    + string(b-probe.line,"99")).
       else /* if opsys = "MSDOS" then */
          dos silent type value(tmp-dir + trim(est.est-no) + ".a"
                                                    + string(b-probe.line,"99")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                                    + string(b-probe.line,"99")).
    END.
    ELSE
    DO:
       RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(b-probe.line,"999"),
                        OUTPUT tmp-dir).

       if opsys eq "unix" then 
         unix silent cp  value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"999"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"999")).
       else
         dos silent copy value(tmp-dir + trim(est.est-no) + ".s" + string(b-probe.line,"999"))
                         value(lv-cebrowse-dir + trim(est.est-no) + ".p" + string(b-probe.line,"999")).

       RUN get-dir-proc(INPUT trim(est.est-no) + ".a" + string(b-probe.line,"999"),
                        OUTPUT tmp-dir).

       if opsys = "unix" then
          unix silent cat value(tmp-dir + trim(est.est-no) + ".a"
                                + string(b-probe.line,"999")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                + string(b-probe.line,"999")).
       else /* if opsys = "MSDOS" then */
          dos silent type value(tmp-dir + trim(est.est-no) + ".a"
                                + string(b-probe.line,"999")) >>
                          value(lv-cebrowse-dir + trim(est.est-no) + ".p"
                                + string(b-probe.line,"999")).
    END.

    tmp-dir = lv-cebrowse-dir.

    RUN cec/probeu3.p (ROWID(b-probe)).
  END.

END PROCEDURE.

PROCEDURE get-dir-proc :
 
    DEF INPUT PARAMETER ip-search AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER op-tmp-dir AS CHAR NO-UNDO.

   DEF VAR viDirCount AS INT NO-UNDO.

   DO viDirCount = 1 TO 3:

      CASE viDirCount:
          WHEN 1 THEN
             op-tmp-dir = lv-cebrowse-dir.
          WHEN 2 THEN
             op-tmp-dir = "users\".
          WHEN 3 THEN
             op-tmp-dir = ".\".
      END CASE.

      IF LOOKUP(SUBSTRING(op-tmp-dir,LENGTH(op-tmp-dir)),"\,/") EQ 0 THEN
         op-tmp-dir = op-tmp-dir + "\".

      op-tmp-dir = REPLACE(op-tmp-dir,"/","\").

      IF viDirCount EQ 3 OR SEARCH(op-tmp-dir + ip-search) NE ? THEN
         LEAVE.
   END.
END PROCEDURE.

PROCEDURE delete-old-part :

  DEF INPUT PARAM ip-q-no LIKE quotehd.q-no NO-UNDO.

  /* delete quoteitm for old part-no */
  DISABLE TRIGGERS FOR LOAD OF quoteitm.
  FOR EACH quoteitm
       WHERE quoteitm.company EQ cocode
         AND quoteitm.loc     EQ locode
         AND quoteitm.q-no    EQ ip-q-no:
    FIND FIRST w-probeit WHERE w-probeit.part-no = quoteitm.part-no NO-LOCK NO-ERROR.
    IF NOT AVAIL w-probeit THEN DELETE quoteitm.
  END.
  /* end delete quoteitm */

END PROCEDURE.



PROCEDURE printProbe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DEFINE INPUT PARAMETER ipPrompt AS LOGICAL NO-UNDO.*/

  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  DEF VAR v-print-fmt AS cha NO-UNDO.
  DEF VAR is-xprint-form AS LOG NO-UNDO.
  DEF VAR v-error AS LOG NO-UNDO.
  DEF VAR v-cinput AS cha FORM "x(255)" NO-UNDO.

  DEF VAR lv-dest AS int NO-UNDO.
  DEF VAR lv-font AS INT NO-UNDO.
  DEF VAR lv-ornt AS cha NO-UNDO.
  DEF VAR lv-lines AS INT NO-UNDO.
  DEF VAR list-name AS cha NO-UNDO.
  DEF VAR init-dir AS cha NO-UNDO.
  DEF VAR ls-fax-file AS cha NO-UNDO.
  DEF VAR ret-code AS INT NO-UNDO.
  DEF VAR ls-mail-file2 AS cha NO-UNDO.
  DEF VAR lv-dir AS CHAR NO-UNDO.
  DEF VAR v-probe-fmt AS CHAR NO-UNDO.

  {est/checkuse.i}

 /* IF ipPrompt THEN DO:
    RUN est/d-estprt.w (OUTPUT v-prt-note,OUTPUT v-prt-box,OUTPUT v-from-dept,
                        OUTPUT v-to-dept,OUTPUT lv-dest,OUTPUT lv-font,
                        OUTPUT lv-ornt,OUTPUT lv-lines,OUTPUT v-error).
    IF v-error THEN RETURN ERROR.

       DEFINE VARIABLE tb_prt-box AS LOGICAL INITIAL no NO-UNDO.
      DEFINE VARIABLE tb_prt-note AS LOGICAL INITIAL yes NO-UNDO.

  END. /* if ipprompt */
  ELSE  */
  ASSIGN
    is-xprint-form = YES
    v-prt-note = YES
    v-prt-box = YES
    v-from-dept = ''
    v-to-dept = 'zzzzz'
    lv-dest = 2
    lv-font = 15
    lv-ornt = 'P'
    lv-lines = 63.

  ASSIGN
     v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
     ls-outfile = lv-cebrowse-dir + trim(est.est-no) + ".p" + string(probe.line,v-probe-fmt).

  FIND CURRENT xest.
  IF xest.est-type LT 7 THEN RUN cec/probeu1.p (RECID(probe)).
  FIND CURRENT xest NO-LOCK.

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt),
                   OUTPUT lv-dir).

  if opsys eq "unix" THEN
     unix silent cp  value(lv-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt))
                     value(ls-outfile).
  ELSE DO:
    find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "CEPrint" no-lock no-error.
    is-xprint-form = avail sys-ctrl and sys-ctrl.char-fld ne 'Text'.
    find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "JOBCARDC" no-lock no-error.
    if avail sys-ctrl THEN ASSIGN v-print-fmt = sys-ctrl.char-fld.
    ELSE v-print-fmt = "".

     IF is-xprint-form THEN lv-lines = 63.
     OUTPUT TO VALUE(ls-outfile) PAGE-SIZE VALUE(lv-lines). /* create .x file with page size */

     input from value(lv-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt)) NO-ECHO.
     repeat:
           v-cinput = "".
           import unformatted v-cinput.

           /*MESSAGE "input" v-cinput.*/
           DEFINE VAR test AS CHAR NO-UNDO.
           test = SUBSTRING(v-cinput,1,2).
            

           if v-cinput eq "" then put skip(1).
           else do:
               IF LOOKUP(SUBSTRING(v-cinput,1,2),"01,02,03,04,05,06,07,08,09,10,11,12") > 0 and
                  SUBSTRING(v-cinput,3,1) EQ "/" AND
                  SUBSTRING(v-cinput,6,1) EQ "/"    THEN PAGE. /*seperate page per form*/
               put unformatted v-cinput skip.
           END.
     end.
/*ASSIGN list-name = v-cinput.*/
  IF NOT is-xprint-form AND v-prt-note THEN RUN print-notes.

     input close.
     OUTPUT CLOSE.
  END.
  ASSIGN
  list-name = ls-outfile
  list-name = init-dir + "PrintProb.txt".
  
  END PROCEDURE.
/*********************************Print Notes******************************/
  PROCEDURE print-notes :

  {custom/notesdef.i}
  DEF VAR v-inst2 AS cha EXTENT 200 NO-UNDO.    
  DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 200 NO-UNDO.
  DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
  DEF VAR lv-k AS INT NO-UNDO.
  
  /*determine number of lines needed*/
  ASSIGN v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

  FOR EACH notes WHERE notes.rec_key = xest.rec_key and
      v-prt-note and notes.note_code >= v-from-dept and
      notes.note_code <= v-to-dept NO-LOCK:
    
    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = k.

    DO i = 1 TO LENGTH(notes.note_text) :        
       IF i - j >= lv-line-chars THEN ASSIGN j = i
                                             lv-got-return = lv-got-return + 1.
              
       v-tmp-lines = ( i - j ) / lv-line-chars.
       {SYS/INC/ROUNDUP.I v-tmp-lines}
       k = v-tmp-lines + lv-got-return +
       IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
    
       IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13) THEN                
       do:
          ASSIGN
             lv-got-return = lv-got-return + 1
             j = i.
       END.
    END.
    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0.
  END.

  lv-k = k.

  {custom/notespr2.i job v-inst2 lv-k "notes.rec_key = xest.rec_key and v-prt-note and notes.note_code >= v-from-dept and notes.note_code <= v-to-dept" }
  
  PUT SKIP(1)
      "Department Notes: " SKIP.

  IF lv-k GT EXTENT(v-dept-inst) THEN lv-k = EXTENT(v-dept-inst).

  DO i = 1 TO lv-k:
     v-dept-inst[i] = v-inst2[i].
     PUT v-dept-inst[i] AT 2 SKIP.
  END.

END PROCEDURE.
/*********************************End Print Notes******************************/
