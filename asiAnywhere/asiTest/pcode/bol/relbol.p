/*------------------------------------------------------------------------
    File        : CorrSpecs.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 21 jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttRelBol NO-UNDO
    FIELD  vSeq     AS INT 
    FIELD  vRelease#   LIKE oe-relh.release#
    FIELD  vTag#    AS cha   
    FIELD  vTrailer  LIKE oe-relh.trailer
    FIELD  vCases  LIKE oe-rell.cases
    FIELD  vIno  AS cha
    FIELD  vIname  AS cha
    FIELD  vOrdNo  LIKE oe-ord.ord-no
    FIELD  vQty  LIKE oe-rell.qty
    FIELD  vLoc  LIKE oe-rell.loc
    FIELD  vLocBin  LIKE oe-rell.loc-bin
    FIELD  vCustNo  LIKE oe-rell.cust-no
    FIELD  vQtyCase  LIKE oe-rell.qty-case
    FIELD  vCasesUnit  LIKE fg-rctd.cases-unit
    FIELD  vPartial  LIKE oe-rell.partial
    
    FIELD  vJobNo LIKE oe-rell.job-no
    FIELD  vJobNo2 LIKE oe-rell.job-no2
    FIELD  vTQty LIKE oe-rell.qty
    FIELD  vLine LIKE oe-rell.line
    FIELD  vWarned AS LOG
    FIELD  vPoNo LIKE oe-boll.po-no

    FIELD  vRelQty AS INTEGER
    FIELD  vScanQty AS INTEGER
    .
DEFINE TEMP-TABLE ttPrintBolpdf NO-UNDO
    FIELD vPdfBolFile AS CHAR .

DEFINE DATASET dsRelBol FOR ttPrintBolpdf.

/*{custom/globdefs.i "new shared"}*/
{sys/inc/VAR.i "new shared" }

{oe/oe-relp1.i NEW}

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER prmSeq        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmRelease      LIKE oe-relh.release#  NO-UNDO.
DEFINE INPUT PARAMETER prmTag        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTrailor        LIKE oe-relh.trailer  NO-UNDO.
DEFINE INPUT PARAMETER prmCases        LIKE oe-rell.cases  NO-UNDO.
DEFINE INPUT PARAMETER prmIno        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmIName        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrdNo        LIKE oe-ord.ord-no NO-UNDO.
DEFINE INPUT PARAMETER prmQty        LIKE oe-rell.qty NO-UNDO.
DEFINE INPUT PARAMETER prmLoc        LIKE oe-rell.loc NO-UNDO.
DEFINE INPUT PARAMETER prmLocBin        LIKE oe-rell.loc-bin NO-UNDO.
DEFINE INPUT PARAMETER prmCustNo        LIKE oe-rell.cust-no NO-UNDO.
DEFINE INPUT PARAMETER prmQtyCase        LIKE oe-rell.qty-case NO-UNDO.
DEFINE INPUT PARAMETER prmCasesUnit        LIKE fg-rctd.cases-unit NO-UNDO.
DEFINE INPUT PARAMETER prmPartial        LIKE oe-rell.partial NO-UNDO.
DEFINE INPUT PARAMETER prmJobNo        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmJobNo2        AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmLine        LIKE oe-rell.line.
DEFINE INPUT PARAMETER prmWarned        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPoNo        LIKE oe-boll.po-no.

DEFINE INPUT PARAMETER prmRelQty        AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmScanQty        AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmFilePath       AS CHAR NO-UNDO .



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRelBol.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

DEFINE OUTPUT PARAMETER prmMailTo AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER prmSubject AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER prmBody AS CHAR NO-UNDO.

IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".                

IF prmSeq       = ?  THEN ASSIGN    prmSeq        = 0.  
IF prmRelease       = ?  THEN ASSIGN    prmRelease        = 0.  
IF prmTag       = ?  THEN ASSIGN    prmTag        = "".  
IF prmTrailor       = ?  THEN ASSIGN    prmTrailor        = "".  
IF prmCases       = ?  THEN ASSIGN    prmCases        = 0.  
IF prmIno       = ?  THEN ASSIGN    prmIno        = "".  
IF prmIName       = ?  THEN ASSIGN    prmIName        = "".  
IF prmOrdNo       = ?  THEN ASSIGN    prmOrdNo        = 0.  
IF prmQty       = ?  THEN ASSIGN    prmQty        = 0.  
IF prmLoc       = ?  THEN ASSIGN    prmLoc        = "".  
IF prmLocBin       = ?  THEN ASSIGN    prmLocBin        = "". 
IF prmCustNo       = ?  THEN ASSIGN    prmCustNo        = "". 
IF prmQtyCase       = ?  THEN ASSIGN    prmQtyCase        = 0. 
IF prmCasesUnit       = ?  THEN ASSIGN    prmCasesUnit        = 0. 
IF prmPartial       = ?  THEN ASSIGN    prmPartial        = 0. 
IF prmJobNo       = ?  THEN ASSIGN    prmJobNo        = "".  
IF prmJobNo2       = ?  THEN ASSIGN    prmJobNo2        = 0.  
IF prmLine       = ?  THEN ASSIGN    prmLine        = 0.        
IF prmWarned       = ?  THEN ASSIGN    prmWarned        = "".     
IF prmPoNo       = ?  THEN ASSIGN    prmPoNo        = "".    


IF prmRelQty       = ?  THEN ASSIGN    prmRelQty        = 0. 
IF prmScanQty       = ?  THEN ASSIGN    prmScanQty        = 0. 


DEF NEW SHARED BUFFER xoe-relh FOR oe-relh.

DEF NEW SHARED VARIABLE pdfname AS CHAR NO-UNDO.

DEF SHARED VAR g-sharpshooter AS LOG NO-UNDO.

DEF TEMP-TABLE tt-rell NO-UNDO LIKE oe-rell
                       FIELD release# LIKE oe-relh.release#
                       FIELD row-id   AS   ROWID
                       INDEX row-id row-id
                       INDEX ord-no company ord-no line i-no
                             po-no rel-no b-ord-no.

DEF TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll
                       FIELD release# LIKE oe-relh.release#
                       INDEX tt-boll b-no ord-no i-no po-no rel-no b-ord-no
                       INDEX release# release# ord-no i-no rel-no b-ord-no po-no.

DEF TEMP-TABLE tt-boll2 NO-UNDO LIKE tt-boll
                       INDEX b-no b-no.

DEF TEMP-TABLE tt-relbol NO-UNDO 
                         FIELD release# LIKE oe-relh.release#
                         FIELD tag# AS cha
                         FIELD i-no AS cha FORM "x(15)"
                         FIELD i-name AS cha FORM "x(30)"
                         FIELD ord-no LIKE oe-ord.ord-no
                         FIELD job-no LIKE oe-rell.job-no
                         FIELD job-no2 LIKE oe-rell.job-no2
                         FIELD loc LIKE oe-rell.loc
                         FIELD loc-bin LIKE oe-rell.loc-bin
                         FIELD cust-no LIKE oe-rell.cust-no
                         FIELD cases LIKE oe-rell.cases
                         FIELD qty-case LIKE oe-rell.qty-case
                         FIELD cases-unit LIKE fg-rctd.cases-unit
                         FIELD partial LIKE oe-rell.partial
                         FIELD qty LIKE oe-rell.qty
                         FIELD t-qty LIKE oe-rell.qty
                         FIELD line LIKE oe-rell.line
                         FIELD seq AS INT
                         FIELD warned AS LOG
                         FIELD po-no LIKE oe-boll.po-no
                         /* gdm - 10160906 */
                         FIELD trailer# LIKE oe-relh.trailer
                         INDEX release# release# ord-no i-no po-no.

DEF TEMP-TABLE tt-csvrelbol NO-UNDO                                                       
                            FIELD seq AS INT
                            FIELD release# LIKE oe-relh.release#
                            FIELD tag# AS cha
                            FIELD trailer# LIKE oe-relh.trailer
                            FIELD i-no AS cha FORM "x(15)"
                            FIELD i-name AS cha FORM "x(30)"
                            FIELD ord-no LIKE oe-ord.ord-no
                            FIELD job-no LIKE oe-rell.job-no
                            FIELD job-no2 LIKE oe-rell.job-no2
                            FIELD loc LIKE oe-rell.loc
                            FIELD loc-bin LIKE oe-rell.loc-bin
                            FIELD cust-no LIKE oe-rell.cust-no
                            FIELD cases LIKE oe-rell.cases
                            FIELD qty-case LIKE oe-rell.qty-case
                            FIELD cases-unit LIKE fg-rctd.cases-unit
                            FIELD partial LIKE oe-rell.partial
                            FIELD qty LIKE oe-rell.qty                     
                            FIELD line LIKE oe-rell.LINE                        
                            FIELD warned AS CHAR
                            FIELD po-no LIKE oe-boll.po-no                                               
                            INDEX release# release# ord-no i-no po-no.
                            

                            /*                       
                            FIELD warned AS LOG                          
                            */


DEFINE NEW SHARED TEMP-TABLE tt-email NO-UNDO
   FIELD release# AS INT FORMAT "->,>>>,>>9"
   FIELD ord-no AS INT FORMAT ">>>>>9"
   FIELD i-no AS CHAR FORMAT "X(15)"
   FIELD part-no AS CHAR FORMAT "X(15)"
   FIELD i-name AS CHAR FORMAT "X(30)"
   FIELD done-what AS cha
   FIELD ord-no2 AS INT FORMAT ">>>>>9"
   FIELD job-no AS CHAR FORMAT "X(6)"
   FIELD job-no2 AS INT FORMAT ">9".

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.
          
DEF BUFFER bf-tmp FOR tt-relbol.

DEF VAR CHAR-hdl AS cha NO-UNDO.
def var v-ship-lu as ch initial ["I,S,B"] NO-UNDO.
def var v-ship-no as int NO-UNDO.
def var v-s-code as char NO-UNDO.
def var v-no-post as int format ">>>9" NO-UNDO.
def var v-tot-post as int format ">>>9" NO-UNDO.
def var v-first-release as log NO-UNDO.
def var v-r-no like inv-head.r-no NO-UNDO.
def var v-ext-price like inv-line.t-price NO-UNDO.
def var v-nxt-r-no as int NO-UNDO.
def var v-po-no like oe-rel.po-no NO-UNDO.
def var v-royal as log NO-UNDO.
def var v-n-bol like oe-ctrl.n-bol NO-UNDO.
def var v-bol-qty like oe-boll.qty NO-UNDO.
def var temp-tax as dec init 0 no-undo.
def var v-hold-list as char NO-UNDO.
DEF VAR v-release# AS INT NO-UNDO.
DEF VAR lv-do-leave-tag AS LOG NO-UNDO.
DEF VAR lv-do-leave-rel AS LOG NO-UNDO.
DEF VAR is-bol-printed AS LOG NO-UNDO.
DEF VAR v-msgreturn AS INT NO-UNDO.
DEF VAR lv-scan-next AS LOG NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-ord-no AS INT NO-UNDO.
DEF VAR v-prgmname AS CHAR NO-UNDO INIT 'b-relbol.'.
DEF VAR v-ran-need-scroll AS LOG NO-UNDO.
DEF VAR v-prev-rowid AS ROWID NO-UNDO.
DEF VAR vfrt-pay AS CHAR NO-UNDO.
DEF VAR vfob-code AS CHAR NO-UNDO.
DEF VAR vfrt-list AS CHAR NO-UNDO.
DEF VAR vfob-list AS CHAR NO-UNDO.
DEF VAR rell-ctr AS INTE NO-UNDO.
DEF VAR hBrowse  AS HANDLE NO-UNDO.
DEF VAR hColumn  AS HANDLE NO-UNDO.
DEF VAR iCounter AS INTEGER NO-UNDO.
DEF VAR ssupdrelpmpt-log AS LOG NO-UNDO.
DEF VAR lv-go-to-unit AS LOG NO-UNDO.

def new shared var out-recid as recid no-undo.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO. 
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.  

v-hold-list = "Royal,Superior,ContSrvc,BlueRidg,Danbury".

DEFINE VARIABLE v-rel-qty AS INTEGER NO-UNDO.

DEFINE VARIABLE v-scan-qty AS INTEGER NO-UNDO.   

 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp
    g_company = prmComp
    g_loc = usercomp.loc
    .


DO TRANSACTION:
  {sys/ref/relpost.i}
  {sys/inc/ssbol.i}
  {sys/inc/relmerge.i}
  {sys/inc/ssbolprint.i}

  find first sys-ctrl
        where sys-ctrl.company eq cocode
          and sys-ctrl.name    eq "BOLFMT"
        no-lock no-error.
  v-royal = avail sys-ctrl and lookup(sys-ctrl.char-fld,v-hold-list) ne 0.

/* gdm - 10160906 */
    {sys/inc/ssbolscan.i}
END.


/**************************************************************************************/ 


IF prmAction = "PrintBol" THEN DO:       
    /*DEF VAR filepath AS CHAR NO-UNDO.
    ASSIGN filepath = SEARCH("MyCSVFolder\Relbol.txt").   */ 

    INPUT FROM VALUE(prmFilePath) APPEND.
        REPEAT TRANSACTION ON ERROR UNDO, LEAVE:
            CREATE tt-csvrelbol.
            /*IMPORT tt-csvrelbol.   */
            IMPORT DELIMITER "," tt-csvrelbol.
        END. /*REPEAT TRANSACTION*/
    INPUT CLOSE.
    
    FOR EACH tt-csvrelbol NO-LOCK:  
        IF tt-csvrelbol.release# = 0 THEN NEXT.

        CREATE tt-relbol.
         ASSIGN 
          tt-relbol.release# = tt-csvrelbol.release#
          tt-relbol.tag# = tt-csvrelbol.tag#
          tt-relbol.i-no = tt-csvrelbol.i-no
          tt-relbol.i-name = tt-csvrelbol.i-name          
          tt-relbol.ord-no = tt-csvrelbol.ord-no 
          tt-relbol.job-no = tt-csvrelbol.job-no
          tt-relbol.job-no2 = tt-csvrelbol.job-no2 
          tt-relbol.loc = tt-csvrelbol.loc
          tt-relbol.loc-bin = tt-csvrelbol.loc-bin
          tt-relbol.cust-no = tt-csvrelbol.cust-no
          tt-relbol.qty = tt-csvrelbol.qty
          tt-relbol.cases  = tt-csvrelbol.cases
          tt-relbol.qty-case = tt-csvrelbol.qty-case
          tt-relbol.cases-unit = tt-csvrelbol.cases-unit
          tt-relbol.partial = tt-csvrelbol.partial
          tt-relbol.line = tt-csvrelbol.line
          tt-relbol.po-no = tt-csvrelbol.po-no
          tt-relbol.warned = if(tt-csvrelbol.warned = "no") THEN FALSE ELSE TRUE           
          .
    END.
    
    RUN print-bol.

    /*CREATE tt-relbol.
    ASSIGN 
          tt-relbol.release# = prmRelease
          tt-relbol.tag# = prmTag
          tt-relbol.i-no = prmIno
          tt-relbol.i-name = prmIName          
          tt-relbol.ord-no = prmOrdNo 
          tt-relbol.job-no = prmJobNo
          tt-relbol.job-no2 = prmJobNo2 
          tt-relbol.loc = prmLoc
          tt-relbol.loc-bin = prmLocBin
          tt-relbol.cust-no = prmCustNo
          tt-relbol.qty = prmQty
          tt-relbol.cases  = prmCases
          tt-relbol.qty-case = prmQtyCase
          tt-relbol.cases-unit = prmCasesUnit
          tt-relbol.partial = prmPartial
          tt-relbol.line = prmLine
          tt-relbol.po-no = prmPoNo
          tt-relbol.warned = if(prmWarned = "no") THEN FALSE ELSE TRUE           
          .       
     
   RUN print-bol.    
    */
END.

IF prmAction = "RelInsert" THEN DO:  

    DEF VAR lv-num-item AS INT NO-UNDO.
    DEF VAR lv-qty-rel AS INT NO-UNDO.
    DEF VAR lv-qty-tag AS INT NO-UNDO.
    

    RUN validate-rel# NO-ERROR.
    RUN validate-tag NO-ERROR.
    RUN validate-item NO-ERROR.  

     /*************************************************************************************************/

   IF TRIM(prmTrailor) EQ "" OR NOT CAN-FIND(FIRST truck WHERE truck.company EQ prmComp AND
           truck.truck EQ prmTrailor) THEN
       DO:          
          cError = "Invalid Trailer#.".
          RETURN.
       END.

    FIND FIRST oe-relh WHERE
         oe-relh.company  EQ prmComp AND
         oe-relh.release# EQ INT(prmRelease)
         NO-LOCK NO-ERROR.

    IF AVAIL oe-relh THEN DO:
       IF TRIM(oe-relh.trailer) NE "" AND
          oe-relh.trailer NE prmTrailor THEN DO:

           cError = "Trailer # does not match Release Trailer#.".
           RETURN.

          /*ll = NO.
          IF NOT g-sharpshooter THEN
             MESSAGE "Trailer # does not match Release Trailer#.  Is this the Correct Trailer#?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
          ELSE
          DO:
             RUN custom/d-msg.w ("Question","","Is this the Correct Trailer#?","",2,"YES,NO", OUTPUT v-msgreturn).
             IF v-msgreturn = 1 THEN ll = YES.
          END.
          IF NOT ll THEN
             RETURN NO-APPLY.
          */
       END.
    END.

     /*************************************************************************************************/

    IF (LASTKEY = -1 OR LASTKEY = 27 /*ESC*/) AND NOT lv-do-leave-tag  THEN RETURN.
   lv-do-leave-tag = NO.


   FIND FIRST oe-relh
       WHERE oe-relh.company  EQ prmComp
         AND oe-relh.release# EQ INT(prmRelease)
       NO-LOCK NO-ERROR.

   FIND FIRST loadtag
       WHERE loadtag.company   EQ prmComp
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ prmTag NO-LOCK NO-ERROR.

    IF NOT AVAIL loadtag THEN DO:
        cError = "Invalid Loadtag for the Release...".
        RETURN.        
   END.

   v-ord-no = 0.
   IF loadtag.ord-no = 0 THEN DO: 
       MESSAGE "iford0".
      FIND first oe-rell WHERE oe-rell.company   EQ prmComp
                           AND oe-rell.r-no      EQ oe-relh.r-no
                           AND oe-rell.tag      EQ loadtag.tag-no                           
                           USE-INDEX r-no NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-rell THEN do:          
         DEF VAR v-ord-no-list AS cha NO-UNDO.
         DEF VAR v-tag-no-list AS cha NO-UNDO.
         DEF VAR v-i-qty AS INT NO-UNDO.
         DEF VAR v-i-rel-qty AS INT NO-UNDO.
         ASSIGN
         v-ord-no-list = ""
         v-tag-no-list = "".
         FOR EACH bf-tmp NO-LOCK WHERE bf-tmp.i-no = loadtag.i-no 
                                 BREAK BY bf-tmp.ord-no:
             IF FIRST-OF(bf-tmp.ord-no) THEN v-ord-no-list = v-ord-no-list + STRING(bf-tmp.ord-no) + ",".
             v-tag-no-list = v-tag-no-list + bf-tmp.tag + ",".
             v-i-qty = v-i-qty + bf-tmp.qty.
         END.
         FOR EACH oe-rell FIELDS(qty) NO-LOCK WHERE
             oe-rell.company = prmComp AND
             oe-rell.r-no  = oe-relh.r-no AND
             oe-rell.i-no = loadtag.i-no
             USE-INDEX r-no:
             v-i-rel-qty = v-i-rel-qty + oe-rell.qty.
         END.
         
      IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
      ELSE DO:
          IF v-i-rel-qty >= v-i-qty THEN DO:
             FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company   EQ prmComp
                        AND oe-rell.r-no      EQ oe-relh.r-no
                        AND oe-rell.i-no      EQ loadtag.i-no
                        USE-INDEX r-no NO-ERROR.
             IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.             
          END.
          ELSE DO:         
              cError = "Loadtag has no order number and FG Item not on any order. FG Item must be added to order entry line with Sell Price".
              RETURN.              
          END.
      END.
   END.
   ELSE DO:
        MESSAGE "ifordn0".
      FIND first oe-rell WHERE oe-rell.company   EQ prmComp
                           AND oe-rell.r-no      EQ oe-relh.r-no
                           AND oe-rell.ord-no    EQ loadtag.ord-no     
                           AND oe-rell.i-no EQ  loadtag.i-no
                         USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL oe-rell THEN v-ord-no = loadtag.ord-no.
      ELSE DO:
       {bol/bolstock.i}
       IF v-ord-no = 0 THEN DO:
         FIND FIRST oe-ord WHERE oe-ord.company = cocode
                             AND oe-ord.ord-no = loadtag.ord-no
                             AND oe-ord.OPENed = YES NO-LOCK NO-ERROR.
         IF AVAIL oe-ord THEN DO:
            IF oe-ord.cust-no = oe-relh.cust-no THEN DO: /* have ord# from release*/
               FIND first oe-rell WHERE oe-rell.company   EQ cocode
                                    AND oe-rell.r-no      EQ oe-relh.r-no
                                    AND oe-rell.tag EQ  loadtag.tag-no
                                  USE-INDEX r-no NO-LOCK NO-ERROR.
               IF NOT AVAIL oe-rell THEN DO:
                  {bol/loadcust.i}
                  IF AVAIL cust AND cust.active = "X" THEN  DO: 
                     FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company   EQ cocode
                              AND oe-rell.r-no      EQ oe-relh.r-no
                              AND oe-rell.i-no      EQ loadtag.i-no                           
                              USE-INDEX r-no NO-ERROR.
                  END.
               END.
               IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
               ELSE v-ord-no = loadtag.ord-no.
            END.
            ELSE DO:
              {bol/loadcust.i}
              IF AVAIL cust AND cust.active = "X" THEN  DO: /* same as stock box ord-no = 0*/
                 FIND first oe-rell WHERE oe-rell.company   EQ cocode
                               AND oe-rell.r-no      EQ oe-relh.r-no
                               AND oe-rell.tag      EQ loadtag.tag-no                           
                               USE-INDEX r-no NO-LOCK NO-ERROR.
                 IF NOT AVAIL oe-rell THEN /* only one line for the item*/
                    FIND FIRST oe-rell WHERE oe-rell.company   EQ cocode
                              AND oe-rell.r-no      EQ oe-relh.r-no
                              AND oe-rell.i-no      EQ loadtag.i-no                           
                              USE-INDEX r-no NO-LOCK NO-ERROR.
                 IF AVAIL oe-rell THEN v-ord-no = oe-rell.ord-no.
              END.
            END.
         END. /* avail oe-ord */
         ELSE DO: /* loadtag.ord-no is invalid */
             FIND first oe-rell WHERE oe-rell.company   EQ cocode
                                  AND oe-rell.r-no      EQ oe-relh.r-no
                                  AND oe-rell.tag EQ  loadtag.tag-no
                                  USE-INDEX r-no NO-LOCK NO-ERROR.
             IF NOT AVAIL oe-rell THEN
                FIND first oe-rell WHERE oe-rell.company   EQ cocode
                                     AND oe-rell.r-no      EQ oe-relh.r-no
                                     AND oe-rell.i-no EQ  loadtag.i-no
                                     USE-INDEX r-no NO-LOCK NO-ERROR.
             IF AVAIL oe-rell THEN  v-ord-no = oe-rell.ord-no.
             ELSE DO:
                /*cError = "Order " loadtag.ord-no " is Closed Reopen it and try." .*/
                cError = "Order is Closed Reopen it and try" .
                RETURN.                
                
                /*MESSAGE "Order " loadtag.ord-no " is Closed. Reopen it and try."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.     
                */                 
             END.
         END.
       END.  /* v-ord-no = 0*/
      END.  /* not avail oe-rell*/
   END. /* loadtag.ord-no <> 0 */   

   


   /*== check multi po# ===*/
   DEF VAR lv-po-cnt AS INT NO-UNDO.
   DEF VAR lv-po-no AS cha NO-UNDO.
   DEF VAR lv-po-from-rell AS LOG NO-UNDO.
   ASSIGN lv-po-cnt = 0
          lv-po-no  = "".
   
   FOR EACH oe-rell
         WHERE oe-rell.company   EQ cocode
           AND oe-rell.r-no      EQ oe-relh.r-no
           AND oe-rell.ord-no    EQ v-ord-no
           AND oe-rell.i-no      EQ loadtag.i-no
         USE-INDEX r-no NO-LOCK BREAK BY oe-rell.po-no:
     IF FIRST-OF(oe-rell.po-no) THEN lv-po-cnt = lv-po-cnt + 1.
     ASSIGN
     lv-po-no = oe-rell.po-no
     lv-po-from-rell = YES.
   END.
  
   
   /* IF lv-po-cnt > 1 THEN RUN addon/bol/d-selpo.w (RECID(oe-relh),RECID(loadtag),OUTPUT lv-po-no). */
      
   /*ELSE IF NOT lv-po-from-rell and CAN-FIND (FIRST itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = loadtag.i-no AND itemfg.i-code = "C") THEN DO:
       {bol/bolpoord.i}
   END.
   */

   IF NOT lv-po-from-rell and CAN-FIND (FIRST itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = loadtag.i-no AND itemfg.i-code = "C") THEN DO:
       {bol/bolpoord.i}
   END.



   IF CAN-FIND(FIRST bf-tmp WHERE bf-tmp.tag# = prmTag
                   AND RECID(bf-tmp) <> RECID(tt-relbol) ) 
   THEN DO:     
       cError = "Tag# already scanned...".
       RETURN.       
   END.
   IF NOT CAN-FIND(FIRST fg-bin
                   WHERE fg-bin.company EQ cocode
                     AND fg-bin.tag     EQ prmTag
                     AND fg-bin.i-no    EQ loadtag.i-no
                     AND fg-bin.job-no  EQ loadtag.job-no
                     AND fg-bin.job-no2 EQ loadtag.job-no2
                     AND fg-bin.qty     GT 0)
   THEN DO:
       cError = "Tag# has no inventory...".
       RETURN.       
   END.
   

   IF relmerge-int EQ 0 AND
      NOT CAN-FIND(FIRST oe-ordl
                   WHERE oe-ordl.company  EQ cocode
                     AND oe-ordl.ord-no   EQ v-ord-no
                     AND oe-ordl.i-no     EQ loadtag.i-no
                     AND ((oe-ordl.job-no EQ loadtag.job-no AND
                           oe-ordl.job-no2 EQ loadtag.job-no2) OR
                          TRIM(oe-ordl.job-no) EQ ""))
   THEN DO:    
     cError = "Job# not on Order...".
     RETURN. 
   END.

   
   FOR EACH oe-ordl
       WHERE oe-ordl.company  EQ cocode
         AND oe-ordl.ord-no   EQ v-ord-no
         AND oe-ordl.i-no     EQ loadtag.i-no
       NO-LOCK
       BREAK BY oe-ordl.job-no  DESC
             BY oe-ordl.job-no2 DESC:
     IF LAST(oe-ordl.job-no) OR
        (oe-ordl.job-no EQ loadtag.job-no AND
         oe-ordl.job-no2 EQ loadtag.job-no2) THEN
       LEAVE.
   END.
   IF NOT AVAIL oe-ordl AND loadtag.ord-no <> 0 THEN DO:   
       cError = "Tag# Order/FG# invalid...".
       RETURN.       
   END.
   
   
   
   IF NOT CAN-FIND(FIRST oe-rell
                   WHERE oe-rell.company EQ oe-relh.company
                     AND oe-rell.r-no    EQ oe-relh.r-no
                     AND oe-rell.ord-no  EQ v-ord-no
                     AND oe-rell.i-no    EQ loadtag.i-no
                   USE-INDEX r-no)     
   THEN DO:
     ll = NO.
     /*IF ssbol-log THEN  DO:
       IF NOT  g-sharpshooter THEN
          MESSAGE "This Order# " + TRIM(STRING(v-ord-no),">>>>>>>>") + " FG# " 
                + TRIM(loadtag.i-no) +
                " is not on release, do you want to ADD TO RELEASE? "
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll. 
       ELSE do:
            RUN custom/d-msg.w ("Question","This Order# " + TRIM(STRING(v-ord-no),">>>>>>>>") + " FG#" + TRIM(loadtag.i-no) +
               " is not on release." , "Do you want to ADD TO RELEASE? ","",2,"Yes,No",OUTPUT v-msgreturn).
            IF v-msgreturn = 1 THEN ll = YES.
       END.
     END.
     ELSE DO:
        IF NOT  g-sharpshooter THEN MESSAGE "Invalid Tag# for the Release#. Try again..." VIEW-AS ALERT-BOX ERROR.
        ELSE RUN custom/d-msg.w ("Error","","Invalid Tag# for this Release#.  Try again...","",1,"OK", OUTPUT v-msgreturn).
     END.
     IF NOT ll THEN RETURN NO-APPLY.
     */

    cError = "Invalid Tag# for this Release#.".
    RETURN.
     
     /*tt-relbol.warned = YES.*/
   END.
       
   

   /*IF NOT tt-relbol.warned THEN DO:
     lv-qty-rel = 0.
     FOR EACH oe-rell
         WHERE oe-rell.company   EQ cocode
           AND oe-rell.r-no      EQ oe-relh.r-no
           AND (oe-rell.ord-no   EQ v-ord-no)
           AND oe-rell.i-no      EQ loadtag.i-no
         USE-INDEX r-no NO-LOCK:
       lv-qty-rel = lv-qty-rel + oe-rell.qty.
     END.
     
     lv-qty-tag = loadtag.pallet-count.
     FOR EACH bf-tmp
         WHERE bf-tmp.release# EQ INT(prmRelease)
           AND (bf-tmp.ord-no   EQ v-ord-no)
           AND bf-tmp.i-no     EQ loadtag.i-no
           AND bf-tmp.warned   EQ NO
           AND ROWID(bf-tmp)   NE ROWID(tt-relbol):
       lv-qty-tag = lv-qty-tag + bf-tmp.qty.
     END.
     
   END.
   */
 


   /*ll = NO.
   IF lv-qty-tag GT lv-qty-rel THEN DO:
        cError = "Qty scanned exceeds qty released for Order# ".
        RETURN.

      /*IF NOT  g-sharpshooter THEN
         MESSAGE "Qty scanned exceeds qty released for Order# " + TRIM(STRING(v-ord-no),">>>>>>>>")
              + " FG# "  + TRIM(loadtag.i-no) +
             ", accept this tag anyway?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
     ELSE DO:
        RUN custom/d-msg.w ("Question","Qty scanned exceeds qty released for Order# " +
                      TRIM(STRING(v-ord-no),">>>>>>>>") + " FG# " +
                      TRIM(loadtag.i-no) + "." , "Accept this tag anyway? ","",2,"Yes,No",OUTPUT v-msgreturn).
        IF v-msgreturn = 1 THEN ll = YES.
     END.
     IF NOT ll THEN DO:
       APPLY "entry" TO tt-relbol.tag# .
       RETURN NO-APPLY.
     END.
     */
     /*FOR FIRST bf-tmp 
         WHERE bf-tmp.release# EQ INT(tt-relbol.release#:SCREEN-VALUE IN BROWSE {&browse-name})
           AND bf-tmp.i-no     EQ loadtag.i-no
           AND ROWID(bf-tmp)   NE ROWID(tt-relbol):
       bf-tmp.warned = YES.
     END. */
   END.
   */     

   RELEASE oe-ord.
   IF v-ord-no NE 0 THEN
   FIND FIRST oe-ord NO-LOCK
       WHERE oe-ord.company EQ cocode
         AND oe-ord.ord-no  EQ v-ord-no
       NO-ERROR.

   RELEASE fg-bin.
   FOR EACH fg-bin NO-LOCK
       WHERE fg-bin.company  EQ cocode
         AND fg-bin.tag      EQ prmTag
         AND fg-bin.i-no     EQ loadtag.i-no
         AND fg-bin.job-no   EQ loadtag.job-no
         AND fg-bin.job-no2  EQ loadtag.job-no2
         AND fg-bin.qty      GT 0
         AND ((AVAIL oe-ord AND fg-bin.cust-no EQ oe-ord.cust-no) OR
              fg-bin.cust-no EQ "")
       USE-INDEX tag
       BREAK BY fg-bin.cust-no DESC
             BY fg-bin.qty:

     IF (fg-bin.cust-no EQ oe-relh.cust-no AND LAST-OF(fg-bin.cust-no)) OR
        LAST(fg-bin.cust-no) THEN LEAVE.
   END.  
   
   
   CREATE tt-relbol.
   ASSIGN tt-relbol.tag# = prmTag
          tt-relbol.release# = prmRelease
          tt-relbol.i-no = loadtag.i-no
          tt-relbol.i-name = loadtag.i-name          
          tt-relbol.ord-no = v-ord-no 
          tt-relbol.job-no = loadtag.job-no
          tt-relbol.job-no2 = loadtag.job-no2
          tt-relbol.loc = IF AVAIL fg-bin THEN fg-bin.loc ELSE loadtag.loc
          tt-relbol.loc-bin = IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE loadtag.loc-bin
          tt-relbol.cust-no = IF AVAIL fg-bin THEN fg-bin.cust-no ELSE ""  
          tt-relbol.qty = IF AVAIL fg-bin THEN fg-bin.qty ELSE loadtag.pallet-count  /* loadtag.qty */
          tt-relbol.cases  = IF AVAIL fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) ELSE loadtag.case-bundle
          tt-relbol.qty-case = IF AVAIL fg-bin THEN fg-bin.case-count ELSE loadtag.qty-case
          tt-relbol.cases-unit = IF AVAIL fg-bin THEN fg-bin.cases-unit ELSE loadtag.case-bundle
          tt-relbol.partial = IF AVAIL fg-bin THEN fg-bin.partial-count ELSE loadtag.partial
          tt-relbol.line = IF AVAIL oe-ordl THEN oe-ordl.LINE ELSE 0
          tt-relbol.po-no = lv-po-no
          tt-relbol.warned = ll.
          .             


   /*IF g-sharpshooter = YES THEN */ DO:
      FIND FIRST itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = prmIno NO-LOCK NO-ERROR.      
   END.

   CREATE bf-tmp.       

       ASSIGN 
          bf-tmp.release# = tt-relbol.release#
          bf-tmp.tag# = tt-relbol.tag#
          bf-tmp.i-no = tt-relbol.i-no
          bf-tmp.i-name = tt-relbol.i-name
          bf-tmp.ord-no = tt-relbol.ord-no
          bf-tmp.job-no = tt-relbol.job-no
          bf-tmp.job-no2 = tt-relbol.job-no2
          bf-tmp.loc = tt-relbol.loc
          bf-tmp.loc-bin = tt-relbol.loc-bin
          bf-tmp.cust-no = tt-relbol.cust-no
          bf-tmp.qty = tt-relbol.qty
          bf-tmp.cases = tt-relbol.cases
          bf-tmp.qty-case = tt-relbol.qty-case
          bf-tmp.cases-unit = tt-relbol.cases-unit
          bf-tmp.partial = tt-relbol.partial
          bf-tmp.line = tt-relbol.line
          bf-tmp.po-no = tt-relbol.po-no
          bf-tmp.warned = tt-relbol.warned.

   
   /*RUN display-qtys.*/

   END.
   /*RETURN NO-APPLY.*/

   prmAction = "Select".

END.



/***********************************************************************************/  




 IF prmAction = "Select" THEN DO: 
  

  FOR EACH tt-relbol NO-LOCK:
      IF AVAIL tt-relbol THEN DO:
          CREATE ttRelBol.
          ASSIGN
              /*ttRelBol.vSeq         = tt-relbol.seq
              ttRelBol.vRelease#    = tt-relbol.release#
              */
              ttRelBol.vTag#        = tt-relbol.tag#
              /*ttRelBol.vTrailer     = tt-relbol.trailer#*/
              ttRelBol.vCases       = tt-relbol.cases
              ttRelBol.vIno         = tt-relbol.i-no
              ttRelBol.vIname       = tt-relbol.i-name
              ttRelBol.vOrdNo       = tt-relbol.ord-no
              ttRelBol.vQty         = tt-relbol.qty
              ttRelBol.vLoc         = tt-relbol.loc         
              ttRelBol.vLocBin      = tt-relbol.loc-bin
              ttRelBol.vCustNo      = tt-relbol.cust-no
              ttRelBol.vQtyCase     = tt-relbol.qty-case
              ttRelBol.vCasesUnit   = tt-relbol.cases-unit
              ttRelBol.vPartial     = tt-relbol.partial

              ttRelBol.vJobNo       = tt-relbol.job-no
              ttRelBol.vJobNo2      = tt-relbol.job-no2          
              ttRelBol.vLine        = tt-relbol.line
              ttRelBol.vWarned      = tt-relbol.warned
              ttRelBol.vPoNo        = tt-relbol.po-no
              .      
       
        END. /*ettCorrInksnd of  eb*/
  END. /* end ttCorrInksof eb*/
 END. /* end of select*/


/*------------------------------------------------------------------------------*/
PROCEDURE validate-rel# :

  IF NOT CAN-FIND(FIRST oe-relh WHERE oe-relh.company = cocode
                                    AND oe-relh.release# = INT(prmRelease)
                                    AND oe-relh.printed AND NOT oe-relh.posted )
  THEN DO:     
      cError = "Invalid Release#. Not printed? or Already posted?".
      RETURN.
  END.

END PROCEDURE.

/*------------------------------------------------------------------------------*/


PROCEDURE validate-tag :

  DEF VAR lv-qty-rel LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-qty-tag LIKE oe-rell.qty NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

      
  IF prmTag = "" THEN DO:   
        cError = "Tag# must be entered.".
        RETURN.
  END.

  IF CAN-FIND(FIRST bf-tmp WHERE bf-tmp.tag# = prmTag
                   AND RECID(bf-tmp) <> RECID(tt-relbol) ) 
  THEN DO:
        cError = "Tag# already scanned...".
        RETURN.
  END.

END PROCEDURE.



/*------------------------------------------------------------------------------*/

PROCEDURE validate-item :

  DEF VAR v-iqty AS INT NO-UNDO.
  DEF VAR v-ttqty AS INT NO-UNDO.

  IF prmIno = "" THEN DO:    
     cError = "Item must be entered. ".
     RETURN.
  END.

  FIND FIRST oe-relh WHERE oe-relh.company = cocode
                       AND oe-relh.release# = v-release# NO-LOCK NO-ERROR.
 
END PROCEDURE.


/*------------------------------------------------------------------------------*/

PROCEDURE display-qtys :

  DEF VAR lv-release# LIKE oe-relh.release# NO-UNDO.
  DEF VAR lv-i-no LIKE oe-rell.i-no NO-UNDO.

  DEF BUFFER b-rell FOR oe-rell.
  DEF BUFFER b-relh FOR oe-relh.


  DO :
    ASSIGN
     v-scan-qty  = 0
     v-rel-qty   = 0
     lv-release# = INT(prmRelease)
     lv-i-no     = prmIno.
   

    IF lv-release# NE 0 AND TRIM(lv-i-no) NE "" THEN DO: 
      FOR EACH bf-tmp
          WHERE bf-tmp.release# EQ lv-release#
            AND bf-tmp.i-no     EQ lv-i-no:      
        v-scan-qty = v-scan-qty + bf-tmp.qty.
      END.
      
      FOR EACH b-relh NO-LOCK
          WHERE b-relh.company  EQ cocode
            AND b-relh.RELEASE# EQ lv-release#,
          EACH b-rell NO-LOCK
          WHERE b-rell.r-no EQ b-relh.r-no
            AND b-rell.i-no EQ lv-i-no
          USE-INDEX r-no:
        v-rel-qty = v-rel-qty + b-rell.qty.
      
      END.
    END.            
  END. 

END PROCEDURE.


/*------------------------------------------------------------------------------*/

PROCEDURE print-bol :

  DEF VAR fil_id AS RECID NO-UNDO.
  DEF VAR nufile AS LOG NO-UNDO.
     

  RUN validate-scan NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN .

  MESSAGE "enterprintbol".


  is-bol-printed = YES.
      
  EMPTY TEMP-TABLE tt-rell.

  RUN create-temp-rel.  

  RUN post-release.

  EMPTY TEMP-TABLE tt-relbol.  
  

  /*RUN dispatch ('open-query').
 
  SESSION:SET-WAIT-STATE(""). 
  */

END PROCEDURE.

/*-------------------------------------------------------------------------------------------------------------------------------*/


PROCEDURE validate-scan :
  
  DEF BUFFER bf-relbol FOR tt-relbol.

  DEF VAR v-msg AS CHAR NO-UNDO.

  FOR EACH bf-tmp BREAK BY bf-tmp.release#:      

    IF LAST-OF(bf-tmp.release#) THEN
    FOR EACH oe-relh
        WHERE oe-relh.company  EQ cocode
          AND oe-relh.release# EQ bf-tmp.release#
        NO-LOCK,
        EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK
        BREAK BY oe-rell.ord-no
              BY oe-rell.i-no:

      IF LAST-OF(oe-rell.i-no) AND
         NOT CAN-FIND(FIRST bf-relbol
                      WHERE bf-relbol.release# EQ oe-relh.release#
                        AND bf-relbol.ord-no   EQ oe-rell.ord-no
                        AND bf-relbol.i-no     EQ oe-rell.i-no)
      THEN DO:
        ASSIGN
         v-msg = "Release/Order#/FG#: " +
                 TRIM(STRING(oe-relh.release#,">>>>>>>>")) + "/" +
                 TRIM(STRING(oe-rell.ord-no),">>>>>>>>") + "/" +
                 TRIM(oe-rell.i-no) +
                 " was not scanned"
         
            ll    = NO.        

        /*IF ssbol-log THEN DO:
           IF NOT g-sharpshooter THEN
              MESSAGE TRIM(v-msg) + ", do you want to DELETE it from Release?"
                      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
           ELSE DO:
             RUN custom/d-msg.w ("Question",trim(v-msg), "Do you want to Delete if from Release? ","",2,"Yes,No",OUTPUT v-msgreturn).
             IF v-msgreturn = 1 THEN ll = YES.
           END.
        END.
        ELSE DO:
           IF NOT g-sharpshooter THEN
              MESSAGE TRIM(v-msg) + "," SKIP
                      "scan all items for the Release before printing BOL..."
              VIEW-AS ALERT-BOX ERROR.
           ELSE RUN custom/d-msg.w ("Error",trim(v-msg),"scan all items for the Release before printing BOL...","",1,"OK",OUTPUT v-msgreturn).
        END.*/
        
        /*IF NOT ll THEN RETURN ERROR.*/
        
      END.
    END.
  END.

END PROCEDURE.

/*-----------------------------------------------------------------------------------------------------------------*/

PROCEDURE create-temp-rel :

  DEF VAR li-nxt-rel-no AS INT NO-UNDO.
  DEF VAR lv-qty LIKE oe-boll.qty NO-UNDO.
  DEF VAR v-s-code AS CHAR NO-UNDO.

  DEF BUFFER bf-rell FOR oe-rell.

  /* Create release line for each temp record without one */
  FOR EACH bf-tmp USE-INDEX release#,
      FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.release# EQ bf-tmp.release#
      USE-INDEX release#
      BREAK BY bf-tmp.release#
            BY bf-tmp.ord-no
            BY bf-tmp.i-no:

    FIND FIRST bf-rell NO-LOCK WHERE bf-rell.r-no EQ oe-relh.r-no NO-ERROR.
    v-s-code = IF AVAIL bf-rell THEN bf-rell.s-code ELSE "B".

    lv-qty = lv-qty + bf-tmp.qty.

    IF LAST-OF(bf-tmp.i-no) THEN DO:
      IF NOT CAN-FIND(FIRST oe-rell
                      WHERE oe-rell.company EQ oe-relh.company
                        AND oe-rell.r-no    EQ oe-relh.r-no
                        AND oe-rell.ord-no  EQ bf-tmp.ord-no
                        AND oe-rell.i-no    EQ bf-tmp.i-no
                      USE-INDEX r-no) THEN DO:

        li-nxt-rel-no = 0.
        FOR EACH bf-rell NO-LOCK
            WHERE bf-rell.company EQ cocode
              AND bf-rell.ord-no  EQ bf-tmp.ord-no
            USE-INDEX ord-no 
            BY bf-rell.rel-no DESC:
    
          li-nxt-rel-no = bf-rell.rel-no.
          LEAVE.  
        END.
        RELEASE bf-rell.

        CREATE oe-rell.
        BUFFER-COPY bf-tmp TO oe-rell
        ASSIGN
         oe-rell.company = oe-relh.company
         oe-rell.r-no    = oe-relh.r-no
         oe-rell.rel-no  = li-nxt-rel-no + 1
         oe-rell.tag     = bf-tmp.tag#
         oe-rell.s-code  = v-s-code
         oe-rell.qty     = lv-qty
         oe-rell.cases   = TRUNC((oe-rell.qty - oe-rell.partial) /
                                 oe-rell.qty-case,0)
         oe-rell.partial = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).

                
         RUN build-email ("ADDED", RECID(bf-tmp)).
      END.      
      lv-qty = 0.
    END.
  END.

  FOR EACH bf-tmp USE-INDEX release#,
      FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.release# EQ bf-tmp.release#
      USE-INDEX release#
      BREAK BY bf-tmp.release#
            BY bf-tmp.ord-no
            BY bf-tmp.i-no:

    IF FIRST-OF(bf-tmp.i-no) THEN
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
          AND oe-rell.ord-no  EQ bf-tmp.ord-no
          AND oe-rell.i-no    EQ bf-tmp.i-no
        USE-INDEX r-no:

      CREATE tt-rell.
      BUFFER-COPY oe-rell TO tt-rell
      ASSIGN
       tt-rell.release# = oe-relh.release#
       tt-rell.row-id   = ROWID(oe-rell).
    END.
  END.

  /* Delete release lines that have no temp record */
  
  FOR EACH bf-tmp USE-INDEX release#,
      FIRST oe-relh NO-LOCK
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.release# EQ bf-tmp.release#
      USE-INDEX release#,
      EACH oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no:

    IF NOT CAN-FIND(FIRST tt-rell WHERE tt-rell.row-id EQ ROWID(oe-rell)) THEN DO:
      /* e-mail logic */
      RUN build-email ("DELETED", RECID(oe-rell)).
      DELETE oe-rell.
    END.    
  END.

  FOR EACH bf-tmp NO-LOCK BREAK BY bf-tmp.release# /*BY bf-tmp.ord-no*/ :
    
    IF LAST-OF(bf-tmp.release#) THEN RUN send-email (bf-tmp.release#).
  END.
  

END PROCEDURE.




/*-------------------------------------------------------------------------------------------*/

PROCEDURE post-release :


DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER bf-rell FOR oe-rell.

DEF VAR v-ship-lu AS ch INIT ["I,S,B"].
DEF VAR v-ship-no AS INT.
DEF VAR save_id AS RECID.
DEF VAR time_stamp AS ch.
DEF VAR v-s-code AS CHAR.
DEF VAR v-first-release AS LOG.
DEF VAR v-r-no LIKE inv-head.r-no.
DEF VAR v-ext-price LIKE inv-line.t-price.
DEF VAR v-nxt-r-no AS INT.
DEF VAR v-po-no LIKE oe-rel.po-no.
DEF VAR v-n-bol LIKE oe-ctrl.n-bol.
DEF VAR v-bol-qty LIKE oe-boll.qty.
DEF VAR temp-tax AS DEC INIT 0 NO-UNDO.
DEF VAR v-relpost-hld LIKE relpost-chr NO-UNDO.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR ll-exception AS LOG NO-UNDO.

{sa/sa-sls01.i}

DO TRANSACTION:
  {sys/inc/boldate.i}
END.

FOR EACH tt-except:
  DELETE tt-except.
END.
 
FOR EACH tt-fg-bin:
  DELETE tt-fg-bin.
END.

ASSIGN
 v-relpost-hld = relpost-chr
 lv-bol-no     = 0.

headblok:
FOR EACH oe-relh NO-LOCK
    WHERE oe-relh.company EQ cocode
      AND oe-relh.posted  EQ NO
      AND oe-relh.printed EQ YES
      AND CAN-FIND(FIRST tt-rell WHERE tt-rell.release# EQ oe-relh.release#)
    USE-INDEX post
  {oe/oe-relp2.i}
END.

EMPTY TEMP-TABLE tt-boll.

FOR EACH report WHERE report.term-id EQ v-term NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id:
  CREATE tt-boll.
  BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll. 
  DELETE oe-boll.
END.

/*ysk nothing printed need delete report for old oe-boll and 
         create new one from update-bol  */
FOR EACH report WHERE report.term-id EQ v-term:
  IF NOT CAN-FIND(FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id)
  THEN DELETE report.
END.

RUN update-bol (v-term).

EMPTY TEMP-TABLE tt-report.

/* Save BOL to be posted in temp-table */
FOR EACH report WHERE report.term-id EQ v-term,
    FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
    FIRST oe-bolh NO-LOCK
    WHERE oe-bolh.b-no    EQ oe-boll.b-no
      AND oe-bolh.printed EQ YES:
    MESSAGE "report" report.term-id.
  CREATE tt-report.
  BUFFER-COPY report TO tt-report.
  DELETE report.
END.



FOR EACH report WHERE report.term-id EQ v-term NO-LOCK:
    FIND FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id NO-ERROR.
    IF AVAIL oe-boll THEN
        MESSAGE "oebollavail" RECID(oe-boll)  oe-boll.b-no.

    FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no NO-ERROR.
    IF AVAIL oe-bolh THEN
        MESSAGE "oebolh.b-no" oe-bolh.b-no.
END.




FOR EACH report WHERE report.term-id EQ v-term,
    FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
    FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no 
    BREAK BY oe-bolh.bol-no:

    MESSAGE "pbol" report.term-id report.rec-id.

  IF FIRST-OF(oe-bolh.bol-no) THEN DO:   
       RUN bol/printBol.p(INPUT oe-bolh.company,
                          INPUT locode,
                          INPUT oe-bolh.cust-no,
                          INPUT oe-bolh.bol-no,
                          INPUT oe-bolh.printed,
                          INPUT oe-bolh.posted,
                          INPUT IF ssbolprint-int = 1 THEN YES ELSE NO).
        CREATE ttPrintBolpdf .
        ASSIGN  ttPrintBolpdf.vPdfBolFile = pdfname .      
  END.
END.

/*
FOR EACH report WHERE report.term-id EQ v-term:   
    MESSAGE "found report" report.term-id . 
  DELETE report.
END.
*/


/* Recreate BOL to be posted from temp-table */
/*FOR EACH tt-report:
    MESSAGE "ttreport" tt-report.term-id.
  CREATE report.
  BUFFER-COPY tt-report TO report.
  DELETE tt-report.
END. */

IF AVAIL tt-report THEN DO:
    FOR EACH report WHERE report.term-id EQ v-term:   
        MESSAGE "found report" report.term-id . 
        DELETE report.
    END.
    FOR EACH tt-report:
        MESSAGE "ttreport" tt-report.term-id.
        CREATE report.
        BUFFER-COPY tt-report TO report.
        DELETE tt-report.
    END.
END.

MESSAGE "bbp3"  v-term.

RUN oe/oe-bolp3.p (v-term).  

MESSAGE "abp3".

delete-blok:
FOR EACH oe-relh
    WHERE oe-relh.company EQ cocode
      AND oe-relh.deleted EQ YES
    USE-INDEX deleted:
  FOR EACH oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no:       
    DELETE oe-rell.
  END. /* each oe-rell */
  DELETE oe-relh.
END. /* each oe-relh */

END PROCEDURE.




/*------------------------------------------------------------------------------*/

PROCEDURE update-bol :

  DEF INPUT PARAM ip-term AS cha NO-UNDO.

  DEF VAR lv-qty LIKE oe-boll.qty NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-weight LIKE oe-boll.weight NO-UNDO.
  DEF VAR lv-freight LIKE oe-boll.freight NO-UNDO.


  FOR EACH tt-boll USE-INDEX tt-boll,

      FIRST tt-rell
      WHERE tt-rell.company  EQ tt-boll.company
        AND tt-rell.ord-no   EQ tt-boll.ord-no
        AND tt-rell.line     EQ tt-boll.line
        AND tt-rell.i-no     EQ tt-boll.i-no
        AND tt-rell.po-no    EQ tt-boll.po-no
        AND tt-rell.rel-no   EQ tt-boll.rel-no
        AND tt-rell.b-ord-no EQ tt-boll.b-ord-no
      USE-INDEX ord-no

      BREAK BY tt-boll.b-no
            BY tt-boll.ord-no
            BY tt-boll.i-no
            BY tt-boll.po-no
            BY tt-boll.rel-no
            BY tt-boll.b-ord-no:

    IF FIRST-OF(tt-boll.b-ord-no) THEN lv-qty = 0.

    lv-qty = lv-qty + tt-boll.qty.

    IF LAST-OF(tt-boll.b-ord-no) THEN
      ASSIGN
       tt-boll.release# = tt-rell.release#
       tt-boll.qty      = lv-qty
       tt-boll.job-no   = ""
       tt-boll.job-no2  = 0
       tt-boll.loc      = ""
       tt-boll.loc-bin  = ""
       tt-boll.tag      = "".    
  END.

  EMPTY TEMP-TABLE tt-boll2.

  FOR EACH tt-boll USE-INDEX release#
      BY tt-boll.release#
      BY tt-boll.ord-no
      BY tt-boll.i-no
      BY tt-boll.rel-no
      BY tt-boll.b-ord-no
      BY tt-boll.po-no:
   

    FOR EACH tt-relbol
        WHERE tt-relbol.release# EQ tt-boll.release#
          AND tt-relbol.ord-no   EQ tt-boll.ord-no
          AND tt-relbol.i-no     EQ tt-boll.i-no
          AND tt-relbol.po-no    EQ tt-boll.po-no
        USE-INDEX release#:

      RUN create-tt-boll2.
    END.

    DELETE tt-boll.
  END.

  RELEASE tt-boll2.

  ASSIGN
   lv-weight  = 0
   lv-freight = 0.

  FOR EACH tt-boll2 USE-INDEX b-no BREAK BY tt-boll2.b-no:
    FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ tt-boll2.b-no NO-LOCK.

    CREATE oe-boll.
    BUFFER-COPY tt-boll2 EXCEPT rec_key TO oe-boll.

    {oe/oe-bolpc.i}

    
    CREATE report.
    ASSIGN
     report.term-id  = ip-term
     report.key-01   = oe-boll.i-no
     report.key-02   = STRING(oe-boll.ord-no,"9999999999")
     report.rec-id   = RECID(oe-boll).

    MESSAGE "procupdreptermid" report.term-id.
    MESSAGE "procupdrepkey1" report.key-01.
    MESSAGE "procupdrepkey2" report.key-02.
    MESSAGE "procupdreprecid" report.rec-id.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ oe-boll.i-no
        NO-ERROR.
    oe-boll.weight = (oe-boll.qty / 100) *
                     (IF AVAIL itemfg THEN itemfg.weight-100 ELSE 0).

    RUN oe/bollfrat.p (ROWID(oe-boll),
                       oe-bolh.cust-no,
                       oe-bolh.ship-id,
                       oe-bolh.carrier,
                       OUTPUT oe-boll.freight).

    ASSIGN
     lv-weight  = lv-weight  + oe-boll.weight
     lv-freight = lv-freight + oe-boll.freight.

    IF LAST-OF(tt-boll2.b-no) THEN DO:
      FIND CURRENT oe-bolh.

      ASSIGN
       oe-bolh.tot-wt  = lv-weight
       oe-bolh.freight = lv-freight
       lv-weight       = 0
       lv-freight      = 0.

      RUN oe/palcalc.p (ROWID(oe-bolh), OUTPUT oe-bolh.tot-pallets).

      FIND CURRENT oe-bolh NO-LOCK.
    END.
  END.

END PROCEDURE.


/*------------------------------------------------------------------------------*/

PROCEDURE create-tt-boll2 :

  CREATE tt-boll2.
  BUFFER-COPY tt-boll EXCEPT rec_key TO tt-boll2
  ASSIGN
   tt-boll2.job-no   = tt-relbol.job-no
   tt-boll2.job-no2  = tt-relbol.job-no2
   tt-boll2.loc      = tt-relbol.loc
   tt-boll2.loc-bin  = tt-relbol.loc-bin
   tt-boll2.cust-no  = tt-relbol.cust-no
   tt-boll2.tag      = tt-relbol.tag#
   tt-boll2.cases    = tt-relbol.cases
   tt-boll2.qty-case = tt-relbol.qty-case
   tt-boll2.partial  = tt-relbol.partial
   tt-boll2.qty      = tt-relbol.qty
   tt-boll2.po-no    = tt-relbol.po-no.

  DELETE tt-relbol.

END PROCEDURE.

PROCEDURE send-email :
  DEF INPUT PARAM ip-release# AS INT NO-UNDO.

  RUN bol/bolemail.p(INPUT ip-release#,
                           INPUT cocode,
                           INPUT v-prgmname,OUTPUT prmMailTo, OUTPUT prmSubject, OUTPUT prmBody ).

END PROCEDURE.

/*-------------------------------------------------------------------------------------*/

PROCEDURE build-email :

   DEF INPUT PARAM ip-done-what AS cha NO-UNDO.
   DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
   DEF BUFFER bf-relbol FOR tt-relbol.
   DEF BUFFER bf-rell FOR oe-rell.

    MESSAGE "ipdonewhat" ip-done-what.

   IF ip-done-what = "Added" THEN DO:
      FIND bf-relbol WHERE RECID(bf-relbol) = ip-recid NO-LOCK NO-ERROR.   
      IF AVAIL bf-relbol THEN DO:
         FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                        AND oe-ordl.ord-no = bf-relbol.ord-no
                        AND oe-ordl.i-no = bf-relbol.i-no 
                        AND oe-ordl.LINE = bf-relbol.LINE
             USE-INDEX ord-no NO-LOCK NO-ERROR.
         IF NOT AVAIL oe-ordl THEN
            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                           AND oe-ordl.ord-no = bf-relbol.ord-no
                           AND oe-ordl.i-no = bf-relbol.i-no
                USE-INDEX ord-no NO-LOCK NO-ERROR.
         IF NOT CAN-FIND(FIRST tt-email WHERE tt-email.release# = bf-relbol.release#
                                          AND tt-email.ord-no = bf-relbol.ord-no
                                          AND tt-email.i-no = bf-relbol.i-no
                                          AND tt-email.done-what = ip-done-what)
         THEN DO:
            CREATE tt-email.
            ASSIGN tt-email.release# = bf-relbol.release#
                   tt-email.ord-no = bf-relbol.ord-no
                   tt-email.i-no = bf-relbol.i-no
                   tt-email.part-no = IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE ""
                   tt-email.i-name = bf-relbol.i-name
                   tt-email.done-what = ip-done-what    
                   tt-email.ord-no2 = bf-relbol.ord-no
                   tt-email.job-no = IF AVAIL oe-ordl THEN oe-ordl.job-no ELSE ''
                   tt-email.job-no2 = IF AVAIL oe-ordl THEN oe-ordl.job-no2 ELSE 0
                   .
         END.
      END.
   END.
   ELSE DO:  /* deleted */
      FIND bf-rell WHERE RECID(bf-rell) = ip-recid NO-LOCK NO-ERROR.
      IF AVAIL bf-rell THEN DO:
         FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                        AND oe-ordl.ord-no = bf-rell.ord-no
                        AND oe-ordl.i-no = bf-rell.i-no 
                        USE-INDEX ord-no NO-LOCK NO-ERROR.
         IF NOT CAN-FIND(FIRST tt-email WHERE tt-email.release# = tt-relbol.release#
                                          AND tt-email.ord-no = tt-relbol.ord-no
                                          AND tt-email.i-no = bf-rell.i-no
                                          AND tt-email.done-what = ip-done-what  )
         THEN DO:
            CREATE tt-email.
            ASSIGN tt-email.release# = tt-relbol.release#
                   tt-email.ord-no = tt-relbol.ord-no
                   tt-email.i-no = bf-rell.i-no
                   tt-email.part-no = IF AVAIL oe-ordl THEN oe-ordl.part-no ELSE ""
                   tt-email.i-name = IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE ""
                   tt-email.done-what = ip-done-what   
                   tt-email.ord-no2 = bf-rell.ord-no
                   tt-email.job-no = IF AVAIL oe-ordl THEN oe-ordl.job-no ELSE ''
                   tt-email.job-no2 = IF AVAIL oe-ordl THEN oe-ordl.job-no2 ELSE 0
                   .
         END.
      END.
   END.


END PROCEDURE.
