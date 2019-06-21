

/*------------------------------------------------------------------------
    File        : BrwsCEstimate.p
    Purpose     : Corrugated Box
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorrugatedBox NO-UNDO
        FIELD vest            AS CHAR FORMAT "x(8)" 
        FIELD vCust           AS CHAR  
        FIELD vCustpart       AS CHAR   
        FIELD vQty            AS INT    
        FIELD vOrder          AS INT    
        FIELD vFgitem         AS CHAR    
        FIELD vStyle          AS CHAR 
        FIELD vItemName       AS CHAR 
        FIELD vFlute          AS CHAR  FORMAT "XXX" 
        FIELD vTest           AS CHAR  FORMAT "x(6)"  
        FIELD vQtySet         AS INT 
        FIELD vLength         AS DEC 
        FIELD vWidth          AS DEC    
        FIELD vDepth          AS DEC    
        FIELD vDie            AS CHAR FORMAT "x(15)"   
        FIELD vCad            AS CHAR FORMAT "x(15)"
        FIELD vPlate          AS CHAR FORMAT "x(15)"
        FIELD vEstDate        AS DATETIME
        FIELD vModi           AS CHAR   
        FIELD vShipTo         AS CHAR    
        FIELD vCreatedBy      AS CHAR   
        FIELD vreckey         AS CHAR
        FIELD vPaper1         AS CHAR
        FIELD vPaper2         AS CHAR
        FIELD vFormno         AS INTEGER 
        FIELD vBlankno        AS INTEGER
        FIELD vBoard          AS CHAR 
        FIELD vLogic          AS CHAR
        FIELD vModDate        AS INT
        .
DEFINE DATASET dsCorrugatedBox FOR ttCorrugatedBox .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmType        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustpart    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmStyle       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipTo      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmDie         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCad         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPlate       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSingle      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSet         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTandem      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmWidthFrom   AS DECIMAL        NO-UNDO.
DEFINE INPUT PARAMETER prmWidthTo     AS DECIMAL        NO-UNDO.
DEFINE INPUT PARAMETER prmLenFrom     AS DECIMAL        NO-UNDO.
DEFINE INPUT PARAMETER prmLenTo       AS DECIMAL        NO-UNDO.
DEFINE INPUT PARAMETER prmDepFrom     AS DECIMAL        NO-UNDO.
DEFINE INPUT PARAMETER prmDepTo       AS DECIMAL        NO-UNDO.
DEFINE INPUT PARAMETER prmPartDscr    AS CHAR           NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrugatedBox.

def var k_frac as dec init 6.25 no-undo.
DEFINE VAR vQty AS DECIMAL NO-UNDO.
DEF BUFFER blast-eb FOR eb.
DEF VAR lv-last-est-no AS cha NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

IF prmUser      = ? THEN ASSIGN prmUser     = "".
IF prmAction    = ? THEN ASSIGN prmAction   = "Select".
IF prmType      = ? THEN ASSIGN prmType     = "".
IF prmComp      = ? THEN ASSIGN prmComp     = "".  
IF prmEstimate  = ? THEN ASSIGN prmEstimate = "".
IF prmCustomer  = ? THEN ASSIGN prmCustomer = "".
IF prmCustpart  = ? THEN ASSIGN prmCustpart = "".
IF prmFgItem    = ? THEN ASSIGN prmFgItem   = "".
IF prmStyle     = ? THEN ASSIGN prmStyle    = "".
IF prmShipTo    = ? THEN ASSIGN prmShipTo   = "". 
IF prmDie       = ? THEN ASSIGN prmDie      = "". 
IF prmCad       = ? THEN ASSIGN prmCad      = "". 
IF prmPlate     = ? THEN ASSIGN prmPlate    = "". 
IF prmSingle    = ? THEN ASSIGN prmSingle   = "". 
IF prmSet       = ? THEN ASSIGN prmSet      = "". 
IF prmTandem    = ? THEN ASSIGN prmTandem   = "". 
IF prmWidthFrom = ? THEN ASSIGN prmWidthFrom = 0. 
IF prmWidthTo   = ? THEN ASSIGN prmWidthTo  = 0. 
IF prmLenFrom   = ? THEN ASSIGN prmLenFrom  = 0. 
IF prmLenTo     = ? THEN ASSIGN prmLenTo    = 0. 
IF prmDepFrom   = ? THEN ASSIGN prmDepFrom  = 0. 
IF prmDepTo     = ? THEN ASSIGN prmDepTo    = 0.
IF prmPartDscr  = ? THEN ASSIGN prmPartDscr = "".
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .
{sys/inc/cerun.i F}
     {sys/inc/cerun.i C}

   FUNCTION display-qty-set RETURNS DECIMAL
     ( /* parameter-definitions */ ) :
     RETURN IF eb.cust-% EQ 0 THEN 1 ELSE eb.cust-%.   /* Function return value. */
   END FUNCTION.

   FUNCTION display-qty RETURNS INTEGER
     ( /* parameter-definitions */ ) :
     IF est.est-type = 4 OR est.est-type = 3 /* tandem conversion */
         THEN RETURN eb.bl-qty.
     ELSE  /* eb.yld-qty <> 0 THEN RETURN INT(eb.eqty * eb.yld-qty).  */
             RETURN INT(eb.eqty)    .
   END FUNCTION.

   FUNCTION display-combo-qty RETURNS DECIMAL
     ( /* parameter-definitions */ ) :
     DEF VAR lv-qty LIKE est-qty.eqty NO-UNDO.
     DEF BUFFER b-eb FOR eb.
     IF AVAIL est-qty AND AVAIL eb THEN DO:
       lv-qty = est-qty.eqty.
       FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
       IF AVAIL b-eb AND b-eb.est-type EQ 8 THEN lv-qty = b-eb.bl-qty.
     END.
     RETURN lv-qty.   /* Function return value. */
   END FUNCTION.


   FUNCTION display-cw-dim RETURNS DECIMAL
       ( input ip-is-corr-style as log, input  ip-dim as decimal ) :
       def var out-dim as dec no-undo.
       if ip-is-corr-style and ip-dim <> 0 then 
          /*round(trunc({1},0) + (({1} - trunc({1},0)) / K_FRAC),2)   sys/inc/k16.i */
          out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
       else out-dim = ip-dim.
       RETURN out-dim.   /* Function return value. */
   END FUNCTION.

   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

                                                   
 IF prmAction = "Select" THEN DO:
    IF prmType = "Corr" THEN DO:
 v-count = 0.
 MAIN-LOOP:

  
  FOR EACH eb WHERE  eb.company = prmComp AND (LOOKUP(eb.cust-no, custcount ) NE 0 OR  custcount = "" ) NO-LOCK,
        FIRST est WHERE est.company = eb.company 
                     AND est.est-no = eb.est-no AND est.est-type >= 5 NO-LOCK, 
            FIRST est-qty WHERE est-qty.company = eb.company and est-qty.est-no = eb.est-no 
                                and est-qty.eqty = eb.eqty NO-LOCK, 
                    FIRST ef WHERE ef.company = eb.company   AND ef.est-no = eb.est-no 
                                       AND ef.form-no = eb.form-no NO-LOCK BY eb.est-no DESC :
                          
             vQty = display-combo-qty().  

             ll-use-margin = NO.

            IF cerunf EQ "Fibre" THEN
                RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

            create ttCorrugatedBox.
            assign
                ttCorrugatedBox.vest            = est.est-no
                ttCorrugatedBox.vCust           = eb.cust-no
                ttCorrugatedBox.vCustpart       = eb.part-no
                ttCorrugatedBox.vQty            = vQty
                ttCorrugatedBox.vOrder          = eb.ord-no
                ttCorrugatedBox.vFgitem         = eb.stock-no
                ttCorrugatedBox.vStyle          = eb.style
                ttCorrugatedBox.vItemName       = eb.part-dscr1
                ttCorrugatedBox.vFlute          = eb.flute
                ttCorrugatedBox.vTest           = eb.test
                ttCorrugatedBox.vQtySet         = eb.yld-qty
                ttCorrugatedBox.vLength         = display-cw-dim(yes,eb.len) 
                ttCorrugatedBox.vWidth          = display-cw-dim(yes,eb.wid) 
                ttCorrugatedBox.vDepth          = display-cw-dim(yes,eb.dep)  
                ttCorrugatedBox.vDie            = eb.die-no
                ttCorrugatedBox.vCad            = eb.cad-no
                ttCorrugatedBox.vPlate          = eb.plate-no
                ttCorrugatedBox.vEstDate        = est.est-date
                ttCorrugatedBox.vModi     = est.updated-id
                ttCorrugatedBox.vShipTo         = eb.ship-id
                ttCorrugatedBox.vCreatedBy      = est.entered-id 
                ttCorrugatedBox.vreckey         = est.rec_key
                ttCorrugatedBox.vFormno         = eb.form-no  
                ttCorrugatedBox.vBlankno        = eb.blank-no 
                ttCorrugatedBox.vBoard          = ef.board
                ttCorrugatedBox.vLogic          = STRING( NOT ( ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre" ))                             
               .
               IF est.mod-date = 01/01/1900 THEN
                   ASSIGN ttCorrugatedBox.vModDate        = 1 .
               ELSE
                   ASSIGN ttCorrugatedBox.vModDate        = 0 .

                       v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/
    END.   /* and of type*/

   IF prmType = "Folding" THEN DO:
 v-count = 0.
 MAIN-LOOP:

  FOR EACH eb WHERE  eb.company = prmComp  AND (LOOKUP(eb.cust-no, custcount) NE 0 OR custcount = "" ) NO-LOCK, 
           FIRST est WHERE est.company = eb.company  AND est.est-no = eb.est-no
                    AND est.est-type >= 1 and est.est-type <= 4 NO-LOCK,
             FIRST est-qty WHERE est-qty.company = est.company  
                 AND est-qty.est-no = est.est-no 
                    AND est-qty.eqty = eb.eqty NO-LOCK, 
               FIRST ef WHERE ef.company = eb.company
                        AND ef.est-no = eb.est-no 
                             AND ef.form-no = eb.form-no NO-LOCK BY eb.est-no DESC :
                     
               ll-use-margin = NO.

            IF cerunf EQ "Fibre" THEN
                RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

              create ttCorrugatedBox.
            assign
                ttCorrugatedBox.vest            = est.est-no
                ttCorrugatedBox.vCust           = eb.cust-no
                ttCorrugatedBox.vCustpart       = eb.part-no
                ttCorrugatedBox.vQty            = display-qty()
                ttCorrugatedBox.vOrder          = eb.ord-no
                ttCorrugatedBox.vFgitem         = eb.stock-no
                ttCorrugatedBox.vStyle          = eb.style
                ttCorrugatedBox.vItemName       = eb.part-dscr1
                ttCorrugatedBox.vFlute          = ef.medium
                ttCorrugatedBox.vTest           = ef.flute
                ttCorrugatedBox.vQtySet         = display-qty-set()
                ttCorrugatedBox.vLength         = eb.len
                ttCorrugatedBox.vWidth          = eb.wid 
                ttCorrugatedBox.vDepth          = eb.dep  
                ttCorrugatedBox.vDie            = eb.die-no
                ttCorrugatedBox.vCad            = eb.cad-no
                ttCorrugatedBox.vPlate          = eb.plate-no
                ttCorrugatedBox.vEstDate        = est.est-date
                ttCorrugatedBox.vModi     = est.updated-id
                ttCorrugatedBox.vShipTo         = eb.ship-id
                ttCorrugatedBox.vCreatedBy      = est.entered-id 
                ttCorrugatedBox.vreckey         = est.rec_key
                ttCorrugatedBox.vFormno           = eb.form-no
                ttCorrugatedBox.vBlankno          = eb.blank-no
                ttCorrugatedBox.vBoard          = ef.board
                ttCorrugatedBox.vLogic           = STRING( NOT ( ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre" ))                
               .
               IF est.mod-date = 01/01/1900 THEN
                   ASSIGN ttCorrugatedBox.vModDate        = 1 .
               ELSE
                   ASSIGN ttCorrugatedBox.vModDate        = 0 .

                       v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
        END.  /**for each **/
    END.     /* else do */
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
  
 IF prmAction = "Search" THEN DO:
 DEFINE VAR Varsingle AS LOGICAL NO-UNDO.
 DEFINE VAR Varset AS LOGICAL NO-UNDO.
 DEFINE VAR Vartendem AS LOGICAL NO-UNDO.
 
 Varsingle  = IF prmSingle = "Yes" THEN   TRUE ELSE FALSE.
 Varset     = IF prmSet = "Yes" THEN   TRUE ELSE FALSE.
 Vartendem  = IF prmTandem = "Yes" THEN  TRUE ELSE FALSE.
 DEF VAR vEstim AS CHAR NO-UNDO.
ASSIGN  vEstim = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate).

IF prmType = "Corr" THEN DO:

   
   
 v-count = 0.
 MAIN-LOOP:
 
   FOR EACH eb WHERE eb.company = prmComp AND (LOOKUP(eb.cust-no, custcount ) NE 0 OR custcount = "" )
                 AND (eb.est-no = vEstim OR vEstim = "" )  
                 AND eb.form-no > 0 AND eb.blank-no > 0  
                 AND ( eb.cust-no BEGINS prmCustomer OR   prmCustomer = "" )                      
                 AND (eb.ship-id BEGINS prmShipTo   OR prmShipTo = "" )                        
                 AND (eb.ship-id EQ prmShipTo OR prmShipTo = ""  /* OR NOT ll-shipto */ )            
                 AND (eb.part-no BEGINS prmCustpart OR prmCustpart = "")                          
                 AND (eb.stock-no BEGINS prmFgItem OR prmFgItem = "")
                 AND (eb.part-dscr1 BEGINS prmPartDscr OR 
                      eb.part-dscr1 MATCHES prmPartDscr OR prmPartDscr = "" )
                 AND (eb.style BEGINS prmStyle OR prmStyle = "")                             
                 AND (eb.die-no BEGINS prmDie OR prmDie = "")                            
                 AND (eb.cad-no BEGINS prmCad OR prmCad = "")                            
                 AND (eb.plate-no BEGINS prmPlate OR prmPlate = "" )
                 AND (eb.len GE TRUNC(prmLenFrom,0) + ((prmLenFrom - TRUNC(prmLenFrom,0)) * k_frac) OR prmLenFrom = 0)
                 AND (eb.len LE TRUNC(prmLenTo,0) + ((prmLenTo - TRUNC(prmLenTo,0)) * k_frac)  OR prmLenTo = 0)
                 AND (eb.wid GE TRUNC(prmWidthFrom,0) + ((prmWidthFrom - TRUNC(prmWidthFrom,0)) * k_frac) OR prmWidthFrom = 0)
                 AND (eb.wid LE TRUNC(prmWidthTo,0) + ((prmWidthTo - TRUNC(prmWidthTo,0)) * k_frac) OR prmWidthTo = 0)
                 AND (eb.dep GE TRUNC(prmDepFrom,0) + ((prmDepFrom - TRUNC(prmDepFrom,0)) * k_frac) OR prmDepFrom = 0)
                 AND (eb.dep LE TRUNC(prmDepTo,0) + ((prmDepTo - TRUNC(prmDepTo,0)) * k_frac) OR prmDepTo = 0 )    NO-LOCK, 

      EACH est WHERE  est.company = prmComp AND est.est-type >= 5
         AND (est.est-no = eb.est-no )
         AND ((est.est-type EQ 5 AND Varsingle) OR (est.est-type EQ 6 AND Varset) OR (est.est-type GT 6 AND Vartendem))  NO-LOCK,

         EACH est-qty WHERE  est-qty.company = est.company 
         AND est-qty.est-no = est.est-no 
         AND est-qty.eqty = eb.eqty NO-LOCK,

         EACH  ef  WHERE ef.company = est.company 
         AND ef.est-no = est.est-no AND ef.form-no = eb.form-no NO-LOCK BY eb.est-no DESC: 
        
            
            ll-use-margin = NO.

            IF cerunf EQ "Fibre" THEN
                RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).

       
           create ttCorrugatedBox.
            assign
                ttCorrugatedBox.vest            = est.est-no
                ttCorrugatedBox.vCust           = eb.cust-no
                ttCorrugatedBox.vCustpart       = eb.part-no
                ttCorrugatedBox.vQty            = display-combo-qty()
                ttCorrugatedBox.vOrder          = eb.ord-no
                ttCorrugatedBox.vFgitem         = eb.stock-no
                ttCorrugatedBox.vStyle          = eb.style
                ttCorrugatedBox.vItemName       = eb.part-dscr1
                ttCorrugatedBox.vFlute          = eb.flute
                ttCorrugatedBox.vTest           = eb.test
                ttCorrugatedBox.vQtySet         = eb.yld-qty
                ttCorrugatedBox.vLength         = display-cw-dim(yes,eb.len) 
                ttCorrugatedBox.vWidth          = display-cw-dim(yes,eb.wid) 
                ttCorrugatedBox.vDepth          = display-cw-dim(yes,eb.dep) 
                ttCorrugatedBox.vDie            = eb.die-no
                ttCorrugatedBox.vCad            = eb.cad-no
                ttCorrugatedBox.vPlate          = eb.plate-no
                ttCorrugatedBox.vEstDate        = est.est-date
                ttCorrugatedBox.vModi     = est.updated-id
                ttCorrugatedBox.vShipTo         = eb.ship-id 
                ttCorrugatedBox.vCreatedBy      = est.entered-id 
                ttCorrugatedBox.vreckey         = est.rec_key
                ttCorrugatedBox.vFormno           = eb.form-no
                ttCorrugatedBox.vBlankno          = eb.blank-no
                ttCorrugatedBox.vBoard          = ef.board
                ttCorrugatedBox.vLogic           = STRING( NOT ( ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre" ))             
                . 
                IF est.mod-date = 01/01/1900 THEN
                   ASSIGN ttCorrugatedBox.vModDate        = 1 .
                ELSE
                   ASSIGN ttCorrugatedBox.vModDate        = 0 .

                  v-count = v-count + 1.
                  IF v-count = 50 THEN LEAVE MAIN-LOOP.
                  
                END.   /* end of for loop*/
            END.  /* end of if corr loop */



            IF prmType = "Folding" THEN DO:
                v-count = 0.
                MAIN-LOOP:
           
             FOR EACH eb WHERE eb.company = prmComp AND (LOOKUP(eb.cust-no, custcount ) NE 0 OR custcount = "" )
                 AND (eb.est-no = vEstim OR vEstim = "" )  
                 AND eb.form-no > 0 AND eb.blank-no > 0  
                 AND ( eb.cust-no BEGINS prmCustomer OR   prmCustomer = "" )                      
                 AND (eb.ship-id BEGINS prmShipTo   OR prmShipTo = "" )                        
                 AND (eb.ship-id EQ prmShipTo OR prmShipTo = ""  /* OR NOT ll-shipto */ )           
                 AND (eb.part-no BEGINS prmCustpart OR prmCustpart = "")                          
                 AND (eb.stock-no BEGINS prmFgItem OR prmFgItem = "")
                 AND (eb.part-dscr1 BEGINS prmPartDscr OR 
                      eb.part-dscr1 MATCHES prmPartDscr OR prmPartDscr = "" )
                 AND (eb.style BEGINS prmStyle OR prmStyle = "")                              
                 AND (eb.die-no BEGINS prmDie OR prmDie = "")                            
                 AND (eb.cad-no BEGINS prmCad OR prmCad = "")                            
                 AND (eb.plate-no BEGINS prmPlate OR prmPlate = "" )
                 AND ((eb.len GE prmLenFrom OR prmLenFrom = 0)  AND (eb.len LE prmLenTo OR prmLenTo = 0))
                 AND ((eb.wid GE prmWidthFrom OR prmWidthFrom = 0) AND (eb.wid LE prmWidthTo OR prmWidthTo = 0))
                 AND ((eb.dep GE prmDepFrom OR prmDepFrom = 0) AND (eb.dep LE prmDepTo OR prmDepTo = 0))   NO-LOCK, 

      EACH est WHERE  est.company = prmComp  AND (est.est-no = eb.est-no )
                AND est.est-type GE 1  AND est.est-type LE 4 
                     AND ((est.est-type EQ 1 AND Varsingle) OR  (est.est-type EQ 2 AND Varset)    OR  (est.est-type GT 2 AND Vartendem)) NO-LOCK,
         
         EACH est-qty WHERE  est-qty.company = est.company 
            AND est-qty.est-no = est.est-no 
              AND est-qty.eqty = eb.eqty NO-LOCK,

         EACH  ef  WHERE ef.company = est.company 
             AND ef.est-no = est.est-no AND ef.form-no = eb.form-no NO-LOCK BY eb.est-no DESC: 
          
                     ll-use-margin = NO.
    
            IF cerunf EQ "Fibre" THEN
                RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).
       
           create ttCorrugatedBox.
            assign
                ttCorrugatedBox.vest            = est.est-no
                ttCorrugatedBox.vCust           = eb.cust-no
                ttCorrugatedBox.vCustpart       = eb.part-no
                ttCorrugatedBox.vQty            = display-qty() 
                ttCorrugatedBox.vOrder          = eb.ord-no
                ttCorrugatedBox.vFgitem         = eb.stock-no
                ttCorrugatedBox.vStyle          = eb.style
                ttCorrugatedBox.vItemName       = eb.part-dscr1
                ttCorrugatedBox.vFlute          = ef.medium
                ttCorrugatedBox.vTest           = ef.flute
                ttCorrugatedBox.vQtySet         = display-qty-set()
                ttCorrugatedBox.vLength         = eb.len 
                ttCorrugatedBox.vWidth          = eb.wid 
                ttCorrugatedBox.vDepth          = eb.dep 
                ttCorrugatedBox.vDie            = eb.die-no
                ttCorrugatedBox.vCad            = eb.cad-no
                ttCorrugatedBox.vPlate          = eb.plate-no
                ttCorrugatedBox.vEstDate        = est.est-date
                ttCorrugatedBox.vModi     = est.updated-id
                ttCorrugatedBox.vShipTo         = eb.ship-id
                ttCorrugatedBox.vCreatedBy      = est.entered-id 
                ttCorrugatedBox.vreckey         = est.rec_key
                ttCorrugatedBox.vBlankno          = eb.blank-no
                ttCorrugatedBox.vBoard          = ef.board
                ttCorrugatedBox.vFormno           = eb.form-no
                ttCorrugatedBox.vLogic           = STRING( NOT ( ll-use-margin AND cerunf = "Fibre" AND cerunc = "Fibre" ))         
                . 
                IF est.mod-date = 01/01/1900 THEN
                   ASSIGN ttCorrugatedBox.vModDate        = 1 .
                ELSE
                   ASSIGN ttCorrugatedBox.vModDate        = 0 .

                  v-count = v-count + 1.
                  IF v-count = 50 THEN LEAVE MAIN-LOOP.
               END.   /* end of for loop*/
          END.  /* end else do */
 END. /* end search */


