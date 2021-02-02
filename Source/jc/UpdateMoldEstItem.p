
/*------------------------------------------------------------------------
    File        : UpdateMoldEstItem.p
    Purpose     : update packing item on molded job estimate  

    Syntax      :

    Description : Job Builder Procedure.

    Author(s)   : Sewa Singh
    Created     : Thur Jan 21 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO. 
DEFINE INPUT PARAMETER lUpdateEstQty AS LOGICAL NO-UNDO.

{est/ttInputEst.i}   
DEFINE VARIABLE lRoutingExist AS LOGICAL NO-UNDO.
DEFINE VARIABLE iLine AS INTEGER NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE BUFFER bff-eb FOR eb.
DEFINE BUFFER bf-ef FOR ef.
DEFINE BUFFER bf-estPacking FOR estPacking.
DEFINE BUFFER bf-est-op FOR est-op.
DEFINE BUFFER xop FOR est-op.
DEFINE BUFFER bf-ttInputEst FOR ttInputEst.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST eb NO-LOCK
    WHERE ROWID(eb) EQ ipriRowid NO-ERROR.
FIND FIRST est NO-LOCK
    WHERE est.company EQ eb.company
    AND est.est-no EQ eb.est-no NO-ERROR .
    
IF lUpdateEstQty THEN
DO:
  RUN pUpdateEstimateQty(ROWID(est)). 
END.    
     
FOR EACH ttInputEst NO-LOCK:
       IF ttInputEst.cFgEstNo NE "" THEN 
       DO:
          FIND FIRST bf-eb NO-LOCK
               WHERE bf-eb.company EQ eb.company
               AND bf-eb.est-no EQ ttInputEst.cFgEstNo
               AND bf-eb.stock-no EQ ttInputEst.cStockNo  NO-ERROR.
               
               IF AVAIL bf-eb THEN
               DO:
                  FIND FIRST ef NO-LOCK
                       WHERE ef.company EQ bf-eb.company
                       AND ef.est-no EQ bf-eb.est-no 
                       AND ef.form-no EQ bf-eb.form-no NO-ERROR.
                  FIND FIRST bff-eb EXCLUSIVE-LOCK
                      WHERE bff-eb.company EQ eb.company
                      AND bff-eb.est-no EQ eb.est-no
                      AND bff-eb.stock-no EQ ttInputEst.cStockNo NO-ERROR.
                  IF avail bff-eb THEN
                  DO:                  
                      ASSIGN
                        bff-eb.cas-no     = bf-eb.cas-no
                        bff-eb.tr-no      = bf-eb.tr-no
                        bff-eb.cas-len    = bf-eb.cas-len
                        bff-eb.cas-wid    = bf-eb.cas-wid
                        bff-eb.cas-dep    = bf-eb.cas-dep
                        bff-eb.cas-wt     = bf-eb.cas-wt
                        bff-eb.tr-len     = bf-eb.tr-len
                        bff-eb.tr-wid     = bf-eb.tr-wid
                        bff-eb.tr-dep     = bf-eb.tr-dep
                        bff-eb.tr-cas     = bf-eb.tr-cas
                        bff-eb.stacks     = bf-eb.stacks
                        bff-eb.stack-code = bf-eb.stack-code
                        bff-eb.cas-pal    = bf-eb.cas-pal
                        bff-eb.tr-cnt     = bf-eb.tr-cnt  .
                        
                      FIND FIRST bf-ef EXCLUSIVE-LOCK
                           WHERE bf-ef.company EQ bff-eb.company
                           AND bf-ef.est-no EQ bff-eb.est-no 
                           AND bf-ef.form-no EQ bff-eb.form-no NO-ERROR.
                      IF AVAIL bf-ef AND AVAIL ef THEN
                      DO:
                         ASSIGN
                         bf-ef.mis-snum[1] = ef.mis-snum[1]
                         bf-ef.mis-bnum[1] = ef.mis-bnum[1]
                         bf-ef.mis-cost[1] = ef.mis-cost[1] 
                         bf-ef.mis-matf[1] = ef.mis-matf[1]
                         bf-ef.mis-labf[1] = ef.mis-labf[1]
                         bf-ef.mis-matm[1] = ef.mis-matm[1]
                         bf-ef.mis-labm[1] = ef.mis-labm[1]
                         bf-ef.mis-simon[1] = ef.mis-simon[1]
                         bf-ef.mis-mkup[1]  = ef.mis-mkup[1]
                         bf-ef.mis-snum[2]  = ef.mis-snum[2]
                         bf-ef.mis-bnum[2]  = ef.mis-bnum[2]
                         bf-ef.mis-cost[2]  = ef.mis-cost[2]
                         bf-ef.mis-matf[2]  = ef.mis-matf[2]
                         bf-ef.mis-labf[2]  = ef.mis-labf[2]
                         bf-ef.mis-matm[2]  = ef.mis-matm[2]
                         bf-ef.mis-labm[2]  = ef.mis-labm[2]
                         bf-ef.mis-simon[2] = ef.mis-simon[2]
                         bf-ef.mis-mkup[2]  = ef.mis-mkup[2]
                         bf-ef.mis-snum[3]  = ef.mis-snum[3]
                         bf-ef.mis-bnum[3]  = ef.mis-bnum[3]
                         bf-ef.mis-cost[3]  = ef.mis-cost[3]
                         bf-ef.mis-matf[3]  = ef.mis-matf[3]
                         bf-ef.mis-labf[3]  = ef.mis-labf[3]
                         bf-ef.mis-matm[3]  = ef.mis-matm[3]
                         bf-ef.mis-labm[3]  = ef.mis-labm[3]
                         bf-ef.mis-simon[3] = ef.mis-simon[3]
                         bf-ef.mis-mkup[3]  = ef.mis-mkup[3]
                         bf-ef.mis-snum[4]  = ef.mis-snum[4]
                         bf-ef.mis-bnum[4]  = ef.mis-bnum[4]
                         bf-ef.mis-cost[4]  = ef.mis-cost[4]
                         bf-ef.mis-matf[4]  = ef.mis-matf[4]
                         bf-ef.mis-labf[4]  = ef.mis-labf[4]
                         bf-ef.mis-matm[4]  = ef.mis-matm[4] 
                         bf-ef.mis-labm[4]  = ef.mis-labm[4]
                         bf-ef.mis-simon[4] = ef.mis-simon[4] 
                         bf-ef.mis-mkup[4]  = ef.mis-mkup[4]
                         bf-ef.mis-snum[5]  = ef.mis-snum[5]
                         bf-ef.mis-bnum[5]  = ef.mis-bnum[5]
                         bf-ef.mis-cost[5]  = ef.mis-cost[5]
                         bf-ef.mis-matf[5]  = ef.mis-matf[5]
                         bf-ef.mis-labf[5]  = ef.mis-labf[5]
                         bf-ef.mis-matm[5]  = ef.mis-matm[5]
                         bf-ef.mis-labm[5]  = ef.mis-labm[5]
                         bf-ef.mis-simon[5] = ef.mis-simon[5]
                         bf-ef.mis-mkup[5]  = ef.mis-mkup[5]
                         bf-ef.mis-snum[6]  = ef.mis-snum[6]
                         bf-ef.mis-bnum[6]  = ef.mis-bnum[6]
                         bf-ef.mis-cost[6]  = ef.mis-cost[6]
                         bf-ef.mis-matf[6]  = ef.mis-matf[6]
                         bf-ef.mis-labf[6]  = ef.mis-labf[6]
                         bf-ef.mis-matm[6]  = ef.mis-matm[6]
                         bf-ef.mis-labm[6]  = ef.mis-labm[6]
                         bf-ef.mis-simon[6] = ef.mis-simon[6]
                         bf-ef.mis-mkup[6]  = ef.mis-mkup[6] 
                         bf-ef.spec-no[1]   = ef.spec-no[1]
                         bf-ef.spec-dscr[1] = ef.spec-dscr[1] 
                         bf-ef.spec-uom[1]  = ef.spec-uom[1]
                         bf-ef.spec-no[2]   = ef.spec-no[2]
                         bf-ef.spec-dscr[2] = ef.spec-dscr[2]
                         bf-ef.spec-uom[2]  = ef.spec-uom[2]
                         bf-ef.spec-no[3]   = ef.spec-no[3]
                         bf-ef.spec-dscr[3] = ef.spec-dscr[3]
                         bf-ef.spec-uom[3]  = ef.spec-uom[3]
                         bf-ef.spec-no[4]   = ef.spec-no[4]
                         bf-ef.spec-dscr[4] = ef.spec-dscr[4]
                         bf-ef.spec-uom[4]  = ef.spec-uom[4]
                         bf-ef.spec-no[5]   = ef.spec-no[5]
                         bf-ef.spec-dscr[5] = ef.spec-dscr[5]
                         bf-ef.spec-uom[5]  = ef.spec-uom[5]
                         bf-ef.spec-no[6]   = ef.spec-no[6]
                         bf-ef.spec-dscr[6] = ef.spec-dscr[6]
                         bf-ef.spec-uom[6]  = ef.spec-uom[6]
                         bf-ef.spec-no[7]   = ef.spec-no[7]
                         bf-ef.spec-dscr[7] = ef.spec-dscr[7]
                         bf-ef.spec-uom[7]  = ef.spec-uom[7] 
                         bf-ef.spec-no[8]   = ef.spec-no[8]
                         bf-ef.spec-dscr[8] = ef.spec-dscr[8]
                         bf-ef.spec-uom[8] = ef.spec-uom[8]
                         bf-ef.spec-qty    = ef.spec-qty.                            
                          
                      END.                            
                                              
                      FOR EACH estPacking NO-LOCK 
                          WHERE estPacking.company EQ bf-eb.company 
                          AND estPacking.estimateNo EQ bf-eb.est-no 
                          AND estPacking.FormNo  EQ bf-eb.form-no 
                          AND estPacking.BlankNo  EQ bf-eb.blank-No :  
                          
                          FIND FIRST bf-estPacking NO-LOCK 
                              WHERE bf-estPacking.company EQ bff-eb.company 
                              AND bf-estPacking.estimateNo EQ bff-eb.est-no 
                              AND bf-estPacking.FormNo  EQ bff-eb.form-no 
                              AND bf-estPacking.BlankNo  EQ bff-eb.blank-No
                              AND bf-estPacking.rmItemID EQ estPacking.rmItemID NO-ERROR.
                          IF NOT AVAIL bf-estPacking THEN
                          DO:                               
                              CREATE bf-estPacking .
                              ASSIGN
                              bf-estPacking.company      = bff-eb.company 
                              bf-estPacking.estimateNo   = bff-eb.est-no
                              bf-estPacking.FormNo       = bff-eb.form-no
                              bf-estPacking.BlankNo      = bff-eb.blank-No 
                              bf-estPacking.rmItemID     = estPacking.rmItemID.
                              
                              BUFFER-COPY estPacking EXCEPT company estimateNo FormNo  BlankNo estPackingID rmItemID rec_key TO bf-estPacking.
                          END.
                      END.
                      RELEASE bf-estPacking NO-ERROR .
                      
                      FOR EACH est-op  NO-LOCK
                          WHERE est-op.company EQ bf-eb.company
                          AND est-op.est-no  EQ bf-eb.est-no                          
                          AND est-op.b-num   NE 0
                          AND est-op.line    LT 500:
                          
                          FOR EACH bf-ttInputEst NO-LOCK:
                                      
                              FIND FIRST bf-est-op NO-LOCK
                                   WHERE bf-est-op.company EQ bff-eb.company
                                   AND bf-est-op.est-no  EQ bff-eb.est-no
                                   AND bf-est-op.s-num   EQ bf-ttInputEst.iFormNo
                                   AND bf-est-op.b-num  EQ bf-ttInputEst.iBlankNo
                                   AND bf-est-op.line    LT 500
                                   AND bf-est-op.m-code  EQ est-op.m-code NO-ERROR.
                              IF NOT AVAIL bf-est-op THEN
                              DO:          
                                  iLine = 1.                              
                                  FOR EACH xop
                                      WHERE xop.company EQ est.company
                                      AND xop.est-no  EQ est.est-no
                                      AND xop.line    LT 500
                                      NO-LOCK
                                      BY xop.line DESCENDING:
                                      iLine = xop.line + 1.
                                      LEAVE.
                                  END.
                                  
                                  CREATE bf-est-op .
                                  ASSIGN
                                  bf-est-op.company  = bff-eb.company 
                                  bf-est-op.est-no   = bff-eb.est-no
                                  bf-est-op.s-num    = bf-ttInputEst.iFormNo
                                  bf-est-op.b-num    = bf-ttInputEst.iBlankNo
                                  bf-est-op.qty      = est.est-qty[1] 
                                  bf-est-op.LINE     = iLine
                                  .                 
                                  BUFFER-COPY est-op EXCEPT company est-no s-num  b-num qty rec_key LINE TO bf-est-op.
                                  
                              END.  /* NOT AVAIL bf-est-op*/                       
                          END.   /* FOR EACH bf-ttInputEst*/
                      END. 
                  END.
                   
               END.       
       END.        
END.  


PROCEDURE pUpdateEstimateQty:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    DEFINE BUFFER bf-est FOR est.
    
    FIND FIRST bf-est NO-LOCK
    WHERE ROWID(bf-est) EQ iprwRowid
    NO-ERROR.
    
    FOR EACH ttInputEst NO-LOCK:
    
     FIND FIRST bf-eb EXCLUSIVE-LOCK
          WHERE bf-eb.company EQ bf-est.company
          AND bf-eb.est-no EQ bf-est.est-no
          AND bf-eb.stock-no EQ ttInputEst.cStockNo NO-ERROR.
          
     IF AVAIL bf-eb THEN
     DO:
         bf-eb.bl-qty = ttInputEst.iQuantityYield.
         bf-eb.eqty   = ttInputEst.iQuantityYield.
                  
         FIND FIRST est-qty
              WHERE est-qty.company EQ bf-est.company
              AND est-qty.est-no  EQ bf-est.est-no
              AND est-qty.eqty    EQ bf-eb.bl-qty
              NO-ERROR.
          
          IF NOT AVAIL est-qty THEN
          FIND FIRST est-qty
              WHERE est-qty.company EQ bf-est.company
                AND est-qty.est-no  EQ bf-est.est-no
              NO-ERROR.

          IF AVAIL est-qty THEN DO:
              FOR EACH est-op
                  WHERE est-op.company EQ bf-est.company
                    AND est-op.est-no  EQ bf-est.est-no
                    AND est-op.qty     EQ est-qty.eqty:
                est-op.qty = bf-eb.bl-qty.
              END.

              est-qty.eqty = bf-eb.bl-qty.
                   
              FIND CURRENT est-qty NO-LOCK.
               
              /*== update all eb,ef eqty field ==*/
              FOR EACH bff-eb WHERE bff-eb.company = est-qty.company AND
                                   bff-eb.est-no = est-qty.est-no:
                  ASSIGN bff-eb.eqty = est-qty.eqty.
              END.  
              FOR EACH bf-ef WHERE bf-ef.company = est-qty.company AND
                                   bf-ef.est-no = est-qty.est-no:
                     bf-ef.eqty = est-qty.eqty.
              END.
              FOR EACH est-flm WHERE est-flm.company = est-qty.company AND
                                   est-flm.est-no = est-qty.est-no:
                     est-flm.eqty = est-qty.eqty.
              END.

              FIND bf-est WHERE bf-est.company = est-qty.company AND
                                bf-est.est-no = est-qty.est-no.
              bf-est.est-qty[1] = est-qty.eqty.
          END.                   
     END.
    END.
    RELEASE bff-eb.
    RELEASE bf-eb.
    RELEASE bf-ef.
    RELEASE est-flm.
    RELEASE bf-est.
    RELEASE est-qty.
    
END PROCEDURE.   
     
