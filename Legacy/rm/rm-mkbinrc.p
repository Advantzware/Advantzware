/* -------------------------------------------------------------------------- */

/*----------------------------------------------------------------------
Program     : rm/rm-mkbinrc.p

Description : raw materials bin rebuild program for recalc of qty

Copyright(c): Advanced Software Services Inc. 2013
Author      : Wade Kaldawi
Created     : 03/06/2013
Notes       :

------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions     ----------------------------------------- */

DEF INPUT PARAMETER ipr-item-row AS ROWID NO-UNDO.

/* Local Variable Definitions ----------------------------------------- */
/*
DEF VAR v-r-qty     AS   DEC                    NO-UNDO.
DEF VAR v-i-qty     AS   DEC                    NO-UNDO.
DEF VAR v-t-qty     AS   DEC                    NO-UNDO.
DEF VAR ld-qty      AS   DEC                    NO-UNDO.
DEF VAR ld-cst      AS   DEC                    NO-UNDO.
DEF VAR lv-uom      AS   CHAR                   NO-UNDO.
  */
/* Include Files              ----------------------------------------- */

/* Function Forwards          ----------------------------------------- */

/* ***************************  Main Block  *************************** */
FIND ITEM WHERE ROWID(ITEM) EQ ipr-item-row EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL ITEM THEN DO:
  RUN rmmkbin1 (INPUT TODAY, INPUT ROWID(ITEM)).
END.

/* **********************  Internal Procedures  *********************** */
PROCEDURE rmmkbin1:
  
  DEF INPUT PARAMETER ip-today AS DATE NO-UNDO.
  DEF INPUT PARAMETER iprow-item AS ROWID NO-UNDO.
  
  DEF BUFFER bf-item FOR ITEM.
  
  FIND bf-ITEM WHERE ROWID(bf-ITEM) EQ iprow-item EXCLUSIVE-LOCK.
  
  DEF BUFFER bf-rm-bin FOR rm-bin.
  
  FOR EACH bf-rm-bin
    WHERE bf-rm-bin.company        EQ bf-item.company
      AND bf-rm-bin.i-no           EQ bf-item.i-no
    USE-INDEX i-no:
    ASSIGN
    bf-rm-bin.qty  = 0
    bf-rm-bin.cost = 0.
  END.
  
  IF ip-today EQ TODAY THEN
  FOR EACH rm-rcpth
    WHERE rm-rcpth.company      EQ bf-item.company
      AND rm-rcpth.i-no         EQ bf-item.i-no
    NO-LOCK USE-INDEX i-no,
    
    EACH rm-rdtlh
      WHERE rm-rdtlh.r-no         EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code    EQ rm-rcpth.rita-code
      USE-INDEX rm-rdtl NO-LOCK
    
    BY rm-rcpth.trans-date
    BY rm-rcpth.r-no:
    
    FIND FIRST bf-rm-bin
      WHERE bf-rm-bin.company EQ bf-item.company
        AND bf-rm-bin.i-no    EQ rm-rcpth.i-no
        AND bf-rm-bin.loc     EQ rm-rdtlh.loc
        AND bf-rm-bin.loc-bin EQ rm-rdtlh.loc-bin
        AND bf-rm-bin.tag     EQ rm-rdtlh.tag
      USE-INDEX loc-bin NO-ERROR.
    
    IF NOT avail bf-rm-bin THEN DO:      
      CREATE bf-rm-bin.
      ASSIGN
      bf-rm-bin.company    = bf-item.company
      bf-rm-bin.loc        = rm-rdtlh.loc
      bf-rm-bin.loc-bin    = rm-rdtlh.loc-bin
      bf-rm-bin.tag        = rm-rdtlh.tag
      bf-rm-bin.i-no       = rm-rcpth.i-no.
    END.
    
    RUN rm-mkbin-i (INPUT ROWID(rm-rcpth),
                    INPUT ROWID(rm-rdtlh),
                    INPUT ROWID(bf-item),
                    INPUT ROWID(bf-rm-bin),
                    INPUT NO).
    
  END. /* each rm-rcpth */
  ELSE
  FOR EACH rm-rcpth
    WHERE rm-rcpth.company      EQ bf-item.company
      AND rm-rcpth.i-no         EQ bf-item.i-no
      AND rm-rcpth.trans-date  LE ip-today
    NO-LOCK USE-INDEX i-no,
    
    EACH rm-rdtlh
        WHERE rm-rdtlh.r-no         EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code    EQ rm-rcpth.rita-code
        USE-INDEX rm-rdtl NO-LOCK
    
    BY rm-rcpth.trans-date
    BY rm-rcpth.r-no:
    
    FIND FIRST bf-rm-bin
      WHERE bf-rm-bin.company EQ bf-item.company
        AND bf-rm-bin.i-no    EQ rm-rcpth.i-no
        AND bf-rm-bin.loc     EQ rm-rdtlh.loc
        AND bf-rm-bin.loc-bin EQ rm-rdtlh.loc-bin
        AND bf-rm-bin.tag     EQ rm-rdtlh.tag
      USE-INDEX loc-bin NO-ERROR.
    IF NOT avail bf-rm-bin THEN DO:
      CREATE bf-rm-bin.
      ASSIGN
        bf-rm-bin.company = bf-item.company
        bf-rm-bin.loc     = rm-rdtlh.loc
        bf-rm-bin.loc-bin = rm-rdtlh.loc-bin
        bf-rm-bin.tag     = rm-rdtlh.tag
        bf-rm-bin.i-no    = rm-rcpth.i-no.
    END.
    RUN rm-mkbin-i (INPUT ROWID(rm-rcpth),
    INPUT ROWID(rm-rdtlh),
    INPUT ROWID(bf-item),
    INPUT ROWID(bf-rm-bin),
    INPUT NO).
    /*  {rm/rm-mkbin.i bf-} */
  END. /* each rm-rcpth */
  
END PROCEDURE.

PROCEDURE rm-mkbin-i:
  
  DEF INPUT PARAMETER ip-rm-rcpth-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-rm-rdtlh-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-item-row     AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-rm-bin-row   AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-use-item-last-cost AS LOG NO-UNDO.
  
  DEF BUFFER bf-item FOR ITEM.
  DEF BUFFER bf-rm-rcpth FOR rm-rcpth.
  DEF BUFFER bf2-rm-rcpth FOR rm-rcpth.
  DEF BUFFER bf-rm-rdtlh FOR rm-rdtlh.
  DEF BUFFER bf-rm-bin FOR rm-bin.

  DEF VAR ld-qty      AS   DEC                    NO-UNDO.
  DEF VAR ld-cst      AS   DEC                    NO-UNDO.
  DEF VAR lv-uom      AS   CHAR                   NO-UNDO.

  FIND bf-item WHERE ROWID(bf-item) EQ ip-item-row NO-LOCK.
  
  FIND bf-rm-rcpth WHERE ROWID(bf-rm-rcpth) EQ ip-rm-rcpth-row
       NO-LOCK.
  
  FIND FIRST bf-rm-rdtlh WHERE ROWID(bf-rm-rdtlh) EQ ip-rm-rdtlh-row
       NO-LOCK.

  FIND FIRST bf-rm-bin WHERE ROWID(bf-rm-bin) EQ ip-rm-bin-row
       NO-LOCK.
  
  ASSIGN
    lv-uom = bf-rm-rcpth.pur-uom
    ld-qty = bf-rm-rdtlh.qty
    ld-cst = bf-rm-rdtlh.cost.


  IF lv-uom EQ "" THEN DO: 
    IF bf-rm-rcpth.rita-code EQ "T" THEN DO:
       RUN getFirstUOM (INPUT ROWID(bf-rm-rcpth), INPUT ROWID(bf-rm-rdtlh),
                        OUTPUT lv-uom).

       IF lv-uom EQ ? THEN 
           lv-uom = ITEM.cons-uom.
    END.
    ELSE
      lv-uom = ITEM.cons-uom.
    IF bf-rm-rcpth.pur-uom EQ "" AND lv-uom GT "" THEN DO:
        FIND bf2-rm-rcpth 
          WHERE ROWID(bf2-rm-rcpth) EQ ROWID(bf-rm-rcpth)
          EXCLUSIVE-LOCK.
        bf-rm-rcpth.pur-uom = lv-uom.
        RELEASE bf2-rm-rcpth.
    END.

  END.    
  
  IF ITEM.cons-uom NE lv-uom THEN DO:
        
    RUN custom/convquom.p (bf-rm-rcpth.company, lv-uom, ITEM.cons-uom,
    ITEM.basis-w, (IF ITEM.r-wid EQ 0 THEN ITEM.s-len ELSE 12),
    (IF ITEM.r-wid EQ 0 THEN ITEM.s-wid ELSE ITEM.r-wid), ITEM.s-dep,
    ld-qty, OUTPUT ld-qty).
    
    RUN custom/convcuom.p (bf-rm-rcpth.company, lv-uom, ITEM.cons-uom,
    ITEM.basis-w, (IF ITEM.r-wid EQ 0 THEN ITEM.s-len ELSE 12),
    (IF ITEM.r-wid EQ 0 THEN ITEM.s-wid ELSE ITEM.r-wid), ITEM.s-dep,
    ld-cst, OUTPUT ld-cst).    
    
  END.
  
  IF ld-cst NE ?                              
    AND (bf-rm-rcpth.rita-code EQ "R" OR (CAN-DO("A,T",bf-rm-rcpth.rita-code) 
    AND  ld-cst NE 0))                          THEN DO:
        
    IF bf-rm-rcpth.rita-code EQ "A" THEN bf-rm-bin.cost = ld-cst.
    RUN rm-post (INPUT ROWID(bf-rm-bin),
                 INPUT ld-qty,
                 INPUT ld-cst).    
    
    IF ip-use-item-last-cost THEN ITEM.last-cost = ld-cst.
    
  END. /* R */
  
  IF INDEX("RATC",bf-rm-rcpth.rita-code) NE 0 THEN DO:

    bf-rm-bin.qty = ld-qty +
                 IF bf-rm-rcpth.rita-code EQ "C" THEN 0 ELSE bf-rm-bin.qty.
    
END.
  ELSE DO:
      
          bf-rm-bin.qty = bf-rm-bin.qty - ld-qty.
  END.

END PROCEDURE.

PROCEDURE rm-post:
  /* Rm Posting - Calculate new average cost for the bin                        */
  DEF INPUT PARAMETER ipr-rm-bin AS ROWID NO-UNDO. /* rm-bin record */
  DEF INPUT PARAMETER ipdLdQty   AS DEC   NO-UNDO. /* Transaction Quantity */
  DEF INPUT PARAMETER ipdLdCost  AS DEC   NO-UNDO.
  
  DEFINE VARIABLE v-r-qty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-i-qty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-t-qty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ldVal1  AS DECIMAL DECIMALS 2 NO-UNDO.
  DEFINE VARIABLE ldVal2  AS DECIMAL DECIMALS 2 NO-UNDO.
  DEF BUFFER bf-rm-bin FOR rm-bin.

  FIND FIRST bf-rm-bin WHERE ROWID(bf-rm-bin) EQ ipr-rm-bin EXCLUSIVE-LOCK NO-ERROR.
  
  IF NOT AVAIL bf-rm-bin THEN
  RETURN.
  
  ASSIGN
    v-r-qty = ipdLdQty        /* ld-qty */
    v-i-qty = bf-rm-bin.qty . /* rm-bin.qty */
  
  IF (v-r-qty + v-i-qty EQ 0) THEN DO:
    IF v-r-qty LT 0 THEN v-r-qty = v-r-qty * -1.
    ELSE
    IF v-i-qty LT 0 THEN v-i-qty = v-i-qty * -1.
  END.
  
  /* Takes first total cost + 2nd total cost / total qty to get avg. cost */
  IF v-r-qty LT 0 OR v-i-qty LE 0 THEN DO:
      /* Make sure these two numbers are not very close to zero */
      /* When added together. So check if there difference is zero */
      /* when they only have 2 decimals */
      ASSIGN
        ldVal1 = abs(v-r-qty)
        ldVal2 = abs(v-i-qty).
      IF v-r-qty + v-i-qty / ABS(v-r-qty) + ABS(v-i-qty) LT .01 
          OR ldVal1 - ldVal2 EQ 0 THEN
        ASSIGN
          v-r-qty = ABS(v-r-qty)
          v-i-qty = ABS(v-i-qty).      
  END.
  
  ASSIGN
    v-t-qty  = v-i-qty + v-r-qty
    bf-rm-bin.cost = ((v-i-qty * bf-rm-bin.cost) + (v-r-qty * ipdLdCost)) / v-t-qty.
  
  IF bf-rm-bin.cost LT 0 THEN bf-rm-bin.cost = bf-rm-bin.cost * -1.
  
  IF bf-rm-bin.cost EQ ? THEN bf-rm-bin.cost = 0.

END PROCEDURE.

PROCEDURE getFirstUOM:
DEF INPUT PARAMETER ipr-rm-rcpth AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-rm-rdtlh AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opc-uom     AS CHAR  NO-UNDO.

DEF BUFFER bf-rm-rcpth FOR rm-rcpth.
DEF BUFFER bf-rm-rdtlh FOR rm-rdtlh.
DEF BUFFER bf2-rm-rcpth FOR rm-rcpth.
DEF BUFFER bf2-rm-rdtlh FOR rm-rdtlh.

FIND bf-rm-rcpth 
  WHERE ROWID(bf-rm-rcpth) EQ ipr-rm-rcpth
  NO-LOCK NO-ERROR.
FIND bf-rm-rdtlh
  WHERE ROWID(bf-rm-rdtlh) EQ ipr-rm-rdtlh
  NO-LOCK NO-ERROR.
IF AVAIL bf-rm-rcpth THEN DO:
  IF bf-rm-rdtlh.tag GT "" THEN DO:
  
    FOR EACH bf2-rm-rcpth 
      WHERE bf2-rm-rcpth.company EQ bf-rm-rcpth.company
        AND bf2-rm-rcpth.i-no    EQ bf-rm-rcpth.i-no
        AND bf2-rm-rcpth.rita-code EQ "R"
      NO-LOCK,
      FIRST bf2-rm-rdtlh
        WHERE bf2-rm-rdtlh.r-no EQ bf2-rm-rcpth.r-no
          AND bf2-rm-rdtlh.rita-code EQ bf2-rm-rcpth.rita-code
          AND bf2-rm-rdtlh.tag    EQ bf-rm-rdtlh.tag
         BY bf2-rm-rcpth.trans-date:
      LEAVE.
    END.

  END.
  IF NOT AVAIL bf2-rm-rcpth AND bf-rm-rdtlh.job-no GT "" THEN DO:
      FOR EACH bf2-rm-rcpth 
        WHERE bf2-rm-rcpth.company EQ bf-rm-rcpth.company
          AND bf2-rm-rcpth.i-no    EQ bf-rm-rcpth.i-no
          AND bf2-rm-rcpth.rita-code EQ "R"
        NO-LOCK,
        FIRST bf2-rm-rdtlh
          WHERE bf2-rm-rdtlh.r-no EQ bf2-rm-rcpth.r-no
            AND bf2-rm-rdtlh.rita-code EQ bf2-rm-rcpth.rita-code
            AND bf2-rm-rdtlh.job-no    EQ bf-rm-rdtlh.job-no
            AND bf2-rm-rdtlh.job-no2   EQ bf-rm-rdtlh.job-no2
           BY bf2-rm-rcpth.trans-date:
        LEAVE.
      END.
  END.
  IF NOT AVAIL bf2-rm-rdtlh THEN DO:
      FOR EACH bf2-rm-rcpth 
        WHERE bf2-rm-rcpth.company EQ bf-rm-rcpth.company
          AND bf2-rm-rcpth.i-no    EQ bf-rm-rcpth.i-no
          AND bf2-rm-rcpth.rita-code EQ "R"
        NO-LOCK BY bf2-rm-rcpth.trans-date:
        LEAVE.
      END.
  END.
  IF AVAIL bf2-rm-rcpth THEN
    opc-uom = bf2-rm-rcpth.pur-uom.
  ELSE
    opc-uom = ?.
END.
                   
END PROCEDURE.
/* end ---------------------------------- copr. 2013  advanced software, inc. */


