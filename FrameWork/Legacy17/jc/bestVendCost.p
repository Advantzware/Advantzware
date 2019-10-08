
DEF INPUT PARAMETER   iprEb             AS ROWID NO-UNDO.
DEF INPUT PARAMETER   ipdQty            AS DEC NO-UNDO.
DEF INPUT PARAMETER   ipcUom            AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER  opdLowestRunCost  AS DEC NO-UNDO.
DEF OUTPUT PARAMETER  opdEstSetup       AS DEC NO-UNDO.
DEF OUTPUT PARAMETER  opcEstUOM         AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER   opcVend           AS CHAR NO-UNDO.

/* DEF /* INPUT PARAMETER */ VAR iprEb AS ROWID NO-UNDO.           */
/* DEF /* INPUT PARAMETER */ VAR ipdQty AS DEC NO-UNDO.            */
/* DEF /* INPUT PARAMETER */ VAR ipcUom AS CHAR NO-UNDO.           */
/* DEF /* OUTPUT PARAMETER */ VAR opdLowestRunCost AS DEC NO-UNDO. */
/* DEF /* OUTPUT PARMATER */ VAR opcVend AS CHAR NO-UNDO.          */

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR last-run-lev AS INT NO-UNDO.
/* DEF VAR i AS INT NO-UNDO. */
DEF VAR cUOM AS CHAR NO-UNDO.
DEF VAR lv-len LIKE item.s-len NO-UNDO.
DEF VAR lv-wid LIKE item.s-wid NO-UNDO.
DEF VAR lv-dep LIKE item.s-dep NO-UNDO.
DEF VAR llSelectedFound AS LOG NO-UNDO.

/* Testing */
/* ipdQty = 500.                                         */
/* ipcUom = "M".                                         */
/* FIND FIRST eb WHERE eb.pur-man AND eb.company = '001' */
/*     AND eb.est-no = "    7883"                        */
/*     AND eb.stock-no EQ "hew-set 9601-2".              */




FIND eb WHERE ROWID(eb) EQ iprEB NO-LOCK NO-ERROR.
FIND FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK NO-ERROR.

/* Log this error when a log is established */
IF NOT AVAIL ef THEN
    RETURN.

ASSIGN
 lv-len  = ef.gsh-len
 lv-wid  = ef.gsh-wid
 lv-dep  = ef.gsh-dep.

FIND itemfg WHERE itemfg.company EQ eb.company
    AND itemfg.i-no EQ eb.stock-no
    NO-LOCK NO-ERROR.
IF NOT AVAIL itemfg THEN
    RETURN.

/* Check for selected vendor for qty */
/* If no selected vendor, choose lowest cost */

llSelectedFound = NO.
IF eb.pur-man THEN
each-vend:
FOR EACH e-itemfg-vend NO-LOCK
    WHERE e-itemfg-vend.company  EQ eb.company
      AND e-itemfg-vend.est-no   EQ eb.est-no
      AND e-itemfg-vend.eqty     EQ eb.eqty
      AND e-itemfg-vend.form-no  EQ eb.form-no
      AND e-itemfg-vend.blank-no EQ eb.blank-no
    BREAK BY e-itemfg-vend.vend-no:                  

    /* Convert give UOM to that of e-itemfg-vend for comparison */
    IF ipcUOM NE e-itemfg-vend.std-uom THEN DO:
      DEF VAR ip-qty AS DEC.
      DEF VAR op-qty AS DEC.

      RUN sys/ref/convquom.p(ipcUom, e-itemfg-vend.std-uom,
                             itemfg.weight-100, lv-len, lv-wid, lv-dep,
                             ip-qty, OUTPUT op-qty).
    END.                                    

    
    last-run-lev = 0.
    DO i = 1 TO EXTENT(e-itemfg-vend.run-qty):
        IF e-itemfg-vend.run-qty[i] EQ 0 THEN
            NEXT.
        
        IF  ipdQty GT last-run-lev   THEN DO:



            IF e-itemfg-vend.run-cost[i] LT opdLowestRunCost
               OR e-itemfg-vend.SELECTED[i] 
               OR opdLowestRunCost EQ 0 THEN DO:

              ASSIGN opdLowestRunCost = e-itemfg-vend.run-cost[i]
                     opcVend          = e-itemfg-vend.vend-no
                     opdEstSetup      = e-itemfg-vend.setups[i]
                     cUOM           = e-itemfg-vend.std-uom
                     opcEstUOM      = e-itemfg-vend.std-uom.
      
              FIND FIRST reftable 
                WHERE reftable.reftable EQ "e-itemfg-vend.std-uom" 
                  AND reftable.company  EQ e-itemfg-vend.company   
                  AND reftable.loc      EQ ""                      
                  AND reftable.code     EQ e-itemfg-vend.est-no    
                  AND reftable.val[1]   EQ e-itemfg-vend.form-no   
                  AND reftable.val[2]   EQ e-itemfg-vend.blank-no
                NO-LOCK NO-ERROR.
              IF AVAIL reftable THEN
                  ASSIGN cUOM = reftable.code2
                         opcEstUOM = reftable.code2.
              IF e-itemfg-vend.SELECTED[i] THEN
                  llSelectedFound = TRUE.
            END. /* if cost is lower or selected */
            
            /* User checked this one off, so use it and exit loop */

        END. /* if qty is greater than previous max */

        last-run-lev = e-itemfg-vend.run-qty[i].

        /* If checked off then use this one and don't look at others */
        IF llSelectedFound THEN
                LEAVE each-vend.

    END. /* Do i = 1 to ... */
    

END. /* If pur-man */

/* Convert stored cost to given uom  */
IF ipcUOM NE cUOM THEN DO:
/*
  RUN sys/ref/convcuom.p(cUom, ipcUOM,
                         itemfg.weight-100, lv-len, lv-wid, lv-dep,
                         opdLowestRunCost, OUTPUT opdLowestRunCost).
  RUN sys/ref/convcuom.p(cUom, ipcUOM,
                         itemfg.weight-100, lv-len, lv-wid, lv-dep,
                         opdEstSetup, OUTPUT opdEstSetup).
*/
END.                                    
