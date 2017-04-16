/* jc/writeFarmFromPO.p */
/* Updates job-farm when po is updated */
DEF INPUT PARAMETER iprPoOrdl AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcJob-no AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcJob-no2 AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcOrdNo   AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcINo     AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcSnum    AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcBNum    AS CHAR NO-UNDO.

DEF BUFFER bfJob-farm FOR job-farm.
DEF BUFFER bfJob-hdr FOR job-hdr.
DEF BUFFER bfItemfg FOR itemfg.
DEF BUFFER bfPoOrdl FOR po-ordl.

DEF VAR dQtyEa AS DEC NO-UNDO.
DEF VAR dCostM LIKE po-ordl.cost.
DEF VAR lHeadFound AS LOG NO-UNDO.
DEF VAR lcCust-NO AS CHAR NO-UNDO.
DEF VAR cJob AS CHAR NO-UNDO.
DEF VAR iJobNo2 AS INT NO-UNDO.
DEF VAR lFromOrd AS LOG NO-UNDO.

FIND bfPoOrdl WHERE ROWID(bfPoOrdl) EQ iprPoOrdl NO-LOCK NO-ERROR.
IF NOT AVAIL bfPoOrdl THEN
    RETURN.

ASSIGN lHeadFound = NO
       cJob       = ""
       iJobNo2    = 0
       lFromOrd   = FALSE.

DO WITH FRAME farm-frame:
    
  IF ipcJob-no GT "" THEN
      ASSIGN cJob = ipcJob-no
             iJobNo2 = INT(ipcJob-no2).
  ELSE IF ipcOrdNo GT "" THEN  DO:
      
      FIND FIRST oe-ordl WHERE oe-ordl.company EQ bfPoOrdl.company
          AND oe-ordl.ord-no EQ INTEGER(ipcOrdNo)
          AND oe-ordl.i-no   EQ  ipcINo
          NO-LOCK NO-ERROR.
      /* assumption is that for farm jobs, order and job are always the same */
      IF NOT AVAIL oe-ordl THEN
          FIND FIRST oe-ordl WHERE oe-ordl.company EQ bfPoOrdl.company
              AND oe-ordl.ord-no EQ INTEGER(ipcOrdNo)
              AND oe-ordl.job-no   EQ  ipcOrdNo
              NO-LOCK NO-ERROR.
      lFromOrd = TRUE.
      IF AVAIL oe-ordl AND oe-ordl.job-no GT "" THEN
          ASSIGN cJob = oe-ordl.job-no
                 iJobNo2 = oe-ordl.job-no2.
      
  END.

  IF cJob EQ "" THEN
      RETURN.
  FIND FIRST bfJob-farm 
    WHERE bfJob-farm.company EQ bfPoOrdl.company
      AND bfJob-farm.job-no  EQ cJob 
      AND bfJob-farm.job-no2 EQ iJobNo2 
      AND bfJob-farm.i-no EQ ipcINo 
      AND (bfJob-farm.frm     EQ INT(ipcSnum) OR lFromOrd)
      AND (bfJob-farm.blank-no EQ INT(ipcBNum) OR lFromOrd)
    EXCLUSIVE-LOCK NO-ERROR.
  
  IF AVAIL bfJob-Farm THEN DO:

    dQtyEa = bfPoOrdl.ord-qty.
    IF bfPoOrdl.pr-qty-uom NE "EA" THEN
        run sys/ref/convquom.p(input bfPoOrdl.pr-qty-uom,
                               input "EA", input 0,
                               input bfPoOrdl.s-len,
                               input bfPoOrdl.s-wid,
                               input 0,
                               input bfPoOrdl.ord-qty,
                               output dQtyEa).
    dCostM = bfPoOrdl.cost.
    IF bfPoOrdl.pr-uom NE "M" THEN
      run sys/ref/convcuom.p(bfPoOrdl.pr-uom, "M", 0, 0, 0, 0,
                             bfPoOrdl.cost, output dCostM). 
    
    ASSIGN
      bfJob-farm.po-no = STRING(bfPoOrdl.po-no)
      /* bfJob-farm.vend-po-qty = po-ordl.qty */
      bfJob-farm.qty          = dQtyEa 
      bfJob-farm.vend-po-qty  = dQtyEa
      bfJob-farm.pur-uom      = bfPoOrdl.pr-qty-uom
      bfJob-farm.po-cost      = dCostM
      bfJob-farm.po-setup     = bfPoOrdl.setup      
/*       bfJob-farm.act-tot-cost = (dCostM * dQtyEa) / 1000 + bfPoOrdl.setup                    */
/*       bfJob-farm.act-cost     = ((dQtyEa / 1000 * dCostM) + bfPoOrdl.setup) / dQtyEa * 1000  */
      .

/*     IF bfPoOrdl.pr-uom NE "M" THEN DO:                                                 */
/*                                                                                        */
/*       run sys/ref/convcuom.p("M",bfPoOrdl.pr-uom , 0, 0, 0, 0,                         */
/*                              bfJob-farm.act-tot-cost, output bfJob-farm.act-tot-cost). */
/*                                                                                        */
/*       run sys/ref/convcuom.p("M",bfPoOrdl.pr-uom , 0, 0, 0, 0,                         */
/*                              bfJob-farm.act-cost, output bfJob-farm.act-cost).         */
/*      END.                                                                              */
       
  END. /* avail bfJob-farm */
END. /* do */


