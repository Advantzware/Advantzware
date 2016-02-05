DEF INPUT PARAMETER ipiJno AS INT NO-UNDO.

DEF BUFFER bf-job-hdr   FOR job-hdr.
DEF BUFFER xest         FOR est.
DEF BUFFER xeb          FOR eb.
DEF BUFFER xef          FOR ef.

DEF VAR ip-cocode       AS CHAR NO-UNDO.
DEF VAR llIsASet        AS LOG  NO-UNDO.
DEF VAR cSetItem        AS CHAR NO-UNDO.

DEF VAR dLowestRunCost  AS DEC  NO-UNDO.
DEF VAR dEstSetup       AS DEC  NO-UNDO.
DEF VAR cEstUOM         AS CHAR NO-UNDO.
DEF VAR cVend           AS CHAR NO-UNDO.
DEF VAR ldMCost AS DEC NO-UNDO.
DEF VAR lv-len LIKE item.s-len NO-UNDO.
DEF VAR lv-wid LIKE item.s-wid NO-UNDO.
DEF VAR lv-dep LIKE item.s-dep NO-UNDO.
DEF VAR liBlank AS INT NO-UNDO.
DEF VAR liForm AS INT NO-UNDO.
DEF BUFFER bf-itemfg FOR itemfg.
DEF VAR liNumSets AS INT NO-UNDO.
DEF VAR liQtyPerSet AS INT NO-UNDO.
DEF VAR llUOMChanged AS LOG NO-UNDO.
{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

FIND FIRST job WHERE job.company EQ cocode
   AND job.job EQ ipiJno NO-LOCK NO-ERROR.
IF NOT AVAIL job THEN
    RETURN.

llUOMChanged = NO.
llIsASet = NO.
FOR EACH job-hdr WHERE job-hdr.company EQ cocode
  AND job-hdr.job EQ ipiJno NO-LOCK,
    FIRST itemfg WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no EQ job-hdr.i-no
        NO-LOCK.
   
    IF itemfg.isaset  THEN DO:

       ASSIGN llIsASet = TRUE
              cSetItem = itemfg.i-no.
       FOR EACH fg-set WHERE fg-set.company EQ job-hdr.company
           AND fg-set.set-no EQ job-hdr.i-no
           NO-LOCK.

           FIND bf-itemfg WHERE bf-itemfg.company EQ fg-set.company
               AND bf-itemfg.i-no EQ fg-set.part-no
               NO-LOCK NO-ERROR.
           IF AVAIL bf-itemfg AND bf-itemfg.pur-man THEN DO:
             /* This is a purchased component, so create the job-farm */
             liNumSets   = job-hdr.qty.
             liQtyPerSet = fg-set.part-qty.
             
             RUN create-job-farm.
             RELEASE job-farm.
           END. /* if item is pur-man */
       END. /* each fg-set */       
    END. /* if fg-item is a set */
    ELSE DO: /* not a set */

         FIND bf-itemfg WHERE rowid(bf-itemfg) EQ ROWID(itemfg)
             NO-LOCK NO-ERROR.
         
         IF AVAIL bf-itemfg AND bf-itemfg.pur-man THEN DO:
           /* This is a purchased component, so create the job-farm */
           liNumSets   = job-hdr.qty.
           liQtyPerSet = 1. /* 1 for non-sets */
           
           RUN create-job-farm.
           RELEASE job-farm.
         END. /* if item is pur-man */
    END.
END. /* each job-hdr */

PROCEDURE create-job-farm :
  FIND FIRST xest
       where xest.company EQ cocode
         and xest.est-no  EQ job.est-no
       NO-ERROR.
  
   IF NOT AVAIL xest THEN
       RETURN.

   FIND FIRST xeb where xeb.company = xest.company 
                   and xeb.est-no = xest.est-no
                   AND xeb.stock-no EQ bf-itemfg.i-no
                 NO-LOCK NO-ERROR.
   
   IF NOT AVAIL xeb THEN
       RETURN.

   RUN jc/bestVendCost.p (INPUT ROWID(xeb),
             INPUT (IF liQtyPerSet GT 0 THEN job-hdr.qty * liQtyPerSet ELSE job-hdr.qty / (- liQtyPerSet)) ,
             INPUT "EA" ,
             OUTPUT dLowestRunCost,
             OUTPUT dEstSetup,
             OUTPUT cEstUOM,
             OUTPUT cVend).

   IF cEstUOM NE "M" THEN DO:
       FIND FIRST xef
          WHERE xef.company EQ xeb.company
            AND xef.est-no  EQ xeb.est-no
            AND xef.form-no EQ xeb.form-no
          NO-LOCK NO-ERROR.

      /* Log this error when a log is established */
      IF AVAIL xef THEN do:            
          ASSIGN
           lv-len  = xef.gsh-len
           lv-wid  = xef.gsh-wid
           lv-dep  = xef.gsh-dep.
      END.

      IF AVAIL bf-itemfg THEN
        RUN sys/ref/convcuom.p(cEstUom, "M",
                   itemfg.weight-100, lv-len, lv-wid, lv-dep,
                   dLowestRunCost, OUTPUT ldMCost).
      
      llUomChanged = YES.
   END.
   ELSE
       ldMCost = dLowestRunCost.

    FIND FIRST job-farm
      WHERE job-farm.company  EQ cocode
        AND job-farm.job      EQ job.job
        AND job-farm.job-no   EQ job.job-no
        AND job-farm.job-no2  EQ job.job-no2
        AND job-farm.frm      EQ xeb.form-no
        AND job-farm.blank-no EQ xeb.blank-no
        AND job-farm.i-no     EQ xeb.stock-no
      EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN liBlank = xeb.blank-no
           liForm  = xeb.form-no.        

    IF NOT AVAIL job-farm THEN DO:
        FIND FIRST po-ordl 
           WHERE po-ordl.company EQ cocode
             AND po-ordl.job-no EQ job.job-no
             AND po-ordl.job-no2 EQ job.job-no2
           NO-LOCK NO-ERROR.
       
           
        CREATE job-farm.
        ASSIGN
          job-farm.company      = cocode            
          job-farm.j-no         = job-hdr.j-no /* needed for the index */
          job-farm.job-no       = job.job-no
          job-farm.job-no2      = job.job-no2
          job-farm.i-no         = xeb.stock-no

          job-farm.qty          = (IF liQtyPerSet GE 0 THEN liNumSets * liQtyPerSet ELSE liNumSets / (- liQtyPerSet)) /*xeb.eqty * xeb.yld-qty */ /* ? */
          job-farm.qty-uom      = "EA"     /* Data appears to be EA */
          job-farm.line         = 0        /* */
          job-farm.blank-no     = liBlank
          job-farm.frm          = liForm
          job-farm.pass         = 0          /* */
          job-farm.job          = job.job.
        IF AVAIL po-ordl THEN
          ASSIGN job-farm.po-no = STRING(po-ordl.po-no).

       ASSIGN
          job-farm.qty-all      = 0
          job-farm.qty-iss      = 0
          job-farm.qty-mr       = 0
          job-farm.qty-wst      = 0            
          /* job-farm.sts          = 0 */. 

       ASSIGN
          job-farm.act-cost     = 0
          job-farm.act-tot-cost = 0
          job-farm.est-cost     = ldMCost
          job-farm.est-no       = xeb.est-no
          job-farm.est-setup    = dEstSetup
          job-farm.est-uom      = cEstUOM
          job-farm.std-cost     = job-farm.est-cost.

       ASSIGN
          job-farm.sc-uom       = cEstUOM 
         /*                         TOTAL Cost                                                           Div BY SetQty */
          job-farm.cost-m       = ((job-farm.std-cost * (IF liQtyPerSet GE 0 THEN liNumSets * liqtyPerSet ELSE liNumSets / (- liQtyPerSet)) ) / 1000 + dEstSetup) / liNumSets  * 1000 
          job-farm.i-dscr[ 1]   = "".

        IF AVAIL job-hdr THEN
        ASSIGN                                
          job-farm.po-setup     = 0
          job-farm.pur-uom      = ""
          job-farm.std-tot-cost = (job-farm.qty / 1000 * job-farm.std-cost) + job-farm.est-setup.
          job-farm.vend-po-qty  = 0.


    END. /* if not avail then do */
    ELSE DO:
        /* Already exists, so update when job changes */
        ASSIGN
           job-farm.sc-uom       = cEstUOM                    
           job-farm.est-cost     = ldmCost
           job-farm.est-no       = xeb.est-no
           job-farm.est-setup    = dEstSetup
           job-farm.est-uom      = cEstUom
           job-farm.std-cost     = ldmCost
           job-farm.cost-m       = ((ldmCost * (IF liQtyPerSet GE 0 THEN liNumSets * liqtyPerSet ELSE liNumSets / (- liQtyPerSet)) ) / 1000 + dEstSetup) / liNumSets  * 1000 
           job-farm.qty          = (IF liQtyPerSet GE 0 THEN liNumSets * liQtyPerSet ELSE liNumSets / (- liQtyPerSet))
           job-farm.std-tot-cost = (job-farm.qty / 1000 * ldmCost) + job-farm.est-setup.

    END.
 
    IF AVAIL bf-itemfg AND llUOMChanged AND cEstUOM NE "M" THEN DO:
        /* Converted to M to calculate, so convert back to original */
       
        RUN sys/ref/convcuom.p("M", cEstUom,
                    itemfg.weight-100, lv-len, lv-wid, lv-dep,
                    job-farm.est-cost, OUTPUT job-farm.est-cost).
       
        RUN sys/ref/convcuom.p("M", cEstUom,
                    itemfg.weight-100, lv-len, lv-wid, lv-dep,
                    job-farm.std-cost, OUTPUT job-farm.std-cost).
    

/*                   RUN sys/ref/convcuom.p("M", cEstUom,                             */
/*                              itemfg.weight-100, lv-len, lv-wid, lv-dep,            */
/*                              job-farm.std-tot-cost, OUTPUT job-farm.std-tot-cost). */

    END.
    RELEASE job-farm.

END PROCEDURE.





