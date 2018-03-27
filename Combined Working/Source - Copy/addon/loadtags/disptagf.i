/* loadtag/disptagf.i  get tag# and display tag information for FG item */
DEF VAR v-cost AS DEC NO-UNDO.

IF "{1}" = "FGItem" THEN DO:
   FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = {2} NO-LOCK NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   find first itemfg
      {sys/look/itemfgrlW.i}
        and itemfg.i-no EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
      no-lock no-error.

   ASSIGN  fg-rctd.po-no:SCREEN-VALUE = IF loadtag.po-no = 0 THEN "" ELSE string(loadtag.po-no)
           fg-rctd.job-no:SCREEN-VALUE = loadtag.job-no 
           fg-rctd.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
        /*   fg-rctd.ord-no:SCREEN-VALUE = STRING(loadtag.ord-no) */
           fg-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
           fg-rctd.i-name:SCREEN-VALUE =  loadtag.i-name 
           fg-rctd.t-qty:SCREEN-VALUE = STRING(loadtag.pallet-count) /*qty*/
           fg-rctd.qty-case:SCREEN-VALUE = string(loadtag.qty-case)
           fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle)
           fg-rctd.cases-unit:SCREEN-VALUE = string(loadtag.case-bundle)
           fg-rctd.loc:SCREEN-VALUE = loadtag.loc
           fg-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
           fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE  
           fg-rctd.partial:SCREEN-VALUE = string(loadtag.partial)
           fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
                                    INT(fg-rctd.cases:SCREEN-VALUE) *
                                    INT(fg-rctd.qty-case:SCREEN-VALUE) +
                                    INT(fg-rctd.partial:SCREEN-VALUE)
                                    ,"->>>,>>>,>>9.99").

   RUN get-fg-bin-cost.
   FIND FIRST job-hdr WHERE job-hdr.company = g_company
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   /*IF AVAIL job-hdr THEN 
      ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost) . */

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
            AND job.job-no2 EQ int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = string(job-hdr.std-tot-cost).
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(reftable.val[5]).
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            and po-ordl.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost).
      END.
     
      else
      if avail itemfg then
        assign
         fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.prod-uom
         fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(itemfg.total-std-cost).
    end.
   
   IF NOT AVAIL job-hdr OR dec(fg-rctd.std-cost:SCREEN-VALUE) = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.i-no = fg-rctd.i-no:SCREEN-VALUE
                           AND oe-ordl.job-no = fg-rctd.job-no:SCREEN-VALUE
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(oe-ordl.cost)
                  fg-rctd.cost-uom:SCREEN-VALUE = "M".
   END.
   
/* not for receipt
    IF g-sharpshooter = YES THEN DO: 
       /*FIND FIRST itemfg WHERE itemfg.company = g_company
                           AND itemfg.i-no = fg-rctd.i-no:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL ITEMfg AND itemfg.ship-meth THEN DO: /* case */
          APPLY "entry" TO fg-rctd.cases.
       END.
       ELSE*/  APPLY "row-leave" TO BROWSE {&browse-name}.
    END.
    ELSE DO:
       IF loadtag.job-no = "" AND loadtag.po-no = 0 THEN APPLY "entry" TO fg-rctd.t-qty.
       ELSE    APPLY "entry" TO fg-rctd.loc.
    END.
    
    IF loadtag.job-no = "" AND loadtag.po-no = 0 THEN APPLY "entry" TO fg-rctd.t-qty.
    ELSE    APPLY "entry" TO fg-rctd.loc.

    RETURN NO-APPLY.
*/
END.
ELSE IF "{1}}" = "RMItem" THEN DO:

END.
