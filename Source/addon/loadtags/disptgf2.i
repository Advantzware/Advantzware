/* loadtag/disptgf2.i  get tag# and display tag information for FG item 
                    lv-do-what is "delete" then  negative value
                    else regular values */
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR lv-loc LIKE fg-rctd.loc NO-UNDO.
DEF VAR lv-loc-bin LIKE fg-rctd.loc-bin NO-UNDO.
DEF VAR lv-qty-case LIKE fg-rctd.qty-case NO-UNDO.
DEF VAR lv-cases LIKE fg-rctd.cases NO-UNDO.
DEF VAR lv-cases-unit LIKE fg-rctd.cases-unit NO-UNDO.
DEF VAR lv-partial LIKE fg-rctd.partial NO-UNDO.


IF "{1}" EQ "FGItem" THEN DO:
   FIND FIRST loadtag NO-LOCK
       WHERE loadtag.company   EQ g_company
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ {2}
       NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
     MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
     fg-rctd.tag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "".
     RETURN NO-APPLY.
   END.

   RELEASE fg-bin.
   IF lv-do-what EQ "Delete" THEN DO:
     FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         BY fg-bin.qty DESC:
       LEAVE.
     END.

     IF NOT AVAIL fg-bin THEN DO:
       MESSAGE "No Inventory On Hand Exists, Tag cannot be deleted..."
           VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
     END.
   END.

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = IF lv-do-what EQ "Delete" THEN loadtag.tot-cases
                                                ELSE loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   IF lv-do-what EQ "Delete" THEN
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   ASSIGN
    fg-rctd.po-no:SCREEN-VALUE      = IF loadtag.po-no = 0 THEN "" ELSE STRING(loadtag.po-no)
    fg-rctd.job-no:SCREEN-VALUE     = loadtag.job-no 
    fg-rctd.job-no2:SCREEN-VALUE    = STRING(loadtag.job-no2)
    fg-rctd.i-no:SCREEN-VALUE       = loadtag.i-no 
    fg-rctd.i-name:SCREEN-VALUE     = loadtag.i-name
    fg-rctd.loc:SCREEN-VALUE        = lv-loc
    fg-rctd.loc-bin:SCREEN-VALUE    = lv-loc-bin
    fg-rctd.qty-case:SCREEN-VALUE   = STRING(lv-qty-case)
    fg-rctd.cases:SCREEN-VALUE      = STRING(lv-cases)
    fg-rctd.cases-unit:SCREEN-VALUE = STRING(lv-cases-unit)
    fg-rctd.partial:SCREEN-VALUE    = STRING(lv-partial)
    fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name} =
                             STRING((INT(fg-rctd.cases:SCREEN-VALUE) *
                                     INT(fg-rctd.qty-case:SCREEN-VALUE)) +
                                    INT(fg-rctd.partial:SCREEN-VALUE),
                                    fg-rctd.t-qty:FORMAT).
   RUN get-fg-bin-cost.

   FIND FIRST job-hdr WHERE job-hdr.company = g_company
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

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
                           AND oe-ordl.job-no2 = loadtag.job-no2 
                        USE-INDEX ITEM
                        NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(oe-ordl.cost)
                  fg-rctd.cost-uom:SCREEN-VALUE = "M".
   END.

END.
ELSE IF "{1}}" = "RMItem" THEN DO:

END.
