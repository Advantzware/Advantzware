def input parameter ipcCompany as char.
def input parameter ipcTag as char.  
def output parameter svpo-no  as char no-undo.  
def output parameter svjob-no  like loadtag.job-no no-undo.
def output parameter svjob-no2  as char no-undo. 
def output parameter svi-no  as char no-undo.
def output parameter svi-name  as char no-undo. 
def output parameter svloc  as char no-undo.  
def output parameter svloc-bin  as char no-undo. 
def output parameter svqty-case  as char no-undo. 
def output parameter svCases  as char no-undo. 
def output parameter svcases-unit  as char no-undo. 
def output parameter svpartial    as char no-undo. 
def output parameter svt-qty  as char no-undo.
def output parameter svcost-uom  as char no-undo  . 
def output parameter svstd-cost  as char no-undo.

/* Usage:

def var ipcCompany as char.
def var ipcTag as char.  
def var svpo-no  as char no-undo.  
def var svjob-no  like loadtag.job-no no-undo.
def var svjob-no2  as char no-undo. 
def var svi-no  as char no-undo.
def var svi-name  as char no-undo. 
def var svloc  as char no-undo.  
def var svloc-bin  as char no-undo. 
def var svqty-case  as char no-undo. 
def var svCases  as char no-undo. 
def var svcases-unit  as char no-undo. 
def var svpartial    as char no-undo. 
def var svt-qty  as char no-undo.
def var svcost-uom  as char no-undo  . 
def var svstd-cost  as char no-undo.  


run fg/getDefaultReceiptVals.p ( input fg-rctd.company, 
                               input fg-rctd.tag  ,  
								output svpo-no  ,  
								output svjob-no  ,
								output svjob-no2  , 
								output svi-no  ,
								output svi-name  , 
								output svloc  ,  
								output svloc-bin  , 
								output svqty-case  , 
								output svCases  , 
								output svcases-unit  , 
								output svpartial    , 
								output svt-qty  ,
								output svcost-uom  ,   
								output svstd-cost    
                              ). 
				*/
{methods/defines/hndldefs.i NEW}
/*{methods/prgsecur.i} */
{methods/defines/globdefs.i &NEW=NEW GLOBAL}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}

ASSIGN
    gcompany = ipcCompany
    gLoc     = "MAIN"
    cocode = gcompany
    locode = gloc  
	g_company = gcompany  
	g_loc = gloc  
    .

DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR lv-loc LIKE fg-rctd.loc NO-UNDO.
DEF VAR lv-loc-bin LIKE fg-rctd.loc-bin NO-UNDO.
DEF VAR lv-qty-case LIKE fg-rctd.qty-case NO-UNDO.
DEF VAR lv-cases LIKE fg-rctd.cases NO-UNDO.
DEF VAR lv-cases-unit LIKE fg-rctd.cases-unit NO-UNDO.
DEF VAR lv-partial LIKE fg-rctd.partial NO-UNDO.



   FIND FIRST loadtag NO-LOCK
       WHERE loadtag.company   EQ g_company
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ ipcTag
       NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
 
     RETURN NO-APPLY.
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
      lv-cases      =  loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.
 message "assign " lv-loc view-as alert-box.

   ASSIGN
    svpo-no      = IF loadtag.po-no = 0 THEN "" ELSE STRING(loadtag.po-no)
    svjob-no     = loadtag.job-no 
    svjob-no2    = STRING(loadtag.job-no2)
    svi-no       = loadtag.i-no 
    svi-name     = loadtag.i-name
    svloc        = lv-loc
    svloc-bin    = lv-loc-bin
    svqty-case   = STRING(lv-qty-case)
    svcases      = STRING(lv-cases)
    svcases-unit = STRING(lv-cases-unit)
    svpartial    = STRING(lv-partial)
    svt-qty  =
                             STRING((INT(svcases) *
                                     INT(svqty-case)) +
                                    INT(svpartial)).
   RUN get-fg-bin-cost.

   FIND FIRST job-hdr WHERE job-hdr.company = g_company
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ svjob-no
            AND job.job-no2 EQ int(svjob-no2)
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ svi-no
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      svstd-cost = string(job-hdr.std-tot-cost).
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
     svstd-cost = STRING(reftable.val[5]).
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(svpo-no)
            and po-ordl.i-no      eq svi-no
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, svcost-uom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        svstd-cost  = STRING(v-cost).
      END.
     
      else
      if avail itemfg then
        assign
         svcost-uom  = itemfg.prod-uom
         svstd-cost  = STRING(itemfg.total-std-cost).
    end.
   
   IF NOT AVAIL job-hdr OR dec(svstd-cost) = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = g_company
                           AND oe-ordl.i-no = svi-no
                           AND oe-ordl.job-no = svjob-no
                           AND oe-ordl.job-no2 = loadtag.job-no2 
                        USE-INDEX ITEM
                        NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN svstd-cost = string(oe-ordl.cost)
                  svcost-uom = "M".
   END.   
   
   
   PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ svi-no
          AND fg-bin.job-no  EQ svjob-no
          AND fg-bin.job-no2 EQ INT(svjob-no2)
          AND fg-bin.loc     EQ svloc
          AND fg-bin.loc-bin EQ svloc-bin
          AND fg-bin.tag     EQ ipctag
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       svstd-cost = STRING(fg-bin.std-tot-cost)
       svcost-uom = STRING(fg-bin.pur-uom).
 

END PROCEDURE.