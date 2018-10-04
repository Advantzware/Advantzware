/* rm/getDefaultReceiptVals.p */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
DEFINE INPUT PARAMETER ipcTag AS CHARACTER.  
DEFINE OUTPUT PARAMETER svpo-no  AS CHARACTER NO-UNDO.  
DEFINE OUTPUT PARAMETER svjob-no  LIKE loadtag.job-no NO-UNDO.
DEFINE OUTPUT PARAMETER svjob-no2  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svi-no  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER svi-name  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svloc  AS CHARACTER NO-UNDO.  
DEFINE OUTPUT PARAMETER svloc-bin  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svqty-case  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svCases  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svcases-unit  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svpartial    AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER svt-qty  AS CHARACTER NO-UNDO.  
DEFINE OUTPUT PARAMETER svqty-uom  as char no-undo. 
DEFINE OUTPUT PARAMETER svcost-uom  AS CHARACTER NO-UNDO  . 
DEFINE OUTPUT PARAMETER svstd-cost  AS CHARACTER NO-UNDO.

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
def var svt-qty  as char no-undo
def var svqty-uom  as char no-undo  . 
def var svcost-uom  as char no-undo  . 
def var svstd-cost  as char no-undo.  


run rm/getDefaultReceiptVals.p ( input fg-rctd.company, 
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
								output svqty-uom
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
    gcompany  = ipcCompany
    gLoc      = "MAIN"
    cocode    = gcompany
    locode    = gloc  
    g_company = gcompany  
    g_loc     = gloc  
    .

DEFINE VARIABLE v-cost        AS DECIMAL NO-UNDO.
DEFINE VARIABLE lv-loc        LIKE fg-rctd.loc NO-UNDO.
DEFINE VARIABLE lv-loc-bin    LIKE fg-rctd.loc-bin NO-UNDO.
DEFINE VARIABLE lv-qty-case   LIKE fg-rctd.qty-case NO-UNDO.
DEFINE VARIABLE lv-cases      LIKE fg-rctd.cases NO-UNDO.
DEFINE VARIABLE lv-cases-unit LIKE fg-rctd.cases-unit NO-UNDO.
DEFINE VARIABLE lv-partial    LIKE fg-rctd.partial NO-UNDO.
DEFINE VARIABLE lv-totalQty   LIKE fg-rctd.partial NO-UNDO.


FIND FIRST loadtag NO-LOCK
    WHERE loadtag.company   EQ g_company
    AND loadtag.item-type EQ YES
    AND loadtag.tag-no    EQ ipcTag
    NO-ERROR.
IF NOT AVAILABLE loadtag THEN 
DO:
    RETURN NO-APPLY.
END.

FIND FIRST rm-bin WHERE
    rm-bin.company EQ g_company
    AND rm-bin.tag EQ ipcTag
    NO-LOCK NO-ERROR.  
IF AVAILABLE rm-bin THEN
    ASSIGN
        lv-loc        = rm-bin.loc
        lv-loc-bin    = rm-bin.loc-bin        
        lv-cases      = rm-bin.qty
        lv-totalQty   = rm-bin.qty
        .

ELSE
    ASSIGN
        lv-loc        = loadtag.loc
        lv-loc-bin    = loadtag.loc-bin
        lv-qty-case   = loadtag.qty-case
        lv-cases      = loadtag.case-bundle
        lv-cases-unit = loadtag.case-bundle
        lv-partial    = loadtag.partial
		lv-totalQty   = loadtag.qty
		.

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
    svt-qty      = STRING(lv-totalQty)
    .  

RUN get-rm-bin-cost.
  
FIND FIRST item NO-LOCK WHERE item.company eq g_company
   AND item.i-no eq loadtag.i-no
   NO-ERROR.  
IF AVAIL item THEN
  ASSIGN  
   svqty-uom = item.cons-uom  
   svcost-uom = item.cons-uom.  
FIND FIRST job-hdr  NO-LOCK WHERE job-hdr.company = g_company
    AND job-hdr.job-no = loadtag.job-no
    AND job-hdr.job-no2 = loadtag.job-no2
    AND job-hdr.i-no = loadtag.i-no NO-ERROR.

IF NOT AVAILABLE job-hdr THEN 
DO:
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
        AND job.job-no  EQ svjob-no
        AND job.job-no2 EQ int(svjob-no2)
        NO-ERROR.
    IF AVAILABLE job THEN
        FIND FIRST reftable NO-LOCK
            WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ svi-no
            NO-ERROR.
END.
IF AVAILABLE job-hdr AND job-hdr.std-tot-cost GT 0 THEN
    svstd-cost = STRING(job-hdr.std-tot-cost).
ELSE 
    IF AVAILABLE reftable AND reftable.val[5] GT 0 THEN
        svstd-cost = STRING(reftable.val[5]).
    ELSE 
    DO:
        FIND FIRST po-ordl NO-LOCK
            WHERE po-ordl.company   EQ cocode           
            AND po-ordl.po-no     EQ int(svpo-no)
            AND po-ordl.i-no      EQ svi-no
            AND po-ordl.item-type EQ NO
            NO-ERROR.
          
        IF AVAILABLE po-ordl THEN 
        DO:
            RUN sys/ref/convcuom.p(po-ordl.pr-uom, svcost-uom, 0,
                po-ordl.s-len, po-ordl.s-wid, 0,
                po-ordl.cost, OUTPUT v-cost). 
            ASSIGN
              svqty-uom   = po-ordl.pr-qty-uom  
              svcost-uom  = po-ordl.pr-uom
              svstd-cost  = STRING(v-cost)  
              .
        END.
     
        ELSE
            IF AVAILABLE itemfg THEN
                ASSIGN
                    svcost-uom = itemfg.prod-uom
                    svstd-cost = STRING(itemfg.total-std-cost)
                    .
    END.
   
IF NOT AVAILABLE job-hdr OR dec(svstd-cost) = 0 THEN 
DO:
    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company = g_company
        AND oe-ordl.i-no = svi-no
        AND oe-ordl.job-no = svjob-no
        AND oe-ordl.job-no2 = loadtag.job-no2 
        USE-INDEX ITEM
         NO-ERROR.
    IF AVAILABLE oe-ordl THEN
        ASSIGN 
            svstd-cost = STRING(oe-ordl.cost)
            svcost-uom = "M"
            .
END.   
   
   
PROCEDURE get-rm-bin-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
  
    FIND FIRST rm-bin NO-LOCK
        WHERE rm-bin.company EQ cocode
        AND rm-bin.i-no    EQ svi-no
        /*AND rm-bin.job-no  EQ svjob-no
        AND rm-bin.job-no2 EQ INT(svjob-no2) */
        AND rm-bin.loc     EQ svloc
        AND rm-bin.loc-bin EQ svloc-bin
        AND rm-bin.tag     EQ ipctag
        NO-ERROR.
    IF AVAILABLE rm-bin THEN
        ASSIGN
            /* svstd-cost = STRING(rm-bin.std-tot-cost) */
            /* svcost-uom = STRING(rm-bin.pur-uom) */
            .
 

END PROCEDURE.
