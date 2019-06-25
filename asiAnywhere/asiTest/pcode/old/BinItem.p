
/*------------------------------------------------------------------------
    File        : Item.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BinItem.i}
{oeinq.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemJob.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEF VAR lv-sort-by AS CHAR INIT "tag" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Tag" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR li-qty-pal AS INT NO-UNDO.
DEF VAR ll-show-zero-bins AS LOG NO-UNDO.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
DEFINE STREAM s1.
    /* ********************  Preprocessor Definitions  ******************** */
/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum)  no-lock:
    FOR EACH itemfg WHERE itemfg.i-no = oe-ordl.i-no NO-LOCK:
        for each fg-bin
            where fg-bin.company eq itemfg.company
            and fg-bin.i-no    eq itemfg.i-no
            and (fg-bin.qty    ne 0 or ll-show-zero-bins)
            no-lock:
            create w-jobs.
            assign   w-jobs.job-no = fg-bin.job-no
                w-jobs.job-no2 = fg-bin.job-no2
                w-jobs.i-no  = itemfg.i-no
                w-jobs.loc  = fg-bin.loc
                w-jobs.loc-bin = fg-bin.loc-bin
                w-jobs.tag = fg-bin.tag
                w-jobs.cust-no = fg-bin.cust-no
                w-jobs.cases = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-jobs.case-count = fg-bin.case-count
                w-jobs.cases-unit = fg-bin.cases-unit
                w-jobs.partial-count = fg-bin.partial-count
                w-jobs.qty = fg-bin.qty
                w-jobs.std-tot-cost = fg-bin.std-tot-cost
                w-jobs.std-mat-cost = fg-bin.std-mat-cost
                w-jobs.std-lab-cost = fg-bin.std-lab-cost
                w-jobs.std-var-cost = fg-bin.std-var-cost
                w-jobs.std-fix-cost = fg-bin.std-fix-cost
                w-jobs.last-cost = fg-bin.last-cost
                w-jobs.sell-uom = fg-bin.pur-uom.
            FIND first job-hdr where job-hdr.company eq fg-bin.company
                                and job-hdr.i-no    eq fg-bin.i-no
                                and job-hdr.job-no  eq fg-bin.job-no
                                and job-hdr.job-no2 eq fg-bin.job-no2
                                use-index i-no no-lock no-error.
            if avail job-hdr then assign w-jobs.j-no = job-hdr.j-no.
            RELEASE w-jobs.
  end. /* each fg-bin */


  IF ll-show-zero-bins THEN DO:
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ itemfg.company
          AND job-hdr.i-no    EQ itemfg.i-no
          AND job-hdr.opened  EQ YES
        USE-INDEX i-no,
        FIRST job NO-LOCK
        WHERE job.company EQ job-hdr.company
          AND job.job     EQ job-hdr.job
          AND job.job-no  EQ job-hdr.job-no
          AND job.job-no2 EQ job-hdr.job-no2:

      RUN create-table (job.std-lab-cost,
                        job.std-mat-cost,
                        job.std-var-cost,
                        job.std-fix-cost).

      RELEASE w-jobs.
    END.

    FOR EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.code2    EQ itemfg.i-no
          AND reftable.company  EQ itemfg.company
          AND reftable.loc      EQ ""
        USE-INDEX code2,
        FIRST job NO-LOCK
        WHERE job.company EQ reftable.company
          AND job.job     EQ INT(reftable.code)
          AND job.opened  EQ YES
          AND NOT CAN-FIND(FIRST job-hdr
                           WHERE job-hdr.company EQ job.company
                             AND job-hdr.job     EQ job.job
                             AND job-hdr.job-no  EQ job.job-no
                             AND job-hdr.job-no2 EQ job.job-no2
                             AND job-hdr.i-no    EQ reftable.code2):

      RUN create-table (reftable.val[1],
                        reftable.val[2],
                        reftable.val[3],
                        reftable.val[4]).

      RELEASE w-jobs.
    END.
  END. /* ll-show-zero-bins */

  for each w-jobs break by w-jobs.job-no by w-jobs.job-no2:
      create w-job.
      assign   w-job.job-no = w-jobs.job-no
               w-job.job-no2 = w-jobs.job-no2
               w-job.job-no-disp = trim(w-job.job-no) + "-" + string(w-job.job-no2,"99")
               w-job.i-no  = w-jobs.i-no
               w-job.j-no  = w-jobs.j-no
               w-job.loc  = w-jobs.loc
               w-job.loc-bin = w-jobs.loc-bin
               w-job.tag = w-jobs.tag
               w-job.cust-no = w-jobs.cust-no
               w-job.cases = w-jobs.cases
               w-job.case-count = w-jobs.case-count
               w-job.cases-unit = w-jobs.cases-unit
               w-job.partial-count = w-jobs.partial-count
               w-job.qty = w-jobs.qty
               w-job.std-tot-cost = w-jobs.std-tot-cost
               w-job.std-mat-cost = w-jobs.std-mat-cost
               w-job.std-lab-cost = w-jobs.std-lab-cost
               w-job.std-var-cost = w-jobs.std-var-cost
               w-job.std-fix-cost = w-jobs.std-fix-cost
               w-job.last-cost = w-jobs.last-cost
               w-job.sell-uom = w-jobs.sell-uom.

      IF w-job.job-no-disp EQ "-00" THEN w-job.job-no-disp = "".
               
      delete w-jobs.
  end. /* each w-jobs */
   END.
END.   /*FOR EACH oe-ordl*/
/***************************************************************************************************************************************/
PROCEDURE create-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-lab LIKE job-hdr.std-lab-cost NO-UNDO.
  DEF INPUT PARAM ip-mat LIKE job-hdr.std-mat-cost NO-UNDO.
  DEF INPUT PARAM ip-var LIKE job-hdr.std-var-cost NO-UNDO.
  DEF INPUT PARAM ip-fix LIKE job-hdr.std-fix-cost NO-UNDO.


  IF NOT CAN-FIND(FIRST w-jobs
                  WHERE w-jobs.job-no  EQ job.job-no
                    AND w-jobs.job-no2 EQ job.job-no2) THEN DO:
    CREATE w-jobs.
    ASSIGN
     w-jobs.job-no       = job.job-no
     w-jobs.job-no2      = job.job-no2
     w-jobs.i-no         = itemfg.i-no
     w-jobs.loc          = job.loc
     w-jobs.std-lab-cost = ip-lab
     w-jobs.std-mat-cost = ip-mat
     w-jobs.std-var-cost = ip-var
     w-jobs.std-fix-cost = ip-fix
     w-jobs.std-tot-cost = w-jobs.std-lab-cost + w-jobs.std-mat-cost +
                           w-jobs.std-var-cost + w-jobs.std-fix-cost
     w-jobs.sell-uom     = "M".
  END.

END PROCEDURE.
