/* -------------------------------------------------- pc/pcprdd3u.p 05/02 JLF      *
    Program Name  : pc/pcprdd3u.p                                                  * 
    Author        :                                                                *
    Purpose       :                                                                * 
    Modified By   : Aj  06/25/2008 Added code to post the data when complete flag  *
                                   is no and job code category is RUN. Also fix    *
                                   the issues with complete flag                   *
                                                                                   *
---------------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-rowid AS ROWID.

{sys/inc/var.i SHARED}.
{sys/form/s-top.f}

DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
DEF VAR v-qty                AS   INT.
/*DEF VAR choice               AS   LOG NO-UNDO.*/
DEF VAR li-units             AS   INT NO-UNDO.

DEF VAR v-up AS INT NO-UNDO.
DEF VAR v-out AS INT NO-UNDO.
def var v-up-hs     like eb.num-up NO-UNDO.
def var v-on        like eb.num-up NO-UNDO.
DEF VAR v-update-fg-rec AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE cFGRecptUnit AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

{pc/pcprdd4u.i NEW}

/*{fg/checkset.f NEW}*/
RUN sys/ref/nk1look.p (INPUT cocode, 
                           INPUT "FGRecptUnit", 
                           INPUT "C", 
                           INPUT NO, 
                           INPUT NO, 
                           INPUT "", 
                           INPUT "", 
                           OUTPUT cFGRecptUnit, 
                           OUTPUT lFound).
IF NOT lFound THEN cFGRecptUnit = "Pallet Counts".

FIND pc-prdd WHERE ROWID(pc-prdd) EQ v-rowid EXCLUSIVE.

IF NOT AVAIL pc-prdd THEN RETURN.

FIND FIRST mach
    {sys/ref/machW.i}
      AND mach.m-code EQ pc-prdd.m-code
    NO-LOCK NO-ERROR.

FIND FIRST job
    WHERE job.company EQ cocode
      AND job.job-no  EQ pc-prdd.job-no
      AND job.job-no2 EQ pc-prdd.job-no2
    USE-INDEX job-no NO-ERROR.
        
ASSIGN
 v-up  = 1
 v-out = 1
 v-on  = 1.

IF AVAIL job THEN
FIND FIRST est
    WHERE est.company EQ job.company
      AND est.est-no  EQ job.est-no
    NO-LOCK NO-ERROR.
v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

RUN pc/pcprdd4u.p (ROWID(pc-prdd)).

FIND FIRST tt-job-hdr NO-ERROR.

IF AVAIL tt-job-hdr THEN DO:
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "AUTOPOST"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company = cocode
     sys-ctrl.name    = "AUTOPOST"
     sys-ctrl.descrip = "Autopost to Finished Goods Receipts?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  END.
 
  IF (NOT pc-prdd.complete OR NOT tt-job-hdr.last-mach) AND
      pc-prdd.qty NE 0 THEN DO:
    /*choice = sys-ctrl.int-fld EQ 1 AND tt-job-hdr.last-mach.*/

    MESSAGE "Update Finished Goods Receipts Automatically?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-update-fg-rec.

   /*     pc-prdd.complete = choice. */
  END.

  IF pc-prdd.qty EQ 0 THEN
     v-update-fg-rec = NO.

  IF ((CAN-FIND(FIRST job-code WHERE job-code.CODE = pc-prdd.CODE AND job-code.cat = "RUN") 
       AND pc-prdd.COMPLETE = NO) 
       OR pc-prdd.COMPLETE) AND
       v-update-fg-rec THEN
  FOR EACH tt-job-hdr,
    FIRST itemfg
    WHERE itemfg.company EQ tt-job-hdr.company
      AND itemfg.i-no    EQ tt-job-hdr.i-no:
        
    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/
    
    RUN fg/autopost.p (ROWID(itemfg), pc-prdd.job-no, pc-prdd.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ pc-prdd.job-no
          AND fg-bin.job-no2 EQ pc-prdd.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = itemfg.i-no
       fg-bin.tag          = ""
       fg-bin.job-no       = pc-prdd.job-no
       fg-bin.job-no2      = pc-prdd.job-no2
       fg-bin.std-mat-cost = tt-job-hdr.std-mat-cost
       fg-bin.std-lab-cost = tt-job-hdr.std-lab-cost
       fg-bin.std-fix-cost = tt-job-hdr.std-fix-cost
       fg-bin.std-var-cost = tt-job-hdr.std-var-cost
       fg-bin.std-tot-cost = tt-job-hdr.std-tot-cost
       fg-bin.last-cost    = tt-job-hdr.std-tot-cost
       fg-bin.case-count   = itemfg.case-count
       fg-bin.cases-unit   = 1
       fg-bin.unit-count   = fg-bin.case-count * fg-bin.cases-unit.

    END.

    IF cFGRecptUnit EQ "Order Item Counts" THEN DO:
        FIND FIRST oe-ordl 
            WHERE oe-ordl.company = cocode
              AND oe-ordl.ord-no  = tt-job-hdr.ord-no
              AND oe-ordl.i-no    = itemfg.i-no 
            NO-LOCK NO-ERROR .
        IF AVAIL oe-ordl THEN
            ASSIGN 
                fg-bin.case-count = oe-ordl.cas-cnt 
                fg-bin.cases-unit  = oe-ordl.cases-unit
                fg-bin.unit-count  = oe-ordl.cases-unit * fg-bin.case-count  .
    END.

    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST reftable
        WHERE reftable.reftable EQ "pc/pcprddu3.p"
          AND reftable.company  EQ pc-prdd.company
          AND reftable.code     EQ /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
        EXCLUSIVE NO-ERROR.
    IF NOT AVAIL reftable THEN CREATE reftable.
    ASSIGN
     reftable.reftable = "pc/pcprddu3.p"
     reftable.company  = pc-prdd.company
     reftable.code     = /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
     reftable.code2    = fg-bin.rec_key /*STRING(RECID(fg-bin))*/
     li-units          = reftable.val[1].

    IF AVAIL est AND INDEX("AP",mach.p-type) LE 0 THEN DO:
      run sys/inc/numupi.p (est.company, est.est-no, pc-prdd.frm, pc-prdd.i-no, output v-up).
      find first ef
          where ef.company eq est.company
            and ef.est-no  eq est.est-no
            and ef.form-no eq pc-prdd.frm
          no-lock no-error.

      IF AVAIL ef THEN DO:
        RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
        v-on = v-up * v-on.
      END.
                      
      find first est-op
          where est-op.company eq est.company
            and est-op.est-no  eq est.est-no
            and est-op.s-num   eq pc-prdd.frm
            and (est-op.b-num  eq pc-prdd.blank-no OR pc-prdd.blank-no eq 0)
            and est-op.m-code  eq pc-prdd.m-code
            and est-op.op-pass eq pc-prdd.pass
            and est-op.dept    eq pc-prdd.dept
            and est-op.line    lt 500
          no-lock no-error.

      if ((avail est-op) and est-op.op-sb)           or
         ((not avail est-op) and mach.p-type ne "B") then do:

        if avail est-op THEN run sys/inc/numout.p (recid(est-op), output v-out).
        else v-out = 1.
        v-up = v-up * v-out.
      end.
      else v-up = 1.

      v-on = v-on / v-up.
    end.
           
    v-up-hs = 1.

    if pc-prdd.dept eq "HS" and
       avail est            and
       mach.therm           and
       mach.p-type eq "S"   then
      run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up-hs).
     RUN pc/d-updbin.w (ROWID(fg-bin), pc-prdd.qty / v-up-hs * v-out * v-up,
                       INPUT-OUTPUT li-units).

    FIND CURRENT fg-bin NO-LOCK.

    ASSIGN
     reftable.val[1] = li-units
     reftable.val[2] = fg-bin.case-count
     reftable.val[3] = fg-bin.cases-unit.
  END. /*end for each*/
END. /*avail tt-job-hdr*/

/* end ---------------------------------- copr. 2002  advanced software, inc. */
