
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-run   AS INT   NO-UNDO.

{sys/inc/var.i SHARED}

DEF NEW SHARED BUFFER xoe-ordl FOR oe-ordl.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.

DEF BUFFER bf-rell FOR oe-rell.

DEF BUFFER bf-fg-rctd FOR fg-rctd.
DEF BUFFER bf-po-ordl FOR po-ordl.
DEF BUFFER bf-po-ord  FOR po-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF BUFFER bf-oe-ord  FOR oe-ord.
DEF BUFFER bf-ref     FOR reftable.

DEF NEW SHARED VAR out-recid AS RECID NO-UNDO.
DEF NEW SHARED VAR relh-recid AS RECID NO-UNDO.
DEF NEW SHARED VAR v-auto     AS LOG NO-UNDO.
DEF BUFFER upd-oe-relh FOR oe-relh.

DEF VAR v-relpost-hld AS CHAR NO-UNDO.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR li-nxt-rel-no AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.
DEF VAR v-n-bol LIKE oe-ctrl.n-bol NO-UNDO.
DEF VAR v-royal AS LOG NO-UNDO.
DEF VAR v-first-release AS LOG NO-UNDO.
DEF VAR v-bol-qty LIKE oe-boll.qty NO-UNDO.
DEF VAR ll-exception AS LOG NO-UNDO.
DEF VAR li-tag-no AS INT NO-UNDO.
DEF VAR lv-new-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR ll-first AS ROWID NO-UNDO.
DEF VAR lv-cust-x LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR vfrt-pay AS CHAR NO-UNDO.
DEF VAR vfob-code AS CHAR NO-UNDO.
DEF VAR vfrt-list AS CHAR NO-UNDO.
DEF VAR vfob-list AS CHAR NO-UNDO.
DEF VAR rell-ctr AS INTE NO-UNDO.
DEF VAR new-freight AS DEC NO-UNDO.
DEF VAR old-freight AS DEC NO-UNDO.
DEF VAR lInvFrt AS LOG NO-UNDO.
DEF VAR dBillAmt AS DECIMAL NO-UNDO.
DEF VAR lEmailBol AS LOG NO-UNDO.
DEF VAR iocPrompt AS CHAR NO-UNDO.
DEF VAR vrRelh AS ROWID NO-UNDO.

      {oe/rep/oe-lad.i NEW}
      /* {oe/oe-bolpi.i NEW}   */
      /* {oe/bolcheck.i NEW}   */
      {oe/closchk.i NEW}
      {custom/formtext.i NEW}
      {oerep/r-bolx.i NEW}

/* Needed for oe/actrelmerge.p */
{oe/chkordl.i NEW}
{oe/relemail.i NEW}

{fg/invrecpt.i}

{sa/sa-sls01.i}

{oe/relcheck.i NEW}

DO TRANSACTION:
  {sys/inc/boldate.i}
  {ce/msfcalc.i}
  {sys/inc/fginvrec.i}
  {sys/ref/relpost.i}
END.

lv-cust-x = "".
FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

IF ip-run EQ 1 AND fginvrec-log THEN
  FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL fg-rctd THEN
  RUN get-ord-recs (ROWID(fg-rctd),
                    BUFFER po-ordl,
                    BUFFER po-ord,
                    BUFFER oe-ordl,
                    BUFFER oe-ord,
                    BUFFER reftable).

IF AVAIL reftable THEN DO TRANSACTION:
  ASSIGN
   ll       = reftable.val[1] NE 0
   ll-first = ROWID(fg-rctd).

  FOR EACH bf-fg-rctd
      WHERE bf-fg-rctd.company   EQ fg-rctd.company
        AND bf-fg-rctd.rita-code EQ "R"
        AND bf-fg-rctd.po-no     EQ fg-rctd.po-no
      BY bf-fg-rctd.r-no:

    RUN get-ord-recs (ROWID(bf-fg-rctd),
                      BUFFER bf-po-ordl,
                      BUFFER bf-po-ord,
                      BUFFER bf-oe-ordl,
                      BUFFER bf-oe-ord,
                      BUFFER bf-ref).
    ASSIGN
     ll       = bf-ref.val[1] NE 0
     ll-first = ROWID(bf-fg-rctd)
     dBillAmt = bf-ref.val[2]
     lEmailBol = bf-ref.val[3] EQ 1
     lInvFrt  = bf-ref.val[2] NE 0.

    LEAVE.
  END.

  IF ROWID(fg-rctd) EQ ll-first THEN DO:

  
    FIND FIRST bf-ref
        WHERE bf-ref.reftable EQ "fg-rctd.user-id"
          AND bf-ref.company  EQ fg-rctd.company
          AND bf-ref.loc      EQ STRING(fg-rctd.r-no,"9999999999")
          AND (bf-ref.val[1] EQ 1
              OR bf-ref.val[3] GT 0)
        NO-LOCK NO-ERROR.
           
    IF NOT AVAIL bf-ref THEN 
      /* Prompt for other information for invoice */    
      RUN prompt-for-invoice (OUTPUT ll, OUTPUT lInvFrt, OUTPUT dBillAmt, OUTPUT lEmailBol).

  END.

/*   END. */
  FOR EACH bf-fg-rctd
      WHERE bf-fg-rctd.company   EQ fg-rctd.company
        AND bf-fg-rctd.rita-code EQ "R"
        AND bf-fg-rctd.po-no     EQ fg-rctd.po-no:

    RUN get-ord-recs (ROWID(bf-fg-rctd),
                      BUFFER bf-po-ordl,
                      BUFFER bf-po-ord,
                      BUFFER bf-oe-ordl,
                      BUFFER bf-oe-ord,
                      BUFFER bf-ref).

    FIND CURRENT bf-ref.
 
    bf-ref.val[1] = INT(ll).
    bf-ref.val[3] = INT(lEmailBol).
    IF lInvFrt THEN
      bf-ref.val[2] = dBillAmt.

    IF ll AND bf-ref.dscr EQ "" THEN DO:
      li-tag-no = 0.

      FOR EACH loadtag
          WHERE loadtag.company   EQ bf-oe-ordl.company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    BEGINS STRING(CAPS(bf-oe-ordl.i-no),"x(15)")      
          NO-LOCK
          BY loadtag.tag-no DESC:
        li-tag-no = INT(SUBSTR(loadtag.tag-no,16,5)).
        LEAVE.
      END. /* repeat*/
  
      CREATE loadtag.
      ASSIGN
       loadtag.company      = bf-oe-ordl.company
       loadtag.tag-no       = STRING(CAPS(bf-oe-ordl.i-no),"x(15)") +
                              STRING(li-tag-no + 1,"99999")
       bf-ref.dscr          = loadtag.tag-no
       loadtag.item-type    = NO /*FGitem*/
       loadtag.po-no        = bf-po-ord.po-no
       loadtag.job-no       = bf-fg-rctd.job-no
       loadtag.job-no2      = bf-fg-rctd.job-no2
       loadtag.ord-no       = bf-oe-ordl.ord-no
       loadtag.i-no         = CAPS(bf-oe-ordl.i-no)
       loadtag.i-name       = bf-oe-ordl.i-name
       loadtag.qty          = bf-fg-rctd.t-qty
       loadtag.qty-case     = bf-fg-rctd.qty-case
       loadtag.case-bundle  = bf-fg-rctd.cases-unit
       loadtag.pallet-count = loadtag.qty-case * loadtag.case-bundle
       loadtag.partial      = bf-fg-rctd.partial
       loadtag.sts          = "Printed"
       loadtag.tag-date     = TODAY
       loadtag.tag-time     = TIME
       bf-fg-rctd.tag       = bf-ref.dscr.
    END.

    ELSE
    IF NOT ll AND bf-ref.dscr NE "" THEN DO:
      FIND FIRST loadtag
          WHERE loadtag.company   EQ bf-oe-ordl.company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ bf-ref.dscr
          NO-ERROR.
      IF AVAIL loadtag THEN DELETE loadtag.
      ASSIGN
       bf-ref.dscr    = ""
       bf-fg-rctd.tag = bf-ref.dscr.
    END.

    FIND CURRENT bf-ref NO-LOCK NO-ERROR.
    FIND itemfg WHERE itemfg.company = bf-oe-ordl.company
                  AND itemfg.i-no    = bf-oe-ordl.i-no
                NO-LOCK NO-ERROR.
    IF AVAIL itemfg 
       AND itemfg.std-tot-cost EQ 0
       AND itemfg.std-mat-cost EQ 0
       AND itemfg.avg-cost EQ 0 THEN
         run fg/updfgcst.p (itemfg.i-no).
  END.
END.

ELSE
IF ip-run EQ 2 THEN DO TRANSACTION:

  FOR EACH w-inv,
      FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-inv.row-id NO-LOCK:

    RUN get-ord-recs (ROWID(fg-rctd),
                      BUFFER po-ordl,
                      BUFFER po-ord,
                      BUFFER oe-ordl,
                      BUFFER oe-ord,
                      BUFFER reftable).
    
    IF NOT AVAIL reftable OR reftable.val[1] EQ 0 THEN DELETE w-inv.
  END.

  FOR EACH w-inv,
      FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-inv.row-id NO-LOCK
      BREAK BY fg-rctd.r-no:

    
    RUN get-ord-recs (ROWID(fg-rctd),
                      BUFFER po-ordl,
                      BUFFER po-ord,
                      BUFFER oe-ordl,
                      BUFFER oe-ord,
                      BUFFER reftable).
    IF AVAIL(reftable) AND (reftable.val[2] GT 0 OR reftable.val[3] EQ 1) THEN
    ASSIGN dBillAmt = reftable.val[2]
           lEmailBol = reftable.val[3] EQ 1
           lInvFrt  = reftable.val[2] GT 0. /* if bill amt gt 0, then assign freight flag */
    
    lv-rowid = ?.
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.ship-id EQ po-ord.ship-id
        NO-LOCK
        BY oe-rel.rel-date:
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
      IF INDEX("SLI",lv-stat) GT 0 THEN DO:
        lv-rowid = ROWID(oe-rel).
        LEAVE.
      END.
    END.
    FIND oe-rel WHERE ROWID(oe-rel) EQ lv-rowid NO-ERROR.

    IF NOT AVAIL oe-rel THEN DO:
      FIND FIRST sys-ctrl
          WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "OECARIER"
          NO-LOCK NO-ERROR.
/* 10051225 */
/*       FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.      */
/*       v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
      RUN oe/get-r-no.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
      CREATE oe-rel.
      ASSIGN
       oe-rel.company   = oe-ordl.company
       oe-rel.r-no      = v-nxt-r-no
       oe-rel.cust-no   = oe-ord.cust-no
       oe-rel.ord-no    = oe-ordl.ord-no
       oe-rel.ship-id   = po-ord.ship-id
       oe-rel.i-no      = oe-ordl.i-no
       oe-rel.line      = oe-ordl.line
       oe-rel.po-no     = IF oe-ordl.po-no NE "" THEN oe-ordl.po-no 
                                                 ELSE oe-ord.po-no
       oe-rel.qty       = fg-rctd.t-qty
       oe-rel.s-comm[1] = oe-ord.s-comm[1]
       oe-rel.s-comm[2] = oe-ord.s-comm[2]
       oe-rel.s-comm[3] = oe-ord.s-comm[3]
       oe-rel.s-name[1] = oe-ord.sname[1]
       oe-rel.s-name[2] = oe-ord.sname[2]
       oe-rel.s-name[3] = oe-ord.sname[3]
       oe-rel.s-pct[1]  = oe-ord.s-pct[1]
       oe-rel.s-pct[2]  = oe-ord.s-pct[2]
       oe-rel.s-pct[3]  = oe-ord.s-pct[3]
       oe-rel.sman[1]   = oe-ord.sman[1]
       oe-rel.sman[2]   = oe-ord.sman[2]
       oe-rel.sman[3]   = oe-ord.sman[3]
       oe-rel.sold-no   = oe-ord.sold-no
       oe-rel.carrier   = oe-ord.carrier.

      FIND FIRST shipto NO-LOCK
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ oe-rel.cust-no
            AND shipto.ship-id EQ oe-rel.ship-id
          NO-ERROR.
      IF AVAIL shipto THEN DO:
        ASSIGN
         oe-rel.ship-addr[1] = shipto.ship-addr[1]
         oe-rel.ship-city    = shipto.ship-city
         oe-rel.ship-state   = shipto.ship-state
         oe-rel.ship-zip     = shipto.ship-zip
         oe-rel.ship-no      = shipto.ship-no
         oe-rel.ship-i[1]    = shipto.notes[1]
         oe-rel.ship-i[2]    = shipto.notes[2]
         oe-rel.ship-i[3]    = shipto.notes[3]
         oe-rel.ship-i[4]    = shipto.notes[4]
         oe-rel.spare-char-1 = shipto.loc.
        IF AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Shipto" THEN
          oe-rel.carrier = shipto.carrier.
      END.
      ELSE DO:
          FIND FIRST fg-bin 
             WHERE fg-bin.company EQ oe-rel.company
               AND fg-bin.i-no    EQ oe-rel.i-no
             NO-LOCK NO-ERROR.
          IF AVAIL fg-bin THEN
              oe-rel.spare-char-1 = fg-bin.loc.                   
      END.
      RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).
    END.

    ASSIGN
     oe-rel.rel-date = TODAY
     oe-rel.cust-no  = oe-ord.cust-no     
     relh-recid      = ?.
     
     /* Setting this allows relmerge logic to combine properly if the user */
     /* has manually changed the loc on fg-rctd                            */
     IF AVAIL(po-ord) AND po-ord.type EQ "D" THEN
       oe-rel.spare-char-1 = fg-rctd.loc.
     
    /* 07011402 {oe/findrelh.i oe-rel oe-rel.cust-no} */
    RUN oe/actrelmerg.p (INPUT ROWID(oe-rel), INPUT "FINDRELH", INPUT-OUTPUT iocPrompt, OUTPUT vrRelh).
    FIND oe-relh WHERE ROWID(oe-relh) EQ vrRElh NO-LOCK NO-ERROR.
    
    IF AVAIL oe-relh THEN relh-recid = RECID(oe-relh).

    IF NOT AVAIL oe-relh                 OR
       FIRST(fg-rctd.r-no)               OR
       fginvrec-chr        EQ "Inv/Item" THEN
      RUN oe/cre-relh.p (RECID(oe-rel)).

    FIND oe-relh WHERE RECID(oe-relh) EQ relh-recid.
          
    w-inv.r-no = oe-relh.r-no.

    FOR EACH oe-rell
        WHERE oe-rell.company EQ cocode
          AND oe-rell.ord-no  EQ oe-rel.ord-no
        NO-LOCK 
        BY oe-rell.rel-no DESC:
      li-nxt-rel-no =  oe-rell.rel-no.
      LEAVE.  
    END.
    li-nxt-rel-no = li-nxt-rel-no + 1.
      
    CREATE oe-rell.
    ASSIGN
     oe-rell.company  = oe-rel.company
     oe-rell.r-no     = oe-relh.r-no
     oe-rell.rel-no   = li-nxt-rel-no
     oe-rell.loc      = locode
     oe-rell.ord-no   = oe-rel.ord-no
     oe-rell.qty      = fg-rctd.t-qty
     oe-rell.i-no     = oe-rel.i-no
     oe-rell.po-no    = oe-rel.po-no
     oe-rell.line     = oe-rel.line
     oe-rell.printed  = YES
     oe-relh.printed  = NO
     oe-rell.posted   = NO
     oe-rell.deleted  = NO
     /** Set link to the planned releases **/
     oe-rell.link-no  = oe-rel.r-no
     oe-rell.s-code   = IF oe-ordl.is-a-component THEN "S" ELSE
                        if avail oe-ctrl and oe-ctrl.ship-from then "B" else "I"
     oe-rell.loc      = fg-rctd.loc
     oe-rell.loc-bin  = fg-rctd.loc-bin
     oe-rell.tag      = fg-rctd.tag
     oe-rell.job-no   = fg-rctd.job-no
     oe-rell.job-no2  = fg-rctd.job-no2
     oe-rell.qty-case = fg-rctd.qty-case
     oe-rell.partial  = fg-rctd.partial
     oe-rell.cases    = TRUNC((oe-rell.qty - oe-rell.partial) /
                              oe-rell.qty-case,0)
     oe-rell.partial  = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).

    ASSIGN
     v-relpost-hld = relpost-chr
     lv-bol-no     = 0.
  END.

  RELEASE oe-relh.

  FIND itemfg WHERE itemfg.company = fg-rctd.company
              AND itemfg.i-no    = fg-rctd.i-no
      NO-LOCK NO-ERROR.
   IF AVAIL itemfg 
       AND itemfg.std-tot-cost EQ 0
       AND itemfg.std-mat-cost EQ 0
       AND itemfg.avg-cost EQ 0 THEN
         run fg/updfgcst.p (itemfg.i-no).

  FOR EACH w-inv,
      EACH oe-relh WHERE oe-relh.r-no EQ w-inv.r-no:
    ASSIGN oe-relh.printed = YES
           oe-relh.spare-char-3 = USERID("NOSWEAT").
  END.

  FOR EACH w-inv,
      FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-inv.row-id NO-LOCK
      BREAK BY w-inv.r-no:
    RUN get-ord-recs (ROWID(fg-rctd),
                      BUFFER po-ordl,
                      BUFFER po-ord,
                      BUFFER oe-ordl,
                      BUFFER oe-ord,
                      BUFFER reftable).
    IF AVAIL(reftable) AND (reftable.val[2] GT 0 OR reftable.val[3] EQ 1) THEN
    ASSIGN dBillAmt = reftable.val[2]
           lEmailBol = reftable.val[3] EQ 1
           lInvFrt  = reftable.val[2] GT 0.
    
    IF FIRST-OF(w-inv.r-no) THEN DO:
      headblok:
      FOR EACH oe-relh WHERE oe-relh.r-no EQ w-inv.r-no
        {oe/oe-relp2.i}
        DO TRANSACTION:
          ASSIGN
           oe-bolh.printed  = YES
           oe-bolh.bol-date = fg-rctd.rct-date. 
          FOR EACH oe-rell where oe-rell.company eq oe-relh.company
                   and oe-rell.r-no    eq oe-relh.r-no
                   USE-INDEX r-no
                NO-LOCK,
            EACH oe-ordl WHERE oe-ordl.company EQ oe-relh.company
                           AND oe-ordl.ord-no EQ oe-rell.ord-no
              EXCLUSIVE-LOCK:
          
              RUN oe/ordlsqty.p (ROWID(oe-ordl),
                             OUTPUT oe-ordl.inv-qty,
                             OUTPUT oe-ordl.ship-qty).
          END.
           IF lInvFrt THEN DO: 
             /* See task 04221405 */
             oe-bolh.frt-pay = "B".
             old-freight = oe-bolh.freight.
             oe-bolh.freight =  dBillAmt.
             new-freight = oe-bolh.freight.
             
             RUN oe/bolfrteq.p (BUFFER oe-bolh, new-freight, old-freight).
/*              IF lEmailBol THEN DO:                                                                                    */
/*                  RUN custom/setUserPrint.p (g_company,'oe-boll_.',                                                    */
/*                        'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert', */
/*                        oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +                                                */
/*                        STRING(oe-bolh.bol-no) + ',' + STRING(oe-bolh.bol-no) +                                        */
/*                        ',,99999999,' + STRING(oe-bolh.printed) + ',' +                                                */
/*                        STRING(oe-bolh.posted) + ',BOL').                                                              */
/*                  RUN listobjs/oe-boll_.w.                                                                             */
/*              END.                                                                                                     */
           END.
            
        END.
      END.
      lv-new-bol-no = oe-bolh.bol-no.
    END.
    w-inv.bol-no = lv-new-bol-no.
  END.

  RUN oe/oe-bolp3.p (v-term).

  FOR EACH w-inv,
      FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-inv.row-id NO-LOCK
      BREAK BY w-inv.bol-no:

    IF FIRST-OF(w-inv.bol-no) THEN
    FOR EACH inv-head
        WHERE inv-head.company EQ fg-rctd.company
          AND inv-head.bol-no  EQ w-inv.bol-no:
      inv-head.inv-date = fg-rctd.rct-date.
      LEAVE.
    END.
  END.
  FOR EACH w-inv,
      FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-inv.row-id NO-LOCK
      BREAK BY fg-rctd.i-no:
      IF LAST-OF(fg-rctd.i-no) THEN DO:
          FIND itemfg WHERE itemfg.company = bf-oe-ordl.company
                      AND itemfg.i-no    = bf-oe-ordl.i-no
                    NO-LOCK NO-ERROR.
           IF AVAIL itemfg 
               AND itemfg.std-tot-cost EQ 0
               AND itemfg.std-mat-cost EQ 0
               AND itemfg.avg-cost EQ 0 THEN
                 run fg/updfgcst.p (itemfg.i-no).
      END.
  END.
END.

RETURN.

PROCEDURE get-ord-recs:
  DEF INPUT PARAM ip-rowid1  AS  ROWID NO-UNDO.

  DEF PARAM BUFFER b-po-ordl FOR po-ordl.
  DEF PARAM BUFFER b-po-ord  FOR po-ord.
  DEF PARAM BUFFER b-oe-ordl FOR oe-ordl.
  DEF PARAM BUFFER b-oe-ord  FOR oe-ord.
  DEF PARAM BUFFER b-ref     FOR reftable.

  DEF BUFFER b-fg-rctd FOR fg-rctd.

  RELEASE b-po-ordl.
  RELEASE b-po-ord.
  RELEASE b-oe-ordl.
  RELEASE b-oe-ord.
  RELEASE b-ref.


  FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ ip-rowid1 NO-LOCK NO-ERROR.

  IF AVAIL b-fg-rctd THEN DO:
    IF INT(b-fg-rctd.po-no) NE 0 AND b-fg-rctd.qty NE 0 THEN
    FIND FIRST b-po-ordl
        WHERE b-po-ordl.company   EQ b-fg-rctd.company
          AND b-po-ordl.po-no     EQ INT(b-fg-rctd.po-no)
          AND b-po-ordl.i-no      EQ b-fg-rctd.i-no
          AND b-po-ordl.job-no    EQ b-fg-rctd.job-no
          AND b-po-ordl.job-no2   EQ b-fg-rctd.job-no2
          AND b-po-ordl.item-type EQ NO
          AND b-po-ordl.ord-no    NE 0
        NO-LOCK NO-ERROR.

    IF AVAIL b-po-ordl THEN
    FIND FIRST b-po-ord
        WHERE b-po-ord.company EQ b-po-ordl.company
          AND b-po-ord.po-no   EQ b-po-ordl.po-no
          AND b-po-ord.type    EQ "D"
        NO-LOCK NO-ERROR.
      
    IF AVAIL b-po-ord THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company  EQ b-po-ordl.company
          AND b-oe-ordl.ord-no   EQ b-po-ordl.ord-no
          AND b-oe-ordl.i-no     EQ b-po-ordl.i-no
          AND b-oe-ordl.vend-no  EQ b-po-ord.vend-no
          AND b-oe-ordl.po-no-po EQ b-po-ord.po-no
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN
    FIND FIRST b-oe-ord
        WHERE b-oe-ord.company EQ b-oe-ordl.company
          AND b-oe-ord.ord-no  EQ b-oe-ordl.ord-no
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST b-ref
        WHERE b-ref.reftable EQ "fg-rctd.user-id"
          AND b-ref.company  EQ b-fg-rctd.company
          AND b-ref.loc      EQ STRING(b-fg-rctd.r-no,"9999999999")
        NO-LOCK NO-ERROR.
  END.
END.

PROCEDURE prompt-for-invoice:
DEF OUTPUT PARAMETER oplCreateInvoice AS LOG NO-UNDO.
DEF OUTPUT PARAMETER oplInvFrt AS LOG NO-UNDO.
DEF OUTPUT PARAMETER opdFrtAmt AS DEC NO-UNDO.
DEF OUTPUT PARAMETER oplEmailBol AS LOG NO-UNDO.
DEFINE VARIABLE lcUserPrompt AS CHARACTER INIT "".


DEFINE VARIABLE ip-parms     AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-values    AS CHARACTER NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE choice       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid       AS LOGICAL   NO-UNDO.
DEF VAR lInvoiceFreight AS LOG NO-UNDO.
DEF VAR lEmailBOL AS LOG NO-UNDO.
DEF VAR dBillableFreight AS DEC NO-UNDO.
DEF VAR lCreateInvoice AS LOG NO-UNDO.

DEFINE VARIABLE lvErrMsg     AS CHARACTER   NO-UNDO.


  ip-parms = 
   /* Box Title */
   "type=literal,name=label11,row=2,col=18,enable=false,width=58,scrval=" + lcUserPrompt + ",FORMAT=X(58)" 
   
    /* Create an Invoice? */
    /*+ "|type=literal,name=label10,row=2.2,col=26,enable=false,width=38,font=5,scrval=" + "Create Invoice for Drop Shipped Purchase Order?" + ",FORMAT=X(58)" */
    + "|type=toggle,name=tb_addinv,row=2.2,col=23,enable=true,width=42,font=5,data-type=logical,label=Create Invoice for Drop Shipped Purchase Order?"

   

    /* Invoice Freight toggle box */
/*    + "|type=literal,name=label9,row=5.7,col=26,enable=false,width=38,font=5,scrval=" + "INVOICE Freight? " + ",FORMAT=X(58)" */
    + "|type=toggle,name=tb_invfrt,row=5.7,col=23,enable=true,width=25,font=5,data-type=logical,label=INVOICE Freight?,depfield=tb_addinv"

    + "|type=literal,name=label7,row=5.7,col=50,enable=false,width=58,font=5,scrval=" + "Billable Freight:" + ",FORMAT=X(58)" 
    + "|type=fill-in,name=fi_BillAmt,row=5.6,col=69,enable=true,width=15,font=5,data-type=decimal,depfield=tb_invfrt" 

    /* Email BOL toggle box */
   /* + "|type=literal,name=label9,row=7.2,col=26,enable=false,width=38,font=5,scrval=" + "Email Bill of Lading? " + ",FORMAT=X(58)" */
    + "|type=toggle,name=tb_emailBol,row=7.2,col=23,enable=true,width=30,font=5,data-type=logical,label=Email Bill of Lading?,depfield=tb_addinv"
     

    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true,width=12,height=3 " 
    /* Box Title */
    + "|type=win,name=fi3,enable=true,width=100,label=         Freight For Invoice to be Created?,FORMAT=X(30),height=14".

    prompt-loop:
    DO WHILE TRUE:
    
        RUN custom/d-prompt.w (INPUT "", ip-parms, "", OUTPUT op-values).
    
        /* Process values using names given above */
        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "default" THEN
              choice = ENTRY(i + 1, op-values) NO-ERROR.
    
            /* Create Invoice */
            IF ENTRY(i, op-values) EQ "tb_addinv" THEN
              lCreateInvoice = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 
    
            /* Invoice Freight */
            IF ENTRY(i, op-values) EQ "tb_invfrt" THEN
              lInvoiceFreight = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 
    
            /* Email BOL */
            IF ENTRY(i, op-values) EQ "tb_emailBol" THEN
              lEmailBol = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR. 
    
            /* Billable Freight */
            IF ENTRY(i, op-values) EQ "fi_BillAmt" THEN
              dBillableFreight = DECIMAL(ENTRY(i + 1, op-values)) NO-ERROR.  
    
        END.
    
        lvErrMsg = "".
        IF choice NE "CANCEL" THEN DO:
            IF lInvoiceFreight AND dBillableFreight EQ 0 THEN DO:
              MESSAGE "If INVOICE Freight is checked, an amount must be entered."
                       VIEW-AS ALERT-BOX ERROR. 
              NEXT.
            END.
            ASSIGN oplCreateInvoice = lCreateInvoice
                   oplInvFrt   = lInvoiceFreight
                   opdFrtAmt   = dBillableFreight
                   oplEmailBol = lEmailBol.
    
        END.
        LEAVE.
    END.
  END PROCEDURE.

PROCEDURE emailBol:
  /* This routine would be used to send the ASN in batch mode */
  DEF INPUT PARAMETER iprBolh AS ROWID NO-UNDO.

  def var ipibegin_bol#         AS INTEGER   .
  def var ipiend_bol#           AS INTEGER   .
  def var ipcbegin_cust         AS CHARACTER .
  def var ipcend_cust           AS CHARACTER .
  def var ipibegin_ord#         AS INTEGER   NO-UNDO.
  def var ipiend_ord#           AS INTEGER   NO-UNDO.
  def var ipcfi_depts           AS CHARACTER NO-UNDO.
  def var ipcfi_specs           AS CHARACTER NO-UNDO.
  def var ipclbl_bolcert        AS CHARACTER NO-UNDO.
  def var ipilines-per-page     AS INTEGER   NO-UNDO.
  def var ipclv-font-name       AS CHARACTER NO-UNDO.
  def var ipclv-font-no         AS CHARACTER NO-UNDO.
  def var ipclv-ornt            AS CHARACTER NO-UNDO.
  def var ipird-dest            AS INTEGER   NO-UNDO.
  def var ipcrd_bolcert         AS CHARACTER NO-UNDO.
  def var ipltb_barcode         AS LOGICAL   NO-UNDO.
  def var ipltb_ComInvoice      AS LOGICAL   NO-UNDO.
  def var ipltb_EMailAdvNotice  AS LOGICAL   NO-UNDO.
  def var ipltb_freight-bill    AS LOGICAL   NO-UNDO.
  def var ipltb_MailBatchMode   AS LOGICAL   NO-UNDO.
  def var ipltb_pallet          AS LOGICAL   NO-UNDO.
  def var ipltb_post-bol        AS LOGICAL   NO-UNDO.
  def var ipltb_posted          AS LOGICAL   NO-UNDO.
  def var ipltb_print-barcode   AS LOGICAL   NO-UNDO.
  def var ipltb_print-binstags  AS LOGICAL   NO-UNDO.
  def var ipltb_print-component AS LOGICAL   NO-UNDO.
  def var ipltb_print-dept      AS LOGICAL   NO-UNDO.
  def var ipltb_print-shipnote  AS LOGICAL   NO-UNDO.
  def var ipltb_print-spec      AS LOGICAL   NO-UNDO.
  def var ipltb_print_ship      AS LOGICAL   NO-UNDO.
  def var ipltb_reprint         AS LOGICAL   NO-UNDO.
  def var ipltd-show-parm       AS LOGICAL   NO-UNDO.

  DEF VAR h_bol AS HANDLE.
  DEF BUFFER bf-oe-bolh FOR oe-bolh.
  /* {methods/defines/hndldefs.i NEW}*/

  /*
  DEFINE NEW SHARED VARIABLE miscflds_reckey AS CHARACTER.
  DEFINE NEW SHARED VARIABLE table_reckey AS CHARACTER.
  DEFINE NEW SHARED VARIABLE Persistent-Handle AS HANDLE.
  DEFINE NEW SHARED VARIABLE ListLogic-Handle AS HANDLE.
    */
  DEFINE VARIABLE run-proc AS CHARACTER.
  DEFINE VARIABLE hsignature AS CHARACTER NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE is-running AS LOGICAL NO-UNDO.
  DEFINE VARIABLE help-page AS INTEGER NO-UNDO.

/*   RUN nosweat/persist.p PERSISTENT SET Persistent-Handle. 
   RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle. 
      {custom/gcompany.i NEW}
      {custom/gloc.i NEW}
      {custom/getcmpny.i NEW}
      {custom/getloc.i NEW}
      {sys/inc/var.i new shared} */


  FOR LAST bf-oe-bolh WHERE ROWID(bf-oe-bolh) EQ iprBolh
         NO-LOCK:

    ipibegin_bol# = bf-oe-bolh.bol-no.
    ipiEnd_bol# = bf-oe-bolh.bol-no.
    ipcEnd_Cust = "zzzzzzzzzz".
    ipiEnd_ord# = 999999999.
    ipird-dest = 2.
    ipltb_reprint = TRUE.

    ipcBegin_cust = bf-oe-bolh.cust-no.
    ipcEnd_cust = bf-oe-bolh.cust-no.
    ipltb_EMailAdvNotice = TRUE.
    ipltb_MailBatchMode = TRUE.
    RUN oerep/bolprtbat.p /* PERSISTENT SET h_bol */
    (
     ipibegin_bol#         ,  
     ipiend_bol#           ,   
     ipcbegin_cust         ,  
     ipcend_cust           , 
     ipibegin_ord#         ,  
     ipiend_ord#           ,   
     ipcfi_depts           ,
     ipcfi_specs           ,  
     ipclbl_bolcert        , 
     ipilines-per-page     ,    
     ipclv-font-name       ,  
     ipclv-font-no         ,  
     ipclv-ornt            ,  
     ipird-dest            ,  
     ipcrd_bolcert         , 
     ipltb_barcode         ,   
     ipltb_ComInvoice      ,    
     ipltb_EMailAdvNotice  ,    
     ipltb_freight-bill    ,    
     ipltb_MailBatchMode   ,    
     ipltb_pallet          ,    
     ipltb_post-bol        ,    
     ipltb_posted          ,    
     ipltb_print-barcode   ,    
     ipltb_print-binstags  ,    
     ipltb_print-component ,   
     ipltb_print-dept      ,    
     ipltb_print-shipnote  ,    
     ipltb_print-spec      ,   
     ipltb_print_ship      ,    
     ipltb_reprint         ,    
     ipltd-show-parm       )   .
  END.

END PROCEDURE.
