/* upditmfg.p */

DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-factor AS DECIMAL NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE BUFFER b-job-hdr FOR job-hdr.

/* gdm - 10060901*/
DEF BUFFER bf-eb FOR eb.

FIND b-job-hdr NO-LOCK WHERE ROWID(b-job-hdr) EQ ip-rowid NO-ERROR.
IF AVAILABLE b-job-hdr THEN DO:

   FIND FIRST itemfg 
        {sys/look/itemfgrlW.i}
      AND itemfg.i-no EQ b-job-hdr.i-no NO-ERROR.

  /* gdm - 10060901 */    
  IF TRIM(b-job-hdr.est-no) NE "" THEN DO:

    IF itemfg.isaset EQ NO THEN
       FIND FIRST bf-eb WHERE
            bf-eb.company  EQ b-job-hdr.company AND
            bf-eb.est-no   EQ b-job-hdr.est-no AND     
            bf-eb.form-no  EQ b-job-hdr.frm AND
            bf-eb.blank-no EQ b-job-hdr.blank-no
            NO-LOCK NO-ERROR.

    IF itemfg.isaset OR
       (AVAIL bf-eb AND NOT bf-eb.pur-man) OR
       (NOT AVAIL bf-eb AND NOT itemfg.pur-man) THEN DO:
       itemfg.q-ono = itemfg.q-ono + (b-job-hdr.qty * ip-factor).    
       
       FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                             AND itemfg-loc.i-no    EQ itemfg.i-no
                             AND itemfg-loc.loc     EQ b-job-hdr.loc:       
          itemfg-loc.q-ono = itemfg-loc.q-ono + (b-job-hdr.qty * ip-factor).   
       END.
    END.
  END.
  ELSE
  /* gdm - end end */    
  IF NOT itemfg.pur-man THEN DO:
     itemfg.q-ono = itemfg.q-ono + (b-job-hdr.qty * ip-factor). 
     FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                           AND itemfg-loc.i-no    EQ itemfg.i-no
                           AND itemfg-loc.loc     EQ b-job-hdr.loc:       
        itemfg-loc.q-ono = itemfg-loc.q-ono + (b-job-hdr.qty * ip-factor).   
     END.     
  END.
  RUN fg/comp-upd.p (RECID(itemfg), b-job-hdr.qty * ip-factor,
                     'q-ono', b-job-hdr.est-no).
  
END.
