def new shared var cocode as char init "001" no-undo.
def new shared var locode as char init 'main' no-undo.
def new shared var x as int no-undo.
def new shared var y as int no-undo.
def new shared var k as int no-undo.

def buffer xoe-relh for oe-relh.
def buffer xoe-rell for oe-rell.
def buffer xoe-bolh for oe-bolh.
def buffer xoe-boll for oe-boll.
def buffer xfg-bin for fg-bin.


disable triggers for load of oe-bolh.

for each oe-bolh where company eq cocode and bol-no eq 26909:
  find first oe-ctrl where oe-ctrl.company = oe-bolh.company no-lock no-error.
  
  for each oe-boll
                  where oe-boll.company eq oe-bolh.company
                    and oe-boll.b-no    eq oe-bolh.b-no
                  use-index b-no:
                           /** DELETE ALL RELATED BACK ORDER RELEASES **/
              FOR EACH oe-rell
                  WHERE oe-rell.company  eq oe-boll.company
                    AND oe-rell.ord-no   eq oe-boll.ord-no
                    AND oe-rell.rel-no   eq oe-boll.rel-no
                    AND oe-rell.b-ord-no gt oe-boll.b-ord-no:

                /* Backup out updates to planned release file. */
                FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no
                    USE-INDEX seq-no NO-ERROR.
                IF AVAIL oe-rel THEN oe-rel.qty = oe-rel.qty + oe-rell.qty.

                /* Delete actual release entry in the planned release file. */
                FIND FIRST oe-rel WHERE oe-rel.link-no EQ oe-rell.r-no
                    USE-INDEX link NO-ERROR.
                IF AVAIL oe-rel THEN DO: 
                  FIND FIRST itemfg-loc 
                      WHERE itemfg-loc.company EQ oe-rel.company
                        AND itemfg-loc.i-no    EQ oe-rel.i-no
                        AND itemfg-loc.loc     EQ oe-rel.spare-char-1
                      EXCLUSIVE-LOCK NO-ERROR.      
                  IF AVAIL itemfg-loc AND oe-rel.spare-dec-1 GT 0 THEN
                      ASSIGN itemfg-loc.q-alloc = itemfg-loc.q-alloc - oe-rel.spare-dec-1
                             itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
                  FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                  DELETE oe-rel.
                END.
                  
                FIND FIRST itemfg
                    WHERE itemfg.company EQ oe-boll.company
                      AND itemfg.i-no    EQ oe-rell.i-no
                    NO-ERROR.
                IF AVAIL itemfg THEN DO: 
                    itemfg.q-back = itemfg.q-back - oe-rell.qty.
                
                    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT oe-rell.loc).
            
                    FIND FIRST itemfg-loc 
                        WHERE itemfg-loc.company EQ itemfg.company
                          AND itemfg-loc.i-no    EQ itemfg.i-no
                          AND itemfg-loc.loc     EQ oe-rell.loc
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL(itemfg-loc) THEN
                      itemfg-loc.q-back = itemfg-loc.q-back - oe-rell.qty.
                    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                END.
                FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-ERROR.
                IF AVAIL oe-relh THEN DO:
                  FIND FIRST xoe-rell
                      WHERE xoe-rell.company EQ oe-relh.company
                        AND xoe-rell.r-no    EQ oe-relh.r-no
                        AND ROWID(xoe-rell)  NE ROWID(oe-rell)
                      USE-INDEX r-no NO-ERROR.
                  IF NOT AVAIL xoe-rell THEN DELETE oe-relh.
                END.

                DELETE oe-rell.
              END. /* each oe-relh */

              /* DELETE TEMPORARY CUSTOMER WAREHOUSE FG-BIN */
              FOR EACH fg-bin
                  WHERE fg-bin.company EQ oe-bolh.company
                    AND fg-bin.bol-no  EQ oe-bolh.bol-no
                    AND fg-bin.cust-no EQ oe-bolh.cust-no
                    AND fg-bin.i-no    EQ oe-boll.i-no
                    AND fg-bin.loc-bin EQ oe-boll.loc-bin
                    AND fg-bin.job-no  EQ oe-boll.job-no
                    AND fg-bin.job-no2 EQ oe-boll.job-no2
                    AND fg-bin.tag     EQ oe-boll.tag
                  USE-INDEX bol-no EXCLUSIVE-LOCK:

                FIND FIRST xfg-bin
                    WHERE xfg-bin.company EQ oe-boll.company
                      AND xfg-bin.i-no    EQ oe-boll.i-no
                      AND xfg-bin.loc     EQ oe-boll.loc
                      AND xfg-bin.loc-bin EQ oe-boll.loc-bin
                      AND xfg-bin.job-no  EQ oe-boll.job-no
                      AND xfg-bin.job-no2 EQ oe-boll.job-no2
                      AND xfg-bin.tag     EQ oe-boll.tag
                      AND xfg-bin.cust-no EQ ""
                    NO-ERROR.
                if not avail xfg-bin then do:
                    create xfg-bin.
                    assign
                     xfg-bin.company    = oe-bolh.company
                     xfg-bin.i-no       = oe-boll.i-no
                     xfg-bin.loc        = oe-boll.loc
                     xfg-bin.loc-bin    = oe-bolh.cust-no
                     xfg-bin.tag        = ""
                     xfg-bin.job-no     = oe-boll.job-no
                     xfg-bin.job-no2    = oe-boll.job-no2
                     xfg-bin.last-count = fg-bin.qty
                     xfg-bin.aging-date = today
                     fg-bin.ord-no      = oe-boll.ord-no.
                end.

                assign
                 xfg-bin.last-date = today
                 xfg-bin.qty       = xfg-bin.qty + fg-bin.qty.

                delete fg-bin.
              end.

                /* Added to put back in back order qty to FG file */
                if not oe-ctrl.u-inv and oe-boll.b-ord-no gt 0 then do:
                  find first itemfg where itemfg.company = oe-bolh.company and
                                          itemfg.i-no = oe-boll.i-no no-error.
                  if avail itemfg THEN DO:
                     RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT oe-boll.loc).
            
                     FIND FIRST itemfg-loc 
                         WHERE itemfg-loc.company EQ itemfg.company
                           AND itemfg-loc.i-no    EQ itemfg.i-no
                           AND itemfg-loc.loc     EQ oe-boll.loc
                         EXCLUSIVE-LOCK NO-ERROR.                  
                     assign itemfg.q-back = itemfg.q-back + oe-boll.qty.
                     IF AVAIL(itemfg-loc) THEN
                       ASSIGN itemfg-loc.q-back = itemfg-loc.q-back + oe-boll.qty.
                     FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                  END.
                end.

                if oe-bolh.bol-no ne 0 and oe-boll.ord-no ne 0 then
                  oe-boll.posted = no.
                  
                find first oe-ordl
                    where oe-ordl.company eq oe-boll.company
                      and oe-ordl.ord-no  eq oe-boll.ord-no
                      and oe-ordl.i-no    eq oe-boll.i-no
                      and oe-ordl.line    eq oe-boll.line
                    no-error.
                if avail oe-ordl then
                  run n:/rcode/oe/ship-qty.p (ROWID(oe-ordl), output oe-ordl.ship-qty).
  end. /* each oe-boll */
  
            /** Back Out Inventory History of the Bill Of Lading **/
              for each fg-rcpth
                  where fg-rcpth.b-no      eq oe-bolh.b-no
                    and fg-rcpth.rita-code ne "R"
                  use-index b-no:
                for each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no:
                  find first fg-bin
                      where fg-bin.company eq oe-bolh.company
                        and fg-bin.i-no    eq fg-rcpth.i-no
                        and fg-bin.loc     eq fg-rdtlh.loc
                        and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                        and fg-bin.tag     eq fg-rdtlh.tag
                        and fg-bin.job-no  eq fg-rcpth.job-no
                        and fg-bin.job-no2 eq fg-rcpth.job-no2
                      no-error.
                  if not avail fg-bin then do:
                    create fg-bin.
                    assign
                     fg-bin.company    = oe-bolh.company
                     fg-bin.i-no       = fg-rcpth.i-no
                     fg-bin.loc        = fg-rdtlh.loc
                     fg-bin.loc-bin    = fg-rdtlh.loc-bin
                     fg-bin.tag        = fg-rdtlh.tag
                     fg-bin.job-no     = fg-rcpth.job-no
                     fg-bin.job-no2    = fg-rcpth.job-no2
                     fg-bin.aging-date = today.
                  end.
                  
                  if fg-rcpth.rita-code eq "S" then
                    fg-bin.qty = fg-bin.qty + fg-rdtlh.qty.
                  else
                    fg-bin.qty = fg-bin.qty - fg-rdtlh.qty.
                  delete fg-rdtlh.
                end. /* each fg-rdtlh */
                delete fg-rcpth.
              end. /* each fg-rcpth */
              
  oe-bolh.posted = no.
end.
