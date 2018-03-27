/* createRelease.i - shared by oe/impord.w & cXML/monitor.w */

PROCEDURE createRelease:
  DEFINE INPUT PARAMETER ipcShipTo AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ipcShipFrom AS CHAR NO-UNDO.

  DEF VAR iNextRelNo AS INT NO-UNDO.

  find first sys-ctrl where sys-ctrl.company eq cocode
                         and sys-ctrl.name    eq "OECARIER"
              no-lock no-error.

   find first shipto
       where shipto.company eq oe-ord.company
         and shipto.cust-no EQ oe-ord.cust-no
         and shipto.ship-id EQ ipcShipTo
       no-lock no-error.

    if not avail shipto then
   find first shipto where shipto.company eq cocode
                       and shipto.cust-no eq oe-ord.cust-no
        no-lock no-error.

   find first bf-oe-rel use-index seq-no no-lock no-error.
   /* 10051225 */
   /* iNextRelNo = (if avail bf-oe-rel then bf-oe-rel.r-no else 0) + 1. */
   RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT iNextRelNo).
   create oe-rel.
   assign oe-rel.company   = cocode
          oe-rel.loc       = locode
          oe-rel.ord-no    = oe-ordl.ord-no
          oe-rel.i-no      = oe-ordl.i-no
          oe-rel.cust-no   = oe-ord.cust-no
          oe-rel.po-no     = if oe-ordl.po-no ne "" then oe-ordl.po-no else oe-ord.po-no
          oe-rel.qty       = oe-ordl.qty /*- v-qty-sum */
          oe-rel.tot-qty   = oe-ordl.qty
          oe-rel.line      = oe-ordl.line
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
          oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" and avail shipto then shipto.carrier
                             else oe-ord.carrier
          oe-rel.r-no      = iNextRelNo.

         IF oereleas-cha eq "LastShip" then
                              oe-rel.rel-date = oe-ord.last-date.
          ELSE IF oereleas-cha EQ "Due Date" THEN
                              oe-rel.rel-date = oe-ordl.req-date.
         ELSE /*DueDate+1Day*/
                           DO:
                              oe-rel.rel-date =oe-ordl.req-date + 1.
                             IF WEEKDAY(oe-rel.rel-date) EQ 7 THEN
                                 oe-rel.rel-date = oe-rel.rel-date + 2.
                              ELSE
                                 IF WEEKDAY(oe-rel.rel-date) EQ 1 THEN
                                    oe-rel.rel-date = oe-rel.rel-date + 1.
                           END.

     if avail shipto then
      assign oe-rel.ship-addr[1] = shipto.ship-addr[1]
             oe-rel.ship-city    = shipto.ship-city
             oe-rel.ship-state   = shipto.ship-state
             oe-rel.ship-zip     = shipto.ship-zip
             oe-rel.ship-no      = shipto.ship-no
             oe-rel.ship-id      = shipto.ship-id
             oe-rel.ship-i[1]    = shipto.notes[1]
             oe-rel.ship-i[2]    = shipto.notes[2]
             oe-rel.ship-i[3]    = shipto.notes[3]
             oe-rel.ship-i[4]    = shipto.notes[4]
             oe-rel.spare-char-1 = IF ipcShipFrom NE "" THEN ipcShipFrom ELSE shipto.loc.
   else assign oe-rel.ship-no   = oe-ord.sold-no
               oe-rel.ship-id   = oe-ord.sold-id
               oe-rel.ship-i[1] = oe-ord.ship-i[1]
               oe-rel.ship-i[2] = oe-ord.ship-i[2]
               oe-rel.ship-i[3] = oe-ord.ship-i[3]
               oe-rel.ship-i[4] = oe-ord.ship-i[4]
               oe-rel.spare-char-1 = IF ipcShipFrom NE "" THEN ipcShipFrom ELSE oe-ord.loc.
   /* Assign itemfg-loc values */
   RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

END PROCEDURE.
