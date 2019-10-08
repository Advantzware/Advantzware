
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-i-no LIKE itemfg.i-no FORMAT "X(15)" NO-UNDO.
DEF VAR lv-qty LIKE fg-bin.qty NO-UNDO.

DEF BUFFER b-fg-bin FOR fg-bin.


MESSAGE "Enter FG Item (Leave blank to exit OR '*' for all:"
        UPDATE lv-i-no.

IF lv-i-no EQ "" THEN LEAVE.

SESSION:SET-WAIT-STATE ("general").

IF lv-i-no EQ "*" THEN
FOR EACH itemfg WHERE company EQ cocode:
  RUN fix-neg.
END.

ELSE
FOR EACH itemfg
    WHERE company EQ cocode
      AND i-no    EQ lv-i-no:
  RUN fix-neg.
END.

SESSION:SET-WAIT-STATE ("").

RETURN.

PROCEDURE fix-neg:

  FOR EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        AND fg-bin.tag     NE ""
        AND fg-bin.qty     LT 0:

    FOR EACH b-fg-bin
        WHERE b-fg-bin.company EQ fg-bin.company
          AND b-fg-bin.i-no    EQ fg-bin.i-no
          AND b-fg-bin.tag     EQ fg-bin.tag
          AND b-fg-bin.qty     GT 0
          AND ROWID(b-fg-bin)  NE ROWID(fg-bin)
        BY b-fg-bin.qty DESC:

      IF fg-bin.qty GE 0 THEN LEAVE.

      ASSIGN
       lv-qty       = MIN(b-fg-bin.qty,fg-bin.qty * -1)
       fg-bin.qty   = fg-bin.qty + lv-qty
       b-fg-bin.qty = b-fg-bin.qty - lv-qty.

      RUN fg/cre-pchr.p (ROWID(fg-bin), "A", lv-qty, fg-bin.partial-count).

      RUN fg/cre-pchr.p (ROWID(b-fg-bin), "A", lv-qty * -1, fg-bin.partial-count * -1).
    END.
  END.

END PROCEDURE.
