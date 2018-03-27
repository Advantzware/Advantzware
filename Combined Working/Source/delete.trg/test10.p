
def buffer xoe-relh for oe-relh.
def buffer xoe-rell for oe-rell.
def buffer xoe-bolh for oe-bolh.
def buffer xoe-boll for oe-boll.
def buffer xfg-bin for fg-bin.

def var v-rel-bal like oe-rell.qty.
def var v-lines-wt as int.
def var v-nxt-bordno like oe-relh.b-ord-no.
def var v-nxt-rno like oe-relh.r-no.
DEF VAR lv-partial LIKE fg-bin.partial-count NO-UNDO.

DEF WORKFILE w-r-no FIELD w-r-no LIKE inv-head.r-no.
DEF WORKFILE w-b-no FIELD w-b-no LIKE inv-line.b-no.

find first inv-head where inv-no = 188642 no-lock.

CREATE w-r-no.
w-r-no = inv-head.r-no.


DO WHILE AVAIL w-r-no.
  FOR EACH inv-line
      WHERE inv-line.r-no EQ w-r-no
        AND NOT CAN-FIND(FIRST w-b-no WHERE w-b-no EQ inv-line.b-no):
    CREATE w-b-no.
    w-b-no = inv-line.b-no.
  END.

  FOR EACH w-b-no,
      EACH inv-line
      WHERE inv-line.b-no EQ w-b-no
        AND NOT CAN-FIND(FIRST w-r-no WHERE w-r-no EQ inv-line.r-no)
      NO-LOCK:
    CREATE w-r-no.
    w-r-no = inv-line.r-no.
  END.

  RELEASE w-r-no.

  FOR EACH w-r-no,
      FIRST inv-line
      WHERE inv-line.r-no EQ w-r-no
        AND NOT CAN-FIND(FIRST w-b-no WHERE w-b-no EQ inv-line.b-no)
      NO-LOCK:
    LEAVE.
  END.
END.

FOR EACH w-b-no,
    FIRST oe-bolh WHERE oe-bolh.b-no EQ w-b-no NO-LOCK,
    EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no
    USE-INDEX b-no:

  FOR EACH xoe-boll
      WHERE xoe-boll.company    EQ oe-boll.company
        AND xoe-boll.ord-no     EQ oe-boll.ord-no
        AND ((xoe-boll.rel-no   EQ oe-boll.rel-no AND
              xoe-boll.b-ord-no GT oe-boll.b-ord-no)    OR
             (xoe-boll.i-no     EQ oe-boll.i-no   AND
              xoe-boll.line     EQ oe-boll.line   AND
              xoe-boll.rel-no   GT oe-boll.rel-no AND
              xoe-boll.s-code   NE "B"            AND
              oe-boll.s-code    NE "B"))
        AND NOT CAN-FIND(FIRST xoe-bolh
                         WHERE xoe-bolh.b-no   EQ xoe-boll.b-no
                           AND xoe-bolh.posted EQ YES)
      NO-LOCK:
    MESSAGE "Can not delete invoice until all subsequent " +
            "invoices and bills of lading are deleted."
            skip
            xoe-boll.ord-no xoe-boll.i-no oe-bolh.bol-no
            xoe-boll.b-ord-no xoe-boll.b-no xoe-boll.bol-no
        VIEW-AS ALERT-BOX ERROR.

  END.
END.

