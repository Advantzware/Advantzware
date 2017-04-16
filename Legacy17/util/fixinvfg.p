
SESSION:SET-WAIT-STATE ("general").

FOR EACH inv-line,
    FIRST oe-ordl
    WHERE oe-ordl.company EQ inv-line.company
      AND oe-ordl.ord-no  EQ inv-line.ord-no
      AND oe-ordl.line    EQ inv-line.line
    NO-LOCK:
  inv-line.i-no = oe-ordl.i-no.
END.

SESSION:SET-WAIT-STATE ("").
