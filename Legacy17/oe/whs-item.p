
DEF PARAM BUFFER io-ordl FOR oe-ordl.
DEF OUTPUT PARAM op-whs-item AS LOG NO-UNDO.

op-whs-item = AVAIL io-ordl AND
              CAN-FIND(FIRST reftable
                       WHERE reftable.reftable EQ "oe-ordl.whs-item"
                         AND reftable.company  EQ io-ordl.company
                         AND reftable.loc      EQ STRING(io-ordl.ord-no,"9999999999")
                         AND reftable.code     EQ io-ordl.i-no
                         AND reftable.code2    EQ STRING(io-ordl.line,"9999999999")
                         AND reftable.val[1]   EQ 1).
