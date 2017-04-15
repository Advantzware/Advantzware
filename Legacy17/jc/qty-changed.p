
DEF PARAM BUFFER io-job FOR job.
DEF OUTPUT PARAM op-qty-changed AS LOG NO-UNDO.

op-qty-changed = AVAIL io-job AND
                 CAN-FIND(FIRST reftable
                          WHERE reftable.reftable EQ "job.qty-changed"
                            AND reftable.company  EQ io-job.company
                            AND reftable.loc      EQ ""
                            AND reftable.code     EQ STRING(io-job.job,"9999999999")).
