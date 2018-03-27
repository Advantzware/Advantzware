/* oerep/runbolci.p 02/06 YSK */
/* Print Commercial Invoice                                                   */
/* -------------------------------------------------------------------------- */
def shared var v-term-id as char no-undo.
for each report   where report.term-id eq v-term-id 
                    AND report.key-03 <> "N" 
        BREAK BY report.key-01 BY report.key-02:
    IF FIRST-OF(report.key-02) THEN DO:
       IF report.key-04 = "FIBRECI" THEN RUN oe/rep/bolfibci.p (report.key-01,report.key-02).
    END.

end. /* for each oe-bolh */


/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
