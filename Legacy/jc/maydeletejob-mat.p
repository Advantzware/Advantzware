       
DEF PARAM BUFFER io-job-mat FOR job-mat.

DEF OUTPUT PARAM op-delete AS LOG NO-UNDO.

          
op-delete = NOT CAN-FIND(FIRST mat-act
                         WHERE mat-act.company EQ io-job-mat.company
                           AND mat-act.job     EQ io-job-mat.job
                           AND mat-act.job-no  EQ io-job-mat.job-no
                           AND mat-act.job-no2 EQ io-job-mat.job-no2
                           AND mat-act.s-num   EQ io-job-mat.frm
                           AND mat-act.b-num   EQ io-job-mat.blank-no
                           AND mat-act.i-no    EQ io-job-mat.i-no
                           AND mat-act.rm-i-no EQ io-job-mat.i-no)      AND
            NOT CAN-FIND(FIRST rm-rctd
                         WHERE rm-rctd.company   EQ io-job-mat.company
                           AND rm-rctd.job-no    EQ io-job-mat.job-no
                           AND rm-rctd.job-no2   EQ io-job-mat.job-no2
                           AND rm-rctd.s-num     EQ io-job-mat.frm
                           AND rm-rctd.b-num     EQ io-job-mat.blank-no
                           AND rm-rctd.i-no      EQ io-job-mat.i-no
                           AND rm-rctd.rita-code EQ "I")                AND
            NOT CAN-FIND(FIRST po-ordl
                         WHERE po-ordl.company   EQ io-job-mat.company
                           AND po-ordl.job-no    EQ io-job-mat.job-no
                           AND po-ordl.job-no2   EQ io-job-mat.job-no2
                           AND po-ordl.i-no      EQ io-job-mat.i-no
                           AND (po-ordl.s-num    EQ ? OR
                                (po-ordl.s-num   EQ io-job-mat.frm AND
                                 po-ordl.b-num   EQ io-job-mat.blank-no))
                         USE-INDEX job-no).

IF op-delete AND io-job-mat.all-flg AND io-job-mat.qty-all NE 0 THEN op-delete = ?.
