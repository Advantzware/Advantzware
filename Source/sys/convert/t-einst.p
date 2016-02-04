/* sys/convert/t-einst.p   Transfer est-inst to notes */
def var ls-key as cha form "x(20)" no-undo.
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF job-hdr.
DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF job.

FOR EACH est WHERE rec_key = "" :

    ls-key = string(today,"99999999") +
                   string(next-value(rec_key_seq,nosweat),"99999999").

    est.rec_key = ls-key.               

    create rec_key.
    assign rec_key.rec_key = est.rec_key
           rec_key.table_name = "est".

    DISP est.est-no est.rec_key.
    FOR each oe-ordl WHERE oe-ordl.company = est.company
                         AND oe-ordl.est-no = est.est-no .
        oe-ordl.rec_key = est.rec_key.
    END.
    FOR EACH  job-hdr WHERE job-hdr.company = est.company
                         AND job-hdr.est-no = est.est-no .
        ASSIGN job-hdr.rec_key = est.rec_key.
        FIND FIRST job OF job-hdr NO-ERROR.
        IF AVAIL job THEN job.rec_key = est.rec_key.
    END.
    PAUSE 0.

END.

PAUSE 0.
FOR EACH est-inst NO-LOCK.
    FIND FIRST est WHERE est.company = est-inst.company 
                     AND est.est-no = est-inst.est-no NO-LOCK NO-ERROR.
    IF AVAIL est THEN DO:
       FIND FIRST notes WHERE notes.rec_key = est.rec_key AND
                              notes.note_code = est-inst.dept AND
                              notes.note_form_no = est-inst.line-no AND
                              notes.note_title = est-inst.inst[1] 
                              NO-LOCK NO-ERROR.
       IF AVAIL notes THEN NEXT.
                               
       CREATE notes.
       ASSIGN notes.rec_key = est.rec_key
           notes.note_code = est-inst.dept
           notes.note_form_no = est-inst.line-no
           notes.note_title = trim(est-inst.inst[1])
           notes.note_text = trim(est-inst.inst[1]) + CHR(10) +
                             trim(est-inst.inst[2]) + CHR(10) +
                             trim(est-inst.inst[3])
           notes.note_date = TODAY .

    
       DISP est-inst.est-no .
       PAUSE 0.
    END.
END.

