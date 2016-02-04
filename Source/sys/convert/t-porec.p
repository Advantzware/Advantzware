/* t-porec.p  from util/reckey.i */
def var ls-key as cha form "x(20)" no-undo.

for each po-ord where po-ord.rec_key = "".


ls-key = string(today,"99999999") +
               string(next-value(rec_key_seq,nosweat),"99999999").

po-ord.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = po-ord.rec_key
      rec_key.table_name = "PO-ORD".


DISP po-ord.po-no po-ord.rec_key.
PAUSE 0.

     /* notes conversion */
     IF po-ord.spec[1] <> "" OR po-ord.spec-i[2] <> "" OR
        po-ord.spec[3] <> "" OR po-ord.spec-i[4] <> "" 
     THEN DO:
        CREATE notes.
        ASSIGN notes.rec_key = po-ord.rec_key
               /*notes.note_form_no = est-inst.line-no
              notes.note_type = "S"                   */
              notes.note_title = TRIM(po-ord.spec-i[1])
              notes.note_text = trim(po-ord.spec-i[1]) + CHR(10) +
                                trim(po-ord.spec-i[2]) + CHR(10) +
                                trim(po-ord.spec-i[3]) + CHR(10) +
                                TRIM(po-ord.spec-i[4])
              notes.note_date = TODAY .
     END.
END.

for each po-ordl where po-ordl.rec_key = "".


ls-key = string(today,"99999999") +
               string(next-value(rec_key_seq,nosweat),"99999999").

po-ordl.rec_key = ls-key.               
create rec_key.
assign rec_key.rec_key = po-ordl.rec_key
      rec_key.table_name = "PO-ORDL".

DISP po-ordl.po-no po-ordl.i-no po-ordl.rec_key.
PAUSE 0.
     /* notes conversion */
     IF po-ordl.spec[1] <> "" OR po-ordl.spec-i[2] <> "" OR
        po-ordl.spec[3] <> "" OR po-ordl.spec-i[4] <> "" 
     THEN DO:
        CREATE notes.
        ASSIGN notes.rec_key = po-ordl.rec_key
               /*notes.note_form_no = est-inst.line-no
              notes.note_type = "S"                   */
              notes.note_title = TRIM(po-ordl.spec-i[1])
              notes.note_text = trim(po-ordl.spec-i[1]) + CHR(10) +
                                trim(po-ordl.spec-i[2]) + CHR(10) +
                                trim(po-ordl.spec-i[3]) + CHR(10) +
                                TRIM(po-ordl.spec-i[4])
              notes.note_date = TODAY .
     END.

END.
