/* rfq/upd-note.p */
DEF INPUT PARAM ip-est-reckey LIKE est.rec_key.
DEF INPUT PARAM ip-rfq-reckey LIKE rfq.rec_key.

DEF BUFFER bf-notes FOR asinos.notes.

FOR EACH asinos.notes WHERE asinos.notes.rec_key = ip-est-reckey:
    DELETE asinos.notes.
END.


FOR EACH nosweat.notes WHERE nosweat.notes.rec_key = ip-rfq-reckey:
    CREATE asinos.bf-notes.
    BUFFER-COPY nosweat.notes EXCEPT nosweat.notes.rec_key TO asinos.bf-notes.
    asinos.bf-notes.rec_key = ip-est-reckey.
END.
