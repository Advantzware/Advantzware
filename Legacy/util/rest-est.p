/* rest-est.p Restore Estimate from backup */

disable triggers for load of asi.est.
disable triggers for load of asi.eb.
disable triggers for load of asi.ef.

for each res.est where int(res.est.est-no) >= 1093 and
           int(res.est.est-no) <= 1103:
    create asi.est.
    buffer-copy res.est to asi.est.



FOR EACH res.est-qty WHERE res.est-qty.company = res.est.company
                       AND res.est-qty.est-no = res.est.est-no:
    create asi.est-qty.
    buffer-copy res.est-qty to asi.est-qty.

END.

FOR EACH res.ef WHERE res.ef.company = res.est.company
                  AND res.ef.est-no = res.est.est-no:
    create asi.ef.
    buffer-copy res.ef to asi.ef.
End.

FOR EACH res.eb WHERE res.eb.company = res.est.company
                 AND res.eb.est-no = res.est.est-no:
    create asi.eb.
    buffer-copy res.eb to asi.eb.
END.

FOR EACH res.est-op WHERE res.est-op.company = res.est.company
                      AND res.est-op.est-no = res.est.est-no :
    create asi.est-op.
    buffer-copy res.est-op to asi.est-op.
END.

FOR EACH res.est-prep WHERE res.est-prep.company = res.est.company
                     AND res.est-prep.est-no = res.est.est-no:
    create asi.est-prep.
    buffer-copy res.est-prep to asi.est-prep.
END.

FOR EACH res.est-inst WHERE res.est-inst.company = res.est.company
                     AND res.est-inst.est-no = res.est.est-no:
    create asi.est-inst.
    buffer-copy res.est-inst to asi.est-inst.
END.

FOR EACH res.est-flm WHERE res.est-flm.company = res.est.company
                     AND res.est-flm.est-no = res.est.est-no:
    create asi.est-flm.
    buffer-copy res.est-flm to asi.est-flm.
END.

FOR EACH res.box-design-hdr WHERE res.box-design-hdr.company EQ res.est.company
                           AND res.box-design-hdr.est-no  EQ res.est.est-no:
    create asi.box-design-hdr.
    buffer-copy res.box-design-hdr to asi.box-design-hdr.

END. 

FOR EACH res2.notes where res2.notes.rec_key   eq res.est.rec_key :
    create nosweat.notes.
    buffer-copy res2.notes to nosweat.notes.
END.

FOR EACH res.reftable
    WHERE res.reftable.reftable EQ "est/getqty.w"
      AND res.reftable.company  EQ res.est.company
      AND res.reftable.loc      EQ res.est.loc
      AND res.reftable.code     EQ res.est.est-no:
    create asi.reftable.
    buffer-copy res.reftable to asi.reftable.
END.

FOR EACH res.probe
    WHERE res.probe.company EQ res.est.company
      AND res.probe.est-no  EQ res.est.est-no:
    create asi.probe.
    buffer-copy res.probe to asi.probe.
END.



end.
