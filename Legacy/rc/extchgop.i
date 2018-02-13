    (if ws_chgop = ? THEN TRUE ELSE {1}.ChgOp begins ws_chgop)
AND (if lo_chgdate = ? THEN TRUE ELSE {1}.ChgDate >= lo_chgdate)
AND (if hi_chgdate = ? THEN TRUE ELSE {1}.ChgDate <= hi_chgdate)

