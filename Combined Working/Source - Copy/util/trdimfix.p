{custom/patchBegin.i 1 20050426 1 "util/TrDimFix.r"}
FOR EACH eb WHERE form-no EQ 0 AND len EQ 0 AND wid EQ 0 AND dep EQ 0:
  ASSIGN
   len    = tr-len
   wid    = tr-wid
   dep    = tr-dep
   tr-len = 0
   tr-wid = 0
   tr-dep = 0.
END.
{custom/patchEnd.i}
