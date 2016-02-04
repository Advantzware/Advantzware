
{est/blankcp1.i}

IF v-fg-copy AND adm-adding-record AND AVAIL b-eb AND lv-copied NE ? THEN DO:
  IF est.est-type EQ 4 THEN DO:
    IF DEC(ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-col:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(b-eb.i-col).
    IF DEC(ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-pass:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-eb.i-pass).
    IF DEC(ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-coat:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-eb.i-coat).
    IF DEC(ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
      ef.f-coat-p:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-eb.i-coat-p).
  END.

  ASSIGN
   eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(b-eb.num-wid)
   eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(b-eb.num-len)
   eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(b-eb.num-up)
   eb.die-in:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(b-eb.die-in).
END.
