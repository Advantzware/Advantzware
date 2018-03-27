def var v-bol-no like oe-bolh.bol-no.
DEF VAR ll-posted AS LOG NO-UNDO.


message "Enter BOL#:" update v-bol-no.

for each oe-bolh where bol-no eq v-bol-no:
  display bol-no ord-no bol-date oe-bolh.company.

  ll-posted = oe-bolh.posted.

  update oe-bolh.posted.

  for each oe-boll where oe-boll.company eq oe-bolh.company
                     and oe-boll.b-no    eq oe-bolh.b-no:
  
    oe-boll.posted = oe-bolh.posted.
  end.

  IF ll-posted AND NOT oe-bolh.posted THEN
    RUN oe/bolpundo.p (BUFFER oe-bolh, OUTPUT ll-posted).
end.
      
