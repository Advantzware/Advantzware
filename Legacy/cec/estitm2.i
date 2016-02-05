/* cec/estitm2.i  due to size limit */  
  for each bf-eb where bf-eb.company eq est-qty.company and
                       bf-eb.est-no eq est-qty.est-no and
                       bf-eb.eqty ne est-qty.eqty:
    bf-eb.eqty = est-qty.eqty.
    RUN update-e-itemfg-vend.
  end.
  for each bf-ef where bf-ef.company eq est-qty.company and
                       bf-ef.est-no eq est-qty.est-no and
                       bf-ef.eqty ne est-qty.eqty:
    bf-ef.eqty = est-qty.eqty.
  end.
  for each bf-est where bf-est.company eq est-qty.company and
                        bf-est.est-no eq est-qty.est-no and
                        bf-est.est-qty[1] ne est-qty.eqty:
    bf-est.est-qty[1] = est-qty.eqty.
  end.
