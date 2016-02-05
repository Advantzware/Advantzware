/* zipcode.i */

NOT CAN-FIND(FIRST address
    WHERE address.zipcode = zipcode.zipcode
      AND address.pref_type = zipcode.pref_type
      AND address.pref# = zipcode.pref#)