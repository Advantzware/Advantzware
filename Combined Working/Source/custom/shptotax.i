   FIND FIRST Shipto NO-LOCK
      Where Shipto.company eq cocode
        and shipto.cust-no eq {1}
        and shipto.ship-id eq {2} NO-ERROR .
    IF AVAIL shipto Then
            {3} = shipto.tax-mandatory .
     else  {3} = NO .
     