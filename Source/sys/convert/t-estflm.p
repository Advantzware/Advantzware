    /* t-estflm.p */
    
    DISABLE TRIGGERS FOR LOAD OF est-flm.
    /*
    FOR EACH est-flm.
            DELETE est-flm.
    END.

    */
   
    INPUT FROM d:\asigui\v8data\dumpdata\est-flm.d NO-ECHO.

    REPEAT :
        INSERT est-flm.
        FIND FIRST est WHERE est.e-num = est-flm.e-num NO-LOCK NO-ERROR.
        IF AVAIL est THEN
           ASSIGN est-flm.company = est.company
                  est-flm.est-no = est.est-no.

        DISP est-flm.est-no est-flm.e-num .
        PAUSE 0.

    END.



  
  FOR EACH est-flm:
      FIND FIRST ef WHERE ef.company = est-flm.company 
                    AND ef.est-no = est-flm.est-no NO-LOCK NO-ERROR.
      ASSIGN est-flm.eqty = ef.eqty.

  END.
