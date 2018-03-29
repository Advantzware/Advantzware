
  IF probe.LINE LT 100 THEN
  DO:
    os-copy value(tmp-dir + trim(est.est-no) + ".v" + string(probe.line,"99"))
            value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,"99")). 
            
    output to value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,"99")) append.
  END.
  ELSE
  DO:
    os-copy value(tmp-dir + trim(est.est-no) + ".v" + string(probe.line,"999"))
            value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,"999")). 
            
    output to value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,"999")) append.
  END.
  
  {cec/probepr2.i 48}
        
  output close.

  IF probe.LINE LT 100 THEN
  DO:
    os-append
         value(tmp-dir + trim(est.est-no) + ".a" + string(probe.line,"99"))
         value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,"99")). 
  END.
  ELSE
  DO:
    os-append
         value(tmp-dir + trim(est.est-no) + ".a" + string(probe.line,"999"))
         value(tmp-dir + trim(est.est-no) + ".s" + string(probe.line,"999")). 
  END.
