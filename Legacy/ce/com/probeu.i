  
  IF probe.LINE LT 100 THEN
  DO:
     RUN get-dir-proc(INPUT trim(est.est-no) + ".v"
                            + string(probe.line,"99"),
                      OUTPUT tmp-dir).
    
     os-copy value(tmp-dir + trim(est.est-no) + ".v" + string(probe.line,"99"))
             value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,"99")). 
             
     output to value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,"99")) append.
  END.
  ELSE
  DO:
     RUN get-dir-proc(INPUT trim(est.est-no) + ".v"
                            + string(probe.line,"999"),
                      OUTPUT tmp-dir).
    
     os-copy value(tmp-dir + trim(est.est-no) + ".v" + string(probe.line,"999"))
             value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,"999")). 
             
     output to value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,"999")) append.
  END.
  
  {ce/probepr2.i 48}
        
  output close.

  IF probe.LINE LT 100 THEN
  DO:
     RUN get-dir-proc(INPUT trim(est.est-no) + ".a"
                            + string(probe.line,"99"),
                      OUTPUT tmp-dir).
    
     os-append
          value(tmp-dir + trim(est.est-no) + ".a" + string(probe.line,"99"))
          value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,"99")). 
  END.
  ELSE
  DO:
     RUN get-dir-proc(INPUT trim(est.est-no) + ".a"
                            + string(probe.line,"999"),
                      OUTPUT tmp-dir).
    
     os-append
          value(tmp-dir + trim(est.est-no) + ".a" + string(probe.line,"999"))
          value(lv-cebrowse-dir + trim(est.est-no) + ".s" + string(probe.line,"999")). 
  END.
