/*------------------------------------------------------------------------
    File        : util/ResetProbeMsf.p
    Purpose     : reset probe.gshQtyInSF

    Syntax      : 

    Description : 

    Author(s)   : Sewa Singh
    Created     : 03.27.2021
    Notes       :
  ----------------------------------------------------------------------*/
  
  DEFINE BUFFER bf-probe FOR probe.
  
  DISABLE TRIGGERS FOR LOAD OF probe.
  
  FOR EACH eb no-lock
      WHERE eb.est-type GE 5,
      EACH bf-probe EXCLUSIVE-LOCK
      WHERE bf-probe.est-no = eb.est-no 
      AND bf-probe.probe-date ne ?
      and bf-probe.gshQtyInSF eq 0:
      
      bf-probe.gshQtyInSF = bf-probe.tot-lbs.        
  END.
  RELEASE bf-probe.
