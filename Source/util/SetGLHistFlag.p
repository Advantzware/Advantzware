/*------------------------------------------------------------------------
    File        : util/SetGLHistFlag.p
    Purpose     : populate year field in GLHist and fix posted field in old data

    Syntax      : 

    Description :  

    Author(s)   : Sewa Singh
    Created     : 03.01.2021
    Notes       :
  ----------------------------------------------------------------------*/
  DEFINE BUFFER bf-glhist FOR glhist.
  DEFINE BUFFER bf-period FOR period.
  FOR EACH bf-glhist :
       
    FIND FIRST bf-period NO-LOCK
         where bf-period.company eq bf-glhist.company
         and bf-period.pst  le bf-glhist.tr-date
         and bf-period.pend ge bf-glhist.tr-date          
        NO-ERROR.             
      
     bf-glhist.glYear = IF AVAIL bf-period THEN bf-period.yr ELSE bf-glhist.glYear.
     IF bf-glhist.entryType EQ "" OR (AVAIL bf-period AND NOT bf-period.pstat) OR (NOT AVAIL bf-period) THEN
     bf-glhist.posted = YES.      
     
  END.
  RELEASE bf-glhist.
