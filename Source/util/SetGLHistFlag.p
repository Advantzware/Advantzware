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
  DEFINE BUFFER bf-first-open-period FOR period.
  FOR EACH bf-glhist :
       
    FIND FIRST bf-period NO-LOCK
         where bf-period.company eq bf-glhist.company
         and bf-period.pst  le bf-glhist.tr-date
         and bf-period.pend ge bf-glhist.tr-date          
        NO-ERROR.             
      
     bf-glhist.glYear = IF AVAIL bf-period THEN bf-period.yr ELSE bf-glhist.glYear.
     
     FIND FIRST bf-first-open-period NO-LOCK
          WHERE bf-first-open-period.company EQ bf-glhist.company
          AND bf-first-open-period.pstat   EQ YES
          NO-ERROR.
     IF (AVAIL bf-first-open-period AND bf-glhist.tr-date LT bf-first-open-period.pst) OR NOT AVAIL bf-period THEN       
     ASSIGN
         bf-glhist.posted = YES
         bf-glhist.entryType = "A".      
     
  END.
  RELEASE bf-glhist.
