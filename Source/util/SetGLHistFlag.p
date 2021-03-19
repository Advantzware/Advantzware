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
  
  /* Only process records where the glyear has not been set OR record is not posted 
     Otherwise, this will try to lock and process EVERY glhist */
  FOR EACH bf-glhist EXCLUSIVE WHERE
    bf-glhist.glYear EQ 0 OR 
    bf-glhist.posted EQ NO:
       
    FIND FIRST bf-period NO-LOCK
        WHERE bf-period.company EQ bf-glhist.company
        AND bf-period.pst  LE bf-glhist.tr-date
        AND bf-period.pend GE bf-glhist.tr-date          
        NO-ERROR.             
     IF AVAIL bf-period THEN ASSIGN  
        bf-glhist.glYear = bf-period.yr.
     
     FIND FIRST bf-first-open-period NO-LOCK
        WHERE bf-first-open-period.company EQ bf-glhist.company
        AND bf-first-open-period.pstat   EQ YES
        NO-ERROR.
     IF (AVAIL bf-first-open-period AND bf-glhist.tr-date LT bf-first-open-period.pst) 
     OR NOT AVAIL bf-period THEN ASSIGN
         bf-glhist.posted = YES
         bf-glhist.entryType = "A".      
     
  END.
  RELEASE bf-glhist.
