/*------------------------------------------------------------------------
    File        : util/ConversionGLTrans.p
    Purpose     : GLTrans table onversion in GLHist 

    Syntax      : 

    Description : 

    Author(s)   : Sewa Singh
    Created     : 01.07.2021
    Notes       :
  ----------------------------------------------------------------------*/
  
  FOR EACH GLTrans:
       
     create glhist.
     assign
        glhist.company   = gltrans.company
        glhist.actnum    = gltrans.actnum
        glhist.jrnl      = gltrans.jrnl
        glhist.period    = gltrans.period
        glhist.tr-dscr   = gltrans.tr-dscr
        glhist.tr-date   = gltrans.tr-date
        glhist.tr-num    = gltrans.trnum
        glhist.tr-amt    = gltrans.tr-amt
        glhist.entryType = "A"
        glhist.glYear    = YEAR(gltrans.tr-date)
        glhist.createdBy = gltrans.createdBy
        glhist.curr-code = gltrans.curr-code
        glhist.ex-rate   = gltrans.ex-rate.
     
    FIND first period NO-LOCK
         where period.company eq gltrans.company
         and period.pst     le gltrans.tr-date
         and period.pend    ge gltrans.tr-date
         and period.pnum    ne gltrans.period
        NO-ERROR.
    
    IF AVAIL period AND period.pstat eq yes THEN
     glhist.posted = NO.
    ELSE ASSIGN
         glhist.posted = YES
         glhist.postedBy = USERID(LDBNAME(1)) .
        
    DELETE GLTrans.        
       
  END.
