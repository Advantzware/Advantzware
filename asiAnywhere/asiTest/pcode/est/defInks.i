    
  EMPTY TEMP-TABLE inks.

   IF DEC(prmColor)  GT 0 OR
      DEC(prmCoat) GT 0 THEN DO:

     ASSIGN
      lv-i-ps[01]   = DEC(prmPs1)
      lv-i-code[01] = prmCode1
      lv-i-dscr[01] = prmDscr1
      lv-i-%[01]    = DEC(prmPer1)
      lv-i-ps[02]   = DEC(prmPs2)
      lv-i-code[02] = prmCode2
      lv-i-dscr[02] = prmDscr2
      lv-i-%[02]    = DEC(prmPer2)
      lv-i-ps[03]   = DEC(prmPs3)
      lv-i-code[03] = prmCode3
      lv-i-dscr[03] = prmDscr3
      lv-i-%[03]    = DEC(prmPer3)
      lv-i-ps[04]   = DEC(prmPs4)
      lv-i-code[04] = prmCode4
      lv-i-dscr[04] = prmDscr4
      lv-i-%[04]    = DEC(prmPer4)
      lv-i-ps[05]   = DEC(prmPs5)
      lv-i-code[05] = prmCode5
      lv-i-dscr[05] = prmDscr5
      lv-i-%[05]    = DEC(prmPer5)
      lv-i-ps[06]   = DEC(prmPs6)
      lv-i-code[06] = prmCode6
      lv-i-dscr[06] = prmDscr6
      lv-i-%[06]    = DEC(prmPer6)
      lv-i-ps[07]   = DEC(prmPs7)
      lv-i-code[07] = prmCode7
      lv-i-dscr[07] = prmDscr7
      lv-i-%[07]    = DEC(prmPer7)
      lv-i-ps[08]   = DEC(prmPs8)
      lv-i-code[08] = prmCode8
      lv-i-dscr[08] = prmDscr8
      lv-i-%[08]    = DEC(prmPer8)
      lv-i-ps[09]   = DEC(prmPs9)
      lv-i-code[09] = prmCode9
      lv-i-dscr[09] = prmDscr9
      lv-i-%[09]    = DEC(prmPer9)
      lv-i-ps[10]   = DEC(prmPs10)
      lv-i-code[10] = prmCode10
      lv-i-dscr[10] = prmDscr10
      lv-i-%[10]    = DEC(prmPer10).     

      DO li = 1 TO EXTENT(lv-i-code):
       CREATE inks.
       ASSIGN
        inks.ps[1] = lv-i-ps[li]
        inks.cd[1] = lv-i-code[li]
        inks.ds[1] = lv-i-dscr[li]
        inks.pc[1] = lv-i-%[li].        
     END.

     RUN est/def-inks.p (ROWID(eb),
                         DEC(prmColor),
                         DEC(prmPass),
                         DEC(prmCoat),
                         DEC(prmCoatPass)).     

     ASSIGN
      li        = 0
      lv-i-ps   = 0
      lv-i-code = ""
      lv-i-dscr = ""
      lv-i-%    = 0 .

     FOR EACH inks BY inks.iv:
       li = li + 1.
       IF li LE EXTENT(lv-i-code) THEN
         ASSIGN
          lv-i-ps[li]   = inks.ps[1]
          lv-i-code[li] = inks.cd[1]
          lv-i-dscr[li] = inks.ds[1]
          lv-i-%[li]    = inks.pc[1].
     END.  


     ASSIGN
        ttCorrInks.vPs1           = (lv-i-ps[01])
        ttCorrInks.vCode1         = lv-i-code[01]      
        ttCorrInks.vDscr1         = lv-i-dscr[01]      
        ttCorrInks.vPer1          = (lv-i-%[01]) 
        ttCorrInks.vPs2           = (lv-i-ps[02])
        ttCorrInks.vCode2         = lv-i-code[02]      
        ttCorrInks.vDscr2         = lv-i-dscr[02]      
        ttCorrInks.vPer2          = (lv-i-%[02]) 
        ttCorrInks.vPs3           = (lv-i-ps[03])
        ttCorrInks.vCode3         = lv-i-code[03]      
        ttCorrInks.vDscr3         = lv-i-dscr[03]      
        ttCorrInks.vPer3          = (lv-i-%[03]) 
        ttCorrInks.vPs4           = (lv-i-ps[04])
        ttCorrInks.vCode4         = lv-i-code[04]      
        ttCorrInks.vDscr4         = lv-i-dscr[04]      
        ttCorrInks.vPer4          = (lv-i-%[04]) 
        ttCorrInks.vPs5           = (lv-i-ps[05])
        ttCorrInks.vCode5         = lv-i-code[05]      
        ttCorrInks.vDscr5         = lv-i-dscr[05]      
        ttCorrInks.vPer5          = (lv-i-%[05]) 
        ttCorrInks.vPs6           = (lv-i-ps[06])
        ttCorrInks.vCode6         = lv-i-code[06]      
        ttCorrInks.vDscr6         = lv-i-dscr[06]      
        ttCorrInks.vPer6          = (lv-i-%[06]) 
        ttCorrInks.vPs7           = (lv-i-ps[07])
        ttCorrInks.vCode7         = lv-i-code[07]      
        ttCorrInks.vDscr7         = lv-i-dscr[07]      
        ttCorrInks.vPer7          = (lv-i-%[07]) 
        ttCorrInks.vPs8           = (lv-i-ps[08])
        ttCorrInks.vCode8         = lv-i-code[08]      
        ttCorrInks.vDscr8         = lv-i-dscr[08]      
        ttCorrInks.vPer8          = (lv-i-%[08]) 
        ttCorrInks.vPs9           = (lv-i-ps[09])
        ttCorrInks.vCode9         = lv-i-code[09]      
        ttCorrInks.vDscr9         = lv-i-dscr[09]      
        ttCorrInks.vPer9          = (lv-i-%[09]) 
        ttCorrInks.vPs10          = (lv-i-ps[10])
        ttCorrInks.vCode10        = lv-i-code[10]      
        ttCorrInks.vDscr10        = lv-i-dscr[10]      
        ttCorrInks.vPer10         = (lv-i-%[10]).

   END.
