
     EMPTY TEMP-TABLE inks.

     ASSIGN
      lv-i-ps[01]   = DEC(prmPs1)
      lv-i-code[01] = prmCode1
      lv-i-dscr[01] = prmDscr1
      lv-i-%[01]    = DEC(prmPer1)
      lv-side[01]   = prmSide1
      lv-i-ps[02]   = DEC(prmPs2)
      lv-i-code[02] = prmCode2
      lv-i-dscr[02] = prmDscr2
      lv-i-%[02]    = DEC(prmPer2)
      lv-side[02]   = prmSide2
      lv-i-ps[03]   = DEC(prmPs3)
      lv-i-code[03] = prmCode3
      lv-i-dscr[03] = prmDscr3
      lv-i-%[03]    = DEC(prmPer3)
      lv-side[03]   = prmSide3
      lv-i-ps[04]   = DEC(prmPs4)
      lv-i-code[04] = prmCode4
      lv-i-dscr[04] = prmDscr4
      lv-i-%[04]    = DEC(prmPer4)
      lv-side[04]   = prmSide4
      lv-i-ps[05]   = DEC(prmPs5)
      lv-i-code[05] = prmCode5
      lv-i-dscr[05] = prmDscr5
      lv-i-%[05]    = DEC(prmPer5)
      lv-side[05]   = prmSide5
      lv-i-ps[06]   = DEC(prmPs6)
      lv-i-code[06] = prmCode6
      lv-i-dscr[06] = prmDscr6
      lv-i-%[06]    = DEC(prmPer6)
      lv-side[06]   = prmSide6
      lv-i-ps[07]   = DEC(prmPs7)
      lv-i-code[07] = prmCode7
      lv-i-dscr[07] = prmDscr7
      lv-i-%[07]    = DEC(prmPer7)
      lv-side[07]   = prmSide7
      lv-i-ps[08]   = DEC(prmPs8)
      lv-i-code[08] = prmCode8
      lv-i-dscr[08] = prmDscr8
      lv-i-%[08]    = DEC(prmPer8)
      lv-side[08]   = prmSide8
      lv-i-ps[09]   = DEC(prmPs9)
      lv-i-code[09] = prmCode9
      lv-i-dscr[09] = prmDscr9
      lv-i-%[09]    = DEC(prmPer9)
      lv-side[09]   = prmSide9
      lv-i-ps[10]   = DEC(prmPs10)
      lv-i-code[10] = prmCode10
      lv-i-dscr[10] = prmDscr10
      lv-i-%[10]    = DEC(prmPer10)
      lv-side[10]   = prmSide10 .

     ASSIGN
      lv-i-ps[11]   = DEC(prmPs11)
      lv-i-code[11] = prmCode11
      lv-i-dscr[11] = prmDscr11
      lv-i-%[11]    = DEC(prmPer11)
      lv-side[11]   = prmSide11
      lv-i-ps[12]   = DEC(prmPs12)
      lv-i-code[12] = prmCode12
      lv-i-dscr[12] = prmDscr12
      lv-i-%[12]    = DEC(prmPer12)
      lv-side[12]   = prmSide12
      lv-i-ps[13]   = DEC(prmPs13)
      lv-i-code[13] = prmCode13
      lv-i-dscr[13] = prmDscr13
      lv-i-%[13]    = DEC(prmPer13)
      lv-side[13]   = prmSide13
      lv-i-ps[14]   = DEC(prmPs14)
      lv-i-code[14] = prmCode14
      lv-i-dscr[14] = prmDscr14
      lv-i-%[14]    = DEC(prmPer14)
      lv-side[14]   = prmSide14
      lv-i-ps[15]   = DEC(prmPs15)
      lv-i-code[15] = prmCode15
      lv-i-dscr[15] = prmDscr15
      lv-i-%[15]    = DEC(prmPer15)
      lv-side[15]   = prmSide15

      /*lv-i-ps[16]   = DEC(eb.i-ps2[16]:SCREEN-VALUE)
      lv-i-code[16] = eb.i-code2[16]:SCREEN-VALUE
      lv-i-dscr[16] = eb.i-dscr2[16]:SCREEN-VALUE
      lv-i-%[16]    = DEC(eb.i-%2[16]:SCREEN-VALUE)
      lv-side[16]   = fi_side-16:SCREEN-VALUE
      lv-i-ps[17]   = DEC(eb.i-ps2[17]:SCREEN-VALUE)
      lv-i-code[17] = eb.i-code2[17]:SCREEN-VALUE
      lv-i-dscr[17] = eb.i-dscr2[17]:SCREEN-VALUE
      lv-i-%[17]    = DEC(eb.i-%2[17]:SCREEN-VALUE)
      lv-side[17]   = fi_side-17:SCREEN-VALUE
      */
      
      /*lv-i-ps[18]   = DEC(eb.i-ps2[18]:SCREEN-VALUE)
      lv-i-code[18] = eb.i-code2[18]:SCREEN-VALUE
      lv-i-dscr[18] = eb.i-dscr2[18]:SCREEN-VALUE
      lv-i-%[18]    = DEC(eb.i-%2[18]:SCREEN-VALUE)
      lv-i-ps[19]   = DEC(eb.i-ps2[19]:SCREEN-VALUE)
      lv-i-code[19] = eb.i-code2[19]:SCREEN-VALUE
      lv-i-dscr[19] = eb.i-dscr2[19]:SCREEN-VALUE
      lv-i-%[19]    = DEC(eb.i-%2[19]:SCREEN-VALUE)
      lv-i-ps[20]   = DEC(eb.i-ps2[20]:SCREEN-VALUE)
      lv-i-code[20] = eb.i-code2[20]:SCREEN-VALUE
      lv-i-dscr[20] = eb.i-dscr2[20]:SCREEN-VALUE
      lv-i-%[20]    = DEC(eb.i-%2[20]:SCREEN-VALUE)*/ .

     DO li = 1 TO EXTENT(lv-i-code):
       CREATE inks.
       ASSIGN
        inks.ps[1] = lv-i-ps[li]
        inks.cd[1] = lv-i-code[li]
        inks.ds[1] = lv-i-dscr[li]
        inks.pc[1] = lv-i-%[li]
        inks.side[1] = lv-side[li].
     END.
     

     RUN est/fold-def-inks.p (ROWID(eb),
                         DEC(prmColor),
                         DEC(prmPass),
                         DEC(prmCoat),
                         DEC(prmCoatPass)).     

     ASSIGN
      li        = 0
      lv-i-ps   = 0
      lv-i-code = ""
      lv-i-dscr = ""
      lv-i-%    = 0
      lv-side   = "" .

     FOR EACH inks BY inks.iv:
       li = li + 1.
       IF li LE EXTENT(lv-i-code) THEN
         ASSIGN
          lv-i-ps[li]   = inks.ps[1]
          lv-i-code[li] = inks.cd[1]
          lv-i-dscr[li] = inks.ds[1]
          lv-i-%[li]    = inks.pc[1]
          lv-side[li]   = "F".
     END.      

     ASSIGN
      ttFoldInks.vPs1       = (lv-i-ps[1])
      ttFoldInks.vCode1     = lv-i-code[1]
      ttFoldInks.vDscr1     = lv-i-dscr[1]
      ttFoldInks.vPer1      = (lv-i-%[1])
      ttFoldInks.vSide1     = lv-side[1]
      ttFoldInks.vPs2       = (lv-i-ps[02])
      ttFoldInks.vCode2     = lv-i-code[02]
      ttFoldInks.vDscr2     = lv-i-dscr[02]
      ttFoldInks.vPer2      = (lv-i-%[02])
      ttFoldInks.vSide2     = lv-side[2]
      ttFoldInks.vPs3       = (lv-i-ps[03])
      ttFoldInks.vCode3     = lv-i-code[03]
      ttFoldInks.vDscr3     = lv-i-dscr[03]
      ttFoldInks.vPer3      = (lv-i-%[03])
      ttFoldInks.vSide3     = lv-side[3]
      ttFoldInks.vPs4       = (lv-i-ps[04])
      ttFoldInks.vCode4     = lv-i-code[04]
      ttFoldInks.vDscr4     = lv-i-dscr[04]
      ttFoldInks.vPer4      = (lv-i-%[04])
      ttFoldInks.vSide4     = lv-side[4]
      ttFoldInks.vPs5       = (lv-i-ps[05])
      ttFoldInks.vCode5     = lv-i-code[05]
      ttFoldInks.vDscr5     = lv-i-dscr[05]
      ttFoldInks.vPer5      = (lv-i-%[05])
      ttFoldInks.vSide5     = lv-side[5]
      ttFoldInks.vPs6       = (lv-i-ps[06])
      ttFoldInks.vCode6     = lv-i-code[06]
      ttFoldInks.vDscr6     = lv-i-dscr[06]
      ttFoldInks.vPer6      = (lv-i-%[06])
      ttFoldInks.vSide6     = lv-side[6]
      ttFoldInks.vPs7       = (lv-i-ps[07])
      ttFoldInks.vCode7     = lv-i-code[07]
      ttFoldInks.vDscr7     = lv-i-dscr[07]
      ttFoldInks.vPer7      = (lv-i-%[07])
      ttFoldInks.vSide7     = lv-side[7]
      ttFoldInks.vPs8       = (lv-i-ps[08])
      ttFoldInks.vCode8     = lv-i-code[08]
      ttFoldInks.vDscr8     = lv-i-dscr[08]
      ttFoldInks.vPer8      = (lv-i-%[08])
      ttFoldInks.vSide8     = lv-side[8]
      ttFoldInks.vPs9       = (lv-i-ps[09])
      ttFoldInks.vCode9     = lv-i-code[09]
      ttFoldInks.vDscr9     = lv-i-dscr[09]
      ttFoldInks.vPer9      = (lv-i-%[09])
      ttFoldInks.vSide9     = lv-side[9]
      ttFoldInks.vPs10      = (lv-i-ps[10])
      ttFoldInks.vCode10    = lv-i-code[10]
      ttFoldInks.vDscr10    = lv-i-dscr[10]
      ttFoldInks.vPer10     = (lv-i-%[10])
      ttFoldInks.vSide10    = lv-side[10]
      .

     ASSIGN
      ttFoldInks.vPs11     = (lv-i-ps[11])
      ttFoldInks.vCode11   = lv-i-code[11]
      ttFoldInks.vDscr11   = lv-i-dscr[11]
      ttFoldInks.vPer11    = (lv-i-%[11])
      ttFoldInks.vSide11   = lv-side[11]
      ttFoldInks.vPs12     = (lv-i-ps[12])
      ttFoldInks.vCode12   = lv-i-code[12]
      ttFoldInks.vDscr12   = lv-i-dscr[12]
      ttFoldInks.vPer12    = (lv-i-%[12])
      ttFoldInks.vSide12   = lv-side[12]
      ttFoldInks.vPs13     = (lv-i-ps[13])
      ttFoldInks.vCode13   = lv-i-code[13]
      ttFoldInks.vDscr13   = lv-i-dscr[13]
      ttFoldInks.vPer13    = (lv-i-%[13])
      ttFoldInks.vSide13   = lv-side[13]
      ttFoldInks.vPs14     = (lv-i-ps[14])
      ttFoldInks.vCode14   = lv-i-code[14]
      ttFoldInks.vDscr14   = lv-i-dscr[14]
      ttFoldInks.vPer14    = (lv-i-%[14])
      ttFoldInks.vSide14   = lv-side[14]
      ttFoldInks.vPs15     = (lv-i-ps[15])
      ttFoldInks.vCode15   = lv-i-code[15]
      ttFoldInks.vDscr15   = lv-i-dscr[15]
      ttFoldInks.vPer15    = (lv-i-%[15])
      ttFoldInks.vSide15   = lv-side[15]
      
      /*eb.i-ps2[16]:SCREEN-VALUE   = STRING(lv-i-ps[16])
      eb.i-code2[16]:SCREEN-VALUE = lv-i-code[16]
      eb.i-dscr2[16]:SCREEN-VALUE = lv-i-dscr[16]
      eb.i-%2[16]:SCREEN-VALUE    = STRING(lv-i-%[16])
      fi_side-16:SCREEN-VALUE      = lv-side[16]
      eb.i-ps2[17]:SCREEN-VALUE   = STRING(lv-i-ps[17])
      eb.i-code2[17]:SCREEN-VALUE = lv-i-code[17]
      eb.i-dscr2[17]:SCREEN-VALUE = lv-i-dscr[17]
      eb.i-%2[17]:SCREEN-VALUE    = STRING(lv-i-%[17])
      fi_side-17:SCREEN-VALUE      = lv-side[17]
      */
      
      /*eb.i-ps2[18]:SCREEN-VALUE   = STRING(lv-i-ps[18])
      eb.i-code2[18]:SCREEN-VALUE = lv-i-code[18]
      eb.i-dscr2[18]:SCREEN-VALUE = lv-i-dscr[18]
      eb.i-%2[18]:SCREEN-VALUE    = STRING(lv-i-%[18])
      eb.i-ps2[19]:SCREEN-VALUE   = STRING(lv-i-ps[19])
      eb.i-code2[19]:SCREEN-VALUE = lv-i-code[19]
      eb.i-dscr2[19]:SCREEN-VALUE = lv-i-dscr[19]
      eb.i-%2[19]:SCREEN-VALUE    = STRING(lv-i-%[19])
      eb.i-ps2[20]:SCREEN-VALUE   = STRING(lv-i-ps[20])
      eb.i-code2[20]:SCREEN-VALUE = lv-i-code[20]
      eb.i-dscr2[20]:SCREEN-VALUE = lv-i-dscr[20]
      eb.i-%2[20]:SCREEN-VALUE    = STRING(lv-i-%[20])*/.

