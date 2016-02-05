
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-i-ps LIKE eb.i-ps2 NO-UNDO.
DEF VAR lv-i-code LIKE eb.i-code2 NO-UNDO.
DEF VAR lv-i-dscr LIKE eb.i-dscr2 NO-UNDO.
DEF VAR lv-i-% LIKE eb.i-%2 NO-UNDO.
DEF VAR lv-side AS CHAR EXTENT 30 NO-UNDO.

     EMPTY TEMP-TABLE inks.

     ASSIGN
      lv-i-ps[01]   = DEC(eb.i-ps2[01]:SCREEN-VALUE)
      lv-i-code[01] = eb.i-code2[01]:SCREEN-VALUE
      lv-i-dscr[01] = eb.i-dscr2[01]:SCREEN-VALUE
      lv-i-%[01]    = DEC(eb.i-%2[01]:SCREEN-VALUE)
      lv-side[01]   = fi_side-1:SCREEN-VALUE
      lv-i-ps[02]   = DEC(eb.i-ps2[02]:SCREEN-VALUE)
      lv-i-code[02] = eb.i-code2[02]:SCREEN-VALUE
      lv-i-dscr[02] = eb.i-dscr2[02]:SCREEN-VALUE
      lv-i-%[02]    = DEC(eb.i-%2[02]:SCREEN-VALUE)
      lv-side[02]   = fi_side-2:SCREEN-VALUE
      lv-i-ps[03]   = DEC(eb.i-ps2[03]:SCREEN-VALUE)
      lv-i-code[03] = eb.i-code2[03]:SCREEN-VALUE
      lv-i-dscr[03] = eb.i-dscr2[03]:SCREEN-VALUE
      lv-i-%[03]    = DEC(eb.i-%2[03]:SCREEN-VALUE)
      lv-side[03]   = fi_side-3:SCREEN-VALUE
      lv-i-ps[04]   = DEC(eb.i-ps2[04]:SCREEN-VALUE)
      lv-i-code[04] = eb.i-code2[04]:SCREEN-VALUE
      lv-i-dscr[04] = eb.i-dscr2[04]:SCREEN-VALUE
      lv-i-%[04]    = DEC(eb.i-%2[04]:SCREEN-VALUE)
      lv-side[04]   = fi_side-4:SCREEN-VALUE
      lv-i-ps[05]   = DEC(eb.i-ps2[05]:SCREEN-VALUE)
      lv-i-code[05] = eb.i-code2[05]:SCREEN-VALUE
      lv-i-dscr[05] = eb.i-dscr2[05]:SCREEN-VALUE
      lv-i-%[05]    = DEC(eb.i-%2[05]:SCREEN-VALUE)
      lv-side[05]   = fi_side-5:SCREEN-VALUE
      lv-i-ps[06]   = DEC(eb.i-ps2[06]:SCREEN-VALUE)
      lv-i-code[06] = eb.i-code2[06]:SCREEN-VALUE
      lv-i-dscr[06] = eb.i-dscr2[06]:SCREEN-VALUE
      lv-i-%[06]    = DEC(eb.i-%2[06]:SCREEN-VALUE)
      lv-side[06]   = fi_side-6:SCREEN-VALUE
      lv-i-ps[07]   = DEC(eb.i-ps2[07]:SCREEN-VALUE)
      lv-i-code[07] = eb.i-code2[07]:SCREEN-VALUE
      lv-i-dscr[07] = eb.i-dscr2[07]:SCREEN-VALUE
      lv-i-%[07]    = DEC(eb.i-%2[07]:SCREEN-VALUE)
      lv-side[07]   = fi_side-7:SCREEN-VALUE
      lv-i-ps[08]   = DEC(eb.i-ps2[08]:SCREEN-VALUE)
      lv-i-code[08] = eb.i-code2[08]:SCREEN-VALUE
      lv-i-dscr[08] = eb.i-dscr2[08]:SCREEN-VALUE
      lv-i-%[08]    = DEC(eb.i-%2[08]:SCREEN-VALUE)
      lv-side[08]   = fi_side-8:SCREEN-VALUE
      lv-i-ps[09]   = DEC(eb.i-ps2[09]:SCREEN-VALUE)
      lv-i-code[09] = eb.i-code2[09]:SCREEN-VALUE
      lv-i-dscr[09] = eb.i-dscr2[09]:SCREEN-VALUE
      lv-i-%[09]    = DEC(eb.i-%2[09]:SCREEN-VALUE)
      lv-side[09]   = fi_side-9:SCREEN-VALUE
      lv-i-ps[10]   = DEC(eb.i-ps2[10]:SCREEN-VALUE)
      lv-i-code[10] = eb.i-code2[10]:SCREEN-VALUE
      lv-i-dscr[10] = eb.i-dscr2[10]:SCREEN-VALUE
      lv-i-%[10]    = DEC(eb.i-%2[10]:SCREEN-VALUE)
      lv-side[10]   = fi_side-10:SCREEN-VALUE.

     ASSIGN
      lv-i-ps[11]   = DEC(eb.i-ps2[11]:SCREEN-VALUE)
      lv-i-code[11] = eb.i-code2[11]:SCREEN-VALUE
      lv-i-dscr[11] = eb.i-dscr2[11]:SCREEN-VALUE
      lv-i-%[11]    = DEC(eb.i-%2[11]:SCREEN-VALUE)
      lv-side[11]   = fi_side-11:SCREEN-VALUE
      lv-i-ps[12]   = DEC(eb.i-ps2[12]:SCREEN-VALUE)
      lv-i-code[12] = eb.i-code2[12]:SCREEN-VALUE
      lv-i-dscr[12] = eb.i-dscr2[12]:SCREEN-VALUE
      lv-i-%[12]    = DEC(eb.i-%2[12]:SCREEN-VALUE)
      lv-side[12]   = fi_side-12:SCREEN-VALUE
      lv-i-ps[13]   = DEC(eb.i-ps2[13]:SCREEN-VALUE)
      lv-i-code[13] = eb.i-code2[13]:SCREEN-VALUE
      lv-i-dscr[13] = eb.i-dscr2[13]:SCREEN-VALUE
      lv-i-%[13]    = DEC(eb.i-%2[13]:SCREEN-VALUE)
      lv-side[13]   = fi_side-13:SCREEN-VALUE
      lv-i-ps[14]   = DEC(eb.i-ps2[14]:SCREEN-VALUE)
      lv-i-code[14] = eb.i-code2[14]:SCREEN-VALUE
      lv-i-dscr[14] = eb.i-dscr2[14]:SCREEN-VALUE
      lv-i-%[14]    = DEC(eb.i-%2[14]:SCREEN-VALUE)
      lv-side[14]   = fi_side-14:SCREEN-VALUE
      lv-i-ps[15]   = DEC(eb.i-ps2[15]:SCREEN-VALUE)
      lv-i-code[15] = eb.i-code2[15]:SCREEN-VALUE
      lv-i-dscr[15] = eb.i-dscr2[15]:SCREEN-VALUE
      lv-i-%[15]    = DEC(eb.i-%2[15]:SCREEN-VALUE)
      lv-side[15]   = fi_side-15:SCREEN-VALUE
      lv-i-ps[16]   = DEC(eb.i-ps2[16]:SCREEN-VALUE)
      lv-i-code[16] = eb.i-code2[16]:SCREEN-VALUE
      lv-i-dscr[16] = eb.i-dscr2[16]:SCREEN-VALUE
      lv-i-%[16]    = DEC(eb.i-%2[16]:SCREEN-VALUE)
      lv-side[16]   = fi_side-16:SCREEN-VALUE
      lv-i-ps[17]   = DEC(eb.i-ps2[17]:SCREEN-VALUE)
      lv-i-code[17] = eb.i-code2[17]:SCREEN-VALUE
      lv-i-dscr[17] = eb.i-dscr2[17]:SCREEN-VALUE
      lv-i-%[17]    = DEC(eb.i-%2[17]:SCREEN-VALUE)
      lv-side[17]   = fi_side-17:SCREEN-VALUE
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
      lv-i-%[20]    = DEC(eb.i-%2[20]:SCREEN-VALUE)*/.

     DO li = 1 TO EXTENT(lv-i-code):
       CREATE inks.
       ASSIGN
        inks.ps[1] = lv-i-ps[li]
        inks.cd[1] = lv-i-code[li]
        inks.ds[1] = lv-i-dscr[li]
        inks.pc[1] = lv-i-%[li]
        inks.side[1] = lv-side[li].
     END.

     RUN est/def-inks.p (ROWID(eb),
                         DEC(eb.i-col:SCREEN-VALUE),
                         DEC(eb.i-pass:SCREEN-VALUE),
                         DEC(eb.i-coat:SCREEN-VALUE),
                         DEC(eb.i-coat-p:SCREEN-VALUE)).

     ASSIGN
      li        = 0
      lv-i-ps   = 0
      lv-i-code = ""
      lv-i-dscr = ""
      lv-i-%    = 0
      lv-side   = "".

     FOR EACH inks BY inks.iv:
       li = li + 1.
       IF li LE EXTENT(lv-i-code) THEN
         ASSIGN
          lv-i-ps[li]   = inks.ps[1]
          lv-i-code[li] = inks.cd[1]
          lv-i-dscr[li] = inks.ds[1]
          lv-i-%[li]    = inks.pc[1]
          lv-side[li]   = inks.side[1].
     END.

     ASSIGN
      eb.i-ps2[01]:SCREEN-VALUE   = STRING(lv-i-ps[1])
      eb.i-code2[01]:SCREEN-VALUE = lv-i-code[1]
      eb.i-dscr2[01]:SCREEN-VALUE = lv-i-dscr[1]
      eb.i-%2[01]:SCREEN-VALUE    = STRING(lv-i-%[1])
      fi_side-1:SCREEN-VALUE      = lv-side[1]
      eb.i-ps2[02]:SCREEN-VALUE   = STRING(lv-i-ps[02])
      eb.i-code2[02]:SCREEN-VALUE = lv-i-code[02]
      eb.i-dscr2[02]:SCREEN-VALUE = lv-i-dscr[02]
      eb.i-%2[02]:SCREEN-VALUE    = STRING(lv-i-%[02])
      fi_side-2:SCREEN-VALUE      = lv-side[2]
      eb.i-ps2[03]:SCREEN-VALUE   = STRING(lv-i-ps[03])
      eb.i-code2[03]:SCREEN-VALUE = lv-i-code[03]
      eb.i-dscr2[03]:SCREEN-VALUE = lv-i-dscr[03]
      eb.i-%2[03]:SCREEN-VALUE    = STRING(lv-i-%[03])
      fi_side-3:SCREEN-VALUE      = lv-side[3]
      eb.i-ps2[04]:SCREEN-VALUE   = STRING(lv-i-ps[04])
      eb.i-code2[04]:SCREEN-VALUE = lv-i-code[04]
      eb.i-dscr2[04]:SCREEN-VALUE = lv-i-dscr[04]
      eb.i-%2[04]:SCREEN-VALUE    = STRING(lv-i-%[04])
      fi_side-4:SCREEN-VALUE      = lv-side[4]
      eb.i-ps2[05]:SCREEN-VALUE   = STRING(lv-i-ps[05])
      eb.i-code2[05]:SCREEN-VALUE = lv-i-code[05]
      eb.i-dscr2[05]:SCREEN-VALUE = lv-i-dscr[05]
      eb.i-%2[05]:SCREEN-VALUE    = STRING(lv-i-%[05])
      fi_side-5:SCREEN-VALUE      = lv-side[5]
      eb.i-ps2[06]:SCREEN-VALUE   = STRING(lv-i-ps[06])
      eb.i-code2[06]:SCREEN-VALUE = lv-i-code[06]
      eb.i-dscr2[06]:SCREEN-VALUE = lv-i-dscr[06]
      eb.i-%2[06]:SCREEN-VALUE    = STRING(lv-i-%[06])
      fi_side-6:SCREEN-VALUE      = lv-side[6]
      eb.i-ps2[07]:SCREEN-VALUE   = STRING(lv-i-ps[07])
      eb.i-code2[07]:SCREEN-VALUE = lv-i-code[07]
      eb.i-dscr2[07]:SCREEN-VALUE = lv-i-dscr[07]
      eb.i-%2[07]:SCREEN-VALUE    = STRING(lv-i-%[07])
      fi_side-7:SCREEN-VALUE      = lv-side[7]
      eb.i-ps2[08]:SCREEN-VALUE   = STRING(lv-i-ps[08])
      eb.i-code2[08]:SCREEN-VALUE = lv-i-code[08]
      eb.i-dscr2[08]:SCREEN-VALUE = lv-i-dscr[08]
      eb.i-%2[08]:SCREEN-VALUE    = STRING(lv-i-%[08])
      fi_side-8:SCREEN-VALUE      = lv-side[8]
      eb.i-ps2[09]:SCREEN-VALUE   = STRING(lv-i-ps[09])
      eb.i-code2[09]:SCREEN-VALUE = lv-i-code[09]
      eb.i-dscr2[09]:SCREEN-VALUE = lv-i-dscr[09]
      eb.i-%2[09]:SCREEN-VALUE    = STRING(lv-i-%[09])
      fi_side-9:SCREEN-VALUE      = lv-side[9]
      eb.i-ps2[10]:SCREEN-VALUE   = STRING(lv-i-ps[10])
      eb.i-code2[10]:SCREEN-VALUE = lv-i-code[10]
      eb.i-dscr2[10]:SCREEN-VALUE = lv-i-dscr[10]
      eb.i-%2[10]:SCREEN-VALUE    = STRING(lv-i-%[10])
      fi_side-10:SCREEN-VALUE      = lv-side[10].

     ASSIGN
      eb.i-ps2[11]:SCREEN-VALUE   = STRING(lv-i-ps[11])
      eb.i-code2[11]:SCREEN-VALUE = lv-i-code[11]
      eb.i-dscr2[11]:SCREEN-VALUE = lv-i-dscr[11]
      eb.i-%2[11]:SCREEN-VALUE    = STRING(lv-i-%[11])
      fi_side-11:SCREEN-VALUE      = lv-side[11]
      eb.i-ps2[12]:SCREEN-VALUE   = STRING(lv-i-ps[12])
      eb.i-code2[12]:SCREEN-VALUE = lv-i-code[12]
      eb.i-dscr2[12]:SCREEN-VALUE = lv-i-dscr[12]
      eb.i-%2[12]:SCREEN-VALUE    = STRING(lv-i-%[12])
      fi_side-12:SCREEN-VALUE      = lv-side[12]
      eb.i-ps2[13]:SCREEN-VALUE   = STRING(lv-i-ps[13])
      eb.i-code2[13]:SCREEN-VALUE = lv-i-code[13]
      eb.i-dscr2[13]:SCREEN-VALUE = lv-i-dscr[13]
      eb.i-%2[13]:SCREEN-VALUE    = STRING(lv-i-%[13])
      fi_side-13:SCREEN-VALUE      = lv-side[13]
      eb.i-ps2[14]:SCREEN-VALUE   = STRING(lv-i-ps[14])
      eb.i-code2[14]:SCREEN-VALUE = lv-i-code[14]
      eb.i-dscr2[14]:SCREEN-VALUE = lv-i-dscr[14]
      eb.i-%2[14]:SCREEN-VALUE    = STRING(lv-i-%[14])
      fi_side-14:SCREEN-VALUE      = lv-side[14]
      eb.i-ps2[15]:SCREEN-VALUE   = STRING(lv-i-ps[15])
      eb.i-code2[15]:SCREEN-VALUE = lv-i-code[15]
      eb.i-dscr2[15]:SCREEN-VALUE = lv-i-dscr[15]
      eb.i-%2[15]:SCREEN-VALUE    = STRING(lv-i-%[15])
      fi_side-15:SCREEN-VALUE      = lv-side[15]
      eb.i-ps2[16]:SCREEN-VALUE   = STRING(lv-i-ps[16])
      eb.i-code2[16]:SCREEN-VALUE = lv-i-code[16]
      eb.i-dscr2[16]:SCREEN-VALUE = lv-i-dscr[16]
      eb.i-%2[16]:SCREEN-VALUE    = STRING(lv-i-%[16])
      fi_side-16:SCREEN-VALUE      = lv-side[16]
      eb.i-ps2[17]:SCREEN-VALUE   = STRING(lv-i-ps[17])
      eb.i-code2[17]:SCREEN-VALUE = lv-i-code[17]
      eb.i-dscr2[17]:SCREEN-VALUE = lv-i-dscr[17]
      eb.i-%2[17]:SCREEN-VALUE    = STRING(lv-i-%[17])
      fi_side-17:SCREEN-VALUE      = lv-side[17]
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
