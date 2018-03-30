/* setFilterFlag.i - set filter flag */

&IF '{1}' NE '' &THEN
FOR EACH {&useTtbl} NO-LOCK:
  {&useTtbl}.{1}Flag = {&useTtbl}.dueDate GE fromDueDate
                   AND {&useTtbl}.dueDate LE toDueDate
                   AND {&useTtbl}.udfField01 GE udfValueLo[1]
                   AND {&useTtbl}.udfField01 LE udfValueHi[1]
                   AND {&useTtbl}.udfField02 GE udfValueLo[2]
                   AND {&useTtbl}.udfField02 LE udfValueHi[2]
                   AND {&useTtbl}.udfField03 GE udfValueLo[3]
                   AND {&useTtbl}.udfField03 LE udfValueHi[3]
                   AND {&useTtbl}.udfField04 GE udfValueLo[4]
                   AND {&useTtbl}.udfField04 LE udfValueHi[4]
                   AND {&useTtbl}.udfField05 GE udfValueLo[5]
                   AND {&useTtbl}.udfField05 LE udfValueHi[5]
                   AND {&useTtbl}.udfField06 GE udfValueLo[6]
                   AND {&useTtbl}.udfField06 LE udfValueHi[6]
                   AND {&useTtbl}.udfField07 GE udfValueLo[7]
                   AND {&useTtbl}.udfField07 LE udfValueHi[7]
                   AND {&useTtbl}.udfField08 GE udfValueLo[8]
                   AND {&useTtbl}.udfField08 LE udfValueHi[8]
                   AND {&useTtbl}.udfField09 GE udfValueLo[9]
                   AND {&useTtbl}.udfField09 LE udfValueHi[9]
                   AND {&useTtbl}.udfField10 GE udfValueLo[10]
                   AND {&useTtbl}.udfField10 LE udfValueHi[10]
                   AND {&useTtbl}.udfField11 GE udfValueLo[11]
                   AND {&useTtbl}.udfField11 LE udfValueHi[11]
                   AND {&useTtbl}.udfField12 GE udfValueLo[12]
                   AND {&useTtbl}.udfField12 LE udfValueHi[12]
                   AND {&useTtbl}.udfField13 GE udfValueLo[13]
                   AND {&useTtbl}.udfField13 LE udfValueHi[13]
                   AND {&useTtbl}.udfField14 GE udfValueLo[14]
                   AND {&useTtbl}.udfField14 LE udfValueHi[14]
                   AND {&useTtbl}.udfField15 GE udfValueLo[15]
                   AND {&useTtbl}.udfField15 LE udfValueHi[15]
                   AND {&useTtbl}.udfField16 GE udfValueLo[16]
                   AND {&useTtbl}.udfField16 LE udfValueHi[16]
                   AND {&useTtbl}.udfField17 GE udfValueLo[17]
                   AND {&useTtbl}.udfField17 LE udfValueHi[17]
                   AND {&useTtbl}.udfField18 GE udfValueLo[18]
                   AND {&useTtbl}.udfField18 LE udfValueHi[18]
                   AND {&useTtbl}.udfField19 GE udfValueLo[19]
                   AND {&useTtbl}.udfField19 LE udfValueHi[19]
                   AND {&useTtbl}.udfField20 GE udfValueLo[20]
                   AND {&useTtbl}.udfField20 LE udfValueHi[20]
                   AND {&useTtbl}.userField01 GE userValueLo[1]
                   AND {&useTtbl}.userField01 LE userValueHi[1]
                   AND {&useTtbl}.userField02 GE userValueLo[2]
                   AND {&useTtbl}.userField02 LE userValueHi[2]
                   AND {&useTtbl}.userField03 GE userValueLo[3]
                   AND {&useTtbl}.userField03 LE userValueHi[3]
                   AND {&useTtbl}.userField04 GE userValueLo[4]
                   AND {&useTtbl}.userField04 LE userValueHi[4]
                   AND {&useTtbl}.userField05 GE userValueLo[5]
                   AND {&useTtbl}.userField05 LE userValueHi[5]
                   AND {&useTtbl}.userField06 GE userValueLo[6]
                   AND {&useTtbl}.userField06 LE userValueHi[6]
                   AND {&useTtbl}.userField07 GE userValueLo[7]
                   AND {&useTtbl}.userField07 LE userValueHi[7]
                   AND {&useTtbl}.userField08 GE userValueLo[8]
                   AND {&useTtbl}.userField08 LE userValueHi[8]
                   AND {&useTtbl}.userField09 GE userValueLo[9]
                   AND {&useTtbl}.userField09 LE userValueHi[9]
                   AND {&useTtbl}.userField10 GE userValueLo[10]
                   AND {&useTtbl}.userField10 LE userValueHi[10]
                   AND {&useTtbl}.userField11 GE userValueLo[11]
                   AND {&useTtbl}.userField11 LE userValueHi[11]
                   AND {&useTtbl}.userField12 GE userValueLo[12]
                   AND {&useTtbl}.userField12 LE userValueHi[12]
                   AND {&useTtbl}.userField13 GE userValueLo[13]
                   AND {&useTtbl}.userField13 LE userValueHi[13]
                   AND {&useTtbl}.userField14 GE userValueLo[14]
                   AND {&useTtbl}.userField14 LE userValueHi[14]
                   AND {&useTtbl}.userField15 GE userValueLo[15]
                   AND {&useTtbl}.userField15 LE userValueHi[15]
                   AND {&useTtbl}.userField16 GE userValueLo[16]
                   AND {&useTtbl}.userField16 LE userValueHi[16]
                   AND {&useTtbl}.userField17 GE userValueLo[17]
                   AND {&useTtbl}.userField17 LE userValueHi[17]
                   AND {&useTtbl}.userField18 GE userValueLo[18]
                   AND {&useTtbl}.userField18 LE userValueHi[18]
                   AND {&useTtbl}.userField19 GE userValueLo[19]
                   AND {&useTtbl}.userField19 LE userValueHi[19]
                   AND {&useTtbl}.userField20 GE userValueLo[20]
                   AND {&useTtbl}.userField20 LE userValueHi[20]
                   AND {&useTtbl}.userField21 GE userValueLo[21]
                   AND {&useTtbl}.userField21 LE userValueHi[21]
                   AND {&useTtbl}.userField22 GE userValueLo[22]
                   AND {&useTtbl}.userField22 LE userValueHi[22]
                   AND {&useTtbl}.userField23 GE userValueLo[23]
                   AND {&useTtbl}.userField23 LE userValueHi[23]
                   AND {&useTtbl}.userField24 GE userValueLo[24]
                   AND {&useTtbl}.userField24 LE userValueHi[24]
                   AND {&useTtbl}.userField25 GE userValueLo[25]
                   AND {&useTtbl}.userField25 LE userValueHi[25]
                   AND {&useTtbl}.userField26 GE userValueLo[26]
                   AND {&useTtbl}.userField26 LE userValueHi[26]
                   AND {&useTtbl}.userField27 GE userValueLo[27]
                   AND {&useTtbl}.userField27 LE userValueHi[27]
                   AND {&useTtbl}.userField28 GE userValueLo[28]
                   AND {&useTtbl}.userField28 LE userValueHi[28]
                   AND {&useTtbl}.userField29 GE userValueLo[29]
                   AND {&useTtbl}.userField29 LE userValueHi[29]
                   AND {&useTtbl}.userField30 GE userValueLo[30]
                   AND {&useTtbl}.userField30 LE userValueHi[30]
                   AND {&useTtbl}.userField31 GE userValueLo[31]
                   AND {&useTtbl}.userField31 LE userValueHi[31]
                   AND {&useTtbl}.userField32 GE userValueLo[32]
                   AND {&useTtbl}.userField32 LE userValueHi[32]
                   AND {&useTtbl}.userField33 GE userValueLo[33]
                   AND {&useTtbl}.userField33 LE userValueHi[33]
                   AND {&useTtbl}.userField34 GE userValueLo[34]
                   AND {&useTtbl}.userField34 LE userValueHi[34]
                   AND {&useTtbl}.userField35 GE userValueLo[35]
                   AND {&useTtbl}.userField35 LE userValueHi[35]
                   AND {&useTtbl}.userField36 GE userValueLo[36]
                   AND {&useTtbl}.userField36 LE userValueHi[36]
                   AND {&useTtbl}.userField37 GE userValueLo[37]
                   AND {&useTtbl}.userField37 LE userValueHi[37]
                   AND {&useTtbl}.userField38 GE userValueLo[38]
                   AND {&useTtbl}.userField38 LE userValueHi[38]
                   AND {&useTtbl}.userField39 GE userValueLo[39]
                   AND {&useTtbl}.userField39 LE userValueHi[39]
                   AND {&useTtbl}.userField40 GE userValueLo[40]
                   AND {&useTtbl}.userField40 LE userValueHi[40]
                   AND {&useTtbl}.userField41 GE userValueLo[41]
                   AND {&useTtbl}.userField41 LE userValueHi[41]
                   AND {&useTtbl}.userField42 GE userValueLo[42]
                   AND {&useTtbl}.userField42 LE userValueHi[42]
                   AND {&useTtbl}.userField43 GE userValueLo[43]
                   AND {&useTtbl}.userField43 LE userValueHi[43]
                   AND {&useTtbl}.userField44 GE userValueLo[44]
                   AND {&useTtbl}.userField44 LE userValueHi[44]
                   AND {&useTtbl}.userField45 GE userValueLo[45]
                   AND {&useTtbl}.userField45 LE userValueHi[45]
                   AND {&useTtbl}.userField46 GE userValueLo[46]
                   AND {&useTtbl}.userField46 LE userValueHi[46]
                   AND {&useTtbl}.userField47 GE userValueLo[47]
                   AND {&useTtbl}.userField47 LE userValueHi[47]
                   AND {&useTtbl}.userField48 GE userValueLo[48]
                   AND {&useTtbl}.userField48 LE userValueHi[48]
                   AND {&useTtbl}.userField49 GE userValueLo[49]
                   AND {&useTtbl}.userField49 LE userValueHi[49]
                   AND {&useTtbl}.userField50 GE userValueLo[50]
                   AND {&useTtbl}.userField50 LE userValueHi[50]
                   AND {&useTtbl}.userField51 GE userValueLo[51]
                   AND {&useTtbl}.userField51 LE userValueHi[51]
                   AND {&useTtbl}.userField52 GE userValueLo[52]
                   AND {&useTtbl}.userField52 LE userValueHi[52]
                   AND {&useTtbl}.userField53 GE userValueLo[53]
                   AND {&useTtbl}.userField53 LE userValueHi[53]
                   AND {&useTtbl}.userField54 GE userValueLo[54]
                   AND {&useTtbl}.userField54 LE userValueHi[54]
                   AND {&useTtbl}.userField55 GE userValueLo[55]
                   AND {&useTtbl}.userField55 LE userValueHi[55]
                   AND {&useTtbl}.userField56 GE userValueLo[56]
                   AND {&useTtbl}.userField56 LE userValueHi[56]
                   AND {&useTtbl}.userField57 GE userValueLo[57]
                   AND {&useTtbl}.userField57 LE userValueHi[57]
                   AND {&useTtbl}.userField58 GE userValueLo[58]
                   AND {&useTtbl}.userField58 LE userValueHi[58]
                   AND {&useTtbl}.userField59 GE userValueLo[59]
                   AND {&useTtbl}.userField59 LE userValueHi[59]
                   AND {&useTtbl}.userField60 GE userValueLo[60]
                   AND {&useTtbl}.userField60 LE userValueHi[60]
                   AND {&useTtbl}.userField61 GE userValueLo[61]
                   AND {&useTtbl}.userField61 LE userValueHi[61]
                   AND {&useTtbl}.userField62 GE userValueLo[62]
                   AND {&useTtbl}.userField62 LE userValueHi[62]
                   AND {&useTtbl}.userField63 GE userValueLo[63]
                   AND {&useTtbl}.userField63 LE userValueHi[63]
                   AND {&useTtbl}.userField64 GE userValueLo[64]
                   AND {&useTtbl}.userField64 LE userValueHi[64]
                   AND {&useTtbl}.userField65 GE userValueLo[65]
                   AND {&useTtbl}.userField65 LE userValueHi[65]
                   AND {&useTtbl}.userField66 GE userValueLo[66]
                   AND {&useTtbl}.userField66 LE userValueHi[66]
                   AND {&useTtbl}.userField67 GE userValueLo[67]
                   AND {&useTtbl}.userField67 LE userValueHi[67]
                   AND {&useTtbl}.userField68 GE userValueLo[68]
                   AND {&useTtbl}.userField68 LE userValueHi[68]
                   AND {&useTtbl}.userField69 GE userValueLo[69]
                   AND {&useTtbl}.userField69 LE userValueHi[69]
                   AND {&useTtbl}.userField70 GE userValueLo[70]
                   AND {&useTtbl}.userField70 LE userValueHi[70]
                   AND {&useTtbl}.userField71 GE userValueLo[71]
                   AND {&useTtbl}.userField71 LE userValueHi[71]
                   AND {&useTtbl}.userField72 GE userValueLo[72]
                   AND {&useTtbl}.userField72 LE userValueHi[72]
                   AND {&useTtbl}.userField73 GE userValueLo[73]
                   AND {&useTtbl}.userField73 LE userValueHi[73]
                   AND {&useTtbl}.userField74 GE userValueLo[74]
                   AND {&useTtbl}.userField74 LE userValueHi[74]
                   AND {&useTtbl}.userField75 GE userValueLo[75]
                   AND {&useTtbl}.userField75 LE userValueHi[75]
                   AND {&useTtbl}.userField76 GE userValueLo[76]
                   AND {&useTtbl}.userField76 LE userValueHi[76]
                   AND {&useTtbl}.userField77 GE userValueLo[77]
                   AND {&useTtbl}.userField77 LE userValueHi[77]
                   AND {&useTtbl}.userField78 GE userValueLo[78]
                   AND {&useTtbl}.userField78 LE userValueHi[78]
                   AND {&useTtbl}.userField79 GE userValueLo[79]
                   AND {&useTtbl}.userField79 LE userValueHi[79]
                   AND {&useTtbl}.userField80 GE userValueLo[80]
                   AND {&useTtbl}.userField80 LE userValueHi[80]
                   AND {&useTtbl}.userField81 GE userValueLo[81]
                   AND {&useTtbl}.userField81 LE userValueHi[81]
                   AND {&useTtbl}.userField82 GE userValueLo[82]
                   AND {&useTtbl}.userField82 LE userValueHi[82]
                   AND {&useTtbl}.userField83 GE userValueLo[83]
                   AND {&useTtbl}.userField83 LE userValueHi[83]
                   AND {&useTtbl}.userField84 GE userValueLo[84]
                   AND {&useTtbl}.userField84 LE userValueHi[84]
                   AND {&useTtbl}.userField85 GE userValueLo[85]
                   AND {&useTtbl}.userField85 LE userValueHi[85]
                   AND {&useTtbl}.userField86 GE userValueLo[86]
                   AND {&useTtbl}.userField86 LE userValueHi[86]
                   AND {&useTtbl}.userField87 GE userValueLo[87]
                   AND {&useTtbl}.userField87 LE userValueHi[87]
                   AND {&useTtbl}.userField88 GE userValueLo[88]
                   AND {&useTtbl}.userField88 LE userValueHi[88]
                   AND {&useTtbl}.userField89 GE userValueLo[89]
                   AND {&useTtbl}.userField89 LE userValueHi[89]
                   AND {&useTtbl}.userField90 GE userValueLo[90]
                   AND {&useTtbl}.userField90 LE userValueHi[90]
                   AND {&useTtbl}.userField91 GE userValueLo[91]
                   AND {&useTtbl}.userField91 LE userValueHi[91]
                   AND {&useTtbl}.userField92 GE userValueLo[92]
                   AND {&useTtbl}.userField92 LE userValueHi[92]
                   AND {&useTtbl}.userField93 GE userValueLo[93]
                   AND {&useTtbl}.userField93 LE userValueHi[93]
                   AND {&useTtbl}.userField94 GE userValueLo[94]
                   AND {&useTtbl}.userField94 LE userValueHi[94]
                   AND {&useTtbl}.userField95 GE userValueLo[95]
                   AND {&useTtbl}.userField95 LE userValueHi[95]
                   AND {&useTtbl}.userField96 GE userValueLo[96]
                   AND {&useTtbl}.userField96 LE userValueHi[96]
                   AND {&useTtbl}.userField97 GE userValueLo[97]
                   AND {&useTtbl}.userField97 LE userValueHi[97]
                   AND {&useTtbl}.userField98 GE userValueLo[98]
                   AND {&useTtbl}.userField98 LE userValueHi[98]
                   AND {&useTtbl}.userField99 GE userValueLo[99]
                   AND {&useTtbl}.userField99 LE userValueHi[99]
                   AND {&useTtbl}.userField100 GE userValueLo[100]
                   AND {&useTtbl}.userField100 LE userValueHi[100]
                   AND {&useTtbl}.userField101 GE userValueLo[101]
                   AND {&useTtbl}.userField101 LE userValueHi[101]
                   AND {&useTtbl}.userField102 GE userValueLo[102]
                   AND {&useTtbl}.userField102 LE userValueHi[102]
                   AND {&useTtbl}.userField103 GE userValueLo[103]
                   AND {&useTtbl}.userField103 LE userValueHi[103]
                   AND {&useTtbl}.userField104 GE userValueLo[104]
                   AND {&useTtbl}.userField104 LE userValueHi[104]
                   AND {&useTtbl}.userField105 GE userValueLo[105]
                   AND {&useTtbl}.userField105 LE userValueHi[105]
                   AND {&useTtbl}.userField106 GE userValueLo[106]
                   AND {&useTtbl}.userField106 LE userValueHi[106]
                   AND {&useTtbl}.userField107 GE userValueLo[107]
                   AND {&useTtbl}.userField107 LE userValueHi[107]
                   AND {&useTtbl}.userField108 GE userValueLo[108]
                   AND {&useTtbl}.userField108 LE userValueHi[108]
                   AND {&useTtbl}.userField109 GE userValueLo[109]
                   AND {&useTtbl}.userField109 LE userValueHi[109]
                   AND {&useTtbl}.userField110 GE userValueLo[110]
                   AND {&useTtbl}.userField110 LE userValueHi[110]
                   AND {&useTtbl}.userField111 GE userValueLo[111]
                   AND {&useTtbl}.userField111 LE userValueHi[111]
                   AND {&useTtbl}.userField112 GE userValueLo[112]
                   AND {&useTtbl}.userField112 LE userValueHi[112]
                   AND {&useTtbl}.userField113 GE userValueLo[113]
                   AND {&useTtbl}.userField113 LE userValueHi[113]
                   AND {&useTtbl}.userField114 GE userValueLo[114]
                   AND {&useTtbl}.userField114 LE userValueHi[114]
                   AND {&useTtbl}.userField115 GE userValueLo[115]
                   AND {&useTtbl}.userField115 LE userValueHi[115]
                   AND {&useTtbl}.userField116 GE userValueLo[116]
                   AND {&useTtbl}.userField116 LE userValueHi[116]
                   AND {&useTtbl}.userField117 GE userValueLo[117]
                   AND {&useTtbl}.userField117 LE userValueHi[117]
                   AND {&useTtbl}.userField118 GE userValueLo[118]
                   AND {&useTtbl}.userField118 LE userValueHi[118]
                   AND {&useTtbl}.userField119 GE userValueLo[119]
                   AND {&useTtbl}.userField119 LE userValueHi[119]
                   AND {&useTtbl}.userField120 GE userValueLo[120]
                   AND {&useTtbl}.userField120 LE userValueHi[120]
                   AND {&useTtbl}.userField121 GE userValueLo[121]
                   AND {&useTtbl}.userField121 LE userValueHi[121]
                   AND {&useTtbl}.userField122 GE userValueLo[122]
                   AND {&useTtbl}.userField122 LE userValueHi[122]
                   AND {&useTtbl}.userField123 GE userValueLo[123]
                   AND {&useTtbl}.userField123 LE userValueHi[123]
                   .
END. /* each {&useTtbl} */
&ENDIF
