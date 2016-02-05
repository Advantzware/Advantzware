/* callttparam.i
   Avoids having to type all the TABLE-HANDLEs for a call to a temp-table handling
   procedure.
   
   Parameters:
     MODE       is either INPUT, OUTPUT or INPUT-OUTPUT and denotes the mode of the
                parameter.
     ARRAYFIELD is the name of the variable or field that contains the array of
                temp-table handles.
     T01 - T64  is any valid code line that could follow the mode. If no value is
                specified for any one of these, the value is taken from the array. 
                examples:
                  &T01 = "TABLE-HANDLE htt"
                  &T02 = "TEMP-TABLE ttCustomer:HANDLE"
                  &T03 = "TABLE ttCustomer" */

&IF DEFINED(MODE) = 0 &THEN
  &SCOPED-DEFINE MODE INPUT-OUTPUT
&ENDIF

&IF DEFINED(T01) <> 0 &THEN
  {&MODE} {&T01},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[01],
&ENDIF

&IF DEFINED(T02) <> 0 &THEN
  {&MODE} {&T02},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[02],
&ENDIF

&IF DEFINED(T03) <> 0 &THEN
  {&MODE} {&T03},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[03],
&ENDIF

&IF DEFINED(T04) <> 0 &THEN
  {&MODE} {&T04},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[04],
&ENDIF

&IF DEFINED(T05) <> 0 &THEN
  {&MODE} {&T05},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[05],
&ENDIF

&IF DEFINED(T06) <> 0 &THEN
  {&MODE} {&T06},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[06],
&ENDIF

&IF DEFINED(T07) <> 0 &THEN
  {&MODE} {&T07},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[07],
&ENDIF

&IF DEFINED(T08) <> 0 &THEN
  {&MODE} {&T08},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[08],
&ENDIF

&IF DEFINED(T09) <> 0 &THEN
  {&MODE} {&T09},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[09],
&ENDIF

&IF DEFINED(T10) <> 0 &THEN
  {&MODE} {&T10},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[10],
&ENDIF

&IF DEFINED(T11) <> 0 &THEN
  {&MODE} {&T11},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[11],
&ENDIF

&IF DEFINED(T12) <> 0 &THEN
  {&MODE} {&T12},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[12],
&ENDIF

&IF DEFINED(T13) <> 0 &THEN
  {&MODE} {&T13},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[13],
&ENDIF

&IF DEFINED(T14) <> 0 &THEN
  {&MODE} {&T14},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[14],
&ENDIF

&IF DEFINED(T15) <> 0 &THEN
  {&MODE} {&T15},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[15],
&ENDIF

&IF DEFINED(T16) <> 0 &THEN
  {&MODE} {&T16},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[16],
&ENDIF

&IF DEFINED(T17) <> 0 &THEN
  {&MODE} {&T17},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[17],
&ENDIF

&IF DEFINED(T18) <> 0 &THEN
  {&MODE} {&T18},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[18],
&ENDIF

&IF DEFINED(T19) <> 0 &THEN
  {&MODE} {&T19},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[19],
&ENDIF

&IF DEFINED(T20) <> 0 &THEN
  {&MODE} {&T20},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[20],
&ENDIF

&IF DEFINED(T21) <> 0 &THEN
  {&MODE} {&T21},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[21],
&ENDIF

&IF DEFINED(T22) <> 0 &THEN
  {&MODE} {&T22},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[22],
&ENDIF

&IF DEFINED(T23) <> 0 &THEN
  {&MODE} {&T23},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[23],
&ENDIF

&IF DEFINED(T24) <> 0 &THEN
  {&MODE} {&T24},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[24],
&ENDIF

&IF DEFINED(T25) <> 0 &THEN
  {&MODE} {&T25},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[25],
&ENDIF

&IF DEFINED(T26) <> 0 &THEN
  {&MODE} {&T26},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[26],
&ENDIF

&IF DEFINED(T27) <> 0 &THEN
  {&MODE} {&T27},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[27],
&ENDIF

&IF DEFINED(T28) <> 0 &THEN
  {&MODE} {&T28},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[28],
&ENDIF

&IF DEFINED(T29) <> 0 &THEN
  {&MODE} {&T29},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[29],
&ENDIF

&IF DEFINED(T30) <> 0 &THEN
  {&MODE} {&T30},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[30],
&ENDIF

&IF DEFINED(T31) <> 0 &THEN
  {&MODE} {&T31},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[31],
&ENDIF

&IF DEFINED(T32) <> 0 &THEN
  {&MODE} {&T32},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[32],
&ENDIF

&IF DEFINED(T33) <> 0 &THEN
  {&MODE} {&T33},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[33],
&ENDIF

&IF DEFINED(T34) <> 0 &THEN
  {&MODE} {&T34},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[34],
&ENDIF

&IF DEFINED(T35) <> 0 &THEN
  {&MODE} {&T35},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[35],
&ENDIF

&IF DEFINED(T36) <> 0 &THEN
  {&MODE} {&T36},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[36],
&ENDIF

&IF DEFINED(T37) <> 0 &THEN
  {&MODE} {&T37},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[37],
&ENDIF

&IF DEFINED(T38) <> 0 &THEN
  {&MODE} {&T38},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[38],
&ENDIF

&IF DEFINED(T39) <> 0 &THEN
  {&MODE} {&T39},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[39],
&ENDIF

&IF DEFINED(T40) <> 0 &THEN
  {&MODE} {&T40},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[40],
&ENDIF

&IF DEFINED(T41) <> 0 &THEN
  {&MODE} {&T41},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[41],
&ENDIF

&IF DEFINED(T42) <> 0 &THEN
  {&MODE} {&T42},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[42],
&ENDIF

&IF DEFINED(T43) <> 0 &THEN
  {&MODE} {&T43},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[43],
&ENDIF

&IF DEFINED(T44) <> 0 &THEN
  {&MODE} {&T44},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[44],
&ENDIF

&IF DEFINED(T45) <> 0 &THEN
  {&MODE} {&T45},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[45],
&ENDIF

&IF DEFINED(T46) <> 0 &THEN
  {&MODE} {&T46},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[46],
&ENDIF

&IF DEFINED(T47) <> 0 &THEN
  {&MODE} {&T47},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[47],
&ENDIF

&IF DEFINED(T48) <> 0 &THEN
  {&MODE} {&T48},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[48],
&ENDIF

&IF DEFINED(T49) <> 0 &THEN
  {&MODE} {&T49},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[49],
&ENDIF

&IF DEFINED(T50) <> 0 &THEN
  {&MODE} {&T50},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[50],
&ENDIF

&IF DEFINED(T51) <> 0 &THEN
  {&MODE} {&T51},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[51],
&ENDIF

&IF DEFINED(T52) <> 0 &THEN
  {&MODE} {&T52},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[52],
&ENDIF

&IF DEFINED(T53) <> 0 &THEN
  {&MODE} {&T53},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[53],
&ENDIF

&IF DEFINED(T54) <> 0 &THEN
  {&MODE} {&T54},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[54],
&ENDIF

&IF DEFINED(T55) <> 0 &THEN
  {&MODE} {&T55},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[55],
&ENDIF

&IF DEFINED(T56) <> 0 &THEN
  {&MODE} {&T56},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[56],
&ENDIF

&IF DEFINED(T57) <> 0 &THEN
  {&MODE} {&T57},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[57],
&ENDIF

&IF DEFINED(T58) <> 0 &THEN
  {&MODE} {&T58},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[58],
&ENDIF

&IF DEFINED(T59) <> 0 &THEN
  {&MODE} {&T59},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[59],
&ENDIF

&IF DEFINED(T60) <> 0 &THEN
  {&MODE} {&T60},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[60],
&ENDIF

&IF DEFINED(T61) <> 0 &THEN
  {&MODE} {&T61},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[61],
&ENDIF

&IF DEFINED(T62) <> 0 &THEN
  {&MODE} {&T62},
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[62],
&ENDIF

&IF DEFINED(T63) <> 0 &THEN
  {&MODE} {&T63},
&ELSE
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[63],
&ENDIF

&IF DEFINED(T64) <> 0 &THEN
  {&MODE} {&T64}
&ELSE 
  {&MODE} TABLE-HANDLE {&ARRAYFIELD}[64]
&ENDIF
  
&IF DEFINED(MODE) <> 1 &THEN
  &UNDEFINE MODE
&ENDIF
