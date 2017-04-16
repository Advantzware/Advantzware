/* startUp.i */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN VALUE(findProgram(codeDir + '{&startDir}/',ID,'/startUp.p')) (OUTPUT continue,OUTPUT commaList).
IF NOT continue THEN RETURN.
&ELSE
IF ID BEGINS 'ASI' THEN commaList = '001,Main'.
IF ID BEGINS 'MXP' THEN commaList = 'AC'.
&ENDIF
