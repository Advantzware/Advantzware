/* char-fld-help.i */

DEFINE VARIABLE ls-cur-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-loc LIKE fg-bin.loc NO-UNDO.
DEFINE VARIABLE lv-loc-bin LIKE fg-bin.loc-bin NO-UNDO.

/* gdm - 11050804 */
DEF VAR ip-chrfld AS CHAR NO-UNDO.
DEF VAR v_chrfld  AS CHAR NO-UNDO.

ls-cur-val = {&nameField}.

IF LOOKUP(ls-cur-val, gvcMultiSelect) GT 0 AND LOOKUP(ls-cur-val,name-fld-list) GT 0 THEN DO:
  RUN windows/w-syschr.w (INPUT ls-cur-val, INPUT {&tableName}.char-fld:SCREEN-VALUE, str-init[LOOKUP(ls-cur-val,name-fld-list)], OUTPUT char-val) .
   {&tableName}.char-fld:SCREEN-VALUE = char-val.
  RETURN NO-APPLY.
END.
ELSE
IF ls-cur-val EQ 'FGWHSBIN' THEN DO:
  IF {&tableName}.char-fld:SCREEN-VALUE EQ 'AUTOPOST' THEN
  ASSIGN
    lv-loc     = ''
    lv-loc-bin = ''.
  ELSE
  ASSIGN
    lv-loc     = SUBSTR({&tableName}.char-fld:SCREEN-VALUE,1,5)
    lv-loc-bin = SUBSTR({&tableName}.char-fld:SCREEN-VALUE,6,8).
  RUN windows/l-fgbin.w (gcompany, lv-loc, lv-loc-bin, OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = STRING(ENTRY(2,char-val),'x(5)') + TRIM(ENTRY(1,char-val)).
END.
ELSE IF ls-cur-val = 'TSPOSTFG' THEN DO:
  RUN windows/l-mach2.w (gcompany,g_loc,'',OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = char-val.
  RETURN NO-APPLY.
END.
ELSE IF ls-cur-val EQ 'RMWHSBIN' THEN DO:
  IF {&tableName}.char-fld:SCREEN-VALUE EQ 'RMITEM' THEN
  ASSIGN
    lv-loc     = ''
    lv-loc-bin = ''.
  ELSE
  ASSIGN
    lv-loc     = SUBSTR({&tableName}.char-fld:SCREEN-VALUE,1,5)
    lv-loc-bin = SUBSTR({&tableName}.char-fld:SCREEN-VALUE,6,8).
  RUN windows/l-rmbin.w (gcompany,lv-loc,lv-loc-bin,OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = STRING(ENTRY(2,char-val),'x(5)') + TRIM(ENTRY(1,char-val)).
END.
ELSE IF ls-cur-val = 'FGMASTER' THEN DO:
  RUN windows/l-itemfg.w (gcompany,g_loc,'',OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = ENTRY(1,char-val).
  RETURN NO-APPLY.
END.
/* gdm - 11050804 */
ELSE IF ls-cur-val = 'CASLABEL' THEN DO:

    ASSIGN ip-chrfld = {&tableName}.char-fld:SCREEN-VALUE.

    RUN sys\ref\char-fld-help.w(INPUT gcompany,
                                INPUT ip-chrfld,
                                OUTPUT v_chrfld).

    IF TRIM(v_chrfld) NE ''
      THEN ASSIGN {&tableName}.char-fld:SCREEN-VALUE = v_chrfld.      
END.
/* gdm - 11110806 */
ELSE IF ls-cur-val = 'GRAPHIC' THEN DO:

    ASSIGN ip-chrfld = {&tableName}.char-fld:SCREEN-VALUE
           ip-chrfld =  IF TRIM(ip-chrfld) EQ "" THEN "GRAPHIC"
                                                 ELSE TRIM(ip-chrfld). 
    
    RUN sys\ref\char-fld-help.w(INPUT gcompany,
                                INPUT ip-chrfld,
                                OUTPUT v_chrfld).

    IF TRIM(v_chrfld) NE ''
      THEN ASSIGN {&tableName}.char-fld:SCREEN-VALUE = v_chrfld.      

END.
/* gdm - 12170903 */
ELSE IF ls-cur-val = 'BARDIR' THEN DO:
   
  MESSAGE "Do you want to display Xprint Values.... "
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    TITLE "" UPDATE lChoice AS LOGICAL.
  if not lChoice then Do:
    ASSIGN ip-chrfld = {&tableName}.descrip. 
    
    RUN sys\ref\char-fld-help.w(INPUT gcompany,
                                INPUT ip-chrfld,
                                OUTPUT v_chrfld).

    IF TRIM(v_chrfld) NE ''
      THEN ASSIGN {&tableName}.char-fld:SCREEN-VALUE = v_chrfld. 
  END.
  ELSE DO: 
  	RUN windows/l-typxpr.w (OUTPUT char-val).
  	IF char-val NE '' THEN
  	{&tableName}.char-fld:SCREEN-VALUE = char-val.
  	RETURN NO-APPLY.    
  END.

END.
ELSE IF ls-cur-val = 'SALESREP' THEN DO:
  RUN windows/l-sman.w (gcompany,OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = ENTRY(1,char-val).
  RETURN NO-APPLY.
END.
ELSE IF ls-cur-val = 'BolPrint' THEN DO:
  RUN windows/l-fgbin2.w (gcompany,"","",OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = ENTRY(1,char-val).
  RETURN NO-APPLY.
END.
ELSE DO:
  RUN windows/l-syschr.w (gcompany,ls-cur-val,FOCUS:SCREEN-VALUE,OUTPUT char-val).
  IF char-val NE '' THEN
  {&tableName}.char-fld:SCREEN-VALUE = ENTRY(1,char-val).
END.
RETURN NO-APPLY.
