

/*------------------------------------------------------------------------
    File        : genpostdist.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttIncomeSmtRatios NO-UNDO
        FIELD inc AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsIncomeSmtRatios FOR ttIncomeSmtRatios.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmclsprd        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbeact1        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact1       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbeact2        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact2       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegact3       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact3       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbeact4        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact4       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegact5       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact5       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPeriod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsIncomeSmtRatios.

     IF prmUser       = ? THEN ASSIGN     prmUser        = "".   
     IF prmAction     = ? THEN ASSIGN     prmAction      = "". 
     IF prmtrnsdt     = ? THEN ASSIGN     prmtrnsdt      = "". 
     IF prmclsprd     = ? THEN ASSIGN     prmclsprd      = "". 
     IF prmbeact1     = ? THEN ASSIGN     prmbeact1      = "".  
     IF prmendact1    = ? THEN ASSIGN     prmendact1     = "".  
     IF prmbeact2     = ? THEN ASSIGN     prmbeact2      = "". 
     IF prmendact2    = ? THEN ASSIGN     prmendact2     = "". 
     IF prmbegact3    = ? THEN ASSIGN     prmbegact3     = "". 
     IF prmendact3    = ? THEN ASSIGN     prmendact3     = "". 
     IF prmbeact4     = ? THEN ASSIGN     prmbeact4      = "". 
     IF prmendact4    = ? THEN ASSIGN     prmendact4     = "". 
     IF prmbegact5    = ? THEN ASSIGN     prmbegact5     = "". 
     IF prmendact5    = ? THEN ASSIGN     prmendact5     = "". 
     IF prmPeriod     = ? THEN ASSIGN     prmPeriod      = 0.
     IF prmOut        = ? THEN ASSIGN     prmOut         = "". 


    

DEFINE VARIABLE beg_acct-1   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE end_acct-1   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE beg_acct-2   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE end_acct-2   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE beg_acct-3   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE end_acct-3   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE beg_acct-4   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE end_acct-4   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE beg_acct-5   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE end_acct-5   AS CHARACTER FORMAT "X(25)":U NO-UNDO.
DEFINE VARIABLE tran-period  AS INTEGER FORMAT ">>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE tran-date    AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE tb_pre       AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.


def new shared var v-ptd as date format "99/99/9999" initial today no-undo.
def new shared var v-s-cos-no like account.actnum no-undo.
def new shared var v-e-cos-no like account.actnum no-undo.
def new shared var v-s-oper-no like account.actnum no-undo.
def new shared var v-e-oper-no like account.actnum no-undo.
def new shared var v-s-gen-no like account.actnum no-undo.
def new shared var v-e-gen-no like account.actnum no-undo.
def new shared var v-s-inc-no like account.actnum no-undo.
def new shared var v-e-inc-no like account.actnum no-undo.
def new shared var v-s-oth-no like account.actnum no-undo.
def new shared var v-e-oth-no like account.actnum no-undo.

def var ptd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-gross as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-gross as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-exp as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-exp as dec format "->>,>>>,>>9.99" no-undo.
def var v-ptd-per as dec format "->>9.99" no-undo.
def var v-ytd-per as dec format "->>9.99" no-undo.
def var pre-close as logical initial no no-undo.
def var v-year like period.yr.
def var v-period like period.pnum.
def var per-loop as int no-undo.

def buffer xperiod for period.

def var save_id as recid no-undo.
def var time_stamp as ch.
time_stamp = string(time, "HH:MMam").


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 v-today = TODAY 
 g_company = cocode
 g_user    = prmUser 
tran-date   =   date(prmtrnsdt) .


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .


   find first company where company.company = cocode no-lock no-error.  
   TRAN-date = TODAY.
   
   RUN gl/gl-inc1.p.


  /* find first period                   
            where period.company eq cocode
            and period.pst     le tran-date
            and period.pend    ge tran-date
            no-lock no-error.
        if avail period then prmPeriod = (period.pnum).
        
        ELSE DO:
            cError = "No Defined Period Exists" .
            RETURN.
        end.*/




  IF prmAction = "inc" THEN DO:


   ASSIGN
       beg_acct-1  =   prmbeact1  
       end_acct-1  =   prmendact1 
       beg_acct-2  =   prmbeact2  
       end_acct-2  =   prmendact2 
       beg_acct-3  =   prmbegact3 
       end_acct-3  =   prmendact3 
       beg_acct-4  =   prmbeact4  
       end_acct-4  =   prmendact4 
       beg_acct-5  =   prmbegact5 
       end_acct-5  =   prmendact5 
       tran-period =   prmPeriod 
       tran-date   =   date(prmtrnsdt) 
       tb_pre      =   IF prmclsprd = "Yes" THEN TRUE ELSE FALSE.
                    




        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "IncomeRatios" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        

        find first period                   
            where period.company eq cocode
            and period.pst     le tran-date
            and period.pend    ge tran-date
            no-lock no-error.
        if avail period then prmPeriod = (period.pnum).
        
        ELSE DO:
            cError = "No Defined Period Exists" .
            RETURN.
        end.


        run run-report. 
        

   
  CREATE ttIncomeSmtRatios.
  
    ASSIGN ttIncomeSmtRatios.inc = vTextFile .

  END.
/*****************************************************************************************/
  
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-top3.f}

FORM HEADER
  skip(1)
  "PTD Post" to 40 "%Sales" to 50
  "YTD Post" to 65 "%Sales" to 75
  "========" to 40 "======" to 50
  "========" to 65 "======" to 75
  with frame head-columns no-box no-labels width 80 PAGE-TOP STREAM-IO.

FORM
  account.dscr at 1 format "x(25)"
  ptd-sales to 40
  v-ptd-per to 50
  ytd-sales to 65
  v-ytd-per to 75
  with frame line-item down no-box no-labels width 80 STREAM-IO.

  assign v-ptd = tran-date
         pre-close = tb_pre
         v-s-cos-no = beg_acct-1
         v-e-cos-no = END_acct-1
         v-s-oper-no = beg_acct-2
         v-e-oper-no = END_acct-2
         v-s-gen-no  = beg_acct-3
         v-e-gen-no = END_acct-3
         v-s-inc-no = beg_acct-4
         v-e-inc-no = END_acct-4
         v-s-oth-no = beg_acct-5
         v-e-oth-no = END_acct-5
         .

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir.

 {sys/inc/outprint.i VALUE(lines-per-page)}

 tot-ytd-sales = 0.
 tot-ptd-sales = 0.
 ptd-sales = 0.
 ytd-sales = 0.
 tot-ptd-cos = 0.
 tot-ytd-cos = 0.
 tot-ptd-gross = 0.
 tot-ytd-gross = 0.
 tot-ptd-gen = 0.
 tot-ytd-gen = 0.

  find first period where period.company = cocode and
                          period.pst <= v-ptd and
                          period.pend >= v-ptd no-lock no-error.
  if avail period then
    assign v-year = period.yr
           v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   x = (56 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (57 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (78 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

  view frame r-top.
  view frame head-columns.

  find first period where period.company = cocode and
			  period.pst <= v-ptd and
			  period.pend >= v-ptd no-lock no-error.
    if avail period then
    assign v-year = period.yr
	   v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   x = (56 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (57 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (78 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

  view frame r-top.
  view frame head-columns.

  /*  Sales Totals */
  for each account where account.company eq cocode and
			 account.type eq "R" use-index type no-lock:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:
      tot-ptd-sales = tot-ptd-sales + glhist.tr-amt.
    END.

    if pre-close then
    do:

      for each gltrans no-lock where gltrans.actnum eq account.actnum and
				     gltrans.company eq cocode:
	if gltrans.tr-date le v-ptd and
	   gltrans.tr-date >= period.pst and
	   gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign tot-ptd-sales = tot-ptd-sales + gltrans.tr-amt.
	end.
      end.
    end.

    do per-loop = 1 to (v-period - 1):
      find first xperiod where xperiod.company = cocode and
			      xperiod.pnum = per-loop and
			      xperiod.yr = v-year
			      no-lock no-error.
      IF AVAIL xperiod THEN
	  FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ per-loop
		    AND glhist.tr-date GE xperiod.pst
	        AND glhist.tr-date LE xperiod.pend:
	    tot-ytd-sales = tot-ytd-sales + glhist.tr-amt.
	  END.
    end.

    assign tot-ytd-sales = tot-ytd-sales + account.cyr-open.

  end.  /* Sales Totals for each */

  assign tot-ytd-sales = tot-ytd-sales + tot-ptd-sales
	 tot-ptd-sales = - tot-ptd-sales
	 tot-ytd-sales = - tot-ytd-sales.

  put "========  Sales  ========" skip(1).

  /*  Sales Totals */
  for each account where account.company eq cocode and
			 account.type eq "R" no-lock
			 use-index type break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-sales = ptd-sales + glhist.tr-amt.
    END.


    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
				     gltrans.company = cocode:
	if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign ptd-sales = ptd-sales + gltrans.tr-amt.
	end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
		   		   xperiod.pnum = per-loop and
				   xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
		      AND glhist.tr-date GE xperiod.pst
	          AND glhist.tr-date LE xperiod.pend:
	      ytd-sales = ytd-sales + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((- ptd-sales / tot-ptd-sales) * 100)
	     ytd-sales = ytd-sales + ptd-sales + account.cyr-open
	     v-ytd-per = ((- ytd-sales / tot-ytd-sales) * 100).
      display account.dscr (- ptd-sales) @ ptd-sales v-ptd-per
			   (- ytd-sales) @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign ptd-sales = 0
	     ytd-sales = 0.
    end.
  end.  /* Sales Totals for each */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Sales" at 1
      tot-ptd-sales to 40
      100.00 to 50
      tot-ytd-sales to 65
      100.00 to 75 skip(1).
  put "====  Cost of Sales  ====" skip(1).

  /*  Cost of Sales */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-cos-no and
			 account.actnum <= v-e-cos-no
			 no-lock use-index account break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-cos = ptd-cos + glhist.tr-amt.
    END.

    if pre-close then
    do:
	for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	  if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	  do:
	    if gltrans.period <> period.pnum then
	      next.
	    assign ptd-cos = ptd-cos + gltrans.tr-amt.
	  end.
	end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
		    	   xperiod.pnum = per-loop and
				   xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
		      AND glhist.tr-date GE xperiod.pst
		      AND glhist.tr-date LE xperiod.pend:
	      ytd-cos = ytd-cos + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-cos / tot-ptd-sales) * 100)
	     ytd-cos = ytd-cos + ptd-cos + account.cyr-open
	     v-ytd-per = ((ytd-cos / tot-ytd-sales) * 100).
      display account.dscr ptd-cos @ ptd-sales v-ptd-per
			   ytd-cos @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-cos = tot-ptd-cos + ptd-cos
	     tot-ytd-cos = tot-ytd-cos + ytd-cos
	     ptd-cos = 0
	     ytd-cos = 0.
    end.
  end.  /* Cost of Sales */

  assign tot-ptd-gross = tot-ptd-sales - tot-ptd-cos
	 tot-ytd-gross = tot-ytd-sales - tot-ytd-cos.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Cost of Sales" at 1 tot-ptd-cos to 40
       ((tot-ptd-cos / tot-ptd-sales) * 100.00) to 50
      tot-ytd-cos to 65
       ((tot-ytd-cos / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Gross Margin" tot-ptd-gross to 40
      ((tot-ptd-gross / tot-ptd-sales) * 100) to 50
      tot-ytd-gross to 65
      ((tot-ytd-gross / tot-ytd-sales) * 100) to 75 skip(1).
  put "===  Operating Expenses  ===" skip(1).

  /*  Operating Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-oper-no and
			 account.actnum <= v-e-oper-no
			 no-lock use-index account break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-oper = ptd-oper + glhist.tr-amt.
    END.

    if pre-close then
    do:
	for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	  if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	  do:
	    if gltrans.period <> period.pnum then
	      next.
	    assign ptd-oper = ptd-oper + gltrans.tr-amt.
	  end.
	end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
				   xperiod.pnum = per-loop and
				   xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
   	      ytd-oper = ytd-oper + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-oper / tot-ptd-sales) * 100)
	     ytd-oper = ytd-oper + ptd-oper + account.cyr-open
	     v-ytd-per = ((ytd-oper / tot-ytd-sales) * 100).
      display account.dscr ptd-oper @ ptd-sales v-ptd-per
			   ytd-oper @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oper = tot-ptd-oper + ptd-oper
	     tot-ytd-oper = tot-ytd-oper + ytd-oper
	     ptd-oper = 0
	     ytd-oper = 0.
    end.
  end.  /* Operating Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1 tot-ptd-oper to 40
       ((tot-ptd-oper / tot-ptd-sales) * 100.00) to 50
	tot-ytd-oper to 65
       ((tot-ytd-oper / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "General & Administrative" skip(1).

  /*  General/Admin Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-gen-no and
			 account.actnum <= v-e-gen-no
			 no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
			    period.pst <= v-ptd and
			    period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum 
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-gen = ptd-gen + glhist.tr-amt.
    END.

    if pre-close then
    do:
	for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	  if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	  do:
	    if gltrans.period <> period.pnum then
	      next.
	    assign ptd-gen = ptd-gen + gltrans.tr-amt.
	  end.
	end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
				  xperiod.pnum = per-loop and
				  xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
	      ytd-gen = ytd-gen + glhist.tr-amt.
	    END.
      end.

      assign v-ptd-per = ((ptd-gen / tot-ptd-sales) * 100)
	     ytd-gen = ytd-gen + ptd-gen + account.cyr-open
	     v-ytd-per = ((ytd-gen / tot-ytd-sales) * 100).
      display account.dscr ptd-gen @ ptd-sales v-ptd-per
			   ytd-gen @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-gen = tot-ptd-gen + ptd-gen
	     tot-ytd-gen = tot-ytd-gen + ytd-gen
	     ptd-gen = 0
	     ytd-gen = 0.
    end.
  end.  /* Gen & Admin Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total General & Admin" at 1 tot-ptd-gen to 40
       ((tot-ptd-gen / tot-ptd-sales) * 100.00) to 50
	tot-ytd-gen to 65
       ((tot-ytd-gen / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Income Tax Expenses" skip(1).

  /*  Income Tax Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-inc-no and
			 account.actnum <= v-e-inc-no
			 no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
			    period.pst <= v-ptd and
			    period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-inc = ptd-inc + glhist.tr-amt.
    END.

    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	if gltrans.tr-date le v-ptd and
	   gltrans.tr-date >= period.pst and
	   gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign ptd-inc = ptd-inc + gltrans.tr-amt.
	end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    FIND FIRST xperiod where xperiod.company = cocode and
				  xperiod.pnum = per-loop and
				  xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
	      ytd-inc = ytd-inc + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-inc / tot-ptd-sales) * 100)
	     ytd-inc = ytd-inc + ptd-inc + account.cyr-open
	     v-ytd-per = ((ytd-inc / tot-ytd-sales) * 100).
      display account.dscr ptd-inc @ ptd-sales v-ptd-per
			   ytd-inc @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-inc = tot-ptd-inc + ptd-inc
	     tot-ytd-inc = tot-ytd-inc + ytd-inc
	     ptd-inc = 0
	     ytd-inc = 0.
    end.
  end.  /* Operating Expenses */

  assign tot-ptd-exp = tot-ptd-oper + tot-ptd-gen + tot-ptd-inc
	 tot-ytd-exp = tot-ytd-oper + tot-ytd-gen + tot-ytd-inc.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Income Tax Expense" at 1 tot-ptd-inc to 40
       ((tot-ptd-inc / tot-ptd-sales) * 100.00) to 50
	tot-ytd-inc to 65
       ((tot-ytd-inc / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1
       tot-ptd-exp to 40
       ((tot-ptd-exp / tot-ptd-sales) * 100.00) to 50
       tot-ytd-exp to 65
       ((tot-ytd-exp / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Net Income Before Taxes"
      (tot-ptd-gross - tot-ptd-exp) to 40 format "->>,>>>,>>9.99"
      (((tot-ptd-gross - tot-ptd-exp) / tot-ptd-sales) * 100.00) to 50
      (tot-ytd-gross - tot-ytd-exp) to 65 format "->>,>>>,>>9.99"
      (((tot-ytd-gross - tot-ytd-exp) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip(1).

  /*  Other Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-oth-no and
			 account.actnum <= v-e-oth-no
			 no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
			    period.pst <= v-ptd and
			    period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period 
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-oth = ptd-oth + glhist.tr-amt.
    END.

    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	if gltrans.tr-date le v-ptd and
	   gltrans.tr-date >= period.pst and
	   gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign ptd-oth = ptd-oth + gltrans.tr-amt.
	end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
				  xperiod.pnum = per-loop and
				  xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
	      ytd-oth = ytd-oth + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-oth / tot-ptd-sales) * 100)
	     ytd-oth = ytd-oth + ptd-oth + account.cyr-open
	     v-ytd-per = ((ytd-oth / tot-ytd-sales) * 100).
      display account.dscr ptd-oth @ ptd-sales v-ptd-per
			   ytd-oth @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oth = tot-ptd-oth + ptd-oth
	     tot-ytd-oth = tot-ytd-oth + ytd-oth
	     ptd-oth = 0
	     ytd-oth = 0.
    end.
  end.  /* Other Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
      
  put "Net Income After Taxes"
      ((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) to 40 format "->>,>>>,>>9.99"
      ((((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) / tot-ptd-sales) * 100.00) to 50
      ((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) to 65 format "->>,>>>,>>9.99"
      ((((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip. 
  
  


END PROCEDURE.
