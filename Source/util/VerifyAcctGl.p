{custom/globdefs.i}

{sys/inc/var.i new shared}


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

find first company where company.company eq cocode no-lock.  



DEFINE VARIABLE iAccountAmt AS INTEGER  NO-UNDO.
DEFINE VARIABLE iGlHistAmt  AS INTEGER  NO-UNDO.
DEFINE VARIABLE cTmpDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-fisc-yr LIKE period.yr NO-UNDO.


DEFINE TEMP-TABLE tt_labels
    FIELD vLabel  AS CHARACTER FORMAT "X(30)"
    EXTENT 6
    INITIAL ["Account#","Account Descr","Period","Account Period Amount",
    "GL Hist Period Amount","Variance"].
CREATE tt_labels.


DEFINE TEMP-TABLE tt_diff
    FIELD cAccount     AS CHARACTER 
    FIELD cAcctDescr   AS CHARACTER
    FIELD iPeriod      AS INTEGER
    FIELD dAccountAmt  AS DECIMAL
    FIELD dGlHistAmt   AS DECIMAL
    FIELD dDiffAmt     AS DECIMAL
         .
find first period
    where period.company eq cocode
      and period.pstat   eq yes
    no-lock no-error.
v-fisc-yr = (if avail period then period.yr else year(today)) -
            int(not company.yend-per).         
 
FOR EACH account WHERE account.company EQ cocode:
     
  FOR EACH period
      WHERE period.company EQ account.company
        AND period.yr      EQ v-fisc-yr
      NO-LOCK by period.yr BY period.pst:
      
      iGlHistAmt = 0.
      
      FOR each glhist
          where glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend         
          NO-LOCK :  
       
         iGlHistAmt = iGlHistAmt + glhist.tr-amt  .
      END. 
       
     IF  account.cyr[period.pnum] NE iGlHistAmt  THEN DO:
     
        CREATE tt_diff.
         ASSIGN 
             tt_diff.cAccount    = account.actnum
             tt_diff.cAcctDescr  = account.dscr
             tt_diff.iPeriod     = period.pnum
             tt_diff.dAccountAmt = account.cyr[period.pnum]
             tt_diff.dGlHistAmt  = iGlHistAmt
             tt_diff.dDiffAmt  = account.cyr[period.pnum] - iGlHistAmt.
             
     END.   /*IF  account.cyr[period.pnum] NE*/
  END.  /* for each period  */
END. 

     FIND FIRST users NO-LOCK
		  WHERE users.user_id EQ USERID(LDBNAME(1))
		  NO-ERROR.
		
		IF AVAIL users AND users.user_program[2] NE "" THEN
		       cTmpDir = users.user_program[2].
		    ELSE
		       cTmpDir = "c:\tmp".




    FIND FIRST tt_diff NO-LOCK NO-ERROR.
    IF AVAILABLE tt_diff THEN DO:
                              
        OUTPUT TO VALUE(cTmpDir + "\acct_variance_list.csv") .
        
        FOR EACH tt_labels:
            EXPORT DELIMITER "," tt_labels.
        END.
        FOR EACH tt_diff NO-LOCK BY tt_diff.cAccount BY tt_diff.iPeriod:
            EXPORT DELIMITER "," tt_diff .
        END.
        OUTPUT CLOSE.
    END.    
    
  SESSION:SET-WAIT-STATE(""). 
