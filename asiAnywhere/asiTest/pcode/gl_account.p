

/*------------------------------------------------------------------------
    File        : gl_account.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGLAccountList NO-UNDO
    FIELD act            AS CHAR
    FIELD actdscr        AS CHAR
    FIELD actype         AS CHAR
    FIELD act_ext        AS CHAR
    FIELD RecKey         AS CHAR
    FIELD cry_opn        AS DEC
    FIELD lyr_opn        AS DEC
    FIELD bud1           AS DEC
    FIELD bud2           AS DEC
    FIELD bud3           AS DEC
    FIELD bud4           AS DEC
    FIELD bud5           AS DEC 
    FIELD bud6           AS DEC 
    FIELD bud7           AS DEC 
    FIELD bud8           AS DEC 
    FIELD bud9           AS DEC 
    FIELD bud10          AS DEC 
    FIELD bud11          AS DEC 
    FIELD bud12          AS DEC 
    FIELD bud13          AS DEC 
    FIELD ly_bud1        AS DEC 
    FIELD ly_bud2        AS DEC 
    FIELD ly_bud3        AS DEC 
    FIELD ly_bud4        AS DEC 
    FIELD ly_bud5        AS DEC 
    FIELD ly_bud6        AS DEC 
    FIELD ly_bud7        AS DEC 
    FIELD ly_bud8        AS DEC 
    FIELD ly_bud9        AS DEC 
    FIELD ly_bud10       AS DEC 
    FIELD ly_bud11       AS DEC 
    FIELD ly_bud12       AS DEC 
    FIELD ly_bud13       AS DEC 
    FIELD lyr1           AS DEC 
    FIELD lyr2           AS DEC 
    FIELD lyr3           AS DEC 
    FIELD lyr4           AS DEC 
    FIELD lyr5           AS DEC 
    FIELD lyr6           AS DEC 
    FIELD lyr7           AS DEC 
    FIELD lyr8           AS DEC 
    FIELD lyr9           AS DEC 
    FIELD lyr10          AS DEC 
    FIELD lyr11          AS DEC 
    FIELD lyr12          AS DEC 
    FIELD lyr13          AS DEC 
    FIELD cyr1           AS DEC 
    FIELD cyr2           AS DEC 
    FIELD cyr3           AS DEC 
    FIELD cyr4           AS DEC 
    FIELD cyr5           AS DEC 
    FIELD cyr6           AS DEC 
    FIELD cyr7           AS DEC 
    FIELD cyr8           AS DEC 
    FIELD cyr9           AS DEC 
    FIELD cyr10          AS DEC 
    FIELD cyr11          AS DEC 
    FIELD cyr12          AS DEC 
    FIELD cyr13          AS DEC 
    FIELD tb_not_disc    AS CHAR
    FIELD btn            AS CHAR
        .

DEFINE DATASET dsGLAccountList FOR ttGLAccountList .

DEFINE INPUT PARAMETER prmUser         AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmact          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr      AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmactype       AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey       AS CHAR         NO-UNDO.

DEFINE INPUT PARAMETER prmcry_opn        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr_opn        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud1           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud2           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud3           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud4           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud5           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud6           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud7           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud8           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud9           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud10          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud11          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud12          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmbud13          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud1        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud2        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud3        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud4        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud5        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud6        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud7        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud8        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud9        AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud10       AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud11       AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud12       AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmly_bud13       AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr1           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr2           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr3           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr4           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr5           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr6           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr7           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr8           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr9           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr10          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr11          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr12          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmlyr13          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr1           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr2           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr3           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr4           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr5           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr6           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr7           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr8           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr9           AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr10          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr11          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr12          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmcyr13          AS DEC          NO-UNDO.
DEFINE INPUT PARAMETER prmtb_not_disc    AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER prmbtn            AS CHAR         NO-UNDO. 
DEF OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLAccountList.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE VARIABLE tb_not-disc AS LOGICAL INITIAL no NO-UNDO.


IF prmUser         = ? THEN ASSIGN prmUser       = "".
IF prmAction       = ? THEN ASSIGN prmAction     = "Select".
IF prmact          = ? THEN ASSIGN prmact        = "".
IF prmactdscr      = ? THEN ASSIGN prmactdscr    = "".
IF prmactype       = ? THEN ASSIGN prmactype     = "".
IF prmRecKey       = ? THEN ASSIGN prmRecKey     = "".

IF prmcry_opn    = ? THEN ASSIGN prmcry_opn   = 0.
IF prmlyr_opn    = ? THEN ASSIGN prmlyr_opn   = 0.
IF prmbud1       = ? THEN ASSIGN prmbud1      = 0.
IF prmbud2       = ? THEN ASSIGN prmbud2      = 0.
IF prmbud3       = ? THEN ASSIGN prmbud3      = 0.
IF prmbud4       = ? THEN ASSIGN prmbud4      = 0.
IF prmbud5       = ? THEN ASSIGN prmbud5      = 0.
IF prmbud6       = ? THEN ASSIGN prmbud6      = 0.
IF prmbud7       = ? THEN ASSIGN prmbud7      = 0.
IF prmbud8       = ? THEN ASSIGN prmbud8      = 0.
IF prmbud9       = ? THEN ASSIGN prmbud9      = 0.
IF prmbud10      = ? THEN ASSIGN prmbud10     = 0.
IF prmbud11      = ? THEN ASSIGN prmbud11     = 0.
IF prmbud12      = ? THEN ASSIGN prmbud12     = 0.
IF prmbud13      = ? THEN ASSIGN prmbud13     = 0.
IF prmly_bud1    = ? THEN ASSIGN prmly_bud1   = 0.
IF prmly_bud2    = ? THEN ASSIGN prmly_bud2   = 0.
IF prmly_bud3    = ? THEN ASSIGN prmly_bud3   = 0.
IF prmly_bud4    = ? THEN ASSIGN prmly_bud4   = 0.
IF prmly_bud5    = ? THEN ASSIGN prmly_bud5   = 0.
IF prmly_bud6    = ? THEN ASSIGN prmly_bud6   = 0.
IF prmly_bud7    = ? THEN ASSIGN prmly_bud7   = 0.
IF prmly_bud8    = ? THEN ASSIGN prmly_bud8   = 0.
IF prmly_bud9    = ? THEN ASSIGN prmly_bud9   = 0.
IF prmly_bud10   = ? THEN ASSIGN prmly_bud10  = 0.
IF prmly_bud11   = ? THEN ASSIGN prmly_bud11  = 0.
IF prmly_bud12   = ? THEN ASSIGN prmly_bud12  = 0.
IF prmly_bud13   = ? THEN ASSIGN prmly_bud13  = 0.
IF prmlyr1       = ? THEN ASSIGN prmlyr1      = 0.
IF prmlyr2       = ? THEN ASSIGN prmlyr2      = 0.
IF prmlyr3       = ? THEN ASSIGN prmlyr3      = 0.
IF prmlyr4       = ? THEN ASSIGN prmlyr4      = 0.
IF prmlyr5       = ? THEN ASSIGN prmlyr5      = 0.
IF prmlyr6       = ? THEN ASSIGN prmlyr6      = 0.
IF prmlyr7       = ? THEN ASSIGN prmlyr7      = 0.
IF prmlyr8       = ? THEN ASSIGN prmlyr8      = 0.
IF prmlyr9       = ? THEN ASSIGN prmlyr9      = 0.
IF prmlyr10      = ? THEN ASSIGN prmlyr10     = 0.
IF prmlyr11      = ? THEN ASSIGN prmlyr11     = 0.
IF prmlyr12      = ? THEN ASSIGN prmlyr12     = 0.
IF prmlyr13      = ? THEN ASSIGN prmlyr13     = 0.
IF prmcyr1       = ? THEN ASSIGN prmcyr1      = 0.
IF prmcyr2       = ? THEN ASSIGN prmcyr2      = 0.
IF prmcyr3       = ? THEN ASSIGN prmcyr3      = 0.
IF prmcyr4       = ? THEN ASSIGN prmcyr4      = 0.
IF prmcyr5       = ? THEN ASSIGN prmcyr5      = 0.
IF prmcyr6       = ? THEN ASSIGN prmcyr6      = 0.
IF prmcyr7       = ? THEN ASSIGN prmcyr7      = 0.
IF prmcyr8       = ? THEN ASSIGN prmcyr8      = 0.
IF prmcyr9       = ? THEN ASSIGN prmcyr9      = 0.
IF prmcyr10      = ? THEN ASSIGN prmcyr10     = 0.
IF prmcyr11      = ? THEN ASSIGN prmcyr11     = 0.
IF prmcyr12      = ? THEN ASSIGN prmcyr12     = 0.
IF prmcyr13      = ? THEN ASSIGN prmcyr13     = 0.
IF prmtb_not_disc = ? THEN ASSIGN prmtb_not_disc = "".
IF prmbtn         = ? THEN ASSIGN prmbtn         = "".



DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. 
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-tot-msf AS DEC NO-UNDO.
def NEW shared var factor# as decimal no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .


/* DEF VAR ll-not-disc AS LOG NO-UNDO.
  
    ll-not-disc = IF prmtb_not_disc EQ "yes" THEN TRUE ELSE FALSE.

    
  FIND FIRST reftable
      WHERE reftable.reftable EQ "GLACCTDISC"
        AND reftable.company  EQ prmComp
        AND reftable.loc      EQ ""
        AND reftable.code     EQ account.actnum
      NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "GLACCTDISC"
     reftable.company  = prmComp
     reftable.loc      = ""
     reftable.code     = account.actnum.
  END.
  ASSIGN
   reftable.val[1] = INT(ll-not-disc)
   tb_not-disc     = ll-not-disc. */



 IF prmAction = "Search" THEN DO:

 
    FOR EACH account WHERE account.company = prmComp
        AND (account.actnum BEGINS prmact OR prmact EQ "")
        AND (account.dscr BEGINS prmactdscr OR prmactdscr EQ "")
        AND (account.TYPE BEGINS prmactype OR prmactype EQ "")  NO-LOCK:
       

            create ttGLAccountList.
            assign
                ttGLAccountList.act         = account.actnum
                ttGLAccountList.actdscr     = account.dscr
                ttGLAccountList.actype      = account.type
                ttGLAccountList.RecKey      = account.rec_key .
            

    END.   /* end of for loop*/
            
 END. /* end search */

IF prmAction = "Add" THEN DO:
    FIND FIRST Account WHERE account.company = prmComp AND
        account.actnum = prmact NO-LOCK NO-ERROR.
    IF AVAIL account   THEN DO:
        cError = " G/L Account already exists with Company '" + prmComp  +  "' Account No " + prmact .
        RETURN.
    END.

END.
 


 IF prmAction = "Add" THEN DO:

     CREATE account .
     ASSIGN 
         account.actnum         = prmact    
         account.company        = prmComp
         account.dscr           = prmactdscr 
         account.cyr-open       = prmcry_opn 
         account.lyr-open       = prmlyr_opn 
         account.type           = prmactype
         account.bud[1]         = prmbud1    
         account.bud[2]         = prmbud2    
         account.bud[3]         = prmbud3    
         account.bud[4]         = prmbud4    
         account.bud[5]         = prmbud5    
         account.bud[6]         = prmbud6    
         account.bud[7]         = prmbud7    
         account.bud[8]         = prmbud8    
         account.bud[9]         = prmbud9    
         account.bud[10]        = prmbud10   
         account.bud[11]        = prmbud11   
         account.bud[12]        = prmbud12   
         account.bud[13]        = prmbud13   
         account.ly-bud[1]      = prmly_bud1 
         account.ly-bud[2]      = prmly_bud2 
         account.ly-bud[3]      = prmly_bud3 
         account.ly-bud[4]      = prmly_bud4 
         account.ly-bud[5]      = prmly_bud5 
         account.ly-bud[6]      = prmly_bud6 
         account.ly-bud[7]      = prmly_bud7 
         account.ly-bud[8]      = prmly_bud8 
         account.ly-bud[9]      = prmly_bud9 
         account.ly-bud[10]     = prmly_bud10
         account.ly-bud[11]     = prmly_bud11
         account.ly-bud[12]     = prmly_bud12
         account.ly-bud[13]     = prmly_bud13
         tb_not-disc            = IF prmtb_not_disc = "True" THEN TRUE ELSE FALSE .

         FIND FIRST reftable
             WHERE reftable.reftable EQ "GLACCTDISC"
             AND reftable.company  EQ prmComp
             AND reftable.loc      EQ ""
             AND reftable.code     EQ account.actnum
             NO-ERROR.
         IF NOT AVAIL reftable THEN DO:
             CREATE reftable.
             ASSIGN
                 reftable.reftable = "GLACCTDISC"
                 reftable.company  = prmComp
                 reftable.loc      = ""
                 reftable.code     = account.actnum.
          END.

          ASSIGN
              reftable.val[1] = INT(tb_not-disc) .


     ASSIGN prmAction = "View"
            prmRecKey = account.rec_key  .
                                 
 END.

 IF prmAction = "Update" THEN DO:
     
     FIND FIRST account WHERE account.company = prmComp 
         AND account.rec_key = prmRecKey EXCLUSIVE-LOCK NO-ERROR.

     ASSIGN
         account.dscr           = prmactdscr 
         account.cyr-open       = prmcry_opn 
         account.lyr-open       = prmlyr_opn 
         account.type           = prmactype
         account.bud[1]         = prmbud1    
         account.bud[2]         = prmbud2    
         account.bud[3]         = prmbud3    
         account.bud[4]         = prmbud4    
         account.bud[5]         = prmbud5    
         account.bud[6]         = prmbud6    
         account.bud[7]         = prmbud7    
         account.bud[8]         = prmbud8    
         account.bud[9]         = prmbud9    
         account.bud[10]        = prmbud10   
         account.bud[11]        = prmbud11   
         account.bud[12]        = prmbud12   
         account.bud[13]        = prmbud13   
         account.ly-bud[1]      = prmly_bud1 
         account.ly-bud[2]      = prmly_bud2 
         account.ly-bud[3]      = prmly_bud3 
         account.ly-bud[4]      = prmly_bud4 
         account.ly-bud[5]      = prmly_bud5 
         account.ly-bud[6]      = prmly_bud6 
         account.ly-bud[7]      = prmly_bud7 
         account.ly-bud[8]      = prmly_bud8 
         account.ly-bud[9]      = prmly_bud9 
         account.ly-bud[10]     = prmly_bud10
         account.ly-bud[11]     = prmly_bud11
         account.ly-bud[12]     = prmly_bud12
         account.ly-bud[13]     = prmly_bud13
         tb_not-disc            = IF prmtb_not_disc = "True" THEN TRUE ELSE FALSE .

         FIND FIRST reftable
             WHERE reftable.reftable EQ "GLACCTDISC"
             AND reftable.company  EQ prmComp
             AND reftable.loc      EQ ""
             AND reftable.code     EQ account.actnum
             NO-ERROR.
         IF NOT AVAIL reftable THEN DO:
             CREATE reftable.
             ASSIGN
                 reftable.reftable = "GLACCTDISC"
                 reftable.company  = prmComp
                 reftable.loc      = ""
                 reftable.code     = account.actnum.
          END.

          ASSIGN
              reftable.val[1] = INT(tb_not-disc) .


     ASSIGN prmAction = "View" .


 END.

IF prmAction = "Delete" THEN DO:
    
    FIND FIRST account WHERE account.company = prmComp 
        AND account.actnum = prmact
        AND account.rec_key = prmRecKey NO-LOCK NO-ERROR.

    IF CAN-FIND(FIRST glhist
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum)  OR
    CAN-FIND(FIRST gltrans
            WHERE gltrans.company EQ account.company
              AND gltrans.actnum  EQ account.actnum) THEN DO:
        cError = "Transactions exist for this account, deletion not permitted..." .
        RETURN .
    END.

END.  /* end of delete validation */

 IF prmAction = "Delete" THEN DO:
    
     FIND FIRST account WHERE account.company = prmComp 
         AND account.rec_key = prmRecKey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL account THEN DELETE account.

     FIND LAST account WHERE account.company = prmComp NO-LOCK NO-ERROR.

     IF AVAIL account THEN
         ASSIGN
         prmRecKey = account.rec_key
         prmAction = "view" .

 END.

 IF prmAction = "View" THEN DO:
     FIND FIRST account WHERE account.company = prmComp 
         AND account.rec_key = prmRecKey NO-LOCK NO-ERROR.

     create ttGLAccountList.
            assign
                ttGLAccountList.act         = account.actnum  
                ttGLAccountList.actdscr     = account.dscr    
                ttGLAccountList.actype      = account.type    
                ttGLAccountList.cry_opn     = account.cyr-open
                ttGLAccountList.lyr_opn     = account.lyr-open
                ttGLAccountList.bud1        = account.bud[1] 
                ttGLAccountList.bud2        = account.bud[2] 
                ttGLAccountList.bud3        = account.bud[3]                 
                ttGLAccountList.bud4        = account.bud[4] 
                ttGLAccountList.bud5        = account.bud[5] 
                ttGLAccountList.bud6        = account.bud[6] 
                ttGLAccountList.bud7        = account.bud[7] 
                ttGLAccountList.bud8        = account.bud[8] 
                ttGLAccountList.bud9        = account.bud[9] 
                ttGLAccountList.bud10       = account.bud[10]
                ttGLAccountList.bud11       = account.bud[11]
                ttGLAccountList.bud12       = account.bud[12]
                ttGLAccountList.bud13       = account.bud[13]
                ttGLAccountList.ly_bud1     = account.ly-bud[1]  
                ttGLAccountList.ly_bud2     = account.ly-bud[2]  
                ttGLAccountList.ly_bud3     = account.ly-bud[3]  
                ttGLAccountList.ly_bud4     = account.ly-bud[4]  
                ttGLAccountList.ly_bud5     = account.ly-bud[5]  
                ttGLAccountList.ly_bud6     = account.ly-bud[6]  
                ttGLAccountList.ly_bud7     = account.ly-bud[7]  
                ttGLAccountList.ly_bud8     = account.ly-bud[8]  
                ttGLAccountList.ly_bud9     = account.ly-bud[9]  
                ttGLAccountList.ly_bud10    = account.ly-bud[10] 
                ttGLAccountList.ly_bud11    = account.ly-bud[11] 
                ttGLAccountList.ly_bud12    = account.ly-bud[12] 
                ttGLAccountList.ly_bud13    = account.ly-bud[13] 
                ttGLAccountList.lyr1        = account.lyr[1] 
                ttGLAccountList.lyr2        = account.lyr[2] 
                ttGLAccountList.lyr3        = account.lyr[3] 
                ttGLAccountList.lyr4        = account.lyr[4] 
                ttGLAccountList.lyr5        = account.lyr[5] 
                ttGLAccountList.lyr6        = account.lyr[6] 
                ttGLAccountList.lyr7        = account.lyr[7] 
                ttGLAccountList.lyr8        = account.lyr[8] 
                ttGLAccountList.lyr9        = account.lyr[9] 
                ttGLAccountList.lyr10       = account.lyr[10]
                ttGLAccountList.lyr11       = account.lyr[11]
                ttGLAccountList.lyr12       = account.lyr[12]
                ttGLAccountList.lyr13       = account.lyr[13]
                /*ttGLAccountList.cyr1        = account.cyr[1] 
                ttGLAccountList.cyr2        = account.cyr[2] 
                ttGLAccountList.cyr3        = account.cyr[3] 
                ttGLAccountList.cyr4        = account.cyr[4] 
                ttGLAccountList.cyr5        = account.cyr[5] 
                ttGLAccountList.cyr6        = account.cyr[6] 
                ttGLAccountList.cyr7        = account.cyr[7] 
                ttGLAccountList.cyr8        = account.cyr[8] 
                ttGLAccountList.cyr9        = account.cyr[9] 
                ttGLAccountList.cyr10       = account.cyr[10] 
                ttGLAccountList.cyr11       = account.cyr[11]
                ttGLAccountList.cyr12       = account.cyr[12]
                ttGLAccountList.cyr13       = account.cyr[13]*/
                
                ttGLAccountList.RecKey      = account.rec_key  .






  DEF VAR li AS INT NO-UNDO.
  DEF VAR ld-period$ AS DEC NO-UNDO EXTENT 20.
  DEF VAR li-fisc-yr AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL account AND account.actnum NE "" THEN
  FIND FIRST reftable
      WHERE reftable.reftable EQ "GLACCTDISC"
        AND reftable.company  EQ prmComp
        AND reftable.loc      EQ ""
        AND reftable.code     EQ account.actnum
      NO-LOCK NO-ERROR.
  tb_not-disc = AVAIL reftable AND reftable.val[1] EQ 1.

  ASSIGN
      ttGLAccountList.tb_not_disc = string(tb_not-disc) .

     find first company where company.company eq account.company no-lock no-error.

    do li = 1 to 13:
      ld-period$[li] = account.cyr[li].
    end.

    for each period
        where period.company eq account.company
          and period.pstat   eq yes
        no-lock,

        each gltrans
        where gltrans.company eq account.company
          and gltrans.actnum  eq account.actnum
          and gltrans.period  eq period.pnum
          and gltrans.tr-date ge period.pst
          and gltrans.tr-date le period.pend
            no-lock:

          ld-period$[period.pnum] = ld-period$[period.pnum] + gltrans.tr-amt.
    end.

    assign
     ttGLAccountList.cyr1  = (ld-period$[01])
     ttGLAccountList.cyr2  = (ld-period$[02])
     ttGLAccountList.cyr3  = (ld-period$[03])
     ttGLAccountList.cyr4  = (ld-period$[04])
     ttGLAccountList.cyr5  = (ld-period$[05])
     ttGLAccountList.cyr6  = (ld-period$[06])
     ttGLAccountList.cyr7  = (ld-period$[07])
     ttGLAccountList.cyr8  = (ld-period$[08])
     ttGLAccountList.cyr9  = (ld-period$[09])
     ttGLAccountList.cyr10 = (ld-period$[10])
     ttGLAccountList.cyr11 = (ld-period$[11])
     ttGLAccountList.cyr12 = (ld-period$[12])
     ttGLAccountList.cyr13 = (ld-period$[13]).

 END.

 IF prmbtn = "btn" THEN DO:

  DEF VAR ll-process AS LOG NO-UNDO.
  

  DEF BUFFER b-account FOR account.


  FIND b-account EXCLUSIVE WHERE ROWID(b-account) EQ ROWID(account)
      NO-WAIT NO-ERROR.

  IF AVAIL b-account THEN DO:
    cError = "Are you sure you wish to copy Current Year Balances to Current Year Budgets?".

    IF ll-process THEN DO: 
      ASSIGN
       prmbud1   = prmcyr1 
       prmbud2   = prmcyr2 
       prmbud3   = prmcyr3 
       prmbud4   = prmcyr4 
       prmbud5   = prmcyr5 
       prmbud6   = prmcyr6 
       prmbud7   = prmcyr7 
       prmbud8   = prmcyr8 
       prmbud9   = prmcyr9 
       prmbud10  = prmcyr10
       prmbud11  = prmcyr11
       prmbud12  = prmcyr12
       prmbud13  = prmcyr13

       b-account.bud[01] = DEC(prmcyr1)
       b-account.bud[02] = DEC(prmcyr2)
       b-account.bud[03] = DEC(prmcyr3)
       b-account.bud[04] = DEC(prmcyr4)
       b-account.bud[05] = DEC(prmcyr5)
       b-account.bud[06] = DEC(prmcyr6)
       b-account.bud[07] = DEC(prmcyr7)
       b-account.bud[08] = DEC(prmcyr8)
       b-account.bud[09] = DEC(prmcyr9)
       b-account.bud[10] = DEC(prmcyr10)
       b-account.bud[11] = DEC(prmcyr11)
       b-account.bud[12] = DEC(prmcyr12)
       b-account.bud[13] = DEC(prmcyr13).
    END.

    FIND CURRENT b-account NO-LOCK NO-ERROR.
  END.


 END.
