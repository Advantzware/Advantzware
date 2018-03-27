/* util/fxacct.p  assign account.n1,n2,n3 values */


disable triggers for load of account.

def var v-n1 as int no-undo.
def var v-n2 as int no-undo.
def var v-n3 as int no-undo.
def var v-del as int no-undo.
def var v-del2 as int no-undo.
/*
FIND FIRST company NO-LOCK .
company.acc-level
acc-dig[1]
acc-dig[2]
acc-dig[3]
  */
for each account.
   v-del = index(actnum,"-").
   v-del2 = index(substring(actnum,v-del + 1),"-").
   
   assign v-n1 = int(substring(actnum,1,v-del - 1))
          v-n2 = int(substring(actnum,v-del + 1, v-del2 - 1))
          v-n3 = int(substring(actnum,v-del + v-del2 + 1))
          .
   
   /*
disp actnum form "x(15)"
   /*n1 n2 n3 */
    v-del form ">9"
     v-del2 form ">9" 
     v-n1 v-n2 v-n3
.
*/

assign account.n1 = v-n1
      account.n2 = v-n2
      account.n3 = v-n3.
