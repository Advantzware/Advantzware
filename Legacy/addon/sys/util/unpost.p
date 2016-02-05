/* unpost utility */

for each machtran where end_date >= 7/30/01 and end_date <= 08/01/01 and
               machtran.posted :
     FOR EACH machemp EXCLUSIVE-LOCK
         WHERE machemp.table_rec_key = machtran.rec_key
           AND machemp.posted:
         machemp.posted = no. 
    END.
    machtran.posted = no.             
end.
for each machtran where end_date = 7/29/01 and
               machtran.posted ,
    EACH machemp 
        WHERE machemp.table_rec_key = machtran.rec_key
          AND machemp.shift Eq "5"
          AND machemp.posted :
   
    machemp.posted = no. 
    machtran.posted = no. 
END.
               
