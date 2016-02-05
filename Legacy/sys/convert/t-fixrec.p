
def buffer bf-est for est.
def var ls-key as cha no-undo.


for each est no-lock by est-no.
find first bf-est where bf-est.rec_key = est.rec_key and
            recid(bf-est) <> recid(est) no-lock no-error.
if avail bf-est then do:
   disp est.rec_key est.est-no.
   /*
   for each bf-est where bf-est.rec_key = est.rec_key and
                         recid(bf-est) <> recid(est).
                         
        disp bf-est.rec_key bf-est.est-no.
        
        ls-key = string(today,"99999999") +
                 string(next-value(rec_key_seq,nosweat),"99999999").

        bf-est.rec_key = ls-key.               

        create rec_key.
        assign rec_key.rec_key = bf-est.rec_key
               rec_key.table_name = "est".


*/
   
   /*
   for each notes where notes.rec_key = est.rec_key.
      disp note_text form "x(30)" view-as fill-in.
      
   */
