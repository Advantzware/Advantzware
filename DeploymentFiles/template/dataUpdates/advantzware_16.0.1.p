def buffer bf-reftable for reftable.
def var cnt as int.
def stream s1.
disable triggers for load of bf-reftable.

output stream s1 to reftable-phone-save.d append.

for each emailcod no-lock:

  for each reftable where reftable.reftable = ""
    and reftable.code = emailcod.emailcod no-lock:
    
    
    find first phone where recid(phone) = int(reftable.rec_key) no-lock no-error.
    if avail phone then do:
      export stream s1 reftable.
      find bf-reftable where rowid(bf-reftable) eq rowid(reftable) exclusive-lock.
      bf-reftable.rec_key = STRING(phone.rec_key).
      cnt = cnt + 1.
    end. /* If matching phone record found */
    
    
    
  end. /* each reftable */
  
  
  
end. /* each emailcod */


output stream s1 close.
