def buffer bf-reftable for reftable.
def var cnt as int.
def stream s1.
DEF STREAM s2.
disable triggers for load of bf-reftable.

output stream s1 to c:\tmp\reftable-phone-save.d append.
OUTPUT STREAM s2 TO c:\tmp\reftable-phone-reckey.d APPEND.
for each emailcod no-lock:

  for each reftable where reftable.reftable = ""
    and reftable.code = emailcod.emailcod no-lock:
    find first phone where recid(phone) = int(reftable.rec_key) no-lock no-error.
    if avail phone then do:
      export stream s1 reftable.
      find bf-reftable where rowid(bf-reftable) eq rowid(reftable) exclusive-lock.
      /* Replaces recid(phone) */
      bf-reftable.rec_key = STRING(phone.rec_key).
      cnt = cnt + 1.
    end.
  end.
end.
output stream s1 close.

for each phone no-lock:

  for each reftable where reftable.reftable = ""
    and reftable.rec_key = phone.TABLE_rec_key no-lock:

  

      export stream s2 reftable.
      find bf-reftable where rowid(bf-reftable) eq rowid(reftable) exclusive-lock.
      /* Replaces recid(phone) */
      bf-reftable.rec_key = STRING(phone.rec_key).
      cnt = cnt + 1.

  end.




end.
output stream s1 close.
