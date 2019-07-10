/* lib/isbackup.p
 *
 */

{lib/protop.i}

define input-output parameter alertStatus as logical.

define variable currDT  as integer no-undo.
define variable fBackUp as integer no-undo.
define variable iBackUp as integer no-undo.

/* is an online backup running?
 */

for each dictdb._userStatus no-lock where _userStatus-operation <> ?:
  if _userStatus-operation = "online backup" then alertStatus = no.
end.

/* if the backup completed within the sample interval conisder it to be active and set alertStatus = no
 */

find dictdb._DbStatus no-lock.

assign
  currDT =  uDateTime()
  fBackUp = ( currDT - string2uDateTime( _dbStatus-fbDate ))
  iBackUp = ( currDT - string2uDateTime( _dbStatus-ibDate ))
.

if fBackup <> ? and fBackup < 300 then alertStatus = no.
if iBackup <> ? and iBackup < 300 then alertStatus = no.

return.
