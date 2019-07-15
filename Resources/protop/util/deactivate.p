/* deactivate.p
 *
 * deactivate all indexes -- you will need to rebuild all indexes after running this.
 *
 */

for each _file no-lock where _tbl-type = "t":

  /* certain well known schemas might need this...
   *
   * if _frozen = yes then _frozen = no.
   *
   */

  for each _index exclusive-lock of _file:
    _index._active = no.
  end.

end.

return.
