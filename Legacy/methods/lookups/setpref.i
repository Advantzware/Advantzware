/* setpref.i */

IF AVAILABLE zipcode THEN
ASSIGN
  s-pref_type = zipcode.pref_type
  s-pref# = zipcode.pref#.
