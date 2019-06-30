/* s2k.p
 *
 * User Defined Metrics
 *
 * This module allows you to collect and monitor user defined and application 
 * specific metrics.
 *
 * Copy lib/usermon.p to "lib/" + ldbname(1) + ".p" and edit appropriately to enable
 *
 * The simplest metrics to monitor are counters that monotonically increase
 * and which bear an interesting relationship to business activity.  "Widgets
 * Shipped" might, for instance, be such a metric in your system.
 *
 */

session:add-super-procedure( this-procedure ).

subscribe to "usermon" anywhere run-procedure "userMon".

return.


/* the real work
 */

define variable i as integer no-undo.

procedure userMon:

  define output parameter ufld1 as decimal   no-undo initial ?.
  define output parameter ulbl1 as character no-undo initial "User Defined:".

  define output parameter ufld2 as decimal   no-undo.
  define output parameter ulbl2 as character no-undo.

  define output parameter ufld3 as decimal   no-undo.
  define output parameter ulbl3 as character no-undo.

  define output parameter ufld4 as decimal   no-undo.
  define output parameter ulbl4 as character no-undo.

  define output parameter ufld5 as decimal   no-undo.
  define output parameter ulbl5 as character no-undo.

  define output parameter ufld6 as decimal   no-undo.
  define output parameter ulbl6 as character no-undo.

  define output parameter ufld7 as decimal   no-undo.
  define output parameter ulbl7 as character no-undo.

  define output parameter ufld8 as decimal   no-undo.
  define output parameter ulbl8 as character no-undo. /* initial "label8:". */

  /***/
  i = i + 1.
  ufld1 = i.
  ufld8 = random( 1, 100 ).
  /***/

  return.

end.
