/* lib/apsvcnx.p
 *
 */

{lib/protop.i}
{lib/tick.i}

{lib/tt_xstat.i}

/* also defined and used in dc/apsv.p... so keep changes in sync!
 */

{lib/tt_apsv.i}

run updTick.

subscribe to "updAppSrvCnx" anywhere run-procedure "updAppSrvCnx".

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.


procedure updAppSrvCnx:

  define input parameter z_apsvDBAccess as decimal.
  define input parameter z_apsvOSRead   as decimal.
  define input parameter z_apsvOSWrite  as decimal.
  define input parameter z_apsvReq      as decimal.
  define input parameter z_apsvRcvd     as decimal.
  define input parameter z_apsvSent     as decimal.

  for each _connect no-lock where _connect-PID <> ?:

    /* assumes that all connections are local (no duplicate PIDs)
     */

    assign
      z_apsvDBAccess   = 0
      z_apsvOSRead     = 0
      z_apsvOSWrite    = 0
    .

    /* the idea here is to try to find an _connect record that has the app server's PID
     * the assumption is that such a connection is from the monitored app server
     *
     * the worry is that the PID in _connect might be a PID from a remote connection that
     * just happens to collide with the app server PID
     *
     * so this should probably also verify the connection type
     *
     * but that gets into issues of what version of Progress supports what values of those
     * fields -- it might end up being easy but it is too complicated for the moment
     *
     */

    find tt_apsv where tt_apsv.apsvPID = _connect-PID no-error.

    if available tt_apsv then
      do:

        find _UserIO no-lock where _UserIO-Id = _Connect-Id no-error.
        assign
          tt_apsv.apsvUsr  = _connect-Usr
          tt_apsv.apsvWait = _Connect-wait
          z_apsvDBAccess   = _UserIO-dbaccess
          z_apsvOSRead     = _UserIO-dbread
          z_apsvOSWrite    = _UserIO-dbwrite
        .

        lastStatement( _Connect-Id, output tt_apsv.apsvLineNum, output tt_apsv.apsvProc ).

        run update_xstat (
          input tt_apsv.xid,
          input string( _connect-PID ),
          input "m1",
          input "m2",
          input "m3",
          input "m4",
          input "m5",
          input z_apsvDBAccess,
          input z_apsvOSRead,
          input z_apsvOSWrite,
          input z_apsvReq,
          input z_apsvRcvd,
          input z_apsvSent
        ).

      end.

  end.

  run age_xstat.

  for each tt_xstat no-lock:

    find tt_apsv where tt_apsv.xid = tt_xstat.xid no-error.
    if available tt_apsv then

      assign
        tt_apsv.apsvDBAccess = tt_xstat.stat1[x] / z
        tt_apsv.apsvOSRead   = tt_xstat.stat2[x] / z
        tt_apsv.apsvOSWrite  = tt_xstat.stat3[x] / z

        /* the following work better as totals rather than rates but if you want to mess around and convince yourself... */

/*      tt_apsv.apsvReq      = tt_xstat.stat4[x] /* / z */
 *      tt_apsv.apsvRcvd     = tt_xstat.stat5[x] /* / z */
 *      tt_apsv.apsvSent     = tt_xstat.stat6[x] /* / z */
 */
      .

  end.

end.

