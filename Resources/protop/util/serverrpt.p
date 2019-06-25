/* serverrpt.p
 *
 */

define variable sql_param_name       as character no-undo format "x(30)"  label "Parameter Name".
define variable sql_param_value      as character no-undo format "x(16)"  label "Value".
define variable sql_param_is-default as character no-undo format "yes/no" label "Default?".

define variable sum_mpb as integer no-undo.
define variable sum_cnx as integer no-undo.

define variable i as integer no-undo format ">>9".
define variable j as integer no-undo format ">>9".

define variable svt as integer no-undo.
define variable mpb as integer no-undo.
define variable ma  as integer no-undo.

define buffer broker for _servers.

output to value( substitute( "&1.servers.rpt", ldbname( 1 ))).

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.5 &THEN

  display "This functionality requires OpenEdge 11.5 or better.".

&ELSE

for each broker no-lock where broker._server-type = "login":

  find_serverType: do i = 1 to 64:
    if       broker._srvParam-name[i] = "-ServerType" then svt = i.
     else if broker._srvParam-name[i] = "-Mpb"        then mpb = i.
     else if broker._srvParam-name[i] = "-Ma"         then ma  = i.
  end.

  i = 0.

  display substitute( "Remote &1 server on port &2", broker._srvParam-value[svt], broker._server-portnum ) format "X(80)" with frame a no-labels width 132.

  j = svt.

  do i = svt to 1 by -1:

    j = j + 1.

    display

      /* i */
      broker._srvParam-name[i] format "x(30)" label "Parameter Name"
      broker._srvParam-value[i] label "Value"
      broker._srvParam-is-default[i] label "Default?"

      space(9)

      /* j */
      broker._srvParam-name[j]       when broker._srvParam-name[j]       <> ? and broker._srvParam-value[11] <> "ABL" @ sql_param_name
      broker._srvParam-value[j]      when broker._srvParam-value[j]      <> ? and broker._srvParam-value[11] <> "ABL" @ sql_param_value
      broker._srvParam-is-default[j] when broker._srvParam-is-default[j] <> ? and broker._srvParam-value[11] <> "ABL" @ sql_param_is-default

     with
      frame b
      width 132
      down
    .

    down 1 with frame b.

  end.

  sum_mpb = sum_mpb + integer( broker._srvParam-value[mpb] ) + 1 no-error.
  sum_cnx = sum_cnx + ( integer( broker._srvParam-value[mpb] ) + 1 ) * integer( broker._srvParam-value[ma] ) no-error.

  for each _servers no-lock where _servers._server-broker-pid = broker._server-broker-pid:

    display
      _servers._server-id         label "Id"       format ">>>>>9"
      _servers._server-num        label "Server#"  format ">>>>>9"
      _servers._server-pid        label "PID"
      _servers._server-broker-pid label "Broker PID"
      _servers._server-type       label "Type"
      _servers._server-portnum    label "Port"
      _servers._server-logins     label "Logins"
     with
      frame c
      width 132
      down
    .

  end.

end.

display substitute( "To fully support these remote servers -Mn should be at least: &1, -n should be at least: &2", sum_mpb + 1, sum_cnx + 5 ) format "X(120)" with no-labels width 132.

&ENDIF

output close.

return.

