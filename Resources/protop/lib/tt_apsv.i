/* lib/tt_apsv.i
 *
 */


define new global shared temp-table tt_apsv no-undo
  field xid          as integer   format ">>9"         label "Id"
  field apsvType     as character format "x(6)"        label "Type"
  field apsvName     as character format "x(20)"       label "Broker"
  field apsvStatus   as character format "x(12)"       label "Status"
  field apsvPort     as integer   format ">>>>9"       label "Port"
  field apsvPID      as integer   format ">>>>>>>>9"   label "PID"
  field apsvUsr      as integer   format ">>>>9"       label "Usr#" initial ?

  field apsvReq      as integer   format "->>>>>>>9"   label "Requests"   /*    {&NOSERIALIZE} */

  field apsvDBAccess as integer   format "->>>>>>>9"   label "Blk Acc" 
  field apsvOSRead   as integer   format "->>>>>>9"    label "OS Rd"       
  field apsvOSWrite  as integer   format "->>>>>>9"    label "OS Wr"       
  field apsvWait     as character format "x(4)"        label "Wait"

  field apsvLineNum  as integer   format "->>>>9"      label "Line#"
  field apsvProc     as character format "x(42)"       label "Program Name"

/*  field apsvRcvd     as integer   format "->>>>>>9"    label "Rcvd"       {&NOSERIALIZE}
 *  field apsvSent     as integer   format "->>>>>>9"    label "Sent"       {&NOSERIALIZE}
 */

  index xid-idx          is unique xid
  index apsvStatus-idx   /* is primary */ apsvStatus descending apsvDBAccess descending apsvPort

/* this TT is haunted by error 1422 */

  index apsvDBAccess-idx is primary apsvDBAccess /* descending */
  index apsvReq-idx      apsvReq      /* descending */
  index apsvOSRead-idx   apsvOSRead   /* descending */
  index apsvOSWrite-idx  apsvOSWrite  /* descending */

/* removing the indexes might make a difference if it comes back */
.
