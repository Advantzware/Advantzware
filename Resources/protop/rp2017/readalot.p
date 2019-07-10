/* readalot.p
 */

define variable j as integer no-undo.
do while true:
  file-info:file-name = "readprobe.flg".
  if file-info:full-pathname = ? then leave.
  case random( 1,   9 ):
    when   1 then for each Invoice       no-lock where false: end.
    when   2 then for each Customer      no-lock where false: end.
    when   3 then for each Item          no-lock where false: end.
    when   4 then for each Order         no-lock where false: end.
    when   5 then for each Order-Line    no-lock where false: end.
    when   6 then for each Salesrep      no-lock where false: end.
    when   7 then for each State         no-lock where false: end.
    when   8 then for each Local-Default no-lock where false: end.
    when   9 then for each Ref-Call      no-lock where false: end.
  end.
  /***
  j = j + 1.
  if j modulo 50 = 0 then pause 1 no-message.
   ***/
end.

/* see page 3 of this thread for discussion of "where false":
 *
 * https://community.progress.com/community_groups/openedge_development/f/19/p/33862/106111#106111
 *
 * George Potemkin writes:
 *
 * Another example:
 * 
 * In the self-service mode
 * 
 * FOR EACH customer NO-LOCK WHERE TRUE: END.
 * 
 * is almost twice slower than
 * 
 * FOR EACH customer NO-LOCK WHERE FALSE: END.
 * 
 * Both queries create exactly the same db activity. The difference: the query
 * with WHERE FALSE does not copy the records from the so-called network
 * buffers to the client's record pool. In other words, the time needed to
 * copy a record from one part of the client's private memory to the another
 * its part almost equals to the time needed to retrieve a record from shared
 * memory using the locking protocols. Why this operation is much longer (tens
 * times) than, for example, a latch lock?
 * 
 */

