/* pthelp.p
 *
 */

procedure protop_help:

  display
    skip(1)
    "                                       Command Keys are Case-Sensitive                                      " skip		/* 02 */
    skip(1)
    "  d = Performance Dashboard              c = Configuration Viewer         O = OS Info (if avail)            " skip		/* 04 */
    skip(1)
    "  t = Table Activity                     l = Latches and Resources        Q = SQL activity                  " skip		/* 06 */
    "  i = Index Activity                     k = Checkpoints                  D = Disk Free Space               " skip		/* 07 */
    "  u = User IO Activity                   b = Blocked Sessions             / = Sequences                     " skip		/* 08 */
    "  x = Active Transactions                r = OE Replication Agents        N = Network Traffic               " skip		/* 09 */
    "  L = Login Brokers                      s = Server Activity              m = Multi-Tenant Info             " skip		/* 10 */
    "  a = Storage Areas                      f = File IO                      w = Who is Connected?             " skip		/* 11 */
    " ^b = Buffer Pools                       B = BigB Guesstimator           ^a = After-Imaging Info            " skip		/* 12 */
    '  j = PASOE                              @ = "Classic" App Servers        e = Application Specific          ' skip		/* 13 */
    skip(1)
    "  U = User Information Viewer            # = Set Usr#                     P = Set Process Id                " skip		/* 15 */
    "                                         * = Client Statement Cache       C = Clear Client Statement Cache  " skip		/* 16 */
    skip(1)
    "  T = Show Table and Index Range Info    6 = Set Table Name               8 = Monitor Users of a Table      " skip		/* 18 */
    "  A = Generate DBAnalys Output           7 = Set Index Name               9 = Monitor Users of an Index     " skip		/* 19 */
    skip(1)
    " ^p = programmer mode                   ^u = SQL update stats script     ^t = show session temp-tables      " skip		/* 21 */
    " ^d = dump & load scripts               ^r = dbanalys review & reports    Y = show session user*stat        " skip		/* 22 */
    "                                                                          y = run profiler on this session  " skip		/* 23 */
    skip(1)
    "  I = Set Refresh Interval               z = Modify Sort Columns          & = Show Global Properties        " skip		/* 25 */
    skip(1)
    "      R = Switch between Raw and Rate Sampling         S = Switch between Auto and On-Demand Sampling       " skip		/* 27 */
    "      p = Pause to allow manual Screen Capture              M = Send Screen Capture as an e-mail            " skip		/* 28 */
    "             Z = Set Top X Row Limit                           ^ = Modify _Lock Scan Limit                  " skip		/* 29 */
    skip(1)
    "                                         <space> = Refresh Sample                                           " skip		/* 31 */
    "                                                h, ? = Help                                                 " skip		/* 32 */
    "                                                  q = Quit                                                  " skip		/* 33 */
    skip(1)
   with
    frame show-help
    title " ProTop Help "
    row ( if screen-lines >= 40 then 4 else 1 )
    centered
    width 110
    overlay
  .

  pause.

  hide frame show-help.

end.

session:add-super-procedure( this-procedure ).

subscribe to "protop_help" anywhere run-procedure "protop_help".

return.
