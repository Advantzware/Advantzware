...ed/asi/exparinv.p                    12/03/99 16:12:29                                                       PROGRESS(R) Page 1   

{} Line Blk
-- ---- ---
      1     /***************************************************************************\
      2     *****************************************************************************
      3     **  Program: e:\asi\patch\edi\ed\exparin
      4     **       By: Chris Heins, for Advanced Software, Inc. for Sonoco Packaging.
      5     ** Descript: (c) 1998 Sonoco, All Rights Reserved.
      6     **
      7     *****************************************************************************
      8     \***************************************************************************/
      9     DEF INPUT PARAM p_file AS char NO-UNDO. /* arinv or inv-head */
     10     DEF INPUT PARAM p_rec  AS RECID NO-UNDO.
     11     DEF STREAM s-export.
     12     /* include files */
     13     {rc/stringv.i}
 1    1     /*
 1    2     07.30.98 by CAH on \\ricky\robj8\ Log#0000:
 1    3     1.  Added arg 1 for sharing. 
 1    4     
 1    5     */
 1    6     def {1} var str_buffa   as character format 'x(30)' no-undo.
 1    7     def {1} var str_buffb   as character format 'x(30)' no-undo.
 1    8     def {1} var str_xx      as integer no-undo.
 1    9     def {1} var str_yy      as integer no-undo.
 1   10     def {1} var str_zz      as integer no-undo.
 1   11     def {1} var str_la      as integer no-undo.
 1   12     def {1} var str_lb      as integer no-undo.
     13      
     14     /* contants and literals */
     15     DEF VAR FILLER          AS char NO-UNDO     INITIAL "".
     16     DEF var ws_plant        AS char NO-UNDO     INITIAL "A720".
     17     DEF VAR export_fid      AS char NO-UNDO     initial "SONO_INV.TXT".
     18     /* local variables */
     19     DEF var tax_basis       AS decimal NO-UNDO  FORMAT '-99999999.99999999'.
     20     DEF var line_tax        AS decimal NO-UNDO  FORMAT '-99999999.99999999'.
     21     DEF var line_count      AS int NO-UNDO.
     22     DEF var retry_counter   AS int  NO-UNDO.
     23     DEF var fancy_date      AS char NO-UNDO.    /* yyyy-mm-dd */
     24     DEF var ws_due-date     AS date NO-UNDO.
     25     DEF var ws_salesacct    AS char NO-UNDO.
     26     DEF var ws_sman         AS char NO-UNDO.    /* misc has no sman, take 1st */
     27     DEF var ws_po-no        AS char NO-UNDO.
     28     DEF var xx              AS int NO-UNDO.
     29     def var ws_tilde        as char no-undo initial "~~".
     30     IF NOT CAN-DO("ar-inv,inv-head", p_file) THEN
     31     RETURN error.
     32     IF p_file = "ar-inv" THEN
     33   1 DO:
     34   1   FIND ar-inv WHERE RECID(ar-inv) = p_rec EXCLUSIVE-LOCK NO-ERROR.
     35   1   IF NOT AVAIL ar-inv THEN
     36   1   RETURN error.
     37   1   
     38   1   if not (ar-inv.posted and ar-inv.printed) then return error.
...ed/asi/exparinv.p                    12/03/99 16:12:29                                                       PROGRESS(R) Page 2   

{} Line Blk
-- ---- ---
     39   1   /* determine if the invoice has any lines, and determine the total taxable */
     40   1   line_count = 0.
     41   1   FOR EACH ar-invl NO-LOCK
     42   1       WHERE ar-invl.x-no = ar-inv.x-no
     43   2       and ar-invl.posted:
     44   2     line_count = line_count + 1.
     45   2     IF ar-invl.tax = TRUE
     46   2       THEN
     47   2     ASSIGN tax_basis = tax_basis + ar-invl.amt.
     48   1   END.
     49     END.    /* ar-inv */
     50     IF p_file = "inv-head" THEN
     51   1 DO:
     52   1   FIND inv-head WHERE RECID(inv-head) = p_rec EXCLUSIVE-LOCK NO-ERROR.
     53   1   IF NOT AVAIL inv-head THEN
     54   1   RETURN error.
     55   1   if inv-head.deleted then return error.
     56   1   /*
     57   1   if not (inv-head.posted and inv-head.printed) then return error.
     58   1   */
     59   1   ASSIGN
     60   1     ws_sman = ?
     61   1     ws_po-no = ?.
     62   1   /* determine if the invoice has any lines, and determine the total taxable */
     63   1   line_count = 0.
     64   1   FOR EACH inv-line NO-LOCK
     65   1       WHERE inv-line.r-no = inv-head.r-no
     66   2       and not (inv-line.deleted):
     67   2     line_count = line_count + 1.
     68   2     IF inv-line.tax = TRUE
     69   2       THEN
     70   2     ASSIGN tax_basis = tax_basis + inv-line.amt.
     71   2     IF ws_sman = ?
     72   2       THEN
     73   2     _sman:
     74   3     DO xx = 1 TO 3:
     75   3       IF inv-line.sman[xx] > "" THEN
     76   4       DO:
     77   4         ASSIGN ws_sman = inv-line.sman[xx].
     78   4         LEAVE _sman.
     79   3       END.
     80   2     END.  /* sman default */
     81   2     IF ws_po-no = ?
     82   2       AND inv-line.po-no > ""
     83   2       THEN
     84   2     ws_po-no = inv-line.po-no.
     85   1   END.
     86   1   FOR EACH inv-misc NO-LOCK
     87   1       WHERE inv-misc.r-no = inv-head.r-no
     88   2       and not(inv-line.deleted):
     89   2     line_count = line_count + 1.
...ed/asi/exparinv.p                    12/03/99 16:12:29                                                       PROGRESS(R) Page 3   

{} Line Blk
-- ---- ---
     90   2     IF inv-misc.tax = TRUE
     91   2       THEN
     92   2     ASSIGN tax_basis = tax_basis + inv-misc.amt.
     93   1   END.
     94     END.    /* inv-head */
     95     IF line_count = 0 THEN
     96     RETURN ERROR.
     97     _open:
     98   1 DO ON error UNDO, RETRY:
     99   1   error-status:error = FALSE.
    100   1   OUTPUT STREAM s-export TO VALUE(export_fid) append.
    101   1   IF OS-ERROR > 0 THEN
...ed/asi/exparinv.p                    12/03/99 16:12:29                                                       PROGRESS(R) Page 4   

     File Name       Line Blk. Type Tran            Blk. Label            
-------------------- ---- --------- ---- --------------------------------
...ed/asi/exparinv.p    0 Procedure Yes                                   
    Buffers: asi.inv-line
             asi.inv-head
             asi.ar-inv

...ed/asi/exparinv.p   32 Do        No                                    
...ed/asi/exparinv.p   41 For       No                                    
    Buffers: asi.ar-invl

...ed/asi/exparinv.p   50 Do        No                                    
...ed/asi/exparinv.p   64 For       No                                    
...ed/asi/exparinv.p   74 Do        No   _sman                            
...ed/asi/exparinv.p   75 Do        No                                    
...ed/asi/exparinv.p   86 For       No                                    
    Buffers: asi.inv-misc

...ed/asi/exparinv.p   98 Do        No   _open                            
