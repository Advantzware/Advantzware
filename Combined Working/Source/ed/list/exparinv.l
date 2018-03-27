...edi/ed/exparinv.p                    12/03/99 16:41:53                                                       PROGRESS(R) Page 1   

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
     29     IF NOT CAN-DO("ar-inv,inv-head", p_file) THEN
     30     RETURN error.
     31     IF p_file = "ar-inv" THEN
     32   1 DO:
     33   1   FIND ar-inv WHERE RECID(ar-inv) = p_rec EXCLUSIVE-LOCK NO-ERROR.
     34   1   IF NOT AVAIL ar-inv THEN
     35   1   RETURN error.
     36   1   
     37   1   if not (ar-inv.posted and ar-inv.printed) then return error.
     38   1   /* determine if the invoice has any lines, and determine the total taxable */
...edi/ed/exparinv.p                    12/03/99 16:41:53                                                       PROGRESS(R) Page 2   

{} Line Blk
-- ---- ---
     39   1   line_count = 0.
     40   1   FOR EACH ar-invl NO-LOCK
     41   1       WHERE ar-invl.x-no = ar-inv.x-no
     42   2       and ar-invl.posted:
     43   2     line_count = line_count + 1.
     44   2     IF ar-invl.tax = TRUE
     45   2       THEN
     46   2     ASSIGN tax_basis = tax_basis + ar-invl.amt.
     47   1   END.
     48     END.    /* ar-inv */
     49     IF p_file = "inv-head" THEN
     50   1 DO:
     51   1   FIND inv-head WHERE RECID(inv-head) = p_rec EXCLUSIVE-LOCK NO-ERROR.
     52   1   IF NOT AVAIL inv-head THEN
     53   1   RETURN error.
     54   1   if inv-head.deleted then return error.
     55   1   /*
     56   1   if not (inv-head.posted and inv-head.printed) then return error.
     57   1   */
     58   1   ASSIGN
     59   1     ws_sman = ?
     60   1     ws_po-no = ?.
     61   1   /* determine if the invoice has any lines, and determine the total taxable */
     62   1   line_count = 0.
     63   1   FOR EACH inv-line NO-LOCK
     64   1       WHERE inv-line.r-no = inv-head.r-no
     65   2       and not (inv-line.deleted):
     66   2     line_count = line_count + 1.
     67   2     IF inv-line.tax = TRUE
     68   2       THEN
     69   2     ASSIGN tax_basis = tax_basis + inv-line.amt.
     70   2     IF ws_sman = ?
     71   2       THEN
     72   2     _sman:
     73   3     DO xx = 1 TO 3:
     74   3       IF inv-line.sman[xx] > "" THEN
     75   4       DO:
     76   4         ASSIGN ws_sman = inv-line.sman[xx].
     77   4         LEAVE _sman.
     78   3       END.
     79   2     END.  /* sman default */
     80   2     IF ws_po-no = ?
     81   2       AND inv-line.po-no > ""
     82   2       THEN
     83   2     ws_po-no = inv-line.po-no.
     84   1   END.
     85   1   FOR EACH inv-misc NO-LOCK
     86   1       WHERE inv-misc.r-no = inv-head.r-no
     87   2       and not(inv-line.deleted):
     88   2     line_count = line_count + 1.
     89   2     IF inv-misc.tax = TRUE
...edi/ed/exparinv.p                    12/03/99 16:41:53                                                       PROGRESS(R) Page 3   

{} Line Blk
-- ---- ---
     90   2       THEN
     91   2     ASSIGN tax_basis = tax_basis + inv-misc.amt.
     92   1   END.
     93     END.    /* inv-head */
     94     IF line_count = 0 THEN
     95     RETURN ERROR.
     96     _open:
     97   1 DO ON error UNDO, RETRY:
     98   1   error-status:error = FALSE.
     99   1   OUTPUT STREAM s-export TO VALUE(export_fid) append.
    100   1   IF OS-ERROR > 0 THEN
...edi/ed/exparinv.p                    12/03/99 16:41:53                                                       PROGRESS(R) Page 4   

     File Name       Line Blk. Type Tran            Blk. Label            
-------------------- ---- --------- ---- --------------------------------
...edi/ed/exparinv.p    0 Procedure Yes                                   
    Buffers: asi.inv-line
             asi.inv-head
             asi.ar-inv

...edi/ed/exparinv.p   31 Do        No                                    
...edi/ed/exparinv.p   40 For       No                                    
    Buffers: asi.ar-invl

...edi/ed/exparinv.p   49 Do        No                                    
...edi/ed/exparinv.p   63 For       No                                    
...edi/ed/exparinv.p   73 Do        No   _sman                            
...edi/ed/exparinv.p   74 Do        No                                    
...edi/ed/exparinv.p   85 For       No                                    
    Buffers: asi.inv-misc

...edi/ed/exparinv.p   97 Do        No   _open                            
