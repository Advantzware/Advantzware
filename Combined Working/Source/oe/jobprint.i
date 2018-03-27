/* oe/jobprint.i */

DEF VAR v-print-job AS LOG NO-UNDO. /* flag for auto job ticket print Task#09180401*/
DEF VAR v-copies AS INT NO-UNDO.
DEF VAR v-printer-name AS cha NO-UNDO.

 DEF VAR lv-format AS cha NO-UNDO.
   DEF VAR lines-per-page AS INT NO-UNDO.
   DEF VAR lv-industry AS cha NO-UNDO.
   DEF VAR init-dir AS cha NO-UNDO.
   DEF VAR tmp-dir AS cha NO-UNDO.
   DEF VAR lv-prt-name AS cha NO-UNDO.
   DEF VAR list-name AS cha NO-UNDO.
   DEF VAR lv-font-no AS INT INIT 11 NO-UNDO.
   DEF VAR lv-ornt AS cha INIT "L" NO-UNDO.
   DEF VAR lv-copy AS INT NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
           and sys-ctrl.name eq "JOBPRINT" no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
    sys-ctrl.company = cocode
    sys-ctrl.name = "JOBPRINT"
    sys-ctrl.descrip = "Print Job Ticket From Order Automatically?"
    sys-ctrl.int-fld = 1
    sys-ctrl.char-fld = SESSION:PRINTER-NAME
    sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
ASSIGN v-print-job = sys-ctrl.log-fld
       v-copies = sys-ctrl.int-fld
       v-printer-name = sys-ctrl.char-fld.

/* print job ticket auto if JOBPRINT = yes task# 09180401 YSK */
IF v-print-job THEN do:

   find first xest where xest.company eq cocode
                     and xest.loc     eq locode
                     and xest.est-no  eq job.est-no use-index est-no no-error.
   IF AVAIL xest AND xest.est-type LE 4 THEN DO:
      {sys/inc/jobcard.i "F"}
      lv-format = sys-ctrl.char-fld.
      lv-industry = "Fold".
      IF lookup(lv-format,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Accord") > 0 THEN lines-per-page = 55.
   END.  
   ELSE DO:
      {sys/inc/jobcard.i "C"}
      lv-industry = "Corr".
      lv-format = sys-ctrl.char-fld.
  END.
  /*
  FIND FIRST user-print WHERE user-print.company    EQ cocode  
                          AND user-print.program-id EQ "JOBPRINT"
                          AND user-print.batch  = "" NO-LOCK NO-ERROR.
  IF NOT AVAIL user-print THEN DO TRANSACTION:
     RUN custom/d-printB.w (OUTPUT lv-prt-name,OUTPUT lv-copy).
     CREATE user-print.
     ASSIGN user-print.company = cocode
            user-print.program-id = "JOBPRINT"
            user-print.BATCH = ""
            user-print.PRINTER-NAME = lv-prt-name
            .
  END.
  IF AVAIL user-print THEN lv-prt-name = user-print.PRINTER-NAME.
  */
  lv-prt-name = v-printer-name.

  {sys/inc/print1.i}
  {sys/inc/outprint.i value(lines-per-page) }

  IF lv-industry EQ "Corr" OR 
     LOOKUP(lv-format,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Keystone,Accord") GT 0 THEN DO:
     PUT UNFORMATTED "<PRINTER" + lv-prt-name  + ">" /* FORM "x(40)" */ 
         /*"<COPIES=" + string(v-copies) + ">" FORM "x(20)"*/ . 
      
  END.

  /* generate ticket */   
  IF lv-format = "FibreFC" THEN do:  
    PUT UNFORMATTED "<FORMAT=11X17><OLANDSCAPE><P10><COPIES=" v-copies ">" .
    RUN cerep/jobfibre.p (lv-format,0). /* gdm - 07130906*/
  END.
  ELSE IF lv-format = "ARTIOS" THEN do:   /* For Fibre */    
       PUT UNFORMATTED "<OLANDSCAPE><P10><COPIES=" v-copies ">" .
       RUN cecrep/jobfibre.p (lv-format).
  END.
  ELSE IF lv-format = "CapCity" THEN do:   /* For Fibre */    
       PUT UNFORMATTED "<OLANDSCAPE><P10><COPIES=" v-copies ">" .
       RUN cecrep/jobcapcity.p (lv-format).
  END.

  /* print ticket*/
   IF lv-industry EQ "Corr" THEN DO:
       DEF VAR lv-return AS INT. 

       FILE-INFO:FILE-NAME = list-name.
       /*RUN printfile (FILE-INFO:FILE-NAME). */
       RUN printFileStat (FILE-INFO:FILE-NAME,OUTPUT lv-return ).
       MESSAGE "printed?" FILE-INFO:FILE-NAME  lv-return VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      IF lookup(lv-format,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Keystone,Accord") > 0
      THEN DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
      END.
      ELSE RUN custom/prntproc.p (list-name, lv-font-no, lv-ornt).
   END.

END.
