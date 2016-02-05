/* custom/startprt.p Print document thru direct context functions 
                     Docs to Fax printer to fax */

{custom/windows.i}
{custom/printfile.i}

  DEFINE VARIABLE windir        AS CHAR.
  DEFINE VARIABLE pdocname      AS MEMPTR.
  DEFINE VARIABLE poutbuf       AS MEMPTR.
  DEFINE VARIABLE lpdocinfo     AS MEMPTR.
  DEFINE VARIABLE pfilename     AS MEMPTR.
 
  DEFINE VARIABLE outsize       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE printerhDC    AS INTEGER   NO-UNDO.     
  DEFINE VARIABLE apistatus     AS INTEGER   NO-UNDO.
 
  DEFINE VARIABLE docname       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE devicebuf     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE driverbuf     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE initbuf       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE outbuf        AS CHARACTER NO-UNDO.  
  DEFINE VARIABLE windowsdir    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE winini        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE printerdev    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE FILENAME      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tekst         AS CHARACTER NO-UNDO.
 
  /* Get printer info from WIN.INI */
  windir = FILL("x", 260).
  RUN GetWindowsDirectoryA IN hpApi(OUTPUT windir,
                                    LENGTH(windir), 
                                    OUTPUT outsize).

  ASSIGN windowsdir = SUBSTRING(windir,1,outsize)
         winini     = windowsdir + "\WIN.INI".   
  LOAD winini.       
  USE winini.
  GET-KEY-VALUE SECTION "windows" KEY "device" VALUE printerdev. 
  UNLOAD winini.
  IF printerdev = "" THEN DO:
      RUN Err ("Could not locate printer device!"). 
      RETURN.
  END. 

  
  ASSIGN devicebuf = ENTRY(1,printerdev)
         driverbuf = ENTRY(2,printerdev)
         outbuf    = ENTRY(3,printerdev).  

 MESSAGE printerdev VIEW-AS ALERT-BOX.
 /*====*/
/* ysk added */
DEF VAR prt-port AS cha NO-UNDO.
/*RUN custom/printapi.p (OUTPUT prt-port, OUTPUT prt-names).*/
RUN custom/d-print2.w (OUTPUT prt-port).
        
ASSIGN devicebuf = prt-port
       driverbuf = "winspool"
       outbuf    = "SHRFAX:".  

/*      
ASSIGN devicebuf = "Fax"
        driverbuf = "winspool"
        outbuf    = "Ne01:".  
 */       
/* end of ysk mods */
/*=======*/


  docname = "c:\tmp\fax.txt".
  /* Setup pointers to the strings needed in the lpdocinfo STRUCT */
  SET-SIZE(pdocname)     = LENGTH(docname) + 1.
  PUT-STRING(pdocname,1) = docname.  
  SET-SIZE(poutbuf)      = LENGTH(outbuf) + 1.
  PUT-STRING(poutbuf,1)  = outbuf.
 
  /* Load up the lpdocinfo STRUCT */
  SET-SIZE(lpdocinfo)    =   4    /* int cbSize          */
                           + 4    /* pointer lpszDocName */
                           + 4.   /* pointer lpszOutput  */
  PUT-LONG(lpdocinfo,1) =  12.    /* size of the STRUCT  */ 
  PUT-LONG(lpdocinfo,5)  = GET-POINTER-VALUE(pdocname). /* pointer to CHAR */ 
  PUT-LONG(lpdocinfo,9)  = GET-POINTER-VALUE(poutbuf).  /* pointer to CHAR */
 
  tekst = "bla bla  HEEEEEEEE".
  tekst = docname.
  /* Print it! */
  RUN adecomm/_setcurs.p ("WAIT").
                                     /* 3rd param outbuf => 0 ysk */
  /*RUN CreateDCA (driverbuf, devicebuf, outbuf, 0, OUTPUT printerhDC). */
  RUN CreateDCA (driverbuf, devicebuf, 0, 0, OUTPUT printerhDC). 

  RUN StartDocA IN hpApi(INPUT printerhDC, 
                         INPUT GET-POINTER-VALUE(lpdocinfo), 
                         OUTPUT apistatus). /* is printjob id */


  RUN StartPage IN hpApi(INPUT printerhDC, OUTPUT apistatus).


  RUN TextOutA IN hpApi(printerhDC, 800, 450, tekst, LENGTH(tekst), OUTPUT apistatus).
 
  IF apistatus=0  /* 0=FALSE */ THEN 
     MESSAGE "There was an error during TextOut "
             VIEW-AS ALERT-BOX ERROR.
 
  RUN EndPage IN hpApi(printerhDC, OUTPUT apistatus).
  RUN EndDoc IN hpApi(printerhDC, OUTPUT apistatus).
  RUN DeleteDC(printerhDC, OUTPUT apistatus).
 
  /* Clean Up */
  SET-SIZE(lpdocinfo) = 0.
  SET-SIZE(pdocname)  = 0.
  SET-SIZE(poutbuf)   = 0.
  RUN adecomm/_setcurs.p ("").
