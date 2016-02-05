/* custom/startprt.p Print document thru direct context functions 
                     Docs to Fax printer to fax */

{custom/windows.i}
{custom/printfile.i}

  DEFINE VARIABLE windir        AS CHAR.
  DEFINE VARIABLE pdocname      AS MEMPTR.
  DEFINE VARIABLE poutbuf       AS MEMPTR.
  DEFINE VARIABLE lpdocinfo     AS MEMPTR.
  DEFINE VARIABLE pfilename     AS MEMPTR.
  DEFINE VARIABLE pdocto      AS MEMPTR.
  DEFINE VARIABLE pfaxto      AS MEMPTR.
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
/* ysk added */
DEF VAR prt-port AS cha NO-UNDO.
/*RUN custom/printapi.p (OUTPUT prt-port, OUTPUT prt-names).*/
RUN custom/d-print2.w (OUTPUT prt-port).
MESSAGE prt-port SKIP
        VIEW-AS ALERT-BOX.
ASSIGN devicebuf = prt-port
       driverbuf = ""
       outbuf    = "NE05".  

/* end of ysk mods */
 
  docname = "c:\tmp\tax.txt".
  /* Setup pointers to the strings needed in the lpdocinfo STRUCT */
  
  SET-SIZE(pdocname)     = LENGTH(docname) + 1.
  PUT-STRING(pdocname,1) = docname.  
  SET-SIZE(poutbuf)      = LENGTH(outbuf) + 1.
  PUT-STRING(poutbuf,1)  = outbuf.
  SET-SIZE(pdocto) = 20.
  PUT-STRING(pdocto,1) = "Yoosun Kim".
  SET-SIZE(pfaxto) = 20.
  PUT-STRING(pfaxto,1) = "2153697801".
  /* Load up the lpdocinfo STRUCT */
  SET-SIZE(lpdocinfo)    =   20    /* int cbSize          */
                           + 20    /* pointer lpszDocName */
                           + 20.   /* pointer lpszOutput  */
  PUT-LONG(lpdocinfo,1) =  80.    /* size of the STRUCT  */ 
  PUT-LONG(lpdocinfo,5)  = GET-POINTER-VALUE(pdocname). /* pointer to CHAR */ 
  PUT-LONG(lpdocinfo,25)  = GET-POINTER-VALUE(pdocto).  /* pointer to CHAR */
  PUT-LONG(lpdocinfo,45)  = GET-POINTER-VALUE(pfaxto).  /* pointer to CHAR */
 
  tekst = "bla bla  HEEEEEEEE".
 
  /* Print it! */
  RUN adecomm/_setcurs.p ("WAIT").

  /*=========
                                     /* 3rd param outbuf => 0 ysk */
  /*RUN CreateDCA (driverbuf, devicebuf, outbuf, 0, OUTPUT printerhDC). */
  RUN CreateDCA (driverbuf, devicebuf, 0, 0, OUTPUT printerhDC). 
  MESSAGE "printer: " printerhdc VIEW-AS ALERT-BOX.

  RUN StartDocA IN hpApi(INPUT printerhDC, 
                         INPUT GET-POINTER-VALUE(lpdocinfo), 
                         OUTPUT apistatus). /* is printjob id */
  ========= */
DEF VAR faxjobid AS cha NO-UNDO.
DEF VAR faxout_info AS cha NO-UNDO.
  RUN FaxStartPrint (INPUT devicebuf, 
                     INPUT GET-POINTER-VALUE(lpdocinfo), 
                     OUTPUT faxjobid,
                     OUTPUT faxout_info,
                     OUTPUT apistatus
                     ). /* is printjob id */
  MESSAGE "start doc:" faxjobid "," apistatus VIEW-AS ALERT-BOX.
  /*
  RUN StartPage IN hpApi(INPUT printerhDC, OUTPUT apistatus).
  */

/* ysk
  RUN TextOutA IN hpApi(printerhDC, 800, 450, tekst, LENGTH(tekst), OUTPUT apistatus).
*/ 
  IF apistatus=0  /* 0=FALSE */ THEN 
     MESSAGE "There was an error during TextOut "
             VIEW-AS ALERT-BOX ERROR.
    /*
  RUN EndPage IN hpApi(printerhDC, OUTPUT apistatus).
  

  RUN EndDoc IN hpApi(printerhDC, OUTPUT apistatus).
  RUN DeleteDC(printerhDC, OUTPUT apistatus).
  */

  /* Clean Up */
  SET-SIZE(lpdocinfo) = 0.
  SET-SIZE(pdocname)  = 0.
  SET-SIZE(poutbuf)   = 0.
  SET-SIZE(pfaxto)   = 0.
  SET-SIZE(pdocto)   = 0.
  RUN adecomm/_setcurs.p ("").




  PROCEDURE FaxStartPrint EXTERNAL "winfax" :
    DEFINE INPUT  PARAMETER PrtName   AS cha.
    DEFINE INPUT  PARAMETER FaxInfo  AS long.
    DEF OUTPUT PARAM outfaxid AS cha.
    DEF OUTPUT PARAM outfaxinfo AS cha.
    DEFINE RETURN PARAMETER oSTATUS AS LONG.
  END PROCEDURE.
