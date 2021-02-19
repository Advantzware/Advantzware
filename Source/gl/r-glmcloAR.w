/* gl/r-glmcloAR.w */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}  

FOR EACH prgrms EXCLUSIVE-LOCK:
  DELETE prgrms.
END.

/*RUN gl/r-glmcloM.w("AR").*/
