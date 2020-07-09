/* ce/copyest.w */

{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

DEFINE VARIABLE ipEstNo AS CHARACTER NO-UNDO.
       
RUN ce/copyestN.w("","",OUTPUT ipEstNo) .
                                          
