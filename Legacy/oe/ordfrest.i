                                   
DEFINE INPUT PARAMETER ip-new-ord AS LOG NO-UNDO.


SESSION:SET-WAIT-STATE("general").



FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

FOR EACH w-ord:
  DELETE w-ord.
END.

FOR EACH tt-oe-ordl:
  DELETE tt-oe-ordl.
END.

ASSIGN
 v-qty-mod = ip-new-ord EQ ?
 v-est-no  = oe-ord.est-no.

RUN util/rjust.p (INPUT-OUTPUT v-est-no,8).
 
FIND FIRST xest WHERE xest.company EQ cocode
                  AND xest.est-no  EQ v-est-no 
                NO-LOCK NO-ERROR.

v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0.

IF avail xest THEN DO:

  IF ip-new-ord THEN DO:

    RUN new-order.

  END. /* if ip-new-ord */

  ELSE DO: /* if not new ord */
    RUN not-new-ord.

  END. /* ... else (not ip-new-ord) */

  SESSION:SET-WAIT-STATE('general').

  /* recalc-estimate creates order lines */
  RUN recalc-estimate.

  RELEASE cust.

  RUN release-shared-buffers.

  /*****************************************************************************/
  /****  BEGIN UPDATE ORDER LINES   ********************************************/
  /*****************************************************************************/

  RUN update-order-lines.


  /*****************************************************************************/
  /****  END UPDATE ORDER LINES     ********************************************/
  /*****************************************************************************/
  IF ll-canceled AND ip-new-ord THEN DO:
    RUN was-canceled.
  
  END. /* if ll-canceled */
  
  /*****************************************************************************/
  /****  BEGIN NOT CANCELED SECTION ********************************************/
  /*****************************************************************************/
  
  IF NOT ll-canceled THEN DO:

    RUN was-not-canceled.

  END. /* if not canceled */

  /*****************************************************************************/
  /****    END NOT CANCELED SECTION ********************************************/
  /*****************************************************************************/



  RUN recalc-itemfg-loc (INPUT ROWID(oe-ord)).


  FIND CURRENT itemfg NO-LOCK NO-ERROR.
  FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  RUN release-shared-buffers.

  RUN dispatch ('open-query').
  RUN dispatch ('row-changed').


END. /* avail xest */









