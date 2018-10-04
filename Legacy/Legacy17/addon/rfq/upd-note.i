/* rfq/upd-note.i */

IF NOT CONNECTED("asinos") THEN CONNECT VALUE("-pf ../asinos.pf") .

IF CONNECTED("asinos") THEN RUN rfq/upd-note.p (est.rec_key, rfq.rec_key).
  
/*DISCONNECT VALUE("asinos") NO-ERROR.*/
