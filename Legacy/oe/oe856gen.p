/*------------------------------------------------------------------------
    File        : oe856gen.p
    Purpose     : 

    Syntax      :

    Description : ASN program to create edi asn records (edshtran ...)

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/***************************************************************************/

{edi/sharedv.i "new"}
def input param bol_rec             as recid   no-undo.
def input param original_print      AS LOGICAL NO-UNDO.
def input param force_asn           AS LOGICAL INITIAL FALSE.

DEFINE VARIABLE run_program AS CHARACTER NO-UNDO.

/*DEF VAR run_ok              AS LOGICAL NO-UNDO.             */
/*def var searchx             as int no-undo.                 */
/*DEF VAR inv_or_credit       AS LOGICAL INITIAL TRUE NO-UNDO.*/
/*def var did_some            as logical no-undo.             */

FIND oe-bolh where recid(oe-bolh) = bol_rec no-lock no-error.
if not avail oe-bolh then return.

ASSIGN
  ws_process_rec    = RECID(oe-bolh)
  .
  
EDI-FIND-LOOP:
DO:
  FIND FIRST edmast WHERE edmast.cust EQ oe-bolh.cust-no NO-LOCK NO-ERROR.
  IF NOT AVAILABLE edmast THEN  LEAVE EDI-FIND-LOOP.
  ws_partner = edmast.partner.

  find edco where edco.company = oe-bolh.company no-lock no-error.
  ASSIGN ws_edmast_rec = RECID(edmast) .
  find first edcode where edcode.partner eq edmast.partner
   and edcode.setid = "856" no-lock no-error.

  /* 856 process (shipment advice) */
  IF original_print OR force_asn THEN
  EDI-856-LOOP: /* 9601 CAH: No ASN on reprinted oe-bolh */
  DO:
    ws_setid = "856".
    run_program = "oe/oe856asn.p".
  
    DO:
      ASSIGN ws_edcode_rec = RECID(edcode).
      RUN VALUE(run_program).      
    END.
  END. /* end of EDI-856-LOOP */
END. /* end of EDI-FIND-LOOP */
