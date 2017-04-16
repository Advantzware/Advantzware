/*custom/mfvalue-rec-key-update.p*/

DEFINE INPUT PARAMETER ip-old-rec_key AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-new-rec_key AS CHAR NO-UNDO.

FOR EACH mfvalues WHERE
    mfvalues.rec_key EQ ip-old-rec_key:
    mfvalues.rec_key = ip-new-rec_key.
END.
