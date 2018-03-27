{ed/sharedv.i}
DEF INPUT PARAM doc_rec AS RECID NO-UNDO.
FIND eddoc WHERE RECID(eddoc) = doc_rec NO-LOCK NO-ERROR.
IF NOT AVAIL eddoc THEN
DO:
  BELL.
  MESSAGE "Could not locate eddoc in cascade deletion: " PROGRAM-NAME(1).
  PAUSE.
  RETURN.
END.
FIND edshtran OF eddoc EXCLUSIVE-LOCK NO-ERROR.
/* 9809 CAH: Added code to search for 810 records is 856 not found */
if not avail edshtran then do:
    find edivtran of eddoc no-lock no-error.
    if not avail edivtran then return.
    else do:
        run ed/fm810del.p (recid(eddoc)).
        return.
    end.    
end.    
FOR EACH edshord
where edshord.partner = edshtran.partner
  and edshord.seq = edshtran.seq:
  DELETE edshord.
END.
FOR EACH edshtare
where edshtare.partner = edshtran.partner
  and edshtare.seq = edshtran.seq:
  DELETE edshtare.
END.
FOR EACH edshpack
where edshpack.partner = edshtran.partner
  and edshpack.seq = edshtran.seq:
  DELETE edshpack.
END.
FOR EACH edshline
where edshline.partner = edshtran.partner
  and edshline.seq = edshtran.seq:
  DELETE edshline.
END.
DELETE edshtran.
