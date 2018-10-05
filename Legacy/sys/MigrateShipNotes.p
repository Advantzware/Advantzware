
/*------------------------------------------------------------------------
    File        : MigrateShipNotes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Oct 05 16:47:06 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE ship_note   AS CHARACTER NO-UNDO.
DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  
FOR EACH shipto NO-LOCK:
    ASSIGN ship_note = shipto.notes[1] + CHR(13) +
                       shipto.notes[2] + CHR(13) +
                       shipto.notes[3] + CHR(13) +
                       shipto.notes[4].
    RUN UpdateNoteShipShipto IN hNotesProcs (shipto.rec_key,
                                             ship_note).
END.

FOR EACH oe-rel NO-LOCK:
    ASSIGN ship_note = oe-rel.ship-i[1] + CHR(13) +
                   oe-rel.ship-i[2] + CHR(13) +
                   oe-rel.ship-i[3] + CHR(13) +
                   oe-rel.ship-i[4].
    RUN UpdateNoteShipOeRel IN hNotesProcs (oe-rel.rec_key,
                                           ship_note).
END.

FOR EACH oe-relh NO-LOCK:
    ASSIGN ship_note = oe-relh.ship-i[1] + CHR(13) +
                   oe-relh.ship-i[2] + CHR(13) +
                   oe-relh.ship-i[3] + CHR(13) +
                   oe-relh.ship-i[4].
    RUN UpdateNoteShipOeRelh IN hNotesProcs (oe-relh.rec_key,
                                             ship_note).
END.

FOR EACH oe-bolh NO-LOCK:
    ASSIGN ship_note = oe-bolh.ship-i[1] + CHR(13) +
                   oe-bolh.ship-i[2] + CHR(13) +
                   oe-bolh.ship-i[3] + CHR(13) +
                   oe-bolh.ship-i[4].
    RUN UpdateNoteShipOeBolh IN hNotesProcs (oe-bolh.rec_key,
                                             ship_note).
END.                                                

DELETE OBJECT hNotesProcs.