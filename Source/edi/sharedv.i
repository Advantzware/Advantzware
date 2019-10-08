/*{rc/loginv.i}*/
/*  EDI Shared Variables */
def {1} shared var ws_company   like edco.company   no-undo label "Company".
def {1} shared var ws_partner   like edmast.partner no-undo.
def {1} shared var ws_docid     like eddoc.docid    no-undo
    label "DocID".
def {1} shared var ws_setid     like edcode.setid   no-undo
    label "Set".
def {1} shared var ws_direction like edcode.direction no-undo
    label "Dir".
def {1} shared var ws_print-opt as logical no-undo
        label "Print?" initial true.

def {1} shared var ws_edco_rec      as recid no-undo.
def {1} shared var ws_edmast_rec    as recid no-undo.
def {1} shared var ws_edcode_rec    as recid no-undo.
def {1} shared var ws_eddoc_rec     as recid no-undo.

def {1} shared var ws_edpotran_rec  as recid no-undo.
def {1} shared var ws_edpoline_rec  as recid no-undo.
def {1} shared var ws_edpoaddon_rec as recid no-undo.

def {1} shared var ws_edivtran_rec  as recid no-undo.
def {1} shared var ws_edivline_rec  as recid no-undo.
def {1} shared var ws_edivaddon_rec as recid no-undo.

def {1} shared var ws_edshtran_rec  as recid no-undo.
def {1} shared var ws_edshord_rec   as recid no-undo.
def {1} shared var ws_edshpack_rec  as recid no-undo.
def {1} shared var ws_edshtare_rec  as recid no-undo.
def {1} shared var ws_edshline_rec  as recid no-undo.

def {1} shared var ws_edi_path  like edco.path-in   no-undo label "EDI Path".

/* 9704 CAH: If set true then 856 is generated simultaneous with invoice */
def {1} shared var ws_856_from_invoice as logical no-undo initial false.
def {1} shared var ws_process_rec  as recid no-undo.

