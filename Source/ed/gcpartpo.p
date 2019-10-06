{ed/sharedv.i}
{rc/fcurrent.i}
{rc/statline.i}
def var new_partner like ws_partner label "Change to" no-undo.
{rc/viewline.i &displayf="ws_partner new_partner"}
update ws_partner new_partner.
find edmast where edmast.partner = ws_partner no-lock.
find edmast where edmast.partner = new_partner no-lock.
for each eddoc
where partner = ws_partner
and setid = "850" exclusive:
    display eddoc.partner eddoc.seq eddoc.fgid eddoc.setid eddoc.adddate
    with  frame f-current.
/*
    find edpotran of eddoc exclusive.
    for each edpoline of edpotran:
        edpoline.partner = new_partner.
    end.
    for each edpoaddon of edpotran:
        edpoaddon.partner = new_partner.
    end.
    
    
    edpotran.partner = new_partner. 
*/    
    EDDOC.PARTNER = NEW_PARTNER.
end.
