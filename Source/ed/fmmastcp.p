{ed/sharedv.i}
def input param from_partner as char no-undo.
def input param to_partner   as char no-undo.
def buffer edmast2      for edmast.
def buffer edcode2      for edcode.
def buffer edicxref2    for edicxref.
def buffer edshipto2    for edshipto.
find edmast where edmast.partner = from_partner no-lock no-error.
if not avail edmast then do:
    bell.
    message color value(c_err) "Cannot access" from_partner "copy aborted".
    pause.
    return.
end.
find edmast2 where edmast2.partner = to_partner no-lock no-error.
if avail edmast2 then do:
    bell.
    message color value(c_err) "Duplicate partner" to_partner "copy aborted".
    pause.
    return.
end.
for each edcode where edcode.partner = edmast.partner:
    create edcode2.
    {ed/cpcode.i edcode2 edcode}
    edcode2.partner = to_partner.
end.
for each edicxref where edicxref.partner = edmast.partner:
    create edicxref2.
    {ed/cpicxref.i edicxref2 edicxref}
    edicxref2.partner = to_partner.
end.
for each edshipto where edshipto.partner = edmast.partner:
    create edshipto2.
    {ed/cpshipto.i edshipto2 edshipto}
    edshipto2.partner = to_partner.
end.
create edmast2.
{ed/cpmast.i edmast2 edmast}
edmast2.partner = to_partner
edmast2.seq = 0.
