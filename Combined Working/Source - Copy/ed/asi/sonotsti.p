{sys/inc/var.i}
os-delete "sono_inv.txt".
for each company:
    cocode = company.company.
for each inv-head no-lock
where inv-head.company = cocode
by inv-date by inv-no:
    status default "Processing inv-head: " + cocode + string(inv-head.r-no).
    run edi/ed/exparinv.p ("inv-head", recid(inv-head)).
end.
for each ar-inv no-lock
where ar-inv.company = cocode
by inv-date by inv-no:
    status default "Processing ar-inv: " + cocode + string(ar-inv.x-no).
    run edi/ed/exparinv.p ("ar-inv", recid(ar-inv)).
end.
pause 0.
end.
status default.
