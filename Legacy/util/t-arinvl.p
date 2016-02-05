for each ar-inv where not posted.
disp x-no inv-no tax-code tax-amt.
for each ar-invl where ar-invl.x-no = ar-inv.x-no.
if ar-inv.tax-code <> "" and tax-amt <> 0 then ar-invl.tax = yes.
disp tax amt.
