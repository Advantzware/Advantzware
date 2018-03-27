{ed/sharedv.i}
find first company where company.company = ws_company no-lock no-error.
ws_co_name =
    if avail company then company.name
    else "COMPANY: " + ws_company + " * UNDEFINED *".
assign
ws_856_from_invoice = false.
