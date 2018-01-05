do ex = 1 to num-entries(erclist):
    erc = integer(ENTRY(ex, erclist)).

find rcerr where rcerr.app = ws_app and rcerr.erc = erc no-lock no-error.
if not avail rcerr then find first rcerr
where rcerr.app = '' and rcerr.erc = erc no-lock no-error.

str_buffb =
    (if avail rcerr then rcerr.errmsg else 'Undefined error').

str_yy = 1.
do while substring(str_buffb,str_yy) matches "*%.%*":
    str_xx = index(substring(str_buffb,str_yy),"%") + (str_yy - 1).
    if substring(str_buffb,str_xx,3) matches "%.%"
    and {rc/isdigit.i "substring(str_buffb,(str_xx + 1),1)"} then do:
        /* replace argument with token */
        erctokenx = integer(substring(str_buffb,str_xx + 1,1)).
        if erctokenx = 0 then erctokenx = erc.
        str_buffa =
            substring(str_buffb,1,str_xx - 1)
          + trim(erctoken[erctokenx])
          + substring(str_buffb,str_xx + 3).
        str_buffb = str_buffa.
    end.
    str_yy = str_xx + 3.
end.

    put {1} unformatted
        skip '*** ' str_buffb ' (' erc ')'.
end.
erclist = ''.
