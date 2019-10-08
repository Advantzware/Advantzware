{rc/loginv.i}
if opsys = "BTOS" THEN DO:
    scr_firstkey = keycode("CODE-UP-ARROW").
    scr_lastkey  = keycode("CODE-DOWN-ARROW").
    HYPERHELP_KEY = KEYCODE("SHIFT-f2").
end.
else if opsys = "MSDOS" then do:
    scr_firstkey = keycode("HOME").
    scr_lastkey  = keycode("END").
    HYPERHELP_KEY = KEYCODE("F22").     /* 9611 CAH = shift-f2 */
end.
else if OPSYS = 'UNIX' THEN DO:
    scr_firstkey = keycode("HOME").
    scr_lastkey  = keycode("ESC-DOWN-ARROW").
    HYPERHELP_KEY = KEYCODE("F12").
end.
scr_firstkeycap = keylabel(scr_firstkey).
scr_lastkeycap  = keylabel(scr_lastkey).
HYPERHELP_KEYCAP = KEYLABEL(HYPERHELP_KEY).
{rc/keymap.i}
run rc/runifok.p ('rc/' + opsys    + '.p'). /*  o/s specific settings */
run rc/runifok.p ('rc/' + terminal + '.p'). /* term specific settings */
ws_perdate = TODAY.
ws_calendar = 1.    /* default, changed by applications as required */
