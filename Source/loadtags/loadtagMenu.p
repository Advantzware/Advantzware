/* loadtagMenu.p - Sewa Singh - 08.27.2020 */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

SESSION:SET-WAIT-STATE("").

RUN loadtags/loadtag-menu.w PERSISTENT (g_company, g_loc).
