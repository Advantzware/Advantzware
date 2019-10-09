/* wipMenu.p - rstark - 8.27.2019 */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

SESSION:SET-WAIT-STATE("").

RUN wip/wip-menu.w PERSISTENT (g_company, g_loc).
