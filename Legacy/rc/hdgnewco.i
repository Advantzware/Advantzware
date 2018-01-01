/*  hdgnewco.i - pass driver file.co.
09.11.96 by CAH on CDS972@812<rprodemo> Log#0000:
1.  Added argument 2 for any additional code to execute inside of newco.  This
might be used for centering heading name and desc.
09.23.92 by CH:
1.  Implemented calendar/calenper to eliminate references to ??Co files.
*/
        if first-of ({1}) then do:
            find rpro.company where rpro.company.co = {1} no-lock no-error.
            if available (rpro.company) then do:
                hdg_name = rpro.company.name.
            end.
            else do:
                hdg_name = 'UNDEFINED COMPANY#:' + string({1},'99').
                hdg_perdate = today.
            end.
            find rpro.calendar where calendar.calendar = ws_calendar
                no-lock no-error.
            if available (calendar) then do:
                find rpro.calenper of calendar where calenper.per = ws_per
                    no-lock no-error.
                if available (calenper) then hdg_perdate = calenper.pedate.
            end.
            if hdg_perdate = ? then hdg_perdate = today.
            hdg_rpt_code = program-name(1).
            {2}
            page {&stream}.
        end.
