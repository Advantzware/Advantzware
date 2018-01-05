    FIND FIRST {&buffer} {&condition}
    AND {&buffer}.{&choose} = {&field} {&lock} {&error}.
    UPDATE                                                      
        {&field} {&help}
    with frame {&frame} {&frame-opt}
    EDITING:
        READKEY.
        if can-do("home,end,page-up,page-down", keylabel(lastkey) ) then do:
        if keylabel(lastkey) = "home" then do:
            find first {&buffer} {&condition} no-lock no-error.
        end.
        else if keylabel(lastkey) = "end" then do:
            find last {&buffer} {&condition} no-lock no-error.
        end.
        else if keylabel(lastkey) = "page-down" then do:
            find next {&buffer} {&condition} no-lock no-error.
        end.
        else if keylabel(lastkey) = "page-up" then do:
            find prev {&buffer} {&condition} no-lock no-error.
        end.

        if avail {&buffer} then do:
            display {&buffer}.{&choose} @ {&field} 
            with frame {&frame}.
            {rc/fmlookup.i {&buffer} "{&lookup}" {&frame} /WHITE}
            release {&buffer}.
            next.
        end.
        else do:
            message color value(c_err)
                "Cannot find"
                (if keylabel(lastkey) = "home" then "first" else
                 if keylabel(lastkey) = "end"  then "last"  else
                 if keylabel(lastkey) = "page-up" then "previous" else
                 if keylabel(lastkey) = "page-down" then "next"
                    else keylabel(lastkey) )
                 "{&buffer}".
            pause 1.
            hide message no-pause.
            next.
        end.         
                 
        end.
            
        IF {rc/termkey.i} THEN DO WITH FRAME {&frame}:
            IF FRAME-FIELD = "{&field}" THEN DO:
                if input frame {&frame} {&field} = ?
                and "{&error}" = "no-error"
                then do:
                    color display value(c_wrn) {&buffer}.{&lookup} 
                    with frame {&frame}.
                    display "*ALL*" @ {&buffer}.{&lookup}
                    with frame {&frame}.
                end.
                else do:
                FIND FIRST {&buffer}
                {&condition} AND {&buffer}.{&choose} = INPUT FRAME {&frame} {&field}
                    {&lock} {&error}.
                {rc/fmlookup.i {&buffer} "{&lookup}" {&frame} /WHITE}
                {&termkey}
                end.
            END.
        END.
        APPLY LASTKEY.
        if go-pending then do:
            {&detgo}
        end.
    END.
