        IF LASTKEY = -1 THEN do:
            APPLY KEYCODE("return").
            next.
        end. /* cannot advance the
            cursor to next field until all frame-field checking has been
            processed */
