def var filename as character format "x(50)" no-undo.
def var quoted_filename as character format "x(50)" no-undo.
def var check_filename as character format "x(50)" no-undo.
def var run_filename as character format "x(50)" no-undo.
def var answer as logical no-undo.
def var ws_delim as char no-undo initial ? format 'x(01)'
    label "Delim".
def var ws_colum as char no-undo initial ? format 'x(60)'
    label "Columns".
