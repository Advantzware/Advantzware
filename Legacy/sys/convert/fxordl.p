/* fxordl.p need to reset e-num in oe-ordl*/

FOR EACH oe-ordl:
    oe-ordl.e-num = 0.
END.
