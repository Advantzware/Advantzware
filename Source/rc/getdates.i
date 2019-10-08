UPDATE
  lo_trandate
  hi_trandate
  validate(input frame {&frame} hi_trandate = ? 
    or input frame {&frame} hi_trandate >= input frame {&frame}
        lo_trandate, 'Ending date cannot precede starting date')
  WITH FRAME {&frame}.
