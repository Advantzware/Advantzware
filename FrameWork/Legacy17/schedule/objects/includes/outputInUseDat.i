/* outputInUseDat.i */

    OUTPUT TO VALUE(clientDat + '{&data}/' + ID + '/inUse.' + loginID + '.dat').
    EXPORT 'Scheduler Pro in Use: ' + STRING(TODAY,'99.99.9999') + ' @ ' +
      STRING(TIME,'hh:mm:ss am') + ' by ' + loginID.
    OUTPUT CLOSE.
