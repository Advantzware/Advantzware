FOR EACH utilities:
    FIND prgrms EXCLUSIVE WHERE 
    	    prgrms.prgmname = REPLACE(utilities.programName,".r",".")
    	    NO-ERROR.
    	IF NOT AVAIL prgrms THEN DO:
    	    CREATE prgrms.
    	END.
    	ASSIGN 
    		    prgrms.prgmname = REPLACE(utilities.programName,".r",".")
    		    prgrms.can_create = "*"
    		    prgrms.can_delete = "*"
    		    prgrms.can_run = "*"
    		    prgrms.can_update = "*"
    		    prgrms.dir_group = "util"
    		    prgrms.image = ""
    		    prgrms.mnemonic = utilities.hotkey
    		    prgrms.modeList = "UTIL"
    		    prgrms.module = utilities.module
    		    prgrms.notes = utilities.notes
    		    prgrms.prgtitle = utilities.description
    		    prgrms.securityLevelDefault = utilities.securityLevel
    		    prgrms.securityLevelUser = utilities.securityLevel
    		    .
  END.