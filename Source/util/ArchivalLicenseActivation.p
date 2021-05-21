
/*------------------------------------------------------------------------
    File        : ArchivalLicenseActivation.p
    Purpose     : Provides limited ASI functionality to customers who need information review, but 
    Syntax      : RUN util/ArchivalLicenseActivation.p (can be run from N-M with superAdmin credentials)
    Description : Ticket 97894 Archival License Key
    Specs       : 1) Create a utility to set archival access for a client that only has Archival access so they cannot use the system, 
                        so making everything read only by doing the following:
                    - Create a .d file of permissions (N-S-8)
                    - Change all ADD permissions to 'ASI' only in N-S-8 permissions (Leaving the other permissions as they are to restrict users 
                        viewing things they were previously allowed to do)
                    - Disable Access to N-S-8 so a user cannot add back permissions
                  2) Verify that the system is accessible to run reports, view any data, but not usable to process things
                  3) Document process which should be:
                    a) Run utility - ArchivalLicenseActivation
                    b) Sets Module access license for 1 year from date of agreement
                    c) Update customer file in Zoho
                    d) Set up billing to bill for the Archival access  

    Test Case   : 1) Verify the process steps related to the software change
                  2) Verify a user other than ASI can run reports, access customers, estimates, orders without issue (Assuming they had permissions 
                        before the utility was run)
    Author(s)   : MYT 
    Created     : Fri May 21 15:47:35 EDT 2021
    Notes       :   Create a non-tmp directory (in CustFiles) to hold existing prgrms records in .d format
                    Export existing prgrms records to a file in that dir (arcLicense.d)
                    FOR EACH prgrms, assign can-create = "ASI", can-delete/can-update need spec
                    SET ALL perms on record "prgrms." to ASI only
                    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR cCurrentDir AS CHAR NO-UNDO.
DEF VAR cOutputDir AS CHAR NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    /* Get the current directory (environment name) */
    RUN util/currdir.p (OUTPUT cCurrentDir).

    /* create a directory in cust files to hold OLD permissions */
    ASSIGN 
        cOutputDir = cCurrentDir + "\CustFiles\ArcLic".
    OS-CREATE-DIR VALUE(cOutputDir).
    
    /* Export pgrms records to a .d file in that directory */
    OUTPUT TO VALUE(cOutputDir + "arcLicense.d").
    FOR EACH prgrms NO-LOCK:
        EXPORT prgrms.
    END.
    
    /* Remove users/groups from permissions */
    FOR EACH prgrms EXCLUSIVE:
        ASSIGN 
            prgrms.can_create = "ASI"
            prgrms.can_delete = prgrms.can_delete /* should this be asi? */ 
            prgrms.can_update = prgrms.can_update /* should this be asi? */
            .
        /* If the program master function, set ALL perms to asi-only */
        IF prgrms.prgmname EQ "prgrms." THEN ASSIGN 
            prgrms.can_create = "ASI"
            prgrms.can_delete = "ASI"
            prgrms.can_update = "ASI"
            prgrms.can_run = "ASI".
    END.
    
    MESSAGE 
        "Archival License permissions set."
        VIEW-AS ALERT-BOX INFO.
        
    
            
    