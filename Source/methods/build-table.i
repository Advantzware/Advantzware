/* build-table.i - rstark - 10.22.2021 */

PROCEDURE build-table:
/*------------------------------------------------------------------------------
  Purpose:                 /** BUILD JOB WORK FILE **/
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTotOnHand  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotOnOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotAlloc   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotBack    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotAvail   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotReOrder AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE w-jobs.
    EMPTY TEMP-TABLE w-job.
    
    lUnspecified = NO.
    IF NOT AVAILABLE {1}itemfg THEN
    RETURN.
    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ {1}itemfg.company
         NO-ERROR.
    FOR EACH {1}itemfg-loc NO-LOCK
        WHERE {1}itemfg-loc.company EQ {1}itemfg.company
          AND {1}itemfg-loc.i-no    EQ {1}itemfg.i-no,
        FIRST loc NO-LOCK
        WHERE loc.company EQ {1}itemfg-loc.company
          AND loc.loc     EQ {1}itemfg-loc.loc
        :
        CREATE w-jobs.
        ASSIGN 
            w-jobs.i-no         = {1}itemfg.i-no
            w-jobs.loc          = {1}itemfg-loc.loc    
            w-jobs.lead-days    = {1}itemfg-loc.lead-days
            w-jobs.ord-level    = {1}itemfg-loc.ord-level
            w-jobs.ord-max      = {1}itemfg-loc.ord-max
            w-jobs.ord-min      = {1}itemfg-loc.ord-min
            w-jobs.onHand       = {1}itemfg-loc.q-onh
            w-jobs.onOrder      = {1}itemfg-loc.q-ono
            w-jobs.allocated    = {1}itemfg-loc.q-alloc
            w-jobs.backOrder    = {1}itemfg-loc.q-back
            w-jobs.qtyAvailable = w-jobs.onHand
                                + w-jobs.onOrder
                                - w-jobs.allocated
            iTotOnHand          = iTotOnHand  + w-jobs.onHand
            iTotonOrder         = iTotOnOrder + w-jobs.onOrder
            iTotAlloc           = iTotAlloc   + w-jobs.allocated
            iTotBack            = iTotBack    + w-jobs.backOrder
            iTotAvail           = iTotAvail   + w-jobs.qtyAvailable
            iTotReOrder         = iTotReOrder + w-jobs.ord-level
            .
        IF AVAILABLE loc THEN
        w-jobs.loc-desc = loc.dscr.    
        RELEASE w-jobs.
    END. /* each {1}itemfg-loc */
    CREATE w-jobs.
    ASSIGN 
        w-jobs.i-no         = {1}itemfg.i-no
        w-jobs.loc          = "*ALL"
        w-jobs.loc-desc     = "ALL Locations"
        w-jobs.lead-days    = {1}itemfg.lead-days
        w-jobs.ord-level    = {1}itemfg.ord-level
        w-jobs.ord-max      = {1}itemfg.ord-max
        w-jobs.ord-min      = {1}itemfg.ord-min
        w-jobs.onHand       = {1}itemfg.q-onh
        w-jobs.onOrder      = {1}itemfg.q-ono
        w-jobs.allocated    = {1}itemfg.q-alloc
        w-jobs.backOrder    = {1}itemfg.q-back
        w-jobs.qtyAvailable = {1}itemfg.q-avail
        .
    IF iTotAlloc   NE {1}itemfg.q-alloc OR
       iTotOnHand  NE {1}itemfg.q-onh   OR
       iTotOnOrder NE {1}itemfg.q-ono   OR
       iTotBack    NE {1}itemfg.q-back  OR
       iTotAvail   NE {1}itemfg.q-avail THEN DO:          
        CREATE w-jobs.
        ASSIGN 
            w-jobs.i-no         = {1}itemfg.i-no
            w-jobs.loc          = "*UNSP"
            w-jobs.loc-desc     = 
            &IF "{&Source}" EQ "OU1" &THEN
            "Orders w/o Releases"
            &ELSE
            "Unspecified Locations"
            &ENDIF
            w-jobs.lead-days    = 0
            w-jobs.ord-level    = 0
            w-jobs.ord-max      = 0
            w-jobs.ord-min      = 0
            w-jobs.onHand       = {1}itemfg.q-onh - iTotOnHand
            w-jobs.onOrder      = {1}itemfg.q-ono - iTotOnOrder
            w-jobs.allocated    = {1}itemfg.q-alloc - iTotAlloc
            w-jobs.backOrder    = {1}itemfg.q-back - iTotBack
            w-jobs.qtyAvailable = {1}itemfg.q-avail - iTotAvail
            lUnspecified        = YES    
            .
    END.

END PROCEDURE.
