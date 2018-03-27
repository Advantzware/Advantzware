
/*------------------------------------------------------------------------
    File        : inventoryArchive.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jun 19 15:25:51 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdCutOff   AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER ipcFromItem AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcToItem   AS CHARACTER NO-UNDO.

{fg/rep/tt-fgbin.i NEW SHARED}

DEFINE VARIABLE dtCutOff    AS DATE      NO-UNDO.
DEFINE VARIABLE v-loc       AS CHARACTER EXTENT 2.
DEFINE VARIABLE v-loc-bin   AS CHARACTER EXTENT 2.
DEFINE VARIABLE zbal        AS DECIMAL.
DEFINE VARIABLE fi_days-old AS INTEGER.
DEFINE VARIABLE v-custown   AS LOG.
DEFINE VARIABLE cFileExt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE jCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-rno      AS INTEGER.  
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN v-loc[1] = ""
       v-loc[2] = "zzzz"
       v-loc-bin[1] = ""
       v-loc-bin[2] = "zzzzz"
       .
/* MESSAGE "cutoff" UPDATE dtcutOff. */

DEFINE STREAM sFg-rcpth.
DEFINE STREAM sFg-rdtlh.
DEFINE STREAM sFg-rcpts.
DEFINE STREAM sFg-rctd.
DEFINE BUFFER b-fg-rctd FOR fg-rctd.
ASSIGN cLogdir = "logs/inventory"
       cFileExt = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY)) + 
                  STRING(DAY(TODAY)) + STRING(TIME)
       .
FILE-INFO:FILE-NAME = cLogDir.
IF NOT FILE-INFO:FILE-TYPE BEGINS "DRW" THEN 
  RETURN.
OUTPUT STREAM sFg-rcpth TO VALUE(cLogDir + "/fg-rcpth." + cFileExt).
OUTPUT STREAM sFg-rdtlh TO VALUE(cLogDir + "/fg-rdtlh." + cFileExt).
OUTPUT STREAM sFg-rcpts TO VALUE(cLogDir + "/fg-rcpts." + cFileExt).
OUTPUT STREAM sFg-rctd TO VALUE(cLogDir + "/fg-rctd." + cFileExt).

dtcutoff = ipdCutOff.
FOR EACH itemfg NO-LOCK
   WHERE itemfg.company EQ ipcCompany 
   AND itemfg.i-no GE ipcFromItem
   AND itemfg.i-no LE ipcToItem:
    
    /* Calculate bins as of the cutoff date, Returns qty, avg cost? */
    RUN fg/rep/tt-fgbin.p (BUFFER itemfg, dtCutoff, "", "zzzzzzzzzz",
        v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
        zbal, fi_days-old, YES, v-custown).
        
    /* For each bin, create a physical count record and delete inventory history */
    /* prior to the cutoff date */
  
    FOR EACH tt-fg-bin :

        FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rctd AND fg-rctd.r-no GT lv-rno THEN
            lv-rno = fg-rctd.r-no.

        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN
            lv-rno = fg-rcpth.r-no.

        DO WHILE TRUE:
            lv-rno = lv-rno + 1.
            IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno 
                USE-INDEX r-no) OR
                CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno 
                USE-INDEX fg-rctd) THEN
                NEXT.
            LEAVE.
        END.
      
        CREATE fg-rctd.
        ASSIGN 
            fg-rctd.rita-code    = "C"      
            fg-rctd.company      = tt-fg-bin.company     
            fg-rctd.i-no         = tt-fg-bin.i-no       
            fg-rctd.job-no       = tt-fg-bin.job-no     
            fg-rctd.job-no2      = tt-fg-bin.job-no2    
            fg-rctd.loc          = tt-fg-bin.loc        
            fg-rctd.loc-bin      = tt-fg-bin.loc-bin    
            fg-rctd.tag          = tt-fg-bin.tag   
            fg-rctd.r-no         = lv-rno
            fg-rctd.std-cost     = tt-fg-bin.std-tot-cost 
            /* fg-rctd.pur-uom      = tt-fg-bin.cost-uom   */
            fg-rctd.tot-wt       = tt-fg-bin.tot-wt        
            fg-rctd.qty-case     = tt-fg-bin.case-count     
            fg-rctd.units-pallet = tt-fg-bin.units-pallet   
            fg-rctd.cases-unit   = tt-fg-bin.cases-unit     
            fg-rctd.t-qty        = tt-fg-bin.qty            
            fg-rctd.partial      = tt-fg-bin.partial-count  
            . 

        FOR EACH fg-rcpth EXCLUSIVE-LOCK WHERE fg-rcpth.company EQ tt-fg-bin.compan
            AND fg-rcpth.i-no EQ tt-fg-bin.i-no
            AND  fg-rcpth.trans-date LT dtCutOff:
            iCnt = iCnt + 1. 
            IF iCnt GT 999 THEN 
            DO:  
                iCnt = 0. 
                PROCESS EVENTS. 
                jCnt = jCnt + 1000. 
                PUBLISH "NUMDEL" (fg-rcpth.i-no, jCnt). 
            END.                 
            FOR EACH fg-rdtlh EXCLUSIVE-LOCK WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no:
                EXPORT STREAM sFg-rdtlh fg-rdtlh.
                DELETE fg-rdtlh.
            END.    
            FOR EACH fg-rctd EXCLUSIVE-LOCK WHERE fg-rctd.r-no EQ fg-rcpth.r-no:
                FOR EACH fg-rcpts EXCLUSIVE-LOCK 
                    WHERE fg-rcpts.company EQ fg-rctd.company 
                    AND fg-rcpts.linker EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999") 
                    :
                    EXPORT STREAM sFg-rcpts fg-rcpts.
                    DELETE fg-rcpts.
                END.
                EXPORT STREAM sFg-rctd fg-rctd.
                DELETE fg-rctd.
                
            END.
            EXPORT STREAM sFg-rcpth fg-rcpth.
            DELETE fg-rcpth.
        END. /* each fg-rcpth */
      
    END. /* each tt-fg-bin */
  

END. /* each itemfg */
