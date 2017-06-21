
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


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE dtCutOff AS DATE NO-UNDO.

FOR EACH itemfg:
    
  /* Calculate bins as of the cutoff date, Returns qty, avg cost? */
  RUN fg/rep/tt-fgbin.p (BUFFER itemfg, dtCutoff, "", "zzzzzzzzzz",
      v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
      zbal, fi_days-old, YES, v-custown).
        
  /* For each bin, create a physical count record and delete inventory history */
  /* prior to the cutoff date */
  FOR EACH tt-bin :
      
      CREATE fg-rctd.
      fg-rctd.rita-code    = "C"      
      fg-rctd.company      = w-fg-bin.company    
      fg-rctd.i-no         = w-fg-bin.i-no       
      fg-rctd.job-no       = w-fg-bin.job-no     
      fg-rctd.job-no2      = w-fg-bin.job-no2    
      fg-rctd.loc          = w-fg-bin.loc        
      fg-rctd.loc-bin      = w-fg-bin.loc-bin    
      fg-rctd.tag          = w-fg-bin.tag        
      fg-rctd.std-tot-cost = w-fg-bin.std-cost  
      fg-rctd.pur-uom      = w-fg-bin.cost-uom  
      fg-rcpth.r-no        = w-fg-bin.r-no
      fg-rcpth.company     = w-fg-bin.company
      fg-rcpth.loc         = w-fg-bin.loc
      fg-rcpth.trans-date  = w-fg-bin.rct-date
      fg-rcpth.po-no       = w-fg-bin.po-no
      fg-rcpth.i-no        = w-fg-bin.i-no
      fg-rcpth.i-name      = w-fg-bin.i-name
      fg-rcpth.job-no      = w-fg-bin.job-no
      fg-rcpth.job-no2     = w-fg-bin.job-no2
      fg-rcpth.pur-uom     = w-fg-bin.pur-uom
      fg-rcpth.rita-code   = w-fg-bin.rita-code
      .
      
      FOR EACH fg-rcpth EXCLUSIVE-LOCK WHERE fg-rcpth.trans-date LT dtCutOff.
        FOR EACH fg-rdtlh EXCLUSIVE-LOCK WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no:
            DELETE fg-rctd.
        END.    
        FOR EACH fg-rctd EXCLUSIVE-LOCK WHERE fg-rctd.r-no EQ fg-rcpth.r-no:
            DELETE fg-rctd.
        END.
        DELETE fg-rcpth.
      END.
  END.
  
  /* Can this be run without a UI? */
  RUN fg/fgpstall.w (INPUT "ALL").
  END.
END.