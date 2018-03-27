
/*------------------------------------------------------------------------
    File        : impfgrcp.p
    Purpose     : 

    Syntax      :

    Description : Import FGItem Receipts

    Author(s)   : 
    Created     : Tue May 10 17:52:05 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE FGReceiptRow LIKE fg-rctd
             field TableRowid as rowid.

DEF TEMP-TABLE tt-rcpth LIKE fg-rcpth.
DEF TEMP-TABLE tt-rdtlh LIKE fg-rdtlh.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.


DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .
{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}

DEF VAR v-fgpostgl AS CHAR NO-UNDO.                        
def var v-post-date as date init today no-undo.

{custom/globdefs.i}
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.
/*                    */
/*assign              */
/* cocode = gcompany. */
/*IF cocode EQ '' THEN*/
/*    cocode = '001'. */

DEF STREAM logFile.
def var vXmlInputFile as char init "Template\FGReceipts.xml"no-undo.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

 file-info:file-name = vXmlInputFile.
 vXmlInputFile = file-info:full-pathname. 
 run PreFGImport.  /*validate records from XML file */
 RUN ImportFGReceipts.
     
 run fg/fgpost.p (input table FGReceiptRow). 
 
  MESSAGE "Posting is completed!"
  VIEW-AS ALERT-BOX.
  

/* **********************  Internal Procedures  *********************** */

PROCEDURE FGPostLog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.
        
   PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
     STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

PROCEDURE gl-from-work:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum
       debits  = 0
       credits = 0.

      RELEASE gltrans.
    end.
  end.


END PROCEDURE.

PROCEDURE ImportFGReceipts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

 /*  write xml files ===
  FOR EACH fg-rctd WHERE rita-code = "R":
      CREATE tt-rctd.
      BUFFER-COPY fg-rctd TO tt-rctd.
  END.

  TEMP-TABLE tt-rctd:WRITE-XML("FILE","c:\temp\fg-rctd.xml", TRUE).
  ==========*/
  
  def var lv-rno as int no-undo.
  
  FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL fg-rctd AND fg-rctd.r-no GT lv-rno THEN lv-rno = fg-rctd.r-no.
  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
 
  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL fg-rctd THEN NEXT.
    LEAVE.
  END.

  empty temp-table FGReceiptRow.
  MESSAGE "import:" vxmlinputfile
  VIEW-AS ALERT-BOX.  
  TEMP-TABLE FGReceiptRow:READ-XML ("File", vXmlInputFile, "Empty",?,NO).

  FOR EACH FGReceiptRow:
   
    create fg-rctd.
    buffer-copy FGReceiptRow to fg-rctd.
    assign fg-rctd.r-no = lv-rno
           fg-rctd.rita-code = "R"
           fg-rctd.trans-time   = TIME
           /*fg-rctd.rct-date    = TODAY
             fg-rctd.units-pallet = 1
             fg-rctd.cases-unit   = 1
             fg-rctd.ext-cost     = 0
           */
           FGReceiptRow.TableRowid = rowid(fg-rctd)
           lv-rno = lv-rno + 1
           .
     MESSAGE "creating fg-rctd: " fg-rctd.r-no ":" fg-rctd.company ":" fg-rctd.i-no 
     VIEW-AS ALERT-BOX.
          
     RUN fg/invrecpt.p (ROWID(fg-rctd), 1).   
  END.

END PROCEDURE.

