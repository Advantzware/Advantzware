

/*------------------------------------------------------------------------
    File        : History.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{History.i}
 
DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmJob as Character no-undo.
DEFINE INPUT PARAMETER prmJob2 as Character no-undo.
DEFINE INPUT PARAMETER prmCode as Character no-undo.
DEFINE INPUT PARAMETER prmDate as Character no-undo.
DEFINE INPUT PARAMETER prmTag as Character no-undo.
DEFINE INPUT PARAMETER prmWareHouse as Character no-undo.
DEFINE INPUT PARAMETER prmPoNo as Character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsHistory.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE li-pallets AS INT NO-UNDO.
DEFINE VARIABLE op-qty-pal AS INT NO-UNDO.
DEFINE VARIABLE vJob   AS CHARACTER  NO-UNDO.

DEFINE BUFFER buff_itemfg FOR itemfg.
DEFINE BUFFER buff_oe-ordl FOR oe-ordl.
DEFINE BUFFER b-eb FOR eb.

IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".
IF prmJob     = ? THEN ASSIGN prmJob = "".
IF prmJob2 = ? THEN ASSIGN prmJob2 = "".
IF prmCode = ? THEN ASSIGN prmCode = "".

IF prmCode = "B" THEN ASSIGN prmCode = "".
IF prmDate = ? THEN ASSIGN prmDate = "".
IF prmTag = ? THEN ASSIGN prmTag = "".
IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmWareHouse = ? THEN ASSIGN prmWareHouse = "".
IF prmPoNo = ? THEN ASSIGN prmPoNo = "".
  
DEF VAR prmComp AS CHAR NO-UNDO.

/* ***************************  Main Block  *************************** */
/*****************************************************************************************************************************/
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "Select" THEN DO:
   /* FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.LINE = int(prmItemNum) NO-LOCK NO-ERROR.   */
    FIND FIRST itemfg where itemfg.company EQ prmComp AND itemfg.i-no = prmItemNum NO-LOCK NO-ERROR.
    FOR EACH fg-rcpth WHERE
        fg-rcpth.company EQ itemfg.company AND
        fg-rcpth.i-no = itemfg.i-no NO-LOCK :
        
        FOR EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                                fg-rdtlh.rita-code = fg-rcpth.rita-code NO-LOCK :
            /* FIND FIRST fg-rctd WHERE fg-rctd.r-no = fg-rdtlh.r-no NO-LOCK NO-ERROR.
            FOR EACH reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND reftable.company  EQ fg-rcpth.company /*AND 
            reftable.loc EQ STRING(fg-rcpth.r-no) */ NO-LOCK:*/
            create ttHistory.
            assign  
                ttHistory.ItemNum        = fg-rcpth.i-no
                ttHistory.po-no          = fg-rcpth.po-no
                ttHistory.job-no         = fg-rcpth.job-no
                ttHistory.job-no2        = fg-rcpth.job-no2
                ttHistory.TDate          = fg-rcpth.trans-date
                ttHistory.RCode          = fg-rcpth.rita-code
                ttHistory.Cust           = fg-rdtlh.cust-no
                ttHistory.Loc            = fg-rdtlh.loc
                ttHistory.Loc-Bin        = fg-rdtlh.loc-bin
                ttHistory.Qty            = fg-rdtlh.qty
                ttHistory.Tag            = fg-rdtlh.tag
                ttHistory.Cost           = fg-rdtlh.cost
                ttHistory.Qty-Case       = fg-rdtlh.qty-case
                ttHistory.Unit           = fg-rdtlh.stacks-unit
                /* ttHistory.usr            = reftable.CODE*/
                ttHistory.i-name         = itemfg.i-name
                ttHistory.q-onh          = itemfg.q-onh
                ttHistory.q-avail        = itemfg.q-avail
               
                .
            IF ttHistory.Tag  <> "" THEN
                ttHistory.Tag  = SUBSTRING(ttHistory.Tag, 16,5).
            
            IF fg-rcpth.rita-code = "R" THEN
                ttHistory.RCode = "Receipt".
            IF fg-rcpth.rita-code = "S" THEN
                ttHistory.RCode = "Shipment".
            IF fg-rcpth.rita-code = "A" THEN
                ttHistory.RCode = "Adjustment".
            IF fg-rcpth.rita-code = "T" THEN
                ttHistory.RCode = "Transfer".
            IF fg-rcpth.rita-code = "C" THEN
                ttHistory.RCode = "Cycle Count".
            IF fg-rcpth.rita-code = "E" THEN
                ttHistory.RCode = "Return".
            
            
            find first fg-bin where fg-bin.company eq fg-rcpth.company
                                and fg-bin.i-no    eq fg-rcpth.i-no
                                and fg-bin.job-no  eq fg-rcpth.job-no
                                and fg-bin.job-no2 eq fg-rcpth.job-no2
                                and fg-bin.loc     eq fg-rdtlh.loc
                                and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                                and fg-bin.tag     eq fg-rdtlh.tag
                                and fg-bin.cust-no eq fg-rdtlh.cust-no no-lock no-error.  
            
            op-qty-pal = (IF fg-rdtlh.qty-case     NE 0 THEN fg-rdtlh.qty-case     ELSE
                IF AVAIL fg-bin AND
                    fg-bin.case-count     NE 0 THEN fg-bin.case-count     ELSE 1) *
                (IF fg-rdtlh.stacks-unit  NE 0 THEN fg-rdtlh.stacks-unit  ELSE
                    IF AVAIL fg-bin AND
                        fg-bin.cases-unit     NE 0 THEN fg-bin.cases-unit     ELSE 1) *
                (IF fg-rdtlh.units-pallet NE 0 THEN fg-rdtlh.units-pallet ELSE
                    IF AVAIL fg-bin AND
                        fg-bin.units-pallet   NE 0 THEN fg-bin.units-pallet   ELSE 1).
                   
                li-pallets = fg-rdtlh.qty / op-qty-pal.
                {sys/inc/roundup.i li-pallets}
                    IF op-qty-pal LT 0 THEN
                        ASSIGN
                        op-qty-pal = op-qty-pal * -1
                        li-pallets = li-pallets * -1.
                    
                 ASSIGN
                     ttHistory.Pallet         = li-pallets
                     ttHistory.QtyPlt         = op-qty-pal.
        END.   /*FOR EACH fg-rdtlh*/
    END.   /**FOR EACH fg-rcpth*/
END.  /*if prmActon*/
 

/*******************************************************************************************************************/
IF prmAction = "Search" THEN DO:
    ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
    /*FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND
         oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.LINE = int(prmItemNum) NO-LOCK NO-ERROR.*/   
    
    FIND FIRST itemfg where itemfg.company EQ prmComp AND
         itemfg.i-no = prmItemNum  NO-LOCK NO-ERROR.
    
    FOR EACH fg-rcpth WHERE fg-rcpth.company EQ prmComp AND
                            fg-rcpth.i-no = itemfg.i-no  AND 
                             (fg-rcpth.job-no BEGINS vJob OR prmJob = "" )  and
                            (fg-rcpth.job-no2 = int(prmJob2) OR prmJob2 = "" ) AND 
                            (fg-rcpth.rita-code BEGINS CAPS(prmCode) OR prmCode = "") AND
                            (fg-rcpth.trans-date GE date(prmDate)  or prmDate  = "" ) AND
                            (fg-rcpth.po-no EQ (prmPoNo) OR prmPoNo EQ "")  NO-LOCK :
        
        FOR EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                                fg-rdtlh.rita-code = fg-rcpth.rita-code  AND
                                (fg-rdtlh.tag = prmTag OR prmTag = "") AND
                                (fg-rdtlh.loc BEGINS prmWareHouse OR prmWareHouse EQ "")  NO-LOCK :
            
            create ttHistory.
            assign  
                ttHistory.ItemNum        = fg-rcpth.i-no
                ttHistory.po-no          = fg-rcpth.po-no
                ttHistory.job-no         = fg-rcpth.job-no
                ttHistory.job-no2        = fg-rcpth.job-no2
                ttHistory.TDate          = fg-rcpth.trans-date
                ttHistory.RCode          = fg-rcpth.rita-code
                ttHistory.Cust           = fg-rdtlh.cust-no
                ttHistory.Loc            = fg-rdtlh.loc
                ttHistory.Loc-Bin        = fg-rdtlh.loc-bin
                ttHistory.Qty            = fg-rdtlh.qty
                ttHistory.Tag            = fg-rdtlh.tag
                ttHistory.Cost           = fg-rdtlh.cost
                ttHistory.Qty-Case       = fg-rdtlh.qty-case
                ttHistory.Unit           = fg-rdtlh.stacks-unit
                /* ttHistory.usr            = reftable.CODE*/
                ttHistory.i-name         = itemfg.i-name
                ttHistory.q-onh          = itemfg.q-onh
                ttHistory.q-avail        = itemfg.q-avail
               
                .
            
            IF ttHistory.Tag  <> "" THEN
                ttHistory.Tag  = SUBSTRING(ttHistory.Tag, 16,5).
            
            IF fg-rcpth.rita-code = "R" THEN
               ttHistory.RCode = "Receipt".
           IF fg-rcpth.rita-code = "S" THEN
               ttHistory.RCode = "Shipment".
           IF fg-rcpth.rita-code = "A" THEN
               ttHistory.RCode = "Adjustment".
           IF fg-rcpth.rita-code = "T" THEN
               ttHistory.RCode = "Transfer".
           IF fg-rcpth.rita-code = "C" THEN
               ttHistory.RCode = "Cycle Count".
           IF fg-rcpth.rita-code = "E" THEN
               ttHistory.RCode = "Return".
           find first fg-bin where fg-bin.company eq fg-rcpth.company
                                and fg-bin.i-no    eq fg-rcpth.i-no
                                and fg-bin.job-no  eq fg-rcpth.job-no
                                           and fg-bin.job-no2 eq fg-rcpth.job-no2
                                and fg-bin.loc     eq fg-rdtlh.loc
                                and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                                and fg-bin.tag     eq fg-rdtlh.tag
                                and fg-bin.cust-no eq fg-rdtlh.cust-no no-lock no-error.  
            
            op-qty-pal = (IF fg-rdtlh.qty-case     NE 0 THEN fg-rdtlh.qty-case     ELSE
                IF AVAIL fg-bin AND
                    fg-bin.case-count     NE 0 THEN fg-bin.case-count     ELSE 1) *
                (IF fg-rdtlh.stacks-unit  NE 0 THEN fg-rdtlh.stacks-unit  ELSE
                    IF AVAIL fg-bin AND
                        fg-bin.cases-unit     NE 0 THEN fg-bin.cases-unit     ELSE 1) *
                (IF fg-rdtlh.units-pallet NE 0 THEN fg-rdtlh.units-pallet ELSE
                    IF AVAIL fg-bin AND
                        fg-bin.units-pallet   NE 0 THEN fg-bin.units-pallet   ELSE 1).
                   
                li-pallets = fg-rdtlh.qty / op-qty-pal.
                {sys/inc/roundup.i li-pallets}
                    IF op-qty-pal LT 0 THEN
                        ASSIGN
                        op-qty-pal = op-qty-pal * -1
                        li-pallets = li-pallets * -1.
                    
                 ASSIGN
                     ttHistory.Pallet         = li-pallets
                     ttHistory.QtyPlt         = op-qty-pal.


        END.   /*FOR EACH fg-rdtlh*/
    END.   /**FOR EACH fg-rcpth*/
END.  /*if prmActon*/
/*********************************************************************************************************************************/ 




