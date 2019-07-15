
using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;
using System.Data.SqlClient;
using System.Web.Mail;

/// <summary>
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class reports : AppServerConnect.AppServer
{
    public reports()
    {
        //
        // TODO: Add constructor logic here
        //
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTranOrderDataSet Transprint(string prmUser, string prmAction, string prmOut, string vBeginCust, string vEndCust, string vBeginPo, string vEndPo, string vBeginItem, string vEndItem, string vBeginJob, string vEndJob, string vBeginJob2, string vEndJob2, Int32 vBeginOrd, Int32 vEndOrd, string vDetailed, string vCloseOrd, string vNewOrd)
    {
        string cError = "";
        dsTranOrderDataSet dsTranOrder = new dsTranOrderDataSet();
        dsTranOrder = null;
        AppServerConnect();
        aoObject.TranOrder(prmUser,prmAction,prmOut, vBeginCust, vEndCust, vBeginPo, vEndPo, vBeginItem, vEndItem, vBeginJob, vEndJob, vBeginJob2, vEndJob2, vBeginOrd, vEndOrd, vDetailed, vCloseOrd, vNewOrd, ref dsTranOrder, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTranOrder;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustReportDataSet FillAlphabeticList(string prmUser, string prmComp)
    {
        dsCustReportDataSet dsCustReport = new dsCustReportDataSet();
        dsCustReport = null;
        AppServerConnect();
        aoObject.CustReport(prmUser, prmComp, ref dsCustReport);
        AppServerDisconnect();
        return dsCustReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAlphaListDataSet AlphabeticList(string prmUser, string prmAction, string prmOut, string vBeginCust, string vEndCust, string vBeginCat, string vEndCat, string vBeginItem, string vEndItem, string vCustWhse, string vSort, string vZero)
    {
        string cError = "";
        dsAlphaListDataSet dsAlphaList = new dsAlphaListDataSet();
        dsAlphaList = null;
        AppServerConnect();
        aoObject.AlphaList(prmUser,prmAction,prmOut, vBeginCust, vEndCust, vBeginCat, vEndCat, vBeginItem, vEndItem, vCustWhse, vSort, vZero, ref dsAlphaList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsAlphaList;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustinvoiceDataSet CustInvoice(string prmUser, string prmAction, string prmOut, string vBeginCust, string vEndCust, string vFromlistclass, string vTolistclass)
    {
        string cError = "";
        dsCustinvoiceDataSet dsCustInv = new dsCustinvoiceDataSet();
        dsCustInv = null;
        AppServerConnect();
        aoObject.CustInv(prmUser,prmAction,prmOut, vBeginCust, vEndCust, vFromlistclass, vTolistclass, ref dsCustInv, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsCustInv;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReOrderDataSet ReOrder(string prmUser, string prmAction, string prmOut, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, string prmBeginCategory, string prmEndCategory, string prmBeginWare, string prmEndWare, bool prmQtyOnHand, bool prIncQoh, bool prmBelow, bool prmPart, bool prmHistory, bool prmDash, string prmRdqoh, DateTime prmDate, string prmPrintStock, string prmPrintPurch, string prmPrintLot, string prmPrintQty, string prmPrintPrice)
    {
        string cError = "";
        dsReOrderDataSet dsReorder = new dsReOrderDataSet();
        dsReorder = null;
        AppServerConnect();
        aoObject.ReOrder(prmUser,prmAction,prmOut, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmBeginCategory, prmEndCategory, prmBeginWare, prmEndWare, prmQtyOnHand, prIncQoh, prmBelow, prmPart, prmHistory, prmDash, prmRdqoh, prmDate,prmPrintStock,  prmPrintPurch,  prmPrintLot,  prmPrintQty,  prmPrintPrice, ref dsReorder, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsReorder;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOpenOrdRepDataSet ReOpenOrder(string prmUser, string prmBeginCust, string prmEndCust, DateTime prmBegOrdate, DateTime prmEndOrdate, string prmBegPo, string prmEndPo, string prmBegJob, string prmEndJob, string prmBegJob2, string prmEndJob2, string prmBegItem, string prmEndItem, string prmBegCad, string prmEndCad, DateTime prmBegDue, DateTime prmEndDue, string prmBegUser, string prmEndUser, string prmBegSman, string prmEndSman, string prmSortcust, string prmSecondary, string prmJobStat, string prmOrdStat, string prmDuedate, string prmWipqty, string prmPrintjob, string prmDroporder, string prmInorder, string prmInqty, string prmInact, string prminjob, string prmOutexcel)
    {
        string cError = "";
        dsOpenOrdRepDataSet dsOpenOrdRep = new dsOpenOrdRepDataSet();
        dsOpenOrdRep = null;
        AppServerConnect();
        aoObject.openordrep(prmUser, prmBeginCust, prmEndCust, prmBegOrdate, prmEndOrdate, prmBegPo, prmEndPo, prmBegJob, prmEndJob, prmBegJob2, prmEndJob2, prmBegItem, prmEndItem, prmBegCad, prmEndCad, prmBegDue, prmEndDue, prmBegUser, prmEndUser, prmBegSman, prmEndSman, prmSortcust, prmSecondary, prmJobStat, prmOrdStat, prmDuedate, prmWipqty, prmPrintjob, prmDroporder, prmInorder, prmInqty, prmInact, prminjob, prmOutexcel, ref dsOpenOrdRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
             HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
             HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsOpenOrdRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderNoRepDataSet SeOrdNoRep(string prmUser, string prmOrderNoAct, Int32 prmBeginOrd, Int32 prmEndOrd, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, DateTime prmBeOrdDate, DateTime prmEndOrdDate, string prmPrintQty, string prmPriCont, string prmOut)
    {

        string cError = "";
        dsOrderNoRepDataSet dsOrderNoRep = new dsOrderNoRepDataSet();
        dsOrderNoRep = null;
        AppServerConnect();
        aoObject.OrdNoRep(prmUser, prmOrderNoAct, prmBeginOrd, prmEndOrd, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmBeOrdDate, prmEndOrdDate, prmPrintQty, prmPriCont,prmOut, ref dsOrderNoRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {

        }
        return dsOrderNoRep;

    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsDueDateRepDataSet SeOrdDueRep(string prmUser, DateTime prmBeginDate, DateTime prmEndDate, string prmBegsman, string prmEndsman, string prmOutexcel)
    //{

    //    string cError = "";
    //    dsDueDateRepDataSet dsDueDateRep = new dsDueDateRepDataSet();
    //    dsDueDateRep = null;
    //    AppServerConnect();
    //    aoObject.duereport(prmUser, prmBeginDate, prmEndDate, prmBegsman, prmEndsman, prmOutexcel, ref dsDueDateRep, out cError);
    //    AppServerDisconnect();
    //    if (cError != "")
    //    {

    //    }
    //    return dsDueDateRep;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsjobblogDataSet SelectJobLog(string prmUser,string prmActJob, string prmBeginCust, string prmEndCust, Int32 prmBeginOrder, Int32 prmEndOrder, string prmBeginItem, string prmEndItem, DateTime prmBeginOrderDate, DateTime prmEndOrderDate, string prmPrintPart, string prmSort, string prmPrintDueDate, string prmCustName,string prmViewItem)
    {
        string cError = "";
        dsjobblogDataSet dsjobblog = new dsjobblogDataSet();
        dsjobblog = null;
        AppServerConnect();
        aoObject.jobblogrep(prmUser,prmActJob, prmBeginCust, prmEndCust, prmBeginOrder, prmEndOrder, prmBeginItem, prmEndItem, prmBeginOrderDate, prmEndOrderDate, prmPrintPart, prmSort, prmPrintDueDate, prmCustName,prmViewItem, ref dsjobblog, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsjobblog;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBackLogRepDataSet SelectBackLogRep(string prmUser, string prmAct, string prmBeginCust, string prmEndCust, string prmBegsman, string prmEndsman, Int32 prmBegord, Int32 prmEndord, string prmBeitem, string prmEnditem, DateTime prmBegdate, DateTime prmEndDate, string prmBeuser, string prmEnduser, string prmPrint, string prminjob, string prmDet, string prmInIt, string prmSub, string prmReLa, string prmSort, string prmPrip, string prmQty, string prmDis, string prmOut)
    {

        string cError = "";
        dsBackLogRepDataSet dsBackLogRep = new dsBackLogRepDataSet();
        dsBackLogRep = null;
        AppServerConnect();
        aoObject.BackLogRep(prmUser,prmAct, prmBeginCust, prmEndCust, prmBegsman, prmEndsman, prmBegord, prmEndord, prmBeitem, prmEnditem, prmBegdate, prmEndDate, prmBeuser, prmEnduser, prmPrint, prminjob, prmDet, prmInIt, prmSub, prmReLa, prmSort, prmPrip, prmQty, prmDis,prmOut, ref dsBackLogRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsBackLogRep;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderBookRepDataSet SedsOrderBookRep(string prmUser, string prmBookAct, string prmBeginCust, string prmEndCust, DateTime prmBegdate, DateTime prmEndDate, string prmBegsman, string prmEndsman, string prmBegPro, string prmEndPro, string prmSquare, string prmPsman, string prmSortOrd, string prmIdesc, string prmTon, string prmPro, string prmMis, string prmComm, string prmAvailMargin, string prmOut)
    {

        string cError = "";
        dsOrderBookRepDataSet dsOrderBookRep = new dsOrderBookRepDataSet();
        dsOrderBookRep = null;
        AppServerConnect();
        aoObject.OrderBook(prmUser,prmBookAct, prmBeginCust, prmEndCust, prmBegdate, prmEndDate, prmBegsman, prmEndsman, prmBegPro, prmEndPro, prmSquare, prmPsman, prmSortOrd, prmIdesc, prmTon, prmPro, prmMis, prmComm, prmAvailMargin, prmOut, ref dsOrderBookRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsOrderBookRep;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDueDateRepDataSet SeOrdDueRep(string prmUser, string prmDueAct, DateTime prmBeginDate, DateTime prmEndDate, string prmBegsman, string prmEndsman, string prmOutexcel, string prmOut)
    {

        string cError = "";
        dsDueDateRepDataSet dsDueDateRep = new dsDueDateRepDataSet();
        dsDueDateRep = null;
        AppServerConnect();
        aoObject.duereport(prmUser,prmDueAct, prmBeginDate, prmEndDate, prmBegsman, prmEndsman, prmOutexcel, prmOut, ref dsDueDateRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsDueDateRep;
    }  
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOntimeDelDataSet Selectontimedel(string prmUser,string prmActTime, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, DateTime prmBegindate, DateTime prmEnddate, DateTime prmBeginBol, DateTime prmEndBol, string prmPrintwei, string prmPrintmsf, string prmPrinttra)
    {
        string cError = "";
        dsOntimeDelDataSet dsOntimeDel = new dsOntimeDelDataSet();
        dsOntimeDel = null;
        AppServerConnect();
        aoObject.OntimedelRep(prmUser,prmActTime, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmBegindate, prmEnddate, prmBeginBol, prmEndBol, prmPrintwei, prmPrintmsf, prmPrinttra, ref dsOntimeDel, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsOntimeDel;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSchedRelShipDataSet SelectShip(string prmUser, string prmShipAct, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, Int32 prmBeginorder, Int32 prmEndorder, string prmBeginship, string prmEndship, string prmBeginsales, string prmEndsales, DateTime prmBeginDate, DateTime prmEndDate, string prmScheduled, string prmActual, string prmLate, string prmBackOrd, string prmpastdate, string prmPost, string prmComplete, string prmInvoice,string prmSort)
    {
        string cError = "";
        dsSchedRelShipDataSet dsSchedRelShip = new dsSchedRelShipDataSet();
        dsSchedRelShip = null;
        AppServerConnect();
        aoObject.SchedShipRep(prmUser, prmShipAct, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmBeginorder, prmEndorder, prmBeginship, prmEndship,prmBeginsales,prmEndsales,prmBeginDate,prmEndDate,prmScheduled,prmActual,prmLate,prmBackOrd,prmpastdate,prmPost,prmComplete,prmInvoice,prmSort, ref dsSchedRelShip, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSchedRelShip;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBackLogPartRepDataSet SeBacklogPart(string prmUser, string prmBackLog, string prmBeCust, string prmEndCust, string prmBeSalesman, string prmEndSalesman, Int32 prmBeOrder, Int32 prmEndOrder, string prmBeItem, string prmEndItem, DateTime prmBemDue, DateTime pemEndDue, string prmPrintpo, string prmIncJob, string prmItemSub, string prmExcludeCo, string prmPriceSale, string pemExcludeTr, string prmSort, string pemQtyOn, string prmOut)
    {
        string cError = "";
        dsBackLogPartRepDataSet dsBackLogPartRep = new dsBackLogPartRepDataSet();
        dsBackLogPartRep = null;
        AppServerConnect();
        aoObject.backlogpartRep(prmUser, prmBackLog, prmBeCust, prmEndCust, prmBeSalesman, prmEndSalesman, prmBeOrder, prmEndOrder, prmBeItem, prmEndItem, prmBemDue, pemEndDue, prmPrintpo, prmIncJob, prmItemSub, prmExcludeCo, prmPriceSale, pemExcludeTr, prmSort, pemQtyOn, prmOut, out cError, ref dsBackLogPartRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsBackLogPartRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFreightRepDataSet SedsFreightRep(string prmUser, string prmFreight, DateTime prmBeInvDate, DateTime prmEndInvDate, string prmBeCust, string prmEndCust, string prmBeJob1, string prmEndJob1, string prmBeJob2, string prmEndJob2, string prmOut)
    {
        string cError = "";
        dsFreightRepDataSet dsFreightRep = new dsFreightRepDataSet();
        dsFreightRep = null;
        AppServerConnect();
        aoObject.freightrep(prmUser, prmFreight, prmBeInvDate, prmEndInvDate, prmBeCust, prmEndCust, prmBeJob1, prmEndJob1, prmBeJob2, prmEndJob2,prmOut, out cError, ref dsFreightRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFreightRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsfgvaluecostDataSet FgValueCost(string prmUser, string prmFgcost, DateTime prmDate, Int32 prmDaysOld, string prmBeginCust, string prmEndCust, string prmBeginWhse, string prmEndWhse, string prmBeginLocBin, string prmEndLocBin, string prmBeginItem, string prmEndItem, string prmBeginCat, string prmEndCat, string prmSort, string prmIcode, string prmPrint, string prmFrom, string prmSellPrice, string prmIncludeZero, string prmIncludeCustWhse, string prmIncludeOnlyCustWhse, string prmPrintCost, string prmDlMat, string prmPrintCustPart, string prmPrintPo, string prmPoType, string prmPrintDate, string prmPrintCompOnly, string prmPrintSubTot, string prmPrintActRelQty)
    {
        string cError = "";
        dsfgvaluecostDataSet dsfgvaluecost = new dsfgvaluecostDataSet();
        dsfgvaluecost = null;
        AppServerConnect();
        aoObject.fgvaluecost(prmUser, prmFgcost, prmDate, prmDaysOld, prmBeginCust, prmEndCust, prmBeginWhse, prmEndWhse, prmBeginLocBin, prmEndLocBin, prmBeginItem, prmEndItem, prmBeginCat, prmEndCat, prmSort, prmIcode, prmPrint, prmFrom, prmSellPrice, prmIncludeZero, prmIncludeCustWhse, prmIncludeOnlyCustWhse,prmPrintCost,prmDlMat,prmPrintCustPart,prmPrintPo,prmPoType,prmPrintDate,  prmPrintCompOnly,  prmPrintSubTot,  prmPrintActRelQty, ref dsfgvaluecost, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsfgvaluecost;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbalanordpoDataSet BalancePo(string prmUser, string prmActBal, string prmBegcust, string prmEndCust, DateTime prmBegOrdDate, DateTime prmEndOrdDate, string prmBegCustPo, string prmEndCustPo, string prmBegJob, string prmEndJob, string prmBegJob2, string prmEndJob2, string prmBegItem, string prmEndItem, string prmBegSman, string prmEndSman, string prmSort, string prmJobStat, string prmOrdStat, string prmPrint, string prmUnderrun, string prmJobqty, string prmZerobal, string prmPofrom, Int32 prmDays, DateTime prmDate, string prmZeroqoh, string prmPageBreak, string prmSchRel, string prmOut)
    {
        string cError = "";
        dsbalanordpoDataSet dsbalanordpo = new dsbalanordpoDataSet();
        dsbalanordpo = null;
        AppServerConnect();
        aoObject.ordbalance(prmUser, prmActBal, prmBegcust, prmEndCust,  prmBegOrdDate, prmEndOrdDate, prmBegCustPo, prmEndCustPo, prmBegJob, prmEndJob, prmBegJob2, prmEndJob2, prmBegItem, prmEndItem, prmBegSman, prmEndSman, prmSort, prmJobStat, prmOrdStat, prmPrint, prmUnderrun, prmJobqty, prmZerobal, prmPofrom, prmDays, prmDate, prmZeroqoh,prmPageBreak,prmSchRel,prmOut, ref dsbalanordpo, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsbalanordpo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbalanordjobDataSet BalancePoJob(string prmUser, string prmActBal, string prmBegcust, string prmEndCust, DateTime prmBegOrdDate, DateTime prmEndOrdDate, string prmBegCustPo, string prmEndCustPo, string prmBegJob, string prmEndJob, string prmBegJob2, string prmEndJob2, string prmBegItem, string prmEndItem, string prmBegSman, string prmEndSman, string prmSort, string prmJobStat, string prmOrdStat, string prmPrint, string prmUnderrun, string prmJobqty, string prmZerobal, string prmPofrom, Int32 prmDays, DateTime prmDate, string prmZeroqoh, string prmPageBreak, string prmSchRel, string prmOut)
    {
        string cError = "";
        dsbalanordjobDataSet dsbalanordjob = new dsbalanordjobDataSet();
        dsbalanordjob = null;
        AppServerConnect();
        aoObject.ordbalancejob(prmUser, prmActBal, prmBegcust, prmEndCust, prmBegOrdDate, prmEndOrdDate, prmBegCustPo, prmEndCustPo, prmBegJob, prmEndJob, prmBegJob2, prmEndJob2, prmBegItem, prmEndItem, prmBegSman, prmEndSman, prmSort, prmJobStat, prmOrdStat, prmPrint, prmUnderrun, prmJobqty, prmZerobal, prmPofrom, prmDays, prmDate, prmZeroqoh, prmPageBreak, prmSchRel,prmOut, ref dsbalanordjob, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsbalanordjob;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBillofLadRepDataSet SeBillofRep(string prmUser, string prmBillAct, string prmBeginCust, string prmEndCust, Int32 prmBeBoll, Int32 prmEndBoll, Int32 prmBeOrder, Int32 prmEndOrder, string prmBillofLad, string prmPallet, string prmPostedBol, string prmPackList, string prmPrint, string prmBolPost)
    {
        string cError = "";
        dsBillofLadRepDataSet dsBillofLadRep = new dsBillofLadRepDataSet();
        dsBillofLadRep = null;
        AppServerConnect();
        aoObject.billofladrep(prmUser, prmBillAct, prmBeginCust, prmEndCust, prmBeBoll, prmEndBoll, prmBeOrder, prmEndOrder, prmBillofLad, prmPallet, prmPostedBol, prmPackList, prmPrint,prmBolPost, ref dsBillofLadRep,out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsBillofLadRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSchedReleaseRepDataSet ScheduleRelease(string prmUser, string prmSchedAct, string prmBeginCust, string prmEndCust, Int32 prmBeOrder, Int32 prmEndOrder, string prmBeItem, string prmEndItem, string prmBeLoc, string prmEndLoc, string prmBeSales, string prmEndSales, string prmBeDate, string prmEndDate, string prmBeCarrier, string prmEndCarrier, string prmBeSpec, string prmEndSpec, string prmBeCat, string prmEndCat, string prmSeheduled, string prmLate, string prmLastShipDate, string prmActual, string prmBackOrder, string prmBillLading, string prmInvPosted, string prmCompleted, string prmSort, string prmPrintOpt1, string prmPrintOpt2, string prmPrintOpt3, string prmPrintDueAlert, string prmPrintPo, string prmSubTotalCust, string prmPrintStats, string prmPrintRelease, string prmSpecNote,string prmOut)
    {
        string cError = "";
        dsSchedReleaseRepDataSet dsSchedReleaseRep = new dsSchedReleaseRepDataSet();
        dsSchedReleaseRep = null;
        AppServerConnect();
        aoObject.SchedRelease(prmUser, prmSchedAct, prmBeginCust, prmEndCust, prmBeOrder, prmEndOrder, prmBeItem, prmEndItem, prmBeLoc, prmEndLoc, prmBeSales, prmEndSales, prmBeDate, prmEndDate, prmBeCarrier, prmEndCarrier, prmBeSpec, prmEndSpec, prmBeCat, prmEndCat, prmSeheduled, prmLate, prmLastShipDate, prmActual, prmBackOrder, prmBillLading, prmInvPosted, prmCompleted, prmSort, prmPrintOpt1, prmPrintOpt2, prmPrintOpt3, prmPrintDueAlert, prmPrintPo, prmSubTotalCust, prmPrintStats, prmPrintRelease, prmSpecNote,prmOut,out cError, ref dsSchedReleaseRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSchedReleaseRep;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSchedRelWoShipDataSet dsSchedRelWoShip(string prmUser, string SchedRel, string vBeginCust, string vEndCust, Int32 vBeginOrder, Int32 vEndOrder, string vBeginItem, string vEndItem, string vBeginLoc, string vEndLoc, string vBeginSalesMan, string vEndSalesMan, DateTime vBeginDate, DateTime vEndDate, string vBeginCarrier, string vEndCarrier, string vScheduled, string vActual, string vLate, string vBackOrder, string vPastLastShip, string vPosted, string vCompleted, string vInvoice, string vPrintComp, string vPrintQtyOnHand, string vSubTotal, string vPrintLastShipDate, string vSort, string vPrintMsf, string vPrint, string vRdPrint3, string vBeginProdCat, string vEndProdCat, string vPrintDue, string prmOut)
    {
        string cError = "";
        dsSchedRelWoShipDataSet dsSchedRelWoShip = new dsSchedRelWoShipDataSet();
        dsSchedRelWoShip = null;
        AppServerConnect();
        aoObject.SchedRelWOShip(prmUser,  SchedRel,  vBeginCust,  vEndCust,  vBeginOrder,  vEndOrder,  vBeginItem,  vEndItem,  vBeginLoc,  vEndLoc,  vBeginSalesMan,  vEndSalesMan,  vBeginDate,  vEndDate,  vBeginCarrier, vEndCarrier,  vScheduled,  vActual,  vLate,  vBackOrder,  vPastLastShip,  vPosted,  vCompleted,  vInvoice,  vPrintComp,  vPrintQtyOnHand,  vSubTotal,  vPrintLastShipDate,  vSort,  vPrintMsf,  vPrint, vRdPrint3, vBeginProdCat, vEndProdCat, vPrintDue, prmOut, ref dsSchedRelWoShip, out cError );
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSchedRelWoShip;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCommissionRepDataSet Commission(string prmUser, string CommRep, string prmPtdYtd, Int32 prmPeriod, DateTime prmBegDate, DateTime prmEndDate, string prmBegSales, string prmEndSales, string prmBegCust, string prmEndCust, string prmCategory, string prmDetailed, string prmPrepCharg, string prmCostProfit, string prmCost, string prmPrintCustPart, string prmOut)
    {
        string cError = "";
        dsCommissionRepDataSet dsCommissionRep = new dsCommissionRepDataSet();
        dsCommissionRep = null;
        AppServerConnect();
        aoObject.CommissionRep(prmUser, CommRep, prmPtdYtd, prmPeriod, prmBegDate, prmEndDate, prmBegSales, prmEndSales, prmBegCust, prmEndCust, prmCategory, prmDetailed, prmPrepCharg,prmCostProfit,  prmCost,  prmPrintCustPart,  prmOut, out cError, ref dsCommissionRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsCommissionRep;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSalesmanRepDataSet SeSalesmanRep(string prmUser, string prmSalesAct, DateTime prmBeDate, string prmBeSales, string prmEndSales,string prmOut)
    {
        string cError = "";
        dsSalesmanRepDataSet dsSalesmanRep = new dsSalesmanRepDataSet();
        dsSalesmanRep = null;
        AppServerConnect();
        aoObject.SalesPerRep(prmUser, prmSalesAct, prmBeDate, prmBeSales, prmEndSales,prmOut, ref dsSalesmanRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSalesmanRep;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInvCustReorder1DataSet SelCustInvReReport1(string prmUser,string prmAct, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, string prmBeginShip, string prmEndShip, string prmBeginCat, string prmEndCat, string prmcustom, string prIncQoh, string prmUse, DateTime prmAsDate, string prmPrintPur, string prmPrintLot)
    {
        string cError = "";
        dsInvCustReorder1DataSet dsInvCustReorder1 = new dsInvCustReorder1DataSet();
        dsInvCustReorder1 = null;
        AppServerConnect();
        aoObject.CustInReReport(prmUser,prmAct, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmBeginShip, prmEndShip, prmBeginCat, prmEndCat, prmcustom, prIncQoh, prmUse, prmAsDate, prmPrintPur, prmPrintLot, ref dsInvCustReorder1, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsInvCustReorder1;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustInvReReport2DataSet SelectCustInvReReport2(string prmUser, string prmReAct, string prmBegCust, string prmEndCust, string prmBegShipto, string prmEndShitto, string prmBegItem, string prmEndItem, string prmBegCat, string prmEndCat, string prmCustInv, string prmQOnHand, string prmTotalAlloc, DateTime prmAsOf, string prmPrint, string prmPrintLotCtl)
    {
        string cError = "";
        dsCustInvReReport2DataSet dsCustInvReReport2 = new dsCustInvReReport2DataSet();
        dsCustInvReReport2 = null;
        AppServerConnect();
        aoObject.CustInvReReport2(prmUser, prmReAct, prmBegCust, prmEndCust, prmBegShipto, prmEndShitto, prmBegItem, prmEndItem, prmBegCat, prmEndCat, prmCustInv, prmQOnHand, prmTotalAlloc, prmAsOf, prmPrint, prmPrintLotCtl, ref dsCustInvReReport2,out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsCustInvReReport2;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInvBySalesmanDataSet SelectInvBySalesman(string prmUser,string prmAction, string vBeginCust, string vEndCust, string vBeginPo, string vEndPo, string vBeSaleman, string vEndSalesman, string vQtyonHand, string vCustWarehouse, string vSort, string prmOut)
    {
        string cError = "";
        dsInvBySalesmanDataSet dsInvBySalesman = new dsInvBySalesmanDataSet();
        dsInvBySalesman = null;
        AppServerConnect();
        aoObject.InvBySalesman(prmUser,prmAction, vBeginCust, vEndCust, vBeginPo, vEndPo, vBeSaleman, vEndSalesman, vQtyonHand, vCustWarehouse, vSort,prmOut, ref dsInvBySalesman, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsInvBySalesman;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReportQtyValDataSet SelectQtycustjob(string prmUser, string prmAction, string prmComp, string prmBeginCust, string prmEndCust, string prmBeginPo, string prmEndPo, string prmBeSaleman, string prmEndSalesman, string prmItemCode, string prmZeroQty, string prmInvWarehouse, string prmReceipt, string prmCustPart, string prmOut)
    {
        string cError = "";
        dsReportQtyValDataSet dsReportQtyVal = new dsReportQtyValDataSet();
        dsReportQtyVal = null;
        AppServerConnect();
        aoObject.rep_qtyval(prmUser, prmAction, prmComp, prmBeginCust, prmEndCust, prmBeginPo, prmEndPo, prmBeSaleman, prmEndSalesman, prmItemCode, prmZeroQty, prmInvWarehouse, prmReceipt, prmCustPart, prmOut, ref dsReportQtyVal, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsReportQtyVal;
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuotePrintDataSet SelectQuotePrint(string prmAction, string prmUser, string prmBeginCust, string prmEndCust, string prmBeginDept, string prmEndDept, Int32 prmBeginQuote, Int32 prmEndQuote, string prmQuoteList, string prmInst, string prmNotesSpanPage, string prmNote, string prmPrtBox, string prmComm, string prmPrtComp, string prmPrint2ndDscr, string prmRsNote, string prmOut)
    {   
        dsQuotePrintDataSet dsQuotePrint = new dsQuotePrintDataSet();
        dsQuotePrint = null;

        string vFile = "";
        string vMailto = "";
        string vBody = "";
        string vMailFrom = null;
        string vSubject = null;
    
        AppServerConnect();        
        aoObject.quoteprint(prmAction, prmUser, prmBeginCust, prmEndCust, prmBeginDept, prmEndDept, prmBeginQuote, prmEndQuote, prmQuoteList, prmInst, prmNotesSpanPage, prmNote, prmPrtBox, prmComm, prmPrtComp, prmPrint2ndDscr, prmRsNote, prmOut, ref dsQuotePrint, out vFile, out vMailto, out vBody);
        AppServerDisconnect();

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        string cmd = "select email_from, subject from email_alerts where program_name = 'Print Quote'";
        string cmd2 = "select email from user_master where Username = '" + prmUser + "'";
        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
        DataSet ds = new DataSet();
        da.Fill(ds);
        DataSet ds2 = new DataSet();
        da2.Fill(ds2);
        conn.Close();

        try
        {
            vMailFrom = Convert.ToString(ds2.Tables[0].Rows[0][0]);
            vSubject = Convert.ToString(ds.Tables[0].Rows[0][1]);
        }
        catch
        {

        }
        finally
        {
            conn.Close();
        }

        /*if (vMailto != "")
        {
            MailMessage om = new MailMessage();
            om.To = vMailto;
            om.To = "sunil.rawal@gmail.com";
            om.From = vMailFrom;
            om.Subject = vSubject;
            om.Body = vBody;
            om.BodyFormat = MailFormat.Html;
            om.Priority = MailPriority.High;
            SmtpMail.SmtpServer = "206.53.59.52";
            SmtpMail.Send(om);
        }
         */

        return dsQuotePrint;
    } 
    
    

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsFgSalesValByCustByRecDateDataSet SelectSalesCustRecDate(string prmUser, string prmAction, string prmOut, DateTime prmDate, string prmBeginCust, string prmEndCust, string prmBeginCustPo, string prmEndCustPo, string prmBeginSman, string prmEndSman, string prmIncludeZeroQty, string prmIncCustWareHouse, string prmCustPart, string prmPrint, string prmSort, Int32 prmOlderThan)
    //{
    //    string cError = "";
    //    dsFgSalesValByCustByRecDateDataSet dsFgSalesValByCustByRecDate = new dsFgSalesValByCustByRecDateDataSet();
    //    dsFgSalesValByCustByRecDate = null;
    //    AppServerConnect();
    //    aoObject.FgSalesValByCustByRecDate(prmUser, prmAction, prmOut, prmDate, prmBeginCust, prmEndCust, prmBeginCustPo, prmEndCustPo, prmBeginSman, prmEndSman, prmIncludeZeroQty, prmIncCustWareHouse, prmCustPart, prmPrint, prmSort, prmOlderThan, out cError, ref dsFgSalesValByCustByRecDate);
    //    AppServerDisconnect();
    //    if (cError != "")
    //    {
    //        HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
    //        HttpContext.Current.Response.Write("<script>history.back()</script>");
    //    }

    //    return dsFgSalesValByCustByRecDate;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReportCustagDataSet Selectcustag(string prmUser, string prmAction, string prmComp, string prmBeginCust, string prmEndCust, string prmBeginPo, string prmEndPo, string prmBeSaleman, string prmEndSalesman, string prmZeroQty, string prmInvWarehouse, string prmOut)
    {
        string cError = "";
        dsReportCustagDataSet dsReportCustag = new dsReportCustagDataSet();
        dsReportCustag = null;
        AppServerConnect();
        aoObject.rep_custag(prmUser, prmAction, prmComp, prmBeginCust, prmEndCust, prmBeginPo, prmEndPo, prmBeSaleman, prmEndSalesman, prmZeroQty, prmInvWarehouse, prmOut, ref dsReportCustag, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsReportCustag;
    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsFgSalesValueByCustByTagDataSet SelectYTDByCust(string prmUser, string prmAction, string prmOut, string prmBeginWare, string prmEndWare, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, string prmIncCust)
    //{
    //    string cError = "";
    //    dsFgSalesValueByCustByTagDataSet dsFgSalesValueByCustByTag = new dsFgSalesValueByCustByTagDataSet();
    //    dsFgSalesValueByCustByTag = null;
    //    AppServerConnect();
    //    aoObject.FgSalesValueByCustByTag_Rep(prmUser, prmAction, prmOut, prmBeginWare, prmEndWare, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmIncCust, out cError, ref dsFgSalesValueByCustByTag);
    //    AppServerDisconnect();
    //    if (cError != "")
    //    {
    //        HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
    //        HttpContext.Current.Response.Write("<script>history.back()</script>");
    //    }

    //    return dsFgSalesValueByCustByTag;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopbtnOrderReportDataSet TopbtnOrderReport(string prmUser, string prmAction, string prmOut, string prmopenclose, string prmBeginCust, string prmEndCust, string prmOrder, string prmCust, string prmOrderDate, string prmFgItem, string prmCustPart, string prmItemName, string prmCustPo, string prmOrderQty, string prmProdQty, string prmShipQty, string prmOnHandQty, string prmSellPrice, string prmUom, string prmUnitCost, string prmPalletCount, string prmSkids, string prmStatus, string prmDueDate, string prmCustName, string prmEst, string prmJob, string prmCad, string prmInvoiceQty, string prmActRelQty, string prmProdBal, string prmOU)
    {
        string cError = "";
        dsTopbtnOrderReportDataSet dsTopbtnOrderReport = new dsTopbtnOrderReportDataSet();
        dsTopbtnOrderReport = null;
        AppServerConnect();
        aoObject.TopbtnOrderReport(prmUser, prmAction, prmOut, prmopenclose, prmBeginCust, prmEndCust, prmOrder, prmCust, prmOrderDate, prmFgItem, prmCustPart, prmItemName, prmCustPo, prmOrderQty, prmProdQty, prmShipQty, prmOnHandQty, prmSellPrice, prmUom, prmUnitCost, prmPalletCount, prmSkids, prmStatus, prmDueDate, prmCustName, prmEst, prmJob, prmCad, prmInvoiceQty, prmActRelQty, prmProdBal, prmOU, ref dsTopbtnOrderReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsTopbtnOrderReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopPrintOrderAckReportDataSet TopOrderAckReport(string prmUser, string prmAction, string prmOut, Int32 prmBeginOrder, Int32 prmEndOrder, string prmBeginCust, string prmEndCust, DateTime prmBeginOrdDate, DateTime prmEndOrdDate, Int32 prmBeginRel, Int32 prmEndRel, string prmReprintAck, string prmConsForm, string prmWareHouse, string prmMonths, string prmSchRel, string prmSpecNotes, string prmShipAddr, string prmActRel, string prmPrintRevised, string prmBillMat)
    {
        string cError = "";
        dsTopPrintOrderAckReportDataSet dsTopPrintOrderAckReport = new dsTopPrintOrderAckReportDataSet();
        dsTopPrintOrderAckReport = null;
        AppServerConnect();
        aoObject.TopPrintOrderAck_Report(prmUser, prmAction, prmOut, prmBeginOrder, prmEndOrder, prmBeginCust, prmEndCust, prmBeginOrdDate, prmEndOrdDate, prmBeginRel, prmEndRel, prmReprintAck, prmConsForm, prmWareHouse, prmMonths, prmSchRel, prmSpecNotes, prmShipAddr, prmActRel, prmPrintRevised, prmBillMat, ref dsTopPrintOrderAckReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsTopPrintOrderAckReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRedFlagListDataSet RedFlagReport(string prmUser, string prmAction, string prmOut, string vFIBegCustPartNo, string vFIBegFgItemNo, string vFIBegVendCode, string vFIBegVendPlantCode, string vFIEndCustPartNo, string vFIEndFgItemNo, string vFIEndVendCode, string vFIEndVendPlantCode, Int32 vFINumberOfWeeks, string vTGPrintRqMaterials)
    {
        string cError = "";
        dsRedFlagListDataSet dsRedFlagList = new dsRedFlagListDataSet();
        dsRedFlagList = null;
        AppServerConnect();
        aoObject.RedFlagRpt(prmUser, prmAction, prmOut, vFIBegCustPartNo, vFIBegFgItemNo, vFIBegVendCode, vFIBegVendPlantCode, vFIEndCustPartNo, vFIEndFgItemNo, vFIEndVendCode, vFIEndVendPlantCode, vFINumberOfWeeks, vTGPrintRqMaterials, ref dsRedFlagList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsRedFlagList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsExceptionsListDataSet ExceptionsReport(string prmUser, string prmAction, string prmOut, string prmFIBegCustPartNo, string prmFIBegFgItemNo, string prmFIBegVendCode, string prmFIBegVendPlantCode, string prmFIEndCustPartNo, string prmFIEndFgItemNo, string prmFIEndVendCode, string prmFIEndVendPlantCode)
    {
        string cError = "";
        dsExceptionsListDataSet dsExceptionsList = new dsExceptionsListDataSet();
        dsExceptionsList = null;
        AppServerConnect();
        aoObject.ExceptionsRpt(prmUser, prmAction, prmOut, prmFIBegCustPartNo, prmFIBegFgItemNo, prmFIBegVendCode, prmFIBegVendPlantCode, prmFIEndCustPartNo, prmFIEndFgItemNo, prmFIEndVendCode, prmFIEndVendPlantCode, ref dsExceptionsList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsExceptionsList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsProdHighlightsReportDataSet SelectProdHighReport(string prmUser, string prmAction, string prmDate, string prmComp, string prmMachine1, string prmMachine2, string prmMachine3, string prmMachine4, string prmMachine5, string prmMachine6, string prmMachine7, string prmMachine8, string prmMachine9, string prmMachine10, string prmMachine11, string prmMachine12, string prmMachine13, string prmMachine14, string prmMachine15, string prmOutexcel)
    {
        string cError = "";
        dsProdHighlightsReportDataSet dsProdHighlightsReport = new dsProdHighlightsReportDataSet();
        dsProdHighlightsReport = null;
        AppServerConnect();
        aoObject.ProdHighReport(prmUser, prmAction, prmDate, prmComp, prmMachine1,prmMachine2, prmMachine3,  prmMachine4,  prmMachine5,  prmMachine6,  prmMachine7,  prmMachine8,  prmMachine9,  prmMachine10,  prmMachine11,  prmMachine12,  prmMachine13,  prmMachine14,  prmMachine15, prmOutexcel, ref dsProdHighlightsReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsProdHighlightsReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSalesRepReportDataSet SelectSalesRepHighReport(string prmUser, string prmAction, string prmDate, string prmComp, string prmOutexcel)
    {
        string cError = "";
        dsSalesRepReportDataSet dsSalesRepReport = new dsSalesRepReportDataSet();
        dsSalesRepReport = null;
        AppServerConnect();
        aoObject.SalesRepHighReport(prmUser, prmAction, prmDate, prmComp, prmOutexcel, ref dsSalesRepReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSalesRepReport;
    }

    
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBookingReportDataSet SelectBookingHighReport(string prmUser, string prmAction, string prmDate, string prmComp, string prmOutexcel)
    {
        string cError = "";
        dsBookingReportDataSet dsBookingReport = new dsBookingReportDataSet();
        dsBookingReport = null;
        AppServerConnect();
        aoObject.BookingsHighReport(prmUser, prmAction, prmDate, prmComp, prmOutexcel, ref dsBookingReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsBookingReport;
    }
    
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInvoiceReportDataSet SelectInvoiceHighReport(string prmUser, string prmAction, string prmDate, string prmComp, string prmOutexcel)
    {
        string cError = "";
        dsInvoiceReportDataSet dsInvoiceReport = new dsInvoiceReportDataSet();
        dsInvoiceReport = null;
        AppServerConnect();
        aoObject.InvoiceHighReport(prmUser, prmAction, prmDate, prmComp, prmOutexcel, ref dsInvoiceReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsInvoiceReport;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSignaturePrintBolDataSet SelectSignatureBol(string prmAction, string prmComp, string prmUser, string prmcustno, Int32 prmbolno, string prmprinted, string prmposted, string prmpostbol, string prmBegDate, string prmEndDate, string prmCarrier, string imagepath, Int32 prmBegOrder, Int32 prmEndOrder, string prmPage, string prmPdfPath)
    {
        string cError = "";
        dsSignaturePrintBolDataSet dsSignaturePrintBol = new dsSignaturePrintBolDataSet();
        dsSignaturePrintBol = null;
        AppServerConnect();
        aoObject.signature_print_bol(prmAction, prmComp, prmUser, prmcustno, prmbolno, prmprinted, prmposted, prmpostbol, prmBegDate, prmEndDate, prmCarrier, imagepath,prmBegOrder,  prmEndOrder, prmPage,prmPdfPath, out cError, ref dsSignaturePrintBol);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSignaturePrintBol;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMachineFileReportDataSet SelectMachFileRep(string prmUser, string prmAction, string prmBegMachine, string prmEndMachine, string prmBegDeptCode, string prmEndDeptCode, string prmShowStandard, string prmOutexcel, string prmOut)
    {

        string cError = "";
        dsMachineFileReportDataSet dsMachineFileReport = new dsMachineFileReportDataSet();
        dsMachineFileReport = null;
        AppServerConnect();
        aoObject.MachineFileRep(prmUser, prmAction, prmBegMachine, prmEndMachine, prmBegDeptCode, prmEndDeptCode, prmShowStandard, prmOutexcel, prmOut, ref dsMachineFileReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsMachineFileReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsStyleFileReportDataSet SelectStyleRep(string prmUser, string prmAction, string prmCor, string prmFold, string prmOutexcel)
    {

        string cError = "";
        dsStyleFileReportDataSet dsStyleFileReport = new dsStyleFileReportDataSet();
        dsStyleFileReport = null;
        AppServerConnect();
        aoObject.StyleReport(prmUser, prmAction,  prmCor,  prmFold,  prmOutexcel, ref dsStyleFileReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsStyleFileReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPrepFileReportDataSet SelectPrepFileRep(string prmUser, string prmAction, string prmBeginPrep, string prmEndPrep, string prmPrintDetail, string prmOut)
    {

        string cError = "";
        dsPrepFileReportDataSet dsPrepFileReport = new dsPrepFileReportDataSet();
        dsPrepFileReport = null;
        AppServerConnect();
        aoObject.PrepFileRep(prmUser, prmAction, prmBeginPrep, prmEndPrep, prmPrintDetail, prmOut, ref dsPrepFileReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsPrepFileReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsControlFileRepDataSet SelectControlRep(string prmUser, string prmAction)
    {

        string cError = "";
        dsControlFileRepDataSet dsControlFileRep = new dsControlFileRepDataSet();
        dsControlFileRep = null;
        AppServerConnect();
        aoObject.ControlRep(prmUser, prmAction, ref dsControlFileRep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsControlFileRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuoteListReportDataSet SelectQtListRep(string prmUser, string prmAction, string prmBegCustomer, string prmEndCustomer, string prmBeginDate, string prmEndDate, string prmBegSalrep, string prmEndSalrep, string prmSortBy, string prmBoCost, string prmOut)
    {

        string cError = "";
        dsQuoteListReportDataSet dsQuoteListReport = new dsQuoteListReportDataSet();
        dsQuoteListReport = null;
        AppServerConnect();
        aoObject.QuoteListRep(prmUser, prmAction, prmBegCustomer, prmEndCustomer, prmBeginDate, prmEndDate, prmBegSalrep, prmEndSalrep, prmSortBy, prmBoCost, prmOut, ref dsQuoteListReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsQuoteListReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEstListReportDataSet SelectEstListRep(string prmUser, string prmAction, string prmBegCustomer, string prmEndCustomer, string prmBegAddDate, string prmEndAddDate, string prmBegModDate, string prmEndModDate, string prmBegEst, string prmEndEst, string prmBegMach, string prmEndMach, string prmBegSalrep, string prmEndSalrep, string prmBreak, string prmBooked, string prmNotBooked, string prmSort, string prmOut)
    {

        string cError = "";
        dsEstListReportDataSet dsEstListReport = new dsEstListReportDataSet();
        dsEstListReport = null;
        AppServerConnect();
        aoObject.EstListRep(prmUser, prmAction, prmBegCustomer, prmEndCustomer, prmBegAddDate, prmEndAddDate, prmBegModDate, prmEndModDate, prmBegEst, prmEndEst, prmBegMach, prmEndMach, prmBegSalrep, prmEndSalrep, prmBreak, prmBooked, prmNotBooked, prmSort, prmOut, ref dsEstListReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsEstListReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPrepMatReportDataSet SelectPrepMatRep(string prmUser, string prmAction, string prmBegCustomer, string prmEndCustomer, string prmBegLastModDate, string prmEndLastModDate, string prmBegLastOrdDate, string prmEndLastOrdDate, string prmBegSalrep, string prmEndSalrep, string prmBegPrepCode, string prmEndPrepCode, string prmDie, string prmPlate, string prmFolding, string prmCorrugated, string prmSortBy, string prmprintDscr, string prmOut)
    {

        string cError = "";
        dsPrepMatReportDataSet dsPrepMatReport = new dsPrepMatReportDataSet();
        dsPrepMatReport = null;
        AppServerConnect();
        aoObject.PrepMatRep(prmUser, prmAction, prmBegCustomer, prmEndCustomer, prmBegLastModDate, prmEndLastModDate, prmBegLastOrdDate, prmEndLastOrdDate, prmBegSalrep, prmEndSalrep, prmBegPrepCode, prmEndPrepCode, prmDie, prmPlate, prmFolding, prmCorrugated, prmSortBy, prmprintDscr, prmOut, ref dsPrepMatReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsPrepMatReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuotedPriceReportDataSet SelectQtdPriceRep(string prmUser, string prmAction, string prmBegCustomer, string prmEndCustomer, string prmBeginDate, string prmEndDate, string prmBegSalrep, string prmEndSalrep, string prmOut)
    {

        string cError = "";
        dsQuotedPriceReportDataSet dsQuotedPriceReport = new dsQuotedPriceReportDataSet();
        dsQuotedPriceReport = null;
        AppServerConnect();
        aoObject.QuotedPriceRep(prmUser, prmAction, prmBegCustomer, prmEndCustomer, prmBeginDate, prmEndDate, prmBegSalrep, prmEndSalrep, prmOut, ref dsQuotedPriceReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsQuotedPriceReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEstimateWmargRepDataSet SelectEstimatemargRep(string prmUser, string prmAction, string prmBegCust, string prmEndCust, string prmBegSman, string prmEndSman, string prmBegEst, string prmEndEst, string prmAddDate, string prmEndAddDate, string prmModDate, string prmEndModDate, string prmOut)
    {
        string cError = "";
        dsEstimateWmargRepDataSet dsEstimateWmargRep = new dsEstimateWmargRepDataSet();
        dsEstimateWmargRep = null;
        AppServerConnect();
        aoObject.EstmarRep(prmUser, prmAction, prmBegCust, prmEndCust, prmBegSman, prmEndSman, prmBegEst, prmEndEst, prmAddDate, prmEndAddDate, prmModDate, prmEndModDate, prmOut, out cError, ref dsEstimateWmargRep);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsEstimateWmargRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEstbysizeReportDataSet SelectEstBySizeRep(string prmUser, string prmAction, string prmBegCustomer, string prmEndCustomer, string prmBeginStyle, string prmEndStyle, string prmBegFlute, string prmEndFlute, string prmBegTest, string prmEndTest, string prmBegDie, string prmEndDie, string prmSortBy, string prmFold, string prmCorr, string prmOut)
    {

        string cError = "";
        dsEstbysizeReportDataSet dsEstbysizeReport = new dsEstbysizeReportDataSet();
        dsEstbysizeReport = null;
        AppServerConnect();
        aoObject.EstBySizeRep(prmUser, prmAction, prmBegCustomer, prmEndCustomer, prmBeginStyle, prmEndStyle, prmBegFlute, prmEndFlute, prmBegTest, prmEndTest, prmBegDie, prmEndDie, prmSortBy, prmFold, prmCorr, prmOut, ref dsEstbysizeReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsEstbysizeReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsYtdcstRepDataSet SelectYtdcstRep(string prmUser, string prmAction, string prmBeWare, string prmEndWare, string prmBeCust, string prmEndCust, string prmBeItem, string prmEndItem, string prmCustOwned, string prmOut)
    {

        string cError = "";
        dsYtdcstRepDataSet dsYtdcstRep = new dsYtdcstRepDataSet();
        dsYtdcstRep = null;
        AppServerConnect();
        aoObject.YtdcstRep(prmUser, prmAction, prmBeWare, prmEndWare, prmBeCust, prmEndCust, prmBeItem, prmEndItem, prmCustOwned, prmOut, out cError, ref dsYtdcstRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsYtdcstRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustimReceiptDateRepDataSet SelectCustimReceiptDateRep(string prmUser, string prmAction, string prmAsof, string prmBeCust, string prmEndCust, string prmBePo, string prmEndPo, string prmBeSman, string prmEndSman, Int32 prmOlder, string prmQtyonHand, string prmWarehouse, string prmCustPart, string prmOrderDue, string prmReceipt, string prmOut)
    {

        string cError = "";
        dsCustimReceiptDateRepDataSet dsCustimReceiptDateRep = new dsCustimReceiptDateRepDataSet();
        dsCustimReceiptDateRep = null;
        AppServerConnect();
        aoObject.CustimRep(prmUser, prmAction, prmAsof, prmBeCust, prmEndCust, prmBePo, prmEndPo, prmBeSman, prmEndSman, prmOlder, prmQtyonHand, prmWarehouse, prmCustPart, prmOrderDue, prmReceipt, prmOut, out cError, ref dsCustimReceiptDateRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsCustimReceiptDateRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsStatusReportDataSet SelectStatusRep(string prmUser, string prmAction, string prmBegCust, string prmEndCust, string prmBeginItem, string prmEndItem, string prmOut)
    {
        string cError = "";
        dsStatusReportDataSet dsStatusReport = new dsStatusReportDataSet();
        dsStatusReport = null;
        AppServerConnect();
        aoObject.statusrep(prmUser, prmAction, prmBegCust, prmEndCust, prmBeginItem, prmEndItem, prmOut, ref dsStatusReport, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsStatusReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRitmListReportDataSet RitmListReport(string prmUser, string prmAction, string prmBeWare, string prmEndWare, string prmBeCust, string prmEndCust, string prmBeItem, string prmEndItem, string prmBeCat, string prmEndCat, string prmZero, string prmCustWhse, string prmSort, string prmProdCat, string prmOut)
    {
        string cError = "";
        dsRitmListReportDataSet dsRitmListReport = new dsRitmListReportDataSet();
        dsRitmListReport = null;
        AppServerConnect();
        aoObject.Ritmlst(prmUser, prmAction, prmBeWare, prmEndWare, prmBeCust, prmEndCust, prmBeItem, prmEndItem, prmBeCat, prmEndCat, prmZero, prmCustWhse, prmSort, prmProdCat, prmOut, out cError, ref dsRitmListReport);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRitmListReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsValJobReportDataSet SelectFgValJob(string prmUser, string prmAction, string prmBegCustomer, string prmEndCustomer, string prmBegCustPO, string prmEndCustPO, string prmItemCode, string prmConJob, string prmIncZer, string prmOut)
    {
        string cError = "";
        dsValJobReportDataSet dsValJobReport = new dsValJobReportDataSet();
        dsValJobReport = null;
        AppServerConnect();
        aoObject.ValjobRep(prmUser, prmAction, prmBegCustomer, prmEndCustomer, prmBegCustPO, prmEndCustPO, prmItemCode, prmConJob, prmIncZer, prmOut, ref dsValJobReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsValJobReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFgCostRepDataSet SelectFgcostRep(string prmUser, string prmAction, string prmAsofdt, string prmBeginCust, string prmEndCust, string prmBeginWhse, string prmEndWhse, string prmBeginItem, string prmEndItem, string prmBeginCat, string prmEndCat, string prmIcode, string prmSort, string prmPrint, string prmCustPart, string prmOut)
    {

        string cError = "";
        dsFgCostRepDataSet dsFgCostRep = new dsFgCostRepDataSet();
        dsFgCostRep = null;
        AppServerConnect();
        aoObject.Fgcost(prmUser, prmAction, prmAsofdt, prmBeginCust, prmEndCust, prmBeginWhse, prmEndWhse, prmBeginItem, prmEndItem, prmBeginCat, prmEndCat, prmIcode, prmSort, prmPrint, prmCustPart, prmOut, out cError, ref dsFgCostRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFgCostRep;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRageinvRepDataSet RageinvRep(string prmUser, string prmAction, string prmAsof, string prmBeSman, string prmEndSman, string prmBeCust, string prmEndCust, string prmBeItem, string prmEndItem, string prmBeJob, string prmEndJob, string prmBeJob2, string prmEndjob2, string prmBeWare, string prmEndWare, string prmInvClass, Int32 prmAgedDay1, Int32 prmAgedDay2, Int32 prmAgedDay3, Int32 prmAgedDay4, string prmRdPrice, string prmQtyValue, string prmComment, string prmSort, string prmCustWhse, string prmPgBreak, string prmTbCurr, string prmCustPart, string prmCost, string prmNegSale, string prmValCust, string prmLastShip, string prmOut)
    {

        string cError = "";
        dsRageinvRepDataSet dsRageinvRep = new dsRageinvRepDataSet();
        dsRageinvRep = null;
        AppServerConnect();
        aoObject.AgeinvRep(prmUser, prmAction, prmAsof,  prmBeSman,  prmEndSman,  prmBeCust,  prmEndCust,  prmBeItem,  prmEndItem,  prmBeJob,  prmEndJob,  prmBeJob2,  prmEndjob2,  prmBeWare,  prmEndWare,  prmInvClass,  prmAgedDay1,  prmAgedDay2,  prmAgedDay3,  prmAgedDay4,  prmRdPrice,  prmQtyValue,  prmComment,  prmSort,  prmCustWhse,  prmPgBreak,  prmTbCurr,  prmCustPart,  prmCost,  prmNegSale,  prmValCust,  prmLastShip, prmOut, out cError, ref dsRageinvRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsRageinvRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFgCostSummRepDataSet SelectFgcostSummRep(string prmUser, string prmAction, string prmAsofdt, string prmBeginCust, string prmEndCust, string prmBeginWhse, string prmEndWhse, string prmBeginItem, string prmEndItem, string prmBeginCat, string prmEndCat, string prmIcode, string prmSort, string prmOut)
    {

        string cError = "";
        dsFgCostSummRepDataSet dsFgCostSummRep = new dsFgCostSummRepDataSet();
        dsFgCostSummRep = null;
        AppServerConnect();
        aoObject.FgCostSumm(prmUser, prmAction, prmAsofdt, prmBeginCust, prmEndCust, prmBeginWhse, prmEndWhse, prmBeginItem, prmEndItem, prmBeginCat, prmEndCat, prmIcode, prmSort, prmOut, out cError, ref dsFgCostSummRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFgCostSummRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReportQtytotDataSet SelectQtytot(string prmUser, string prmAction, string prmBeginCust, string prmEndCust, string prmAddQty, string prmSellprc, string prmSummary, string prmOut)
    {
        string cError = "";
        dsReportQtytotDataSet dsReportQtytot = new dsReportQtytotDataSet();
        dsReportQtytot = null;
        AppServerConnect();
        aoObject.qtytot(prmUser, prmAction, prmBeginCust, prmEndCust, prmAddQty, prmSellprc, prmSummary, prmOut, ref dsReportQtytot, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }

        return dsReportQtytot;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUnfgsmRepDataSet SelectUnFgSummRep(string prmUser, string prmAction, string prmBegOrdDate, string prmEndOrdDate, string prmBeginCust, string prmEndCust, string prmBeginItem, string prmEndItem, string prmBeginWhse, string prmEndWhse, string prmOrdStatus, string prmOut)
    {

        string cError = "";
        dsUnfgsmRepDataSet dsUnfgsmRep = new dsUnfgsmRepDataSet();
        dsUnfgsmRep = null;
        AppServerConnect();
        aoObject.Unfgsm(prmUser, prmAction, prmBegOrdDate, prmEndOrdDate, prmBeginCust, prmEndCust, prmBeginItem, prmEndItem, prmBeginWhse, prmEndWhse, prmOrdStatus, prmOut, out cError, ref dsUnfgsmRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsUnfgsmRep;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRticketRepDataSet ReRticket(string prmUser, string prmAction, string prmbeginJob1, Int32 prmbeginJob2, string prmendJob1, Int32 prmendJob2, string prmtbFold, string prmtbRS, string prmtbCorr, string prmtbPR, string prmtbReprint, string prmtbDC, string prmtbBox, string prmtbGL, string prmtbSW, string prmtbApprove, string prmspecCodes, Int32 prmrevsnNo, string prmtbPrtLabel, string prmtbCommitted, string prmtbPrtSetHeader, string prmtbPromptShip, string prmdeptCodes, string prmtbFreezeNote, string prmtbDeptNote, string prmTBSampleReq, Int32 prmflJobord, string prmrdPrintSpeed, string prmtbFgimage, string prmtbMakeHold, string prmtbPrtMch, string prmtbPrtSellprc, string prmtbTray2, string prmtbAppUunprinted, string prmtbPrtRev, string rmtbPrtShipto)
    {

        string cError = "";
        dsRticketRepDataSet dsRticketRep = new dsRticketRepDataSet();
        dsRticketRep = null;
        AppServerConnect();
        aoObject.Rticket(prmUser, prmAction, prmbeginJob1, prmbeginJob2, prmendJob1, prmendJob2, prmtbFold, prmtbRS, prmtbCorr, prmtbPR, prmtbReprint, prmtbDC, prmtbBox, prmtbGL, prmtbSW, prmtbApprove, prmspecCodes, prmrevsnNo, prmtbPrtLabel, prmtbCommitted, prmtbPrtSetHeader, prmtbPromptShip, prmdeptCodes, prmtbFreezeNote, prmtbDeptNote, prmTBSampleReq, prmflJobord, prmrdPrintSpeed, prmtbFgimage, prmtbMakeHold, prmtbPrtMch, prmtbPrtSellprc, prmtbTray2, prmtbAppUunprinted, prmtbPrtRev, rmtbPrtShipto, out cError, ref dsRticketRep);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsRticketRep;
    }
   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsfgpostDataSet dsdsfgpost(string prmUser, string prmfgpost, Int32 prmBeginSeq, Int32 prmEndSeq, string prmBeginUsrid, string prmEndUsrid, string prmBeDate, string prmEndDate, string prmBeItem, string prmEndItem, string prmBeJob, string prmEndJob, string prmBeWare, string prmEndWare, string prmPstDate, string prmRecept, string prmShipmnt, string prmTrnsfr, string prmAdjstmnt, string prmCrdRtn, string prmItmcod, string prmcostsell, string prmitmcustp, string prmNamPoVn, string prmUomJob, string prmGlActNm, string prmTcost, string prmGrndTotl, string prmtrnstype, string prmOut, string prmsetup)
    {
        string cError = "";
        dsfgpostDataSet dsfgpost = new dsfgpostDataSet();
        dsfgpost = null;
        AppServerConnect();
        aoObject.fgpost(prmUser, prmfgpost, prmBeginSeq, prmEndSeq, prmBeginUsrid, prmEndUsrid, prmBeDate, prmEndDate, prmBeItem, prmEndItem, prmBeJob, prmEndJob, prmBeWare, prmEndWare, prmPstDate, prmRecept, prmShipmnt, prmTrnsfr, prmAdjstmnt, prmCrdRtn, prmItmcod, prmcostsell, prmitmcustp, prmNamPoVn, prmUomJob, prmGlActNm, prmTcost, prmGrndTotl, prmtrnstype, prmOut, prmsetup, out cError, ref dsfgpost);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsfgpost;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTransPostDataSet dsdsfgpost(string prmUser, string prmtrnspost, string prmBeginTag, string prmEndTag, string prmBeginUsrid, string prmEndUsrid, string prmPstDate, string prmGlActNm, string prmshwinv, string prmOut)
    {
        string cError = "";
        dsTransPostDataSet dsTransPost = new dsTransPostDataSet();
        dsTransPost = null;
        AppServerConnect();
        aoObject.trnspost(prmUser, prmtrnspost, prmBeginTag, prmEndTag, prmBeginUsrid, prmEndUsrid, prmPstDate, prmGlActNm, prmshwinv, prmOut, out cError, ref dsTransPost);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTransPost;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSharpShooterLoadTagDataSet SharpShooterLoadTag(string prmUser, string prmAction, string prmOrd_lst, string prmJob_lst, Int32 prmBeg_ord, Int32 prmEnd_ord, string prmBeg_job, Int32 prmBeg_job2, string prmEnd_job, Int32 prmEnd_job2, string prmBeg_itm, string prmEnd_itm, string prmJobPur_rct, string prmReturn, string prmReprnt_tg, string prmScnCs_lbl, string prmOrdJb_stat, string prmPrntPo_frm, string prmFrm_dt, string prmTo_dt, string prmPrntSt_comp, string prmTrnsRls_lot, string prmPrntDpt_not, string prmPrntPst_bol, string prmPrntShp_id, string prmIncl_Over, string prmLwd_16th, string prmAuto_prnt, string prmFrez_Chos, string prmLabl_matrx, Int32 prmLabl_pallt, Int32 prmPrnt_form, string prmText_file, string prmDpt_lst, string prmShp_id, string prmExtra, string ip_rowid, string extra, string prmOut, string prmReckey)
    {
        string cError = "";
        dsSharpShooterLoadTagDataSet dsSharpShooterLoadTag = new dsSharpShooterLoadTagDataSet();
        dsSharpShooterLoadTag = null;
        AppServerConnect();
        aoObject.ss_lodtg(prmUser, prmAction, prmOrd_lst, prmJob_lst, prmBeg_ord, prmEnd_ord, prmBeg_job, prmBeg_job2, prmEnd_job, prmEnd_job2, prmBeg_itm, prmEnd_itm, prmJobPur_rct, prmReturn, prmReprnt_tg, prmScnCs_lbl, prmOrdJb_stat, prmPrntPo_frm, prmFrm_dt, prmTo_dt, prmPrntSt_comp, prmTrnsRls_lot, prmPrntDpt_not, prmPrntPst_bol, prmPrntShp_id, prmIncl_Over, prmLwd_16th, prmAuto_prnt, prmFrez_Chos, prmLabl_matrx, prmLabl_pallt, prmPrnt_form, prmText_file, prmDpt_lst, prmShp_id, prmExtra, ip_rowid, extra, prmOut, prmReckey, out cError, ref dsSharpShooterLoadTag);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsSharpShooterLoadTag;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPoLoadtagLookpoDataSet PoLoadtagLookpo(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        string cError = "";
        dsPoLoadtagLookpoDataSet dsPoLoadtagLookpo = new dsPoLoadtagLookpoDataSet();
        dsPoLoadtagLookpo = null;
        AppServerConnect();
        aoObject.Ld_polook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsPoLoadtagLookpo);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsPoLoadtagLookpo;
    }

}