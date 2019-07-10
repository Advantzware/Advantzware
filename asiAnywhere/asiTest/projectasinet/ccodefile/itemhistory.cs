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

/// <summary>
/// Summary description for itemhistory
/// </summary>
[System.ComponentModel.DataObject]
public class itemhistory:AppServerConnect.AppServer
{
	public itemhistory()
	{
		//
		// TODO: Add constructor logic here
		//
	}

   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsHistoryDataSet SelectHistory(string prmUser, string prmAction, string prmOrderNum, string prmItemNum, string prmJob, string prmJob2, string prmCode, string prmDate, string prmTag)
    {

        dsHistoryDataSet dsHistory = new dsHistoryDataSet();
        dsHistory = null;
        AppServerConnect();
        aoObject.History(prmUser, prmAction,prmOrderNum, prmItemNum, prmJob, prmJob2, prmCode, prmDate, prmTag, ref dsHistory);
        AppServerDisconnect();

        return dsHistory;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBroFGrcptDataSet SelectRcpt(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmRecKey, string prmllSetParts)
    {

        dsBroFGrcptDataSet dsBroFGrcpt = new dsBroFGrcptDataSet();
        dsBroFGrcpt = null;
        AppServerConnect();
        aoObject.fgrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmRecKey, prmllSetParts, ref dsBroFGrcpt);
        AppServerDisconnect();

        return dsBroFGrcpt;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewFGrcptDataSet ViewRcpt(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmStdCost, string prmCost_Uom, string prmTQty, string prmFrtCost, string prmExtCost, string prmStackCode, string prmCreatedBy, string prmCreate2, string prmTotWt, string prmRecKey)
    {
        string cError = "";
        dsViewFGrcptDataSet dsViewFGrcpt = new dsViewFGrcptDataSet();
        dsViewFGrcpt = null;
        AppServerConnect();
        aoObject.fgviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmStdCost, prmCost_Uom, prmTQty, prmFrtCost, prmExtCost, prmStackCode, prmCreatedBy, prmCreate2, prmTotWt, prmRecKey, out cError, ref dsViewFGrcpt);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsViewFGrcpt;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateViewRece(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmStdCost, string prmCost_Uom, string prmTQty, string prmFrtCost, string prmExtCost, string prmStackCode, string prmCreatedBy, string prmCreate2, string prmTotWt, string prmRecKey)
    {
        string cError = "";
        dsViewFGreceDataSet dsViewFGrece = new dsViewFGreceDataSet();
        dsViewFGrece = null;
        AppServerConnect();
        aoObject.receviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmStdCost, prmCost_Uom, prmTQty, prmFrtCost, prmExtCost, prmStackCode, prmCreatedBy, prmCreate2, prmTotWt, prmRecKey, out cError, ref dsViewFGrece);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsLpofgLookupDataSet PoSelLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsLpofgLookupDataSet dsLpofgLookup = new dsLpofgLookupDataSet();
        dsLpofgLookup = null;
        AppServerConnect();
        aoObject.lpofglook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsLpofgLookup);
        AppServerDisconnect();
        return dsLpofgLookup;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewFGreceDataSet ViewRece(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmStdCost, string prmCost_Uom, string prmTQty, string prmFrtCost, string prmExtCost, string prmStackCode, string prmCreatedBy, string prmCreate2, string prmTotWt, string prmRecKey)
    {
        string cError = "";
        dsViewFGreceDataSet dsViewFGrece = new dsViewFGreceDataSet();
        dsViewFGrece = null;
        AppServerConnect();
        aoObject.receviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmStdCost, prmCost_Uom, prmTQty, prmFrtCost, prmExtCost, prmStackCode, prmCreatedBy, prmCreate2, prmTotWt, prmRecKey, out cError, ref dsViewFGrece);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsViewFGrece;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsmoveViewFGreceDataSet MoveRece(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmStdCost, string prmCost_Uom, string prmTQty, string prmFrtCost, string prmExtCost, string prmStackCode, string prmCreatedBy, string prmCreate2, string prmTotWt, string prmRecKey)
    {
        string cError = "";
        dsmoveViewFGreceDataSet dsmoveViewFGrece = new dsmoveViewFGreceDataSet();
        dsmoveViewFGrece = null;
        AppServerConnect();
        aoObject.moveviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmStdCost, prmCost_Uom, prmTQty, prmFrtCost, prmExtCost, prmStackCode, prmCreatedBy, prmCreate2, prmTotWt, prmRecKey, out cError, ref dsmoveViewFGrece);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsmoveViewFGrece;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsmoveViewFGreceDataSet MoveRecepost(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmStdCost, string prmCost_Uom, string prmTQty, string prmFrtCost, string prmExtCost, string prmStackCode, string prmCreatedBy, string prmCreate2, string prmTotWt, string prmRecKey, ref string verror)
    {
        string cError = "";
        dsmoveViewFGreceDataSet dsmoveViewFGrece = new dsmoveViewFGreceDataSet();
        dsmoveViewFGrece = null;
        AppServerConnect();
        aoObject.moveviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmStdCost, prmCost_Uom, prmTQty, prmFrtCost, prmExtCost, prmStackCode, prmCreatedBy, prmCreate2, prmTotWt, prmRecKey, out cError, ref dsmoveViewFGrece);
        AppServerDisconnect();
        if (cError != "")
        {
            verror = cError;
        }

        return dsmoveViewFGrece;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dstrnsFGrcptDataSet SelecttrnsRcpt(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmRecKey, string prmOut, string prmllSetParts)
    {

        dstrnsFGrcptDataSet dstrnsFGrcpt = new dstrnsFGrcptDataSet();
        dstrnsFGrcpt = null;
        AppServerConnect();
        aoObject.trnsfgrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmRecKey, prmOut, prmllSetParts, ref dstrnsFGrcpt);
        AppServerDisconnect();

        return dstrnsFGrcpt;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTrnsViewFGreceDataSet trnsViewRece(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmLoc2, string prmLocBin2, string prmTagno2, string prmRecKey, string prmcontrans, Int32 prmSeq, string prmOut)
    {
        string cError = "";
        dsTrnsViewFGreceDataSet dsTrnsViewFGrece = new dsTrnsViewFGreceDataSet();
        dsTrnsViewFGrece = null;
        AppServerConnect();
        aoObject.trnsviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmLoc2, prmLocBin2, prmTagno2, prmRecKey, prmcontrans, prmSeq, prmOut, out cError, ref dsTrnsViewFGrece);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsTrnsViewFGrece;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validtrnsViewRece(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmRcptDate, string prmTagno, string prmTransTime, string prmJob_no2, string prmName, string prmLoc, string prmLocBin, string prmCases, string prmQty_Cas, string prmCasUnit, string prmPartial, string prmLoc2, string prmLocBin2, string prmTagno2, string prmRecKey, string prmcontrans, Int32 prmSeq, string prmOut)
    {
        string cError = "";
        dsTrnsViewFGreceDataSet dsTrnsViewFGrece = new dsTrnsViewFGreceDataSet();
        dsTrnsViewFGrece = null;
        AppServerConnect();
        aoObject.trnsviewrcpt(prmUser, prmAction, prmFgItem, prmJobno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmLoc2, prmLocBin2, prmTagno2, prmRecKey, prmcontrans, prmSeq, prmOut, out cError, ref dsTrnsViewFGrece);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCountsRcptDataSet selectcountrcpt(string prmUser, string prmAction, string prmFgItem, string prmName, string prmJobno, Int32 prmJobno2, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmloc, string prmlocbin, Decimal prmTqty, Int32 prmCases, Int32 prmQty_Cas, Int32 prmCasUnit, Int32 prmPartial, string prmRecKey, string prmllSetParts, string prmTransTime)
    {
        string cError = "";
        dsCountsRcptDataSet dsCountsRcpt = new dsCountsRcptDataSet();
        dsCountsRcpt = null;
        AppServerConnect();
        aoObject.cuntsrcpt(prmUser, prmAction, prmFgItem, prmName, prmJobno, prmJobno2, prmPono, prmSeqno, prmRcptDate, prmTagno, prmloc, prmlocbin, prmTqty, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmRecKey, prmllSetParts, prmTransTime, out cError, ref dsCountsRcpt);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsCountsRcpt;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReturnsFinishGdsDataSet SelectReturnsGoods(string prmUser, string prmAction, string prmFgItem, string prmJobno, string prmPono, string prmSeqno, string prmRcptDate, string prmTagno, string prmTransTime, Int32 prmJob_no2, string prmName, string prmLoc, string prmLocBin, Int32 prmCases, Int32 prmQty_Cas, Int32 prmCasUnit, Int32 prmPartial, decimal prmStdCost, string prmCost_Uom, decimal prmTQty, decimal prmExtCost, Int32 prmInvNo, string prmRecKey)
    {
        string cError = "";
        dsReturnsFinishGdsDataSet dsReturnsFinishGds = new dsReturnsFinishGdsDataSet();
        dsReturnsFinishGds = null;
        AppServerConnect();
        aoObject.retrnsgds(prmUser, prmAction, prmFgItem, prmJobno, prmPono, prmSeqno, prmRcptDate, prmTagno, prmTransTime, prmJob_no2, prmName, prmLoc, prmLocBin, prmCases, prmQty_Cas, prmCasUnit, prmPartial, prmStdCost, prmCost_Uom, prmTQty, prmExtCost, prmInvNo, prmRecKey, out cError, ref dsReturnsFinishGds);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsReturnsFinishGds;
    }
}
