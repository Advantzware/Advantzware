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
public class release:AppServerConnect.AppServer
{
    public release()
	{
		//
		// TODO: Add constructor logic here
		//
	}

   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReleaseOrderlistDataSet ReleaseOrderlist(string prmAction, string prmComp, string prmUser, Int32 prmrellno, Int32 prmordno, string prmpono, string prmcustno, string prmpartno, string prmshipid, string prmino, string prmreldate, string prmjobno, Int32 prmjobno2, string prmcarrier, string prmtrailer, string prmposted, string prmship1, string prmship2, string prmship3, string prmship4, string prmReckey, string prmextra)
    {
        string cError = "";
        dsReleaseOrderlistDataSet dsReleaseOrderlist = new dsReleaseOrderlistDataSet();
        dsReleaseOrderlist = null;
        AppServerConnect();
        aoObject.Rellord_list(prmAction, prmComp, prmUser, prmrellno, prmordno, prmpono, prmcustno, prmpartno, prmshipid, prmino, prmreldate, prmjobno, prmjobno2, prmcarrier, prmtrailer, prmposted, prmship1, prmship2, prmship3, prmship4, prmReckey, prmextra, ref dsReleaseOrderlist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsReleaseOrderlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateRelOrdlist(string prmAction, string prmComp, string prmUser, Int32 prmrellno, Int32 prmordno, string prmpono, string prmcustno, string prmpartno, string prmshipid, string prmino, string prmreldate, string prmjobno, Int32 prmjobno2, string prmcarrier, string prmtrailer, string prmposted, string prmship1, string prmship2, string prmship3, string prmship4, string prmReckey, string prmextra)
    {
        string cError = "";
        dsReleaseOrderlistDataSet dsReleaseOrderlist = new dsReleaseOrderlistDataSet();
        dsReleaseOrderlist = null;
        AppServerConnect();
        aoObject.Rellord_list(prmAction, prmComp, prmUser, prmrellno, prmordno, prmpono, prmcustno, prmpartno, prmshipid, prmino, prmreldate, prmjobno, prmjobno2, prmcarrier, prmtrailer, prmposted, prmship1, prmship2, prmship3, prmship4, prmReckey, prmextra, ref dsReleaseOrderlist, out cError);
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
    public dsViewReleaseOrderDataSet ViewReleaseOrder(string prmAction, string prmComp, string prmUser, Int32 prmordno2, string prmino2, string prmpono2, Int32 prmqty, string prmtag, string prmloc, string prmlocbin, string prmjobno, Int32 prmjobno2, string prmcustno, Int32 prmcases, Int32 prmqtycas, Int32 prmpartial, Int32 prmrelno2, Int32 prmbordno, string prmscod, string prmpartno, Int32 prmlinkno, string prmReckey, string prmextra2)
    {
        string cError = "";
        dsViewReleaseOrderDataSet dsViewReleaseOrder = new dsViewReleaseOrderDataSet();
        dsViewReleaseOrder = null;
        AppServerConnect();
        aoObject.Rellord_view(prmAction, prmComp, prmUser, prmordno2, prmino2, prmpono2, prmqty, prmtag, prmloc, prmlocbin, prmjobno, prmjobno2, prmcustno, prmcases, prmqtycas, prmpartial, prmrelno2, prmbordno, prmscod, prmpartno, prmlinkno, prmReckey, prmextra2, ref dsViewReleaseOrder, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsViewReleaseOrder;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateViewRelOrd(string prmAction, string prmComp, string prmUser, Int32 prmordno2, string prmino2, string prmpono2, Int32 prmqty, string prmtag, string prmloc, string prmlocbin, string prmjobno, Int32 prmjobno2, string prmcustno, Int32 prmcases, Int32 prmqtycas, Int32 prmpartial, Int32 prmrelno2, Int32 prmbordno, string prmscod, string prmpartno, Int32 prmlinkno, string prmReckey, string prmextra2)
    {
        string cError = "";
        dsViewReleaseOrderDataSet dsViewReleaseOrder = new dsViewReleaseOrderDataSet();
        dsViewReleaseOrder = null;
        AppServerConnect();
        aoObject.Rellord_view(prmAction, prmComp, prmUser, prmordno2, prmino2, prmpono2, prmqty, prmtag, prmloc, prmlocbin, prmjobno, prmjobno2, prmcustno, prmcases, prmqtycas, prmpartial, prmrelno2, prmbordno, prmscod, prmpartno, prmlinkno, prmReckey, prmextra2, ref dsViewReleaseOrder, out cError);
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
    public dsPrintReleaseOrdDataSet PrintReleaseOrd(string prmUser, string prmrelprt, string prmbegcust, Int32 prmbegrel, Int32 prmbegord, string prmbegdate, string prmbegdelz, string prmbegloc, string prmbeglocbin, string prmendcust, Int32 prmendrel, Int32 prmendord, string prmenddate, string prmenddelz, string prmendloc, string prmendlocbin, string prmpsted, string prmprinted, string prmrel_prfm, string prmbinloc, string prmsrtdelz, string prmprtdelz, string prmasscomp, string prmcustprt, string prmprtpric, string prmsrtbinloc, string prmprtwht, string prmpstrel, string prmmulrel, string prmextagbin, string prmextra)
    {
        string cError = "";
        dsPrintReleaseOrdDataSet dsPrintReleaseOrd = new dsPrintReleaseOrdDataSet();
        dsPrintReleaseOrd = null;
        AppServerConnect();
        aoObject.relprt(prmUser, prmrelprt, prmbegcust, prmbegrel, prmbegord, prmbegdate, prmbegdelz, prmbegloc, prmbeglocbin, prmendcust, prmendrel,prmendord,prmenddate,prmenddelz,prmendloc,prmendlocbin,prmpsted,prmprinted,prmrel_prfm,prmbinloc,prmsrtdelz,prmprtdelz,prmasscomp,prmcustprt,prmprtpric,prmsrtbinloc,prmprtwht,prmpstrel,prmmulrel,prmextagbin,prmextra, out cError, ref dsPrintReleaseOrd);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsPrintReleaseOrd;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderCustLookupDataSet OrderCustLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmcust)
    {
        string cError = "";
        dsOrderCustLookupDataSet dsOrderCustLookup = new dsOrderCustLookupDataSet();
        dsOrderCustLookup = null;
        AppServerConnect();
        aoObject.ordcust_look(prmAction, prmUser, prmField, prmCondition, prmText, prmcust, ref dsOrderCustLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsOrderCustLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustomerPOLookupDataSet CustomerPOLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, Int32 prmord)
    {
        string cError = "";
        dsCustomerPOLookupDataSet dsCustomerPOLookup = new dsCustomerPOLookupDataSet();
        dsCustomerPOLookup = null;
        AppServerConnect();
        aoObject.custpo_look(prmAction, prmUser, prmField, prmCondition, prmText, prmord, ref dsCustomerPOLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCustomerPOLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustomerItemLookupDataSet CustomerItemLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, Int32 prmord)
    {
        string cError = "";
        dsCustomerItemLookupDataSet dsCustomerItemLookup = new dsCustomerItemLookupDataSet();
        dsCustomerItemLookup = null;
        AppServerConnect();
        aoObject.custitm_look(prmAction, prmUser, prmField, prmCondition, prmText, prmord, ref dsCustomerItemLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCustomerItemLookup;
    }
   
    
}