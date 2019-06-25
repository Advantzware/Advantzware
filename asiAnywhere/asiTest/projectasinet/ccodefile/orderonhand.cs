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
/// Summary description for orderonhand
/// </summary>
[System.ComponentModel.DataObject]
public class orderonhand:AppServerConnect.AppServer
{
	public orderonhand()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderFgDataSet SelectOnHand(string prmUser, string prmAction, string prmOrderNum,string prmItemNum, string prmOrderOnHand)
    {

        dsOrderFgDataSet dsOrderFg = new dsOrderFgDataSet();
        dsOrderFg = null;
        AppServerConnect();
        aoObject.OrderOnHand(prmUser, prmAction, prmOrderNum, prmItemNum ,prmOrderOnHand, ref dsOrderFg);
        AppServerDisconnect();

        return dsOrderFg;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsJobVarianceDataSet OnHandQty(string prmUser, string prmAction, string prmOrderNum, string prmItemNum, string prmJob, string prmJob2, string prmCustomer, string prmEst, string prmOpen, string prmClosed)
    {

        dsJobVarianceDataSet dsJobVariance = new dsJobVarianceDataSet();
        dsJobVariance = null;
        AppServerConnect();
        aoObject.JobVariance(prmUser, prmAction, prmOrderNum, prmItemNum, prmJob, prmJob2, prmCustomer, prmEst, prmOpen, prmClosed, ref dsJobVariance);
        AppServerDisconnect();

        return dsJobVariance;
    }
}
