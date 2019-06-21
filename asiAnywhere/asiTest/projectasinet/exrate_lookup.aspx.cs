using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class exrate_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
       
        CurrLabel.Text = Request.QueryString["currc"].ToString();
        RateLabel.Text = Request.QueryString["ratec"].ToString();
        if (RateLabel.Text == "")
            RateLabel.Text = "1";
        if (!Page.IsPostBack)
        {
            
        }

    }
   
}
