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

public partial class quantity_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        //Response.Write(Session["genestno"]);
        if (!Page.IsPostBack)
        {
            //ddlSearchOperation.SelectedIndex = 1;
        }

    }
    
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
}
