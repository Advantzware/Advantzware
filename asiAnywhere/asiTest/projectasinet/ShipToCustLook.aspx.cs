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

public partial class shiptoLookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
       
        //ObjectDataSource1.SelectParameters["prmShip"].DefaultValue = Convert.ToString(Session["cust"]);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            //ddlSearchOperation.SelectedIndex = 1;
            //ObjectDataSource1.SelectParameters["prmShip"].DefaultValue = Convert.ToString(Session["cust_shipid_look"]);
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        string lookval = Request.QueryString["look"];
        Response.Redirect("ShipToCustLook.aspx?look=" + lookval);
    }
    protected void Page_UnLoad(object sender, EventArgs e)
    {
        //Session["cust_shipid_look"] = null;
    }

    protected void GridView1_Unload(object sender, EventArgs e)
    {

    }
}
