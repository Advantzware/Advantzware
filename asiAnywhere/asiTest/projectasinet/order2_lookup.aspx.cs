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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;



public partial class Order2 : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource7.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
        }


    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        //ddl_order.Text = this.GridView1.SelectedValue.ToString();        
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource7.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource7.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource7.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource7.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        ObjectDataSource7.SelectParameters["prmCust"].DefaultValue = Convert.ToString(Session["customer_fglookup_val"]);


    }

    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("order2_lookup.aspx");
    }
}
