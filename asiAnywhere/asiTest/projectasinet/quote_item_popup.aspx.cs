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

public partial class quote_item_popup: System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
           
        }
       
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
    //    ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
    //    ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
    //    ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
    //    ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void Select_item(object sender, EventArgs e)
    {
        string valueget = "";
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                valueget = valueget + "," + ((Label)row.FindControl("Label1")).Text ;
            }
        }
       
        Corrugated quote1 = new Corrugated();
        quote1.QuoteItemPupup("AddItem", "Admin", Convert.ToInt32(Session["quote_no"]), "", valueget);
        Response.Write("<script>window.opener.location.href='ViewQuote.aspx'; self.close();</script>");
    }
    protected void grid_unload(object sender, EventArgs e)
    {
        
    }
}
