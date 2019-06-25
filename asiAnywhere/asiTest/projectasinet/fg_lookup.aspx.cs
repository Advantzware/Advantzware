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

public partial class fg_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        
        
        if (!Page.IsPostBack)
        {
            //ddlSearchOperation.SelectedIndex = 1;

            if (Request.QueryString["fgitm1"] != "")
            {
                txtSearchValue.Text = Convert.ToString(Request.QueryString["fgitm1"]);
                ddlSearchField.SelectedIndex = 0;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
                ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
            }
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "fg";
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        //Session["view_item_fgitem_look"] = GridView1.SelectedRow.Cells[1].Text;
        //Session["view_item_est_look"] = GridView1.SelectedRow.Cells[8].Text;
        ////Response.Write(Session["genestno"]);
        //GridView1.Visible = false;
        //GridView2.Visible = true;
        //tblSearch.Visible = false;

    }
   
}
