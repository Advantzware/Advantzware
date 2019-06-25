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

public partial class fgitemall_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sea";
        if (!Page.IsPostBack)
        {
            gridviewdiv.Visible = false;
            searchdiv.Visible = false;
            selectitem.Visible = true;
            Session["fgitemall_lookup_type"] = null;
            ddlSearchOperation.SelectedIndex = 0;

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
        txtSearchValue.Text = "";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void Button_rm_Click(object sender, EventArgs e)
    {
        gridviewdiv.Visible = true;
        searchdiv.Visible = true;
        selectitem.Visible = false;
        
        Session["fgitemall_lookup_type"] = "RMItem";
    }
    protected void Button_fg_Click(object sender, EventArgs e)
    {
        gridviewdiv.Visible = true;
        searchdiv.Visible = true;
        selectitem.Visible = false;
       
        Session["fgitemall_lookup_type"] = "ItemFG";
    }


    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            if (Convert.ToString(Session["fgitemall_lookup_type"]) == "RMItem")
            {
                e.Row.Cells[3].Visible = false;
                e.Row.Cells[4].Visible = false;
                e.Row.Cells[5].Visible = false;
            }
        }
        catch { }
    }
}
