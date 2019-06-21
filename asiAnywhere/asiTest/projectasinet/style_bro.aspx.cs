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

public partial class style_bro : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (!Page.IsPostBack)
        {
            
        }
        if (Page.IsPostBack)
        {
            
        }
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        
        GridView1.SelectedIndex = Convert.ToInt32(Session["style_bro_image_index"]);
        if (Session["style_bro_image_index"] == null)
        {
            try
            {
                
                GridView1.SelectedIndex = 0;
                string selectedimage = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
                Session["style_bro_image_name"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
                Session["style_bro_image_box_name"] = ((Label)GridView1.SelectedRow.FindControl("Labelboxdesign")).Text;
                Session["style_bro_image_design"] = GridView1.SelectedRow.Cells[4].Text;
                Session["style_bro_image_desc"] = GridView1.SelectedRow.Cells[5].Text;

            }
            catch { }
        }
        //if (Session["style_bro_image_index"] != null)
        //{
        //    try
        //    {
            
        //    string selectedimage = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
        //    Session["style_bro_image_name"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
        //    Session["style_bro_image_box_name"] = ((Label)GridView1.SelectedRow.FindControl("Labelboxdesign")).Text;
        //    Session["style_bro_image_design"] = GridView1.SelectedRow.Cells[4].Text;
        //    Session["style_bro_image_desc"] = GridView1.SelectedRow.Cells[5].Text;
            
        //    }
        //    catch { }
        //}
       
             
     }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        Session["style_bro_image_index"] = null;
        Session["style_bro_prmaction"] = "search";
        Session["style_bro_prmtext"] = txtSearchValue.Text.Trim();
        Response.Write("<script>location.href='style_bro.aspx'</script>");
        
        
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
            Session["style_bro_image_index"] = GridView1.SelectedIndex;
            string selectedimage = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
            Session["style_bro_image_name"] = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
            Session["style_bro_image_box_name"] = ((Label)GridView1.SelectedRow.FindControl("Labelboxdesign")).Text;
            Session["style_bro_image_design"] = GridView1.SelectedRow.Cells[4].Text;
            Session["style_bro_image_desc"] = GridView1.SelectedRow.Cells[5].Text;
           
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        
        Session["style_bro_image_index"] = null;
        Session["style_bro_prmaction"] = "sea";
        txtSearchValue.Text = "";
        Response.Write("<script>location.href='style_bro.aspx'</script>");
    }
    protected void lnk_boxdesign_Click(object sender, EventArgs e)
    {
        Response.Redirect("style_box_design.aspx");
    }
    protected void lnk_3boxdesign_Click(object sender, EventArgs e)
    {
        Response.Redirect("style_3d_image.aspx");
    }
}
