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

public partial class StyleLookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (!Page.IsPostBack)
        {
            ImageButton1.Visible = false;
        }
        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
        }
       
        //UserClass.CheckLogin(Page);
        //UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //TextBox2.Visible = false;
        Label1.Visible = false;
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
        foreach (GridViewRow gv in GridView1.Rows)
        {
            string selectedimage = ((Label)GridView1.SelectedRow.FindControl("Label11")).Text;
            TextBox2.Text = selectedimage;
        }
        string img = TextBox2.Text.ToString();

        //string final = "";
        
                     
        if (img != "" && img != null)
        {
            //string[] radioval = img.Split(new char[] { ':' });
            //string first = radioval[0];
            //string second = radioval[1];
            //Response.Write(first);
            //Response.Write("\n");
            //Response.Write(second);

            /*if (second != "" && second != null)
            {
                string[] radioval1 = second.Split(new char[] { '\\' });
                string third = radioval1[0];
                string fourth = radioval1[1];
                final = fourth;
                //Response.Write(third);
                //Response.Write("\n");
                //Response.Write(fourth);
                Response.Write(final);
            } */
            
            ImageButton1.Visible = true;            
            //ImageButton1.ImageUrl = img;
                        
            ImageButton1.ImageUrl = @"/3D" + img;         
            
        }


        else
        {
            Label1.Text = "No Image Exists";
            Label1.Visible = true;
            ImageButton1.Visible = false;
        }

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("StyleLookup.aspx");
    }

}
