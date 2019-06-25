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

public partial class style_3d_image : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (!Page.IsPostBack)
        {
            Label1.Visible = false;
            ImageButton1.Visible = false;
        }
             //UserClass.CheckLogin(Page);
        //UserClass UserLogin = (UserClass)Session["User"];
        Label3.Text = Convert.ToString(Session["style_bro_image_design"]);
        Label2.Text = Convert.ToString(Session["style_bro_image_desc"]);
        if (Session["style_bro_image_name"] != "" && Session["style_bro_image_name"] != null)
        {

            ImageButton1.Visible = true;
            //ImageButton1.ImageUrl = img;

            ImageButton1.ImageUrl = @"/3D" + Convert.ToString(Session["style_bro_image_name"]);

        }


        else
        {
            Label1.Text = "No Image Exists";
            Label1.Visible = true;
            ImageButton1.Visible = false;
        }
        
    }


    protected void style_bro_click(object sender, EventArgs e)
    {
        Response.Redirect("style_bro.aspx");
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
