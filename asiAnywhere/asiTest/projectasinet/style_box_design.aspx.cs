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
using System.IO;

public partial class style_box_design : System.Web.UI.Page
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
        if (Session["style_bro_image_box_name"] != "" && Session["style_bro_image_box_name"] != null)
        {

            ImageButton1.Visible = true;
            //ImageButton1.ImageUrl = img;
            string str1 = Convert.ToString(Session["style_bro_image_box_name"]);

            string firstchar = str1.Substring(0, 1);
            string laststr = str1.Substring(1, str1.Length - 1);

            if (firstchar == "p" || firstchar == "P" || firstchar == "q" || firstchar == "Q")
            {
                str1 = "D" + laststr;
            }
                        
            Response.Write(str1);
            ImageButton1.ImageUrl = str1;            
            string str2 = Path.GetFileName(str1);

            try
            {

                Response.Clear();
                string fileName = str2;
                System.IO.FileStream fs = null;
                fs = System.IO.File.Open(str1, System.IO.FileMode.Open);
                byte[] btFile = new byte[fs.Length];
                fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                             
                Response.OutputStream.Write(btFile, 0, btFile.Length);
                fs.Close();

                //Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                //Response.ContentType = "application/octet-stream";
                //Response.BinaryWrite(btFile);
                Response.End();
                fs = null;
            }
            catch { }
          

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
