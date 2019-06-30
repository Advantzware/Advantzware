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

public partial class fold_box_design : System.Web.UI.Page
{    
    string COMPANY;
    protected void Page_Load(object sender, EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        FormView1.ChangeMode(FormViewMode.ReadOnly);

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
           
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_box_design.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;


            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label compname = (Label)Master.FindControl("lblComp");
            Label username = (Label)Master.FindControl("lblUser");
            Label labelname = (Label)Master.FindControl("lbl_page");
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            labelname.Text = "Folding Box";
            COMPANY = compname.Text;
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
       
        
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                /*ImageButton img_box = (ImageButton)Master.FindControl("img_BoxDesign");
                img_box.ImageUrl = "~/Images/box_design_1.jpg";*/
                Image img_mov_col = (Image)Master.FindControl("Image5");
                img_mov_col.Visible = false;

                if (Session["view_3d"] == null)
                {
                    Label img_3d_label = (Label)FormView1.FindControl("vbox_3d_imageLabel");
                    Label box_img_label = (Label)FormView1.FindControl("vbox_imageLabel");
                    img_3d_label.Visible = false;
                    box_img_label.Visible = true;
                    Image view_img = (Image)FormView1.FindControl("Image1");

                    string imgpath = box_img_label.Text;
                    string firstchar = imgpath.Substring(0, 1);
                    string laststr = imgpath.Substring(1, imgpath.Length - 1);
                                        
                    if (firstchar != "")
                    {
                        imgpath = "D" + laststr;
                    }

                    if (File.Exists(imgpath))
                    {
                        string imgfilename = Path.GetFileName(imgpath);
                        string dirPath = Server.MapPath(@"Images\Rcode\2D");
                        string destimgpath = Server.MapPath(@"Images\Rcode\2D" + "\\" + imgfilename);

                        string[] files = Directory.GetFiles(dirPath);
                        foreach (string file in files)
                            File.Delete(file);

                        File.Copy(imgpath, destimgpath);
                        view_img.ImageUrl = "~/Images/Rcode/2D/" + imgfilename;
                    }

                }

                if (Session["view_3d"] != null)
                {

                    Label img_3d = (Label)FormView1.FindControl("vbox_3d_imageLabel");
                    Label box_img = (Label)FormView1.FindControl("vbox_imageLabel");
                    box_img.Visible = false;
                    img_3d.Visible = true;
                    Image view_img = (Image)FormView1.FindControl("Image1");

                    string imgpath = img_3d.Text;
                    string firstchar = imgpath.Substring(0, 1);
                    string laststr = imgpath.Substring(1, imgpath.Length - 1);
                                        
                    if (firstchar != "")
                    {
                        imgpath = "D" + laststr;
                    }

                    if (File.Exists(imgpath))
                    {
                        string imgfilename = Path.GetFileName(imgpath);
                        string dirPath = Server.MapPath(@"Images\Rcode\3D");
                        string destimgpath = Server.MapPath(@"Images\Rcode\3D" + "\\" + imgfilename);

                        string[] files = Directory.GetFiles(dirPath);
                        foreach (string file in files)
                            File.Delete(file);

                        File.Copy(imgpath, destimgpath);
                        view_img.ImageUrl = "~/Images/Rcode/3D/" + imgfilename;
                    }

                   
                }
            }
            catch { }

        }

        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            if (Session["view_3d"] == null)
            {
                TextBox img_box_text = (TextBox)FormView1.FindControl("vbox_imageTextBox");
                TextBox img_3d_text = (TextBox)FormView1.FindControl("vbox_3d_imageTextBox");
                Image img_box = (Image)FormView1.FindControl("Image1");
                
                img_3d_text.Visible = false;
                img_box_text.Visible = true;

                string imgpath = img_box_text.Text;
                string firstchar = imgpath.Substring(0, 1);
                string laststr = imgpath.Substring(1, imgpath.Length - 1);
                
                if (firstchar != "")
                {
                    imgpath = "D" + laststr;
                }

                if (File.Exists(imgpath))
                {
                    string imgfilename = Path.GetFileName(imgpath);
                    string dirPath = Server.MapPath(@"Images\Rcode\2D");
                    string destimgpath = Server.MapPath(@"Images\Rcode\2D" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    img_box.ImageUrl = "~/Images/Rcode/2D/" + imgfilename;
                }

                //img_box.ImageUrl = @"/images/RSCGL.jpg";
            }
            else
            {
                TextBox img_box_text = (TextBox)FormView1.FindControl("vbox_imageTextBox");
                TextBox img_3d_text = (TextBox)FormView1.FindControl("vbox_3d_imageTextBox");
                Image img_3d = (Image)FormView1.FindControl("Image1");
                img_3d_text.Visible = true;
                img_box_text.Visible = false;


                string imgpath = img_3d_text.Text;
                string firstchar = imgpath.Substring(0, 1);
                string laststr = imgpath.Substring(1, imgpath.Length - 1);
                
                if (firstchar != "")
                {
                    imgpath = "D" + laststr;
                }

                if (File.Exists(imgpath))
                {
                    string imgfilename = Path.GetFileName(imgpath);
                    string dirPath = Server.MapPath(@"Images\Rcode\3D");
                    string destimgpath = Server.MapPath(@"Images\Rcode\3D" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    img_3d.ImageUrl = "~/Images/Rcode/3D/" + imgfilename;
                }

                //img_3d.ImageUrl = @"/images/Rsc3d.jpg";

                TextBox design_dscr = (TextBox)FormView1.FindControl("vdesign_dscrTextBox");                
                design_dscr.Enabled = false;                
                design_dscr.BackColor = System.Drawing.Color.Turquoise;                
            }
        }
    }
    protected void btn_3dimage_click(object sender, EventArgs e)
    {
        string dieimage = "";
        try
        {

            Label img_3d = (Label)FormView1.FindControl("vbox_3d_imageLabel");
            Label box_img = (Label)FormView1.FindControl("vbox_imageLabel");
            box_img.Visible = false;
            img_3d.Visible = true;
            Image view_img = (Image)FormView1.FindControl("Image1");

            string imgpath = img_3d.Text;
            string firstchar = imgpath.Substring(0, 1);
            string laststr = imgpath.Substring(1, imgpath.Length - 1);
           
            if (firstchar != "")
            {
                imgpath = "D" + laststr;
            }
            dieimage = imgpath;

            if (File.Exists(imgpath))
            {
                string imgfilename = Path.GetFileName(imgpath);
                string dirPath = Server.MapPath(@"Images\Rcode\3D");
                string destimgpath = Server.MapPath(@"Images\Rcode\3D" + "\\" + imgfilename);

                string[] files = Directory.GetFiles(dirPath);
                foreach (string file in files)
                    File.Delete(file);

                File.Copy(imgpath, destimgpath);
                view_img.ImageUrl = "~/Images/Rcode/3D/" + imgfilename;
            }

            //view_img.ImageUrl = @"/images/Rsc3d.jpg";

        }
        catch { }
        Session["view_3d"] = "1";
        
        if (Convert.ToString(Session["fold_box_design_down_cad"]) == "3D")
        {
            string str1 = dieimage;
            string str2 = Path.GetFileName(str1);
            string path = str2;

            if (path != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                {
                    if (File.Exists(str1))
                    {
                        string fileName = str2;
                        System.IO.FileStream fs = null;
                        fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
                        byte[] btFile = new byte[fs.Length];
                        fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                        fs.Close();
                        Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                        Response.ContentType = "application/octet-stream";
                        Response.BinaryWrite(btFile);
                        Response.End();
                        fs = null;
                    }
                }
            }
        }
       
        Session["fold_box_design_down_cad"] = "3D";
    }
    protected void btn_Update_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox dscr = (TextBox)FormView1.FindControl("vdesign_dscrTextBox");
        TextBox boxpath = (TextBox)FormView1.FindControl("vbox_imageTextBox");
        TextBox d3path = (TextBox)FormView1.FindControl("vbox_3d_imageTextBox");
        FileUpload FileUpload1 = (FileUpload)FormView1.FindControl("FileUpload1");

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = COMPANY;
        ObjectDataSource1.SelectParameters["prmDesignDscr"].DefaultValue = dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFormNo"].DefaultValue = Convert.ToString(Session["order_folding_formno"]);
        ObjectDataSource1.SelectParameters["prmEstNo"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
        ObjectDataSource1.SelectParameters["prmBoxImage"].DefaultValue = boxpath.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBox3DImage"].DefaultValue = d3path.Text.Trim();

        if (FileUpload1.FileName != "")
        {
            string str1 = "D:/Rcode/ASIIMAGE/";
            string str11 = FileUpload1.PostedFile.FileName;
            string str12 = Path.GetFileName(str11);
            string str3 = str1;  //give the path where you want to upload the file.
            FileUpload1.PostedFile.SaveAs(Path.Combine(str3, str12));
            string str2 = Path.GetFileNameWithoutExtension(str11);
            if (Session["view_3d"] == null)
            {               
                ObjectDataSource1.SelectParameters["prmBoxImage"].DefaultValue = str1 + str12;
            }
            else
            {               
                ObjectDataSource1.SelectParameters["prmBox3DImage"].DefaultValue = str1 + str12;
            }
        }
                
        Response.Write("<script>window.location.href='fold_box_design.aspx'</script>");
    }
    protected void btn_UpdateCancel_Click(object sender, EventArgs e)
    {
       
        Response.Write("<script>window.location.href='fold_box_design.aspx'</script>");
    }

    protected void die_button_click(object sender, EventArgs e)
    {
        Label dieimage = (Label)FormView1.FindControl("die_label");
        string str1 = dieimage.Text.Trim();
        if (File.Exists(str1))
        {
            string str2 = Path.GetFileName(str1);
            string path = str2;

            string firstchar = str1.Substring(0, 1);
            string laststr = str1.Substring(1, str1.Length - 1);

            if (firstchar == "z" || firstchar == "Z")
            {
                str1 = "D" + laststr;
            }

            if (path != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                {

                    string fileName = str2;
                    System.IO.FileStream fs = null;
                    fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
                    byte[] btFile = new byte[fs.Length];
                    fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                    fs.Close();
                    Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                    Response.ContentType = "application/octet-stream";
                    Response.BinaryWrite(btFile);
                    Response.End();
                    fs = null;
                }
            }


        }
        else
        {
            HttpContext.Current.Response.Write("<script>alert('No Die Image available. Check Die Image!')</script>");
        }

    }

    protected void btn_cad2d_click(object sender, EventArgs e)
    {
        
        Session["view_3d"] = null;
        string dieimage = "";
        try
        {            
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;

            if (Session["view_3d"] == null)
            {
                Label img_3d_label = (Label)FormView1.FindControl("vbox_3d_imageLabel");
                Label box_img_label = (Label)FormView1.FindControl("vbox_imageLabel");
                img_3d_label.Visible = false;
                box_img_label.Visible = true;
                Image view_img = (Image)FormView1.FindControl("Image1");
                
                string imgpath = box_img_label.Text;
                string firstchar = imgpath.Substring(0, 1);
                string laststr = imgpath.Substring(1, imgpath.Length - 1);

                //if (firstchar == "z" || firstchar == "Z")
                if (firstchar != "")
                {
                    imgpath = "D" + laststr;
                }
                dieimage = imgpath ;
                if (File.Exists(imgpath))
                {
                    string imgfilename = Path.GetFileName(imgpath);
                    string dirPath = Server.MapPath(@"Images\Rcode\2D");
                    string destimgpath = Server.MapPath(@"Images\Rcode\2D" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    view_img.ImageUrl = "~/Images/Rcode/2D/" + imgfilename;
                    
                }
               
                if(Convert.ToString(Session["fold_box_design_down_cad"]) == "cad")
                {
                    string str1 = dieimage;
                    string str2 = Path.GetFileName(str1);
                    string path = str2;

                    if (path != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                        {
                            if (File.Exists(str1))
                            {
                                string fileName = str2;
                                System.IO.FileStream fs = null;
                                fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
                                byte[] btFile = new byte[fs.Length];
                                fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                                fs.Close();
                                Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                                Response.ContentType = "application/octet-stream";
                                Response.BinaryWrite(btFile);
                                Response.End();
                                fs = null;
                            }
                        }
                    }
                } 
               
            }
            
            Session["fold_box_design_down_cad"] = "cad";
        }
        catch { }

    }
    
}
