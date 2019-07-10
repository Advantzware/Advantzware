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
using System.Diagnostics;
using System.IO;
using System.Text;

public partial class corr_box_design : System.Web.UI.Page
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
            string vPage = "corr_box_design.aspx";
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
            COMPANY = compname.Text;
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            labelname.Text = "Corrugated Box";
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
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {

            if (Session["view_3d"] == null)
            {                
                TextBox img_box_text = (TextBox)FormView1.FindControl("vbox_imageTextBox");
                TextBox img_3d_text = (TextBox)FormView1.FindControl("vbox_3d_imageTextBox");
                Image img_box = (Image)FormView1.FindControl("Image1");
                
                img_3d_text.Visible = false;
                img_box_text.Visible = true;

                try
                {
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
                }
                catch { }
               
                //img_box.ImageUrl = @"/images/RSCGL.jpg";
            }
            else
            {                
                TextBox img_box_text = (TextBox)FormView1.FindControl("vbox_imageTextBox");
                TextBox img_3d_text = (TextBox)FormView1.FindControl("vbox_3d_imageTextBox");
                Image img_3d = (Image)FormView1.FindControl("Image1");
                img_3d_text.Visible = true;
                img_box_text.Visible = false;
                try
                {
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
                    TextBox lscore = (TextBox)FormView1.FindControl("vlscoreTextBox");
                    design_dscr.Enabled = false;
                    lscore.Enabled = false;
                    design_dscr.BackColor = System.Drawing.Color.Turquoise;
                    lscore.BackColor = System.Drawing.Color.Turquoise;
                }
                catch { }
            }
            TextBox total = (TextBox)FormView1.FindControl("vlcum_scoreLabel");
            Label tot1 = (Label)FormView1.FindControl("Label1");
            Label tot2 = (Label)FormView1.FindControl("Label2");
            Label tot3 = (Label)FormView1.FindControl("Label3");
            Label tot4 = (Label)FormView1.FindControl("Label4");
            Label tot5 = (Label)FormView1.FindControl("Label5");

            try
            {
                string strData2 = total.Text.Trim();
                string[] separator2 = new string[] { " " };
                string[] strSplitArr2 = strData2.Split(separator2, StringSplitOptions.RemoveEmptyEntries);
                tot1.Text = strSplitArr2[0];
                tot2.Text = strSplitArr2[1];
                tot3.Text = strSplitArr2[2];
                tot4.Text = strSplitArr2[3];
                tot5.Text = strSplitArr2[4];
            }
            catch { }

            try
            {
                if (tot1.Text == "")
                    tot1.Visible = false;
                if (tot2.Text == "")
                    tot2.Visible = false;
                if (tot3.Text == "")
                    tot3.Visible = false;
                if (tot4.Text == "")
                    tot4.Visible = false;
                if (tot5.Text == "")
                    tot5.Visible = false;
            }
            catch { }


            Label wtotal = (Label)FormView1.FindControl("vwscoreLabel");
            Label wtot1 = (Label)FormView1.FindControl("wtot1Label");
            Label wtot2 = (Label)FormView1.FindControl("wtot2Label");
            Label wtot3 = (Label)FormView1.FindControl("wtot3Label");
            try
            {
                string strwtotal = wtotal.Text.Trim();
                string[] septot = new string[] { " " };
                string[] strSplitot = strwtotal.Split(septot, StringSplitOptions.RemoveEmptyEntries);
                wtot1.Text = strSplitot[0];
                wtot2.Text = strSplitot[1];
                wtot3.Text = strSplitot[2];
            }
            catch { }

            try
            {
                if (wtot1.Text == "")
                    wtot1.Visible = false;
                if (wtot2.Text == "")
                    wtot2.Visible = false;
                if (wtot3.Text == "")
                    wtot3.Visible = false;
            }
            catch { }

            TextBox wscore = (TextBox)FormView1.FindControl("vwcum_scoreTextBox");
            TextBox wsc1 = (TextBox)FormView1.FindControl("TextBox1");
            TextBox wsc2 = (TextBox)FormView1.FindControl("TextBox2");
            TextBox wsc3 = (TextBox)FormView1.FindControl("TextBox3");
            try
            {
                string strwscore = wscore.Text.Trim();
                string[] sepscor = new string[] { " " };
                string[] strSplitscore = strwscore.Split(sepscor, StringSplitOptions.RemoveEmptyEntries);
                wsc1.Text = strSplitscore[0];
                wsc2.Text = strSplitscore[1];
                wsc3.Text = strSplitscore[2];
            }
            catch { }

            try
            {
                if (wsc1.Text == "")
                    wsc1.Visible = false;
                if (wsc2.Text == "")
                    wsc2.Visible = false;
                if (wsc3.Text == "")
                    wsc3.Visible = false;              
            }
            catch { }

            TextBox score = (TextBox)FormView1.FindControl("vlscoreTextBox");
            TextBox sc1 = (TextBox)FormView1.FindControl("TextBoxvlscore");
            TextBox sc2 = (TextBox)FormView1.FindControl("TextBoxvlscore2");
            TextBox sc3 = (TextBox)FormView1.FindControl("TextBoxvlscore3");
            TextBox sc4 = (TextBox)FormView1.FindControl("TextBoxvlscore4");
            TextBox sc5 = (TextBox)FormView1.FindControl("TextBoxvlscore5");
            try
            {
                string strscore1 = score.Text.Trim();                

                string[] sepscor2 = new string[] { " " };
                string[] strSplit3 = strscore1.Split(sepscor2, StringSplitOptions.RemoveEmptyEntries);
                sc1.Text = strSplit3[0];
                sc2.Text = strSplit3[1];
                sc3.Text = strSplit3[2];
                sc4.Text = strSplit3[3] ;
                sc5.Text = strSplit3[4];
            }
            catch { }

            try
            {
                if (sc1.Text == "")
                    sc1.Visible = false;
                if (sc2.Text == "")
                    sc2.Visible = false;
                if (sc3.Text == "")
                    sc3.Visible = false;
                if (sc4.Text == "")
                    sc4.Visible = false;
                if (sc5.Text == "")
                    sc5.Visible = false;
            }
            catch { }


        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            TextBox score = (TextBox)FormView1.FindControl("vlscoreLabel");
            TextBox total = (TextBox)FormView1.FindControl("vlcum_scoreLabel");
            Label sc1label = (Label)FormView1.FindControl("sc1Label");
            Label sc2label = (Label)FormView1.FindControl("sc2Label");
            Label sc3label = (Label)FormView1.FindControl("sc3Label");
            Label sc4label = (Label)FormView1.FindControl("sc4Label");
            Label sc5label = (Label)FormView1.FindControl("sc5Label");
            Label tot1 = (Label)FormView1.FindControl("Label1");
            Label tot2 = (Label)FormView1.FindControl("Label2");
            Label tot3 = (Label)FormView1.FindControl("Label3");
            Label tot4 = (Label)FormView1.FindControl("Label4");
            Label tot5 = (Label)FormView1.FindControl("Label5");
            try
            {
                string score2 = score.Text;            

                string strData = score2.Trim();
                string[] separator = new string[] { " " };
                string[] strSplitArr = strData.Split(separator, StringSplitOptions.RemoveEmptyEntries);
                strSplitArr = strData.Split(separator, StringSplitOptions.RemoveEmptyEntries);
                sc1label.Text = strSplitArr[0];
                sc2label.Text = strSplitArr[1];
                sc3label.Text = strSplitArr[2];
                sc4label.Text = strSplitArr[3];
                sc5label.Text = strSplitArr[4];                
            }
            catch { }

            try
            {
                if (sc1label.Text == "")
                    sc1label.Visible = false;
                if (sc2label.Text == "")
                    sc2label.Visible = false;
                if (sc3label.Text == "")
                    sc3label.Visible = false;
                if (sc4label.Text == "")
                    sc4label.Visible = false;
                if (sc5label.Text == "")
                    sc5label.Visible = false; 
            }
            catch { }

            try
            {
                string strData2 = total.Text.Trim();
                string[] separator2 = new string[] { " " };
                string[] strSplitArr2 = strData2.Split(separator2, StringSplitOptions.RemoveEmptyEntries);
                tot1.Text = strSplitArr2[0];
                tot2.Text = strSplitArr2[1];
                tot3.Text = strSplitArr2[2];
                tot4.Text = strSplitArr2[3];
                tot5.Text = strSplitArr2[4];
            }
            catch { }

            try
            {
                if (tot1.Text == "")
                    tot1.Visible = false;
                if (tot2.Text == "")
                    tot2.Visible = false;
                if (tot3.Text == "")
                    tot3.Visible = false;
                if (tot4.Text == "")
                    tot4.Visible = false;
                if (tot5.Text == "")
                    tot5.Visible = false;
            }
            catch { }

            TextBox wtotal = (TextBox)FormView1.FindControl("vwcum_scoreLabel");
            Label wtot1 = (Label)FormView1.FindControl("wtot1Label");
            Label wtot2 = (Label)FormView1.FindControl("wtot2Label");
            Label wtot3 = (Label)FormView1.FindControl("wtot3Label");
            try
            {
                string strwtotal = wtotal.Text.Trim();
                string[] septot = new string[] { " " };
                string[] strSplitot = strwtotal.Split(septot, StringSplitOptions.RemoveEmptyEntries);
                wtot1.Text = strSplitot[0];
                wtot2.Text = strSplitot[1];
                wtot3.Text = strSplitot[2];
            }
            catch { }

            try
            {
                if (wtot1.Text == "")
                    wtot1.Visible = false;
                if (wtot2.Text == "")
                    wtot2.Visible = false;
                if (wtot3.Text == "")
                    wtot3.Visible = false;              
            }
            catch { }

            TextBox wscore = (TextBox)FormView1.FindControl("vwscoreLabel");
            Label wsc1 = (Label)FormView1.FindControl("wsc1Label");
            Label wsc2 = (Label)FormView1.FindControl("wsc2Label");
            Label wsc3 = (Label)FormView1.FindControl("wsc3Label");
            try
            {
                string strwscore = wscore.Text.Trim();
                string[] sepscor = new string[] { " " };
                string[] strSplitscore = strwscore.Split(sepscor, StringSplitOptions.RemoveEmptyEntries);
                wsc1.Text = strSplitscore[0];
                wsc2.Text = strSplitscore[1];
                wsc3.Text = strSplitscore[2];
            }
            catch { }

            try
            {
                if (wsc1.Text == "")
                    wsc1.Visible = false;
                if (wsc2.Text == "")
                    wsc2.Visible = false;
                if (wsc3.Text == "")
                    wsc3.Visible = false;
            }
            catch { }

            try
            {
                /*ImageButton img_box = (ImageButton)Master.FindControl("img_BoxDesign");
                img_box.ImageUrl = "~/Images/box_design_1.jpg";*/
                Image img_mov_col = (Image)Master.FindControl("Image5");
                img_mov_col.Visible = false;
                if (Session["view_3d"] == null )
                {
                    Label img_3d_label = (Label)FormView1.FindControl("vbox_3d_imageLabel");
                    Label box_img_label = (Label)FormView1.FindControl("vbox_imageLabel");
                    box_img_label.Visible = true;
                    img_3d_label.Visible = false;

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


                    //view_img.ImageUrl = box_img_label.Text;
                    //view_img.ImageUrl = @"/images/RSCGL.jpg";
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

                    //view_img.ImageUrl = @"/images/Rsc3d.jpg";


                    Session["view_3d"] = "1";

                }
            }
            catch { }


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


            Session["view_3d"] = "1";
        }
        catch { }
        
        if (Convert.ToString(Session["corr_box_design_down_cad"]) == "3D")
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
        
        Session["corr_box_design_down_cad"] = "3D";
    }
    protected void btn_Update_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox dscr = (TextBox)FormView1.FindControl("vdesign_dscrTextBox");
        TextBox lscore = (TextBox)FormView1.FindControl("vlscoreTextBox");
        TextBox boxpath = (TextBox)FormView1.FindControl("vbox_imageTextBox");
        TextBox d3path = (TextBox)FormView1.FindControl("vbox_3d_imageTextBox");
        //TextBox wcumscore = (TextBox)FormView1.FindControl("vwcum_scoreTextBox");

        TextBox wscore1 = (TextBox)FormView1.FindControl("TextBox1");
        TextBox wscore2 = (TextBox)FormView1.FindControl("TextBox2");
        TextBox wscore3 = (TextBox)FormView1.FindControl("TextBox3");
        string wcumscore =  " " + wscore1.Text.Trim() + "    " + wscore2.Text.Trim() + "    " + wscore3.Text.Trim() + " ";

        TextBox score1 = (TextBox)FormView1.FindControl("TextBoxvlscore");
        TextBox score2 = (TextBox)FormView1.FindControl("TextBoxvlscore2");
        TextBox score3 = (TextBox)FormView1.FindControl("TextBoxvlscore3");
        TextBox score4 = (TextBox)FormView1.FindControl("TextBoxvlscore4");
        TextBox score5 = (TextBox)FormView1.FindControl("TextBoxvlscore5");
        FileUpload FileUpload1 = (FileUpload)FormView1.FindControl("FileUpload1");
        string score = "" + score1.Text.Trim() + "    " + score2.Text.Trim() + "             " + score3.Text.Trim() + "            " + score4.Text.Trim() + "            " + score5.Text.Trim() + "   ";


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = COMPANY;
        ObjectDataSource1.SelectParameters["prmDesignDscr"].DefaultValue = dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFormNo"].DefaultValue = Convert.ToString(Session["order_corrugated_formno"]);
        ObjectDataSource1.SelectParameters["prmEstNo"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
        ObjectDataSource1.SelectParameters["prmBoxImage"].DefaultValue = boxpath.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBox3DImage"].DefaultValue = d3path.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLScore"].DefaultValue = score ;
        ObjectDataSource1.SelectParameters["prmWCumScore"].DefaultValue = wcumscore;
        if (FileUpload1.FileName != "")
        {
            try
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
            catch { }
        }



        
        Response.Write("<script>window.location.href='corr_box_design.aspx'</script>");
       
    }
    protected void btn_UpdateCancel_Click(object sender, EventArgs e)
    {
       
        Response.Write("<script>window.location.href='corr_box_design.aspx'</script>");
    }
    protected void on_click_image(object sender, EventArgs e)
    {
        //Image view_img = (Image)FormView1.FindControl("Image1");
        //Label att = (Label)FormView1.FindControl("vbox_imageLabel");
        //Image img2 = (Image)FormView1.FindControl("Image");
        //string str1 = att.Text;
        //string str2 = Path.GetFileName(str1);
        //string path = str2;
        
        //if (path != "")
        //{
        //    if (!Request.Browser.Browser.Contains("Safari"))
        //    {

        //        string fileName = str2;
        //        System.IO.FileStream fs = null;
        //        fs = System.IO.File.Open(str1, System.IO.FileMode.Open); ;
        //        byte[] btFile = new byte[fs.Length];
        //        fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
        //        fs.Close();
        //        Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
        //        Response.ContentType = "application/octet-stream";
        //        Response.BinaryWrite(btFile);
        //        Response.End();
        //        fs = null;  
        //    }
        //}
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
        string dieimage = "";
        Session["view_3d"] = null;
        try
        {
            /*ImageButton img_box = (ImageButton)Master.FindControl("img_BoxDesign");
            img_box.ImageUrl = "~/Images/box_design_1.jpg";*/
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;

            Label box_img_label = (Label)FormView1.FindControl("vbox_imageLabel");
            box_img_label.Visible = true;

            if (Session["view_3d"] == null )
            {
                Label img_3d_label = (Label)FormView1.FindControl("vbox_3d_imageLabel");                
                img_3d_label.Visible = false;
                Image view_img = (Image)FormView1.FindControl("Image1");

                string imgpath = box_img_label.Text;
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
                    string dirPath = Server.MapPath(@"Images\Rcode\2D");
                    string destimgpath = Server.MapPath(@"Images\Rcode\2D" + "\\" + imgfilename);

                    string[] files = Directory.GetFiles(dirPath);
                    foreach (string file in files)
                        File.Delete(file);

                    File.Copy(imgpath, destimgpath);
                    view_img.ImageUrl = "~/Images/Rcode/2D/" + imgfilename;
                }


            }
        }
        catch { }
               
        if(Convert.ToString(Session["corr_box_design_down_cad"]) == "cad")
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
       
        Session["corr_box_design_down_cad"] = "cad";
    }

    
}

